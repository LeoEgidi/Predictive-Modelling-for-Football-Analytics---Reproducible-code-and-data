compare_foot <- function(source, test_data, metric = c("accuracy", "brier", "ACP", "pseudoR2", "RPS")) {
  
  # Validate metric
  allowed_metrics <- c("accuracy", "brier", "ACP", "pseudoR2", "RPS")
  metric <- match.arg(metric, choices = allowed_metrics, several.ok = TRUE)
  
  # Validate data
  required_cols <- c("home_team", "away_team", "homegoals", "awaygoals")
  missing_cols <- setdiff(required_cols, names(test_data))
  if (length(missing_cols) > 0) {
    stop(paste("test_data is missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  test_data$outcome <- ifelse(test_data$homegoals > test_data$awaygoals, 1,
                              ifelse(test_data$homegoals == test_data$awaygoals, 2, 3))
  test_data$outcome <- factor(test_data$outcome, levels = 1:3, labels = c("Home Win", "Draw", "Away Win"))
  
  N_prev <- nrow(test_data)
  
  # Function to compute RPS
  compute_RPS <- function(cum_pred, actual) {
    # cum_pred: matrix of cumulative predicted probabilities
    # actual: vector of actual outcomes as factors
    
    # Cumulative observed probabilities
    acum <- matrix(0, nrow = length(actual), ncol = 3)
    acum[actual == "Home Win", ] <- matrix(rep(c(1, 1, 1), sum(actual == "Home Win")), ncol = 3, byrow = TRUE)
    acum[actual == "Draw", ]      <- matrix(rep(c(0, 1, 1), sum(actual == "Draw")), ncol = 3, byrow = TRUE)
    acum[actual == "Away Win", ] <- matrix(rep(c(0, 0, 1), sum(actual == "Away Win")), ncol = 3, byrow = TRUE)
    
    # Squared differences for m=1 and m=2
    squared_diff <- (cum_pred[, 1:2] - acum[, 1:2])^2
    
    # RPS per observation
    rps_per_obs <- rowSums(squared_diff) / 2  # For 3 categories: n_cat - 1
    
    # Average RPS over all observations
    mean(rps_per_obs)
  }
  
  results <- list()
  
  for (item_name in names(source)) {
    item <- source[[item_name]]
    
    model_results <- list()
    
    if (inherits(item, c("stanFoot", "stanfit"))) {
      
      # Check that model is of class stanFoot or stanfit
      if (!inherits(item, c("stanFoot", "stanfit"))) {
        warning(paste("Source item", item_name, "is not of class 'stanFoot' or 'stanfit'. Skipping."))
        next
      }
      
      if (inherits(item, "stanFoot")) {
        sims <- rstan::extract(item$fit)
      } else if (inherits(item, "stanfit")) {
        sims <- rstan::extract(item)
      }
      
      # Check 'y_prev' is in the samples
      if (!"y_prev" %in% names(sims)) {
        warning(paste("Model", item_name, "does not contain 'y_prev' in its samples. Ensure the model was fitted with 'predict' set appropriately. Skipping."))
        next
      }
      
      S <- dim(sims$y_prev)[1]
      N_model <- dim(sims$y_prev)[2]
      
      if (N_model != N_prev) {
        warning(paste("Number of matches in 'y_prev' for model", item_name, "does not match the number of matches in 'test_data'. Skipping."))
        next
      }
      
      # Predicted probabilities matrix
      prob_q_model <- matrix(NA, N_prev, 3)
      
      for (n in 1:N_prev) {
        prob_q_model[n, 1] <- sum(sims$y_prev[, n, 1] > sims$y_prev[, n, 2]) / S
        prob_q_model[n, 2] <- sum(sims$y_prev[, n, 1] == sims$y_prev[, n, 2]) / S
        prob_q_model[n, 3] <- sum(sims$y_prev[, n, 1] < sims$y_prev[, n, 2]) / S
      }
      
      # Ensure probabilities sum to 1
      prob_q_model <- prob_q_model / rowSums(prob_q_model)
      
    } else if (is.matrix(item)) {
      # Probability matrices
      prob_q_model <- item
      
      # Validate matrix dimensions
      if (nrow(prob_q_model) != N_prev) {
        warning(paste("Probability matrix", item_name, "does not have the correct number of rows. Expected", N_prev, "but got", nrow(prob_q_model), ". Skipping."))
        next
      }
      
      if (ncol(prob_q_model) != 3) {
        warning(paste("Probability matrix", item_name, "does not have exactly 3 columns (Home Win, Draw, Away Win). Skipping."))
        next
      }
      
      # Check for NAs
      na_rows <- apply(prob_q_model, 1, function(x) any(is.na(x)))
      if (any(na_rows)) {
        num_na <- sum(na_rows)
        warning(paste("Probability matrix", item_name, "contains", num_na, "rows with NAs. These rows will be removed from evaluation."))
        prob_q_model <- prob_q_model[!na_rows, , drop = FALSE]
        test_data_subset <- test_data[!na_rows, , drop = FALSE]
        current_N_prev <- nrow(prob_q_model)
        
        if (current_N_prev == 0) {
          warning(paste("After removing NA rows, no data remains for matrix", item_name, ". Skipping."))
          next
        }
        
        # Update outcome for remaining data
        actual_outcomes <- test_data_subset$outcome
      } else {
        actual_outcomes <- test_data$outcome
      }
      
      # Ensure probabilities sum to 1
      row_sums <- rowSums(prob_q_model)
      if (any(abs(row_sums - 1) > 1e-6)) {
        warning(paste("Probabilities in matrix", item_name, "do not sum to 1. Normalizing the probabilities."))
        prob_q_model <- prob_q_model / row_sums
      }
      
    } else {
      warning(paste("Source item", item_name, "is neither a model object nor a probability matrix. Skipping."))
      next
    }
    
    # If item is a matrix and rows were removed, use 'test_data_subset'
    if (is.matrix(item) && exists("test_data_subset")) {
      outcomes <- actual_outcomes
    } else {
      outcomes <- test_data$outcome
    }
    
    # Compute cumulative predicted probabilities for RPS
    cum_pred <- t(apply(prob_q_model, 1, cumsum))  # N_prev x 3
    
    # Compute Metrics
    if ("RPS" %in% metric) {
      RPS <- compute_RPS(cum_pred, outcomes)
      model_results$RPS <- round(RPS, 4)
    }
    
    if ("accuracy" %in% metric) {
      predicted_outcomes <- apply(prob_q_model, 1, which.max)
      accuracy <- mean(predicted_outcomes == as.numeric(outcomes))
      model_results$accuracy <- round(accuracy, 4)
    }
    
    if ("brier" %in% metric) {
      brier_res <- matrix(0, nrow = nrow(prob_q_model), ncol = 3)
      for (n in 1:nrow(prob_q_model)) {
        brier_res[n, as.numeric(outcomes[n])] <- 1
      }
      brier <- mean(rowSums((brier_res - prob_q_model)^2))
      model_results$brier <- round(brier, 4)
    }
    
    if ("pseudoR2" %in% metric) {
      log_prob_sum <- 0
      for (n in 1:nrow(prob_q_model)) {
        prob_n <- prob_q_model[n, as.numeric(outcomes[n])]
        prob_n <- max(prob_n, .Machine$double.eps)  # Avoid log(0)
        log_prob_sum <- log_prob_sum + log(prob_n)
      }
      pseudoR2 <- exp(log_prob_sum / nrow(prob_q_model))
      model_results$pseudoR2 <- round(pseudoR2, 4)
    }
    
    if ("ACP" %in% metric) {
      true_probs <- numeric(nrow(prob_q_model))
      for (n in 1:nrow(prob_q_model)) {
        true_class <- as.numeric(outcomes[n])
        true_probs[n] <- prob_q_model[n, true_class]
      }
      ACP <- mean(true_probs)
      model_results$ACP <- round(ACP, 4)
    }
    
    results[[item_name]] <- model_results
  }
  
  if (length(results) == 0) {
    stop("No valid models or probability matrices were provided in 'source'.")
  }
  
  # Convert results to data frame
  results_df <- do.call(rbind, lapply(names(results), function(item_name) {
    cbind(Model = item_name, as.data.frame(results[[item_name]]))
  }))
  
  rownames(results_df) <- NULL
  
  return(results_df)
}
