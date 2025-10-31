library(footBayes)
library(dplyr)
library(xtable)

italy_2009 <- read.csv("I1.csv", sep=",")
head(italy_2009)
italy_2009_ristr <- italy_2009[, c(2,3,4,5,6)]
ranking_2009 <-  as.data.frame(cbind(unique(italy_2009$HomeTeam), 
                                     c(17, 14, 15, 5, 1,
                                       2, 10, 20, 8, 7,
                                       18, 3, 11, 9, 16,
                                       4, 12, 19, 6, 13 )))
season <- rep(1, nrow(italy_2009_ristr))
italy_2009_ristr[,1] <- season
colnames(italy_2009_ristr) <- c("periods", "home_team", "away_team",
                                "home_goals", "away_goals")

# bet 365
B365 <- cbind(italy_2009$B365H, italy_2009$B365D, italy_2009$B365A)
mat_prob_B365 <- apply(B365, 1, function(row) {
  probs <- 1 / row    # Calcola l'inverso delle quote
  probs / sum(probs)  # Normalizza la probabilità in modo che la somma sia 1
})

mat_prob_B365 <- t(mat_prob_B365)  # Trasponi per ottenere 380x3

# bwin
BW <- cbind(italy_2009$BWH, italy_2009$BWD, italy_2009$BWA)
mat_prob_BW <- apply(BW, 1, function(row) {
  probs <- 1 / row    
  probs / sum(probs)  
})

mat_prob_BW <- t(mat_prob_BW)  

# Gamebookers
GB <- cbind(italy_2009$GBH, italy_2009$GBD, italy_2009$GBA)
mat_prob_GB <- apply(GB, 1, function(row) {
  probs <- 1 / row    
  probs / sum(probs)  
})

mat_prob_GB <- t(mat_prob_GB)  

# Interwetten
IW <- cbind(italy_2009$IWH, italy_2009$IWD, italy_2009$IWA)
mat_prob_IW <- apply(IW, 1, function(row) {
  probs <- 1 / row    
  probs / sum(probs)  
})

mat_prob_IW <- t(mat_prob_IW)  

# Ladbrokes
LB <- cbind(italy_2009$LBH, italy_2009$LBD, italy_2009$LBA)
mat_prob_LB <- apply(LB, 1, function(row) {
  probs <- 1 / row    
  probs / sum(probs)  
})

mat_prob_LB <- t(mat_prob_LB)  

# Sportingbet
SB <- cbind(italy_2009$SBH, italy_2009$SBD, italy_2009$SBA)
mat_prob_SB <- apply(SB, 1, function(row) {
  probs <- 1 / row    
  probs / sum(probs)  
})

mat_prob_SB <- t(mat_prob_SB)  


# William Hill
WH <- cbind(italy_2009$WHH, italy_2009$WHD, italy_2009$WHA)
mat_prob_WH <- apply(WH, 1, function(row) {
  probs <- 1 / row    
  probs / sum(probs)  
})

mat_prob_WH <- t(mat_prob_WH)  

# Stan James
SJ <- cbind(italy_2009$SJH, italy_2009$SJD, italy_2009$SJA)
mat_prob_SJ <- apply(SJ, 1, function(row) {
  probs <- 1 / row    
  probs / sum(probs)  
})

mat_prob_SJ <- t(mat_prob_SJ)  

# VC Bet
VC <- cbind(italy_2009$VCH, italy_2009$VCD, italy_2009$VCA)
mat_prob_VC <- apply(VC, 1, function(row) {
  probs <- 1 / row    
  probs / sum(probs)  
})

mat_prob_VC <- t(mat_prob_VC)  

# Blue Square
BS <- cbind(italy_2009$BSH, italy_2009$BSD, italy_2009$BSA)
mat_prob_BS <- apply(BS, 1, function(row) {
  probs <- 1 / row    
  probs / sum(probs)  
})

mat_prob_BS <- t(mat_prob_BS)  



sim <- function(n_train, n_test){
  dp_stan <- stan_foot(data = italy_2009_ristr,
                          model="double_pois",
                          predict = n_test) # dp
  bp_stan <- stan_foot(data = italy_2009_ristr,
                     model="biv_pois",
                     predict = n_test) # bp
  bp_stan_infl <- stan_foot(data = italy_2009_ristr,
                     model="diag_infl_biv_pois",
                     predict = n_test) # inflated bp



  dp_simple_probs <- foot_prob(object = dp_stan, data = italy_2009_ristr)
  bp_probs <- foot_prob(object = bp_stan, data = italy_2009_ristr)
  bp_probs_infl <- foot_prob(object = bp_stan_infl, data = italy_2009_ristr)
  test_data <- italy_2009_ristr[(n_train+1):(n_train+n_test), ]
  


compare_foot(list(dp = dp_stan, bp = bp_stan,
                  bp_infl = bp_stan_infl), test_data = test_data )


## Betting strategy
  obs_result <- c()
  obs_result[test_data$homegoals>test_data$awaygoals] <- 1
  obs_result[test_data$homegoals==test_data$awaygoals] <- 2
  obs_result[test_data$homegoals<test_data$awaygoals] <- 3


  strategy <- function(bookies = c("B365", "BS", "BW", "GB", "IW", "LB", "SJ", "VC", "WH", "SB"),
                       obs_results,
                       delta_t,
                       kelly_t,
                       single_bet,
                       criterion = c("Dixon", "Kelly", "HER"),
                       prob_model  ){

    select_bet <- function(matrice, indice) {
      bet <- sapply(1:nrow(matrice), function(i) matrice[i, indice[i]])
      return(bet)
    }
  if (criterion =="Dixon"){
    choose_bookies<-function(string){
    dixon_ratio <- as.matrix(prob_model$prob_table[,3:5])/get(paste("mat_prob_", string, sep=""))[(n_train+1):(n_test+n_train), ]
    index_bets <- as.numeric(as.vector(apply(as.matrix(dixon_ratio),1, function(x) which.max(x)[max(x)>delta_t] )))

    
    odds <- select_bet(get(string)[(n_train+1):(n_test+n_train), ],index_bets)
    return <- -sum(rep(single_bet, sum(!is.na(odds))))  + sum(single_bet*odds[!is.na(odds)]*(index_bets==obs_result)[!is.na(odds)])
    return(round(100*(return/sum(rep(single_bet, sum(!is.na(odds))))),2) )
    }
  
    return(sapply(bookies, choose_bookies))
  }else if(criterion == "Kelly"){
    choose_bookies_kelly <- function(string){
    b <- (get(string)[(n_train+1):(n_test+n_train), ]-1) 
    wager <- prob_model$prob_table[,3:5] - (1-prob_model$prob_table[,3:5])/b
    reverse_odds <- (1-prob_model$prob_table[,3:5])/prob_model$prob_table[,3:5]
    
    cond1 <- as.numeric(as.vector(as.matrix(b/reverse_odds))) > kelly_t
    cond2 <- as.numeric(as.vector(as.matrix(wager)))>0
    total_wager <- sum(as.numeric(as.vector(as.matrix(wager)))[cond1], na.rm = TRUE)
    wager_norm <- wager/total_wager
    return <- -sum(wager_norm[wager_norm >0], na.rm = TRUE) +  sum( select_bet(wager_norm*get(string)[(n_train+1):(n_test+n_train), ]*(cond1),obs_result), na.rm = TRUE)
    return(round(100*return,2))
        }
  return(sapply(bookies, choose_bookies_kelly))
  }else if(criterion == "HER"){
    choose_bookies_her<-function(string){
      ratio <- as.matrix(prob_model$prob_table[,3:5])*get(string)[(n_train+1):(n_test+n_train), ]
      index_bets <- as.numeric(as.vector(apply(as.matrix(ratio),1, function(x) which.max(x)[max(x)>delta_t] )))
      
      
      odds <- select_bet(get(string)[(n_train+1):(n_test+n_train), ],index_bets)
      return <- -sum(rep(single_bet, sum(!is.na(odds))))  + sum(single_bet*odds[!is.na(odds)]*(index_bets==obs_result)[!is.na(odds)])
      return(round(100*(return/sum(rep(single_bet, sum(!is.na(odds))))),2) )
    }
    
    return(sapply(bookies, choose_bookies_her))
    }
  }  


# Dixon
# delta plot
  delta_t_vals <- seq(0.8,2, by=0.01)
  strategy_vals <- sapply(delta_t_vals, function(delta_t) strategy(bookies = c("B365", "BS", "BW", "GB", "IW", "LB", "SB", "SJ", "VC", "WH"),
                                                           obs_results = obs_result,
                                                           delta_t,
                                                           single_bet = 1,
                                                           criterion = "Dixon",
                                                           prob_model = dp_simple_probs)) 
  colors <- c("#E41A1C", # Rosso
              "#377EB8", # Blu
              "#4DAF4A", # Verde
              "#984EA3", # Viola
              "#FF7F00", # Arancione
              "#FFFF33", # Giallo
              "#A65628", # Marrone
              "#F781BF", # Rosa
              "#999999", # Grigio
              "#66C2A5") # Verde acqua
  
 
  #pdf(file = paste("strategy_ntrain_",n_train, ".pdf",sep=""), height = 9, width = 5)
  #par(mfcol = c(3, 1), mar = c(2, 2, 2, 2), oma = c(4, 4, 4, 4), las = 1)
  plot(delta_t_vals, strategy_vals[1,], type ="l",  ylim=c(-100,30),
     xlab = expression(delta), main = paste("Dixon & Coles strategy (n_train = ", n_train, ")", sep=""),
     cex.main =1.2, cex.lab =1.2, col= colors[1],
     ylab = "Profit (%)")
  abline(h=0, col = "gray", lwd =2, lty =2)
  for (j in 2:10)
    lines(delta_t_vals, strategy_vals[j,], col= colors[j])
    legend(0.8, -60, col =colors, lty = 1, c("Bet365", "Blue Square", "Bet&Win", "Gamebookers", "Interwetten",
                                          "Ladbrokes","Sportingbet", "Stan James", "VC Bet", "William Hill" ), ncol = 2, cex =0.5)
  #dev.off()

  
  # Highest expected return (HER)
  strategy_vals <- sapply(delta_t_vals, function(delta_t) strategy(bookies = c("B365", "BS", "BW", "GB", "IW", "LB", "SB", "SJ", "VC", "WH"),
                                                                   obs_results = obs_result,
                                                                   delta_t,
                                                                   single_bet = 1,
                                                                   criterion = "HER",
                                                                   prob_model = dp_simple_probs)) 
  
  #pdf(file = paste("her_strategy_ntrain_",n_train, ".pdf",sep=""), height = 6, width = 8)
  plot(delta_t_vals, strategy_vals[1,], type ="l",  ylim=c(-100,30),
       xlab = expression(delta), main = paste("HER strategy (n_train = ", n_train, ")", sep=""),
       cex.main =1.2, cex.lab =1.2, col= colors[1],
       ylab = "Profit (%)")
  abline(h=0, col = "gray", lwd =2, lty =2)
  for (j in 2:10)
    lines(delta_t_vals, strategy_vals[j,], col= colors[j])
  legend(0.8, -60, col =colors, lty = 1, c("Bet365", "Blue Square", "Bet&Win", "Gamebookers", "Interwetten",
                                           "Ladbrokes","Sportingbet", "Stan James", "VC Bet", "William Hill" ), ncol = 2, cex =0.5)
  
  
  # Kelly
  kelly_t_vals <- seq(1,2, by=0.01)
  strategy_vals_k <- sapply(kelly_t_vals, function(kelly_t) strategy(bookies = c("B365", "BS", "BW", "GB", "IW", "LB", "SB", "SJ", "VC", "WH"),
                                                                     obs_results = obs_result,
                                                                     delta_t = 1,
                                                                     kelly_t,
                                                                     single_bet = 1,
                                                                     criterion = "Kelly",
                                                                     prob_model = dp_simple_probs)) 
  
  #pdf(file = paste("kelly_strategy_ntrain_",n_train, ".pdf",sep=""), height = 6, width = 8)
  plot(kelly_t_vals, strategy_vals_k[1,], type ="l",  ylim=c(-200,30),
       xlab = expression(gamma), main = paste("Kelly's strategy (n_train = ", n_train, ")", sep=""),
       cex.main =1.2, cex.lab =1.2, col = colors[1],
       ylab = "Profit (%)")
  abline(h=0, col = "gray", lwd =2, lty =2)
  for (j in 2:10)
    lines(kelly_t_vals, strategy_vals_k[j,], col= colors[j])
  legend(1, -120, col = colors, lty = 1, c("Bet365", "Blue Square", "Bet&Win", "Gamebookers", "Interwetten",
                                           "Ladbrokes","Sportingbet", "Stan James", "VC Bet", "William Hill" ), ncol =2, cex =0.5)
  #dev.off()
  
  
 
 
  
  
  tab <- compare_foot(list(Bet365 = mat_prob_B365[(n_train+1):(n_train+n_test), ],
                    BET_WIN = mat_prob_BW[(n_train+1):(n_train+n_test), ],
                    Blue_Square = mat_prob_BS[(n_train+1):(n_train+n_test), ],
                    Game_Bookers = mat_prob_GB[(n_train+1):(n_train+n_test), ],
                    Interwetten = mat_prob_IW[(n_train+1):(n_train+n_test), ],
                    Ladbrokes = mat_prob_LB[(n_train+1):(n_train+n_test), ],
                    Sportingbet = mat_prob_SB[(n_train+1):(n_train+n_test), ],
                    Stan_James = mat_prob_SJ[(n_train+1):(n_train+n_test), ],
                    VC_Bet = mat_prob_VC[(n_train+1):(n_train+n_test), ],
                    William_Hill = mat_prob_WH[(n_train+1):(n_train+n_test), ],
                    double_pois = as.matrix(dp_simple_probs$prob_table[,3:5]),
                    biv_pois = as.matrix(bp_probs$prob_table[,3:5]),
                    biv_pois_infl = as.matrix(bp_probs_infl$prob_table[,3:5])), test_data = test_data)
  tab[,2:5] <- round(tab[,2:5], 3)
  return(tab)
}

par(mfcol = c(3, 2), mar = c(2, 2, 2, 2), oma = c(4, 4, 4, 4), las = 1)
sim1 <- sim(n_train = 190, n_test = 190)
sim2 <- sim(n_train = 340, n_test = 40)

mtext("Margin tolerance", side = 1, outer = TRUE, line = 2)
mtext("Net profit (%)", side = 2, outer = TRUE, line = 2, las = 0)

xtable(sim1)
xtable(sim2)
