### Installation and packages
# install.packages("footBayes")
# devtools::install_github("leoegidi/footBayes")

library(footBayes)
library(devtools)
library(dplyr)
library(bayesplot)
library(ggplot2)
library(loo)

### Data

data(italy)
italy_2009 <- subset(italy[, c(2,3,4,6,7)], Season =="2009")
colnames(italy_2009) <- c("periods", "home_team", "away_team",
                          "home_goals", "away_goals") # rename columns
head(italy_2009)


### Fit Stan models
## no dynamics, no predictions
## 4 Markov chains, 'n_iter' iterations each

n_iter <- 2000    # number of MCMC iterations

dp_stan <- stan_foot(data = italy_2009,
                     model="double_pois",
                     home_effect = TRUE,
                     chains = 4,
                     iter = n_iter)    # double poisson

biv_stan <- stan_foot(data = italy_2009,
                      model="biv_pois",
                      home_effect = TRUE,
                      chains = 4,
                      iter = n_iter) # biv poisson

print(dp_stan, pars =c("home", "sigma_att", "sigma_def"))
print(biv_stan, pars =c("home", "rho",
                         "sigma_att", "sigma_def"))

### Fit MLE models
## wald-type CI

dp_mle <- mle_foot(data = italy_2009, model="double_pois",
                    interval = "Wald") # mle biv poisson

biv_mle <- mle_foot(data = italy_2009, model="biv_pois",
                     interval = "Wald") # mle biv poisson

tab_dp <- rbind(dp_mle$home)
rownames(tab_dp) <- c("home")
tab_dp

tab_biv <- rbind(biv_mle$home, round(log(biv_mle$corr),2))
rownames(tab_biv) <- c("home", "rho")
tab_biv

## Marginal posterior with bayesplot

color_scheme_set("gray")
posterior1 <- as.matrix(biv_stan$fit)
mcmc_areas(posterior1, regex_pars=c("home", "rho",
                                      "sigma_att", "sigma_def"))+
xaxis_text(on =TRUE, size=ggplot2::rel(2.9))+
  yaxis_text(on =TRUE, size=ggplot2::rel(2.9))+
  scale_y_discrete(labels = c("home", expression(rho), expression(sigma[att]), expression(sigma[def]))
                     #((parse(text= model_names)))
                   )+
  theme(plot.title = element_text(hjust = 0.5, size =rel(2.6)))

ggsave("parameters_biv_stan.pdf", width = 8, height = 7)


## Football static abilities

pdf("att_def.pdf", width = 6, height = 5)
foot_abilities(biv_stan, italy_2009)
dev.off()

pdf("att_def_mle.pdf", width = 6, height = 5)
foot_abilities(biv_mle, italy_2009)
dev.off()

### Fit Stan models
## weekly dynamics, no predictions
## 4 Markov chains, 'n_iter' iterations each

dp_stan_dyn <- stan_foot(data = italy_2009,
                          model="double_pois",
                          home_effect = TRUE,
                          dynamic_type ="weekly",
                          cores = 4,
                          iter = n_iter) # dynamic dp poisson

biv_stan_dyn <- stan_foot(data = italy_2009,
                         model="biv_pois",
                         home_effect = TRUE,
                         dynamic_type ="weekly",
                         cores = 4,
                         iter = n_iter) # dynamic biv poisson

print(dp_stan_dyn, pars =c("home", "sigma_att", "sigma_def"))
print(biv_stan_dyn, pars =c("home", "rho", "sigma_att", "sigma_def"))


## Football dynamic abilities
## Plotting abilities: credible 50% intervals

foot_abilities(dp_stan_dyn, italy_2009)
foot_abilities(biv_stan_dyn, italy_2009)
ggsave("att_def_dynamic2.pdf", width =7, height = 9)
ggsave("att_def_dynamic2bis.pdf", width =7, height = 12)


## Changing prior distributions

biv_stan_t <- stan_foot(data = italy_2009,
                           model="biv_pois",
                           home_effect = TRUE,
                           chains = 4,
                           prior_par = list(ability = student_t(4,0,NULL),
                                            ability_sd = laplace(0,1)),
                           iter = n_iter) # biv poisson

## graphical posteriors comparison

posterior1_t <- as.matrix(biv_stan_t$fit)
model_names <- c("Gauss. + Cauchy", "Stud+Laplace")
color_scheme_set(scheme = "gray")
gl_posterior <- cbind(posterior1[,"sigma_att"],
                        posterior1_t[,"sigma_att"])
colnames(gl_posterior)<-c("sigma_att", "sigma_att_t")
mcmc_areas(gl_posterior, pars=c("sigma_att", "sigma_att_t"))+
  xaxis_text(on =TRUE, size=ggplot2::rel(2.5))+
  yaxis_text(on =TRUE, size=ggplot2::rel(1.9))+
  scale_y_discrete(labels = ((parse(text= model_names))))+
  ggtitle(expression(sigma[att]))+
  theme(plot.title = element_text(hjust = 0.5, size =rel(2.6)))
  #theme_gray()
ggsave("changing_priors.pdf", width =8, height = 5)


## Predict the last 2 match-days
dp_stan_pred <- stan_foot(data = italy_2009,
                           model="double_pois",
                           home_effect = TRUE,
                           predict = 20,
                           iter = n_iter)  # double poisson

biv_stan_pred <- stan_foot(data = italy_2009,
                           model="biv_pois",
                           home_effect = TRUE,
                           predict = 20,
                           iter = n_iter)  # biv poisson
foot_prob(object = biv_stan_pred, data = italy_2009)
ggsave("pred_2days.pdf", width = 12, height = 10)


## Rank reconstruction
# aggregated plot

foot_rank(data = italy_2009, object = dp_stan)
foot_rank(data = italy_2009, object = biv_stan)
ggsave(file = "in_sample_rank_aggr.pdf", width = 8, height =6)

# team-specific plot

foot_rank(data = italy_2009, object = dp_stan, visualize = "individual")
foot_rank(data = italy_2009, object = biv_stan, visualize = "individual")
ggsave(file = "in_sample_rank_ind.pdf", width = 10, height =8)


## Rank predictions

# aggregated plot

foot_rank(data = italy_2009, object = dp_stan_pred)
foot_rank(data = italy_2009, object = biv_stan_pred)
ggsave(file = "out_sample_rank_aggr.pdf", width = 8, height =6)

# team-specific plot

foot_rank(data = italy_2009, object = dp_stan_pred,  visualize = "individual",
          teams = c( "Inter", "AS Roma","AC Milan", "Sampdoria"))
foot_rank(data = italy_2009, object = biv_stan_pred,  visualize = "individual",
            teams = c( "Inter", "AS Roma","AC Milan", "Sampdoria"))
ggsave(file = "out_sample_rank_ind.pdf", width = 8, height =7)


## PP checks: aggregated goal's differences and ordered goal differences

pp_foot(data = italy_2009, object = dp_stan,
        type = "aggregated")
pp_foot(data = italy_2009, object = biv_stan,
          type = "aggregated")
ggsave("model_checking1.pdf", width =8, height =5)

pp_foot(data = italy_2009, object = dp_stan,
        type = "matches")
pp_foot(data = italy_2009, object = biv_stan,
        type = "matches")
ggsave("model_checking2.pdf", width =8, height =5)


## PPC densities overlay with the bayesplot package

# extracting the replications

sims <-rstan::extract(biv_stan$fit)
goal_diff <- italy_2009$home_goals-italy_2009$away_goals

# plotting data density vs replications densities

ppc_dens_overlay(goal_diff, sims$y_rep[,,1]-sims$y_rep[,,2], bw = 0.5)
ggsave("ppc_dens.pdf", width =8, height =5)


### Model comparisons
## LOOIC, loo function

# extract pointwise log-likelihood

log_lik_dp <- extract_log_lik(dp_stan$fit)            # static dp
log_lik_dp_dyn <- extract_log_lik(dp_stan_dyn$fit)    # dynamic dp
log_lik_biv <- extract_log_lik(biv_stan$fit)          # static biv-pois
log_lik_biv_t <- extract_log_lik(biv_stan_t$fit)      # static biv pois (stud+laplace prior)
log_lik_biv_dyn <- extract_log_lik(biv_stan_dyn$fit)  # dynamic biv-pois

# compute loo

loo_dp <- loo(log_lik_dp)
loo_biv <- loo(log_lik_biv)
loo_biv_t <- loo(log_lik_biv_t)
loo_dp_dyn <- loo(log_lik_dp_dyn)
loo_biv_dyn <- loo(log_lik_biv_dyn)


# compare five looic

compare(loo_dp, loo_biv,
        loo_biv_t, loo_dp_dyn, loo_biv_dyn)

