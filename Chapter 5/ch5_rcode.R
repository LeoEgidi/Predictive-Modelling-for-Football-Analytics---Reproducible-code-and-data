### Installation and packages
# install.packages("footBayes")
# devtools::install_github("leoegidi/footBayes")

library(footBayes)
library(devtools)
library(dplyr)
library(bayesplot)
library(ggplot2)
library(loo)
library(regista)
library(arm)

### Data

data(italy)
italy_2009 <- subset(italy[, c(2,3,4,6,7)], Season =="2009")
colnames(italy_2009) <- c("periods", "home_team", "away_team",
                          "home_goals", "away_goals") # rename columns
head(italy_2009)


###  Other models

## Diagonal-inflated bivariate Poisson
# Static
dibiv_stan <- stan_foot(data = italy_2009,
                        model= "diag_infl_biv_pois",
                        home_effect = TRUE)
print(dibiv_stan, pars = c("home", "rho", "sigma_att", "sigma_def", "prob_of_draws"))

# Dynamic
dibiv_stan_dyn <- stan_foot(data = italy_2009,
                            model= "diag_infl_biv_pois",
                            home_effect = TRUE,
                            dynamic_type = "weekly",
                            cores = 4)
print(dibiv_stan_dyn, pars = c("home", "rho", "sigma_att", "sigma_def", "prob_of_draws"))


## Skellam
# static
skellam_stan <-  stan_foot(data = italy_2009,
                           model= "skellam",
                           home_effect = TRUE)
print(skellam_stan, pars = c("home", "sigma_att", "sigma_def"))

pdf("skellam_att_def.pdf", width = 6, height = 5)
foot_abilities(skellam_stan, italy_2009)
dev.off()

skellam_mle <-   mle_foot(data =  italy_2009,  model= "skellam")

# dynamic
skellam_stan_dyn <-  stan_foot(data = italy_2009,
                               model= "skellam",
                               home_effect = TRUE,
                               dynamic_type = "weekly",
                               cores = 4)
print(skellam_stan_dyn, pars = c("home", "sigma_att", "sigma_def"))

foot_abilities(skellam_stan_dyn, italy_2009)
ggsave("skellam_att_def_dyn.pdf", width =7, height = 9)
ggsave("skellam_att_def_dyn2.pdf", width =7, height = 12)


## Zero-inflated Skellam
# Static
zeroinfl_skellam_stan <-  stan_foot(data = italy_2009,
                                    model= "zero_infl_skellam",
                                    home_effect = TRUE)
print(zeroinfl_skellam_stan, pars = c("home", "sigma_att", "sigma_def", "prob_of_draws"))

# dynamic
zeroinfl_skellam_stan_dyn <-  stan_foot(data = italy_2009,
                                    model= "zero_infl_skellam",
                                    home_effect = TRUE,
                                    dynamic_type = "weekly",
                                    cores = 4)
print(zeroinfl_skellam_stan_dyn, pars = c("home", "sigma_att", "sigma_def", "prob_of_draws"))



## Student-t

# static
student_stan <- stan_foot(data = italy_2009,
                          model= "student_t",
                          home_effect = TRUE)
print(student_stan, pars= c("home", "beta", "sigma_a", "sigma_y"))

pdf("abilities.pdf", width = 6, height = 5)
foot_abilities(student_stan, data = italy_2009)
dev.off()

student_mle <- mle_foot(data = italy_2009,
                        model= "student_t")

# dynamic
student_stan_dyn <- stan_foot(data = italy_2009,
                              model= "student_t",
                              home_effect = TRUE,
                              dynamic_type = "weekly",
                              cores = 4)
print(student_stan_dyn, pars= c("home","beta", "sigma_a", "sigma_y"))

foot_abilities(student_stan_dyn, data = italy_2009)
ggsave("abilities_dyn.pdf", width =7, height = 9)
ggsave("abilities_dyn2.pdf", width =7, height = 12)

### Model comparison with loo

log_lik_dibiv <- extract_log_lik(dibiv_stan$fit)    # statid dibiv
log_lik_dibiv_dyn <- extract_log_lik(dibiv_stan_dyn$fit) # dynamic dibiv
log_lik_skellam <- extract_log_lik(skellam_stan$fit)    # static skellam
log_lik_skellam_dyn <- extract_log_lik(skellam_stan_dyn$fit) # dynamic skellam
log_lik_zeroinfl_skellam <- extract_log_lik(zeroinfl_skellam_stan$fit) # static zeroinfl skellam
log_lik_zeroinfl_skellam_dyn <- extract_log_lik(zeroinfl_skellam_stan_dyn$fit) # dynamic zeroinfl skellam
log_lik_student <- extract_log_lik(student_stan$fit) # static student
log_lik_student_dyn <- extract_log_lik(student_stan_dyn$fit) # dynamic student


loo_dibiv <- loo(log_lik_dibiv)
loo_dibiv_dyn <- loo(log_lik_dibiv_dyn)
loo_skellam <- loo(log_lik_skellam)
loo_skellam_dyn <- loo(log_lik_skellam_dyn)
loo_zeroinfl_skellam <- loo(log_lik_zeroinfl_skellam)
loo_zeroinfl_skellam_dyn <- loo(log_lik_zeroinfl_skellam_dyn)
loo_student <- loo(log_lik_student)
loo_student_dyn <- loo(log_lik_student_dyn)


compare(loo_dibiv, loo_dibiv_dyn)
compare(loo_skellam, loo_skellam_dyn,
        loo_zeroinfl_skellam, loo_zeroinfl_skellam_dyn,
        loo_student, loo_student_dyn)

### Adding covariates
## Ranking based on Serie A 2008-2009 rank positions
ranking_2009 <-  as.data.frame(cbind(rep(1,20),
                               unique(italy_2009$home_team),
                               c(17, 14, 1, 15, 5,
                                 2, 10, 20, 8, 7,
                                 18, 3, 6, 11, 9,
                                 16, 4, 12, 19, 13 )))
colnames(ranking_2009) <- c("periods", "team", "rank_points") # rename!
ranking_2009$rank_points <- as.numeric(as.vector(ranking_2009$rank_points))
# numeric check

# HMC fit
dp_stan_rank <- stan_foot(data = italy_2009,
                          model="double_pois",
                          home_effect = TRUE,
                          ranking = ranking_2009) # dp + ranking
print(dp_stan_rank, pars = c("home", "gamma", "sigma_att", "sigma_def"))

# extract pointwise log-likelihood
log_lik_dp_rank <- extract_log_lik(dp_stan_rank$fit)  # static dp + ranking
loo_dp_rank <- loo(log_lik_dp_rank)
loo_dp_rank


### Scaled double Poisson (Dixon and Coles)

dc_fit <- dixoncoles(hgoal = italy_2009$home_goals, agoal = italy_2009$away_goals, 
                     hteam = factor(italy_2009$home_team),
                     ateam = factor(italy_2009$away_team),
                     data = italy_2009)
dc_offensive <- dc_fit$par[grep("off", names(dc_fit$par), ignore.case = TRUE)]
dc_defensive <- dc_fit$par[grep("def", names(dc_fit$par), ignore.case = TRUE)]

ord <- sort.int(dc_offensive, decreasing =TRUE,
                index.return = TRUE)$ix
ord2 <- sort.int(dc_defensive, decreasing =FALSE,
                 index.return = TRUE)$ix

sel_teams <- gsub("off___", "", names(dc_offensive), ignore.case = TRUE)

pdf("att_def_dc.pdf", width = 6, height = 5)
coefplot(as.vector(rev(dc_offensive[ord])), 
              rep(0, length(sel_teams)),  
              varnames=rev(sel_teams[ord]),
              main="Attack (red)/Defense (blue) abilities (Dixon & Coles)\n",
              vertical=TRUE,
              v.axis=TRUE, h.axis=TRUE,
              cex.var=0.7, cex.pts=0.9,
              var.las=2,  xlab=NULL, ylab=NULL, mar=c(1,5,4.1,1.8),
              plot=TRUE,  offset=0.1,
              cex.main =0.9, pch = 16, lwd = 2,
              col="red", xlim= c(-1.5, 1.5))

coefplot(as.vector(rev(dc_defensive[ord])), 
         rep(0, length(sel_teams)),  
         varnames=rev(sel_teams[ord]),
         main="Attack (red)/Defense (blue) abilities (Dixon & Coles)\n",
         vertical=TRUE,
         v.axis=TRUE, h.axis=TRUE,
         cex.var=0.7, cex.pts=0.9,
         var.las=2,  xlab=NULL, ylab=NULL, mar=c(1,5,4.1,1.8),
         plot=TRUE,  offset=0.1,
         cex.main =0.9, pch = 16, lwd = 2,
              col="blue", add=TRUE)
dev.off()

dc_fit$par[grep("hfa", names(dc_fit$par), ignore.case = TRUE)] # home effect
dc_fit$par[grep("rho", names(dc_fit$par), ignore.case = TRUE)] # correlation
