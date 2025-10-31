### Installation and packages
# install.packages("footBayes")
# devtools::install_github("leoegidi/footBayes")

library(footBayes)
library(devtools)
library(dplyr)
library(bayesplot)
library(ggplot2)
library(loo)

#### EURO 2020
### Data

euro_data <- read.table("euro.csv", sep=",", header = TRUE)
euro_data <- euro_data[,-1]
colnames(euro_data) <- c("periods", "home_team", "away_team",
                         "home_goals", "away_goals")
rankings <- read.csv("ranking_euro.csv", sep=";")
View(euro_data)

# 1 groupstage

ngames_prev <- 12
euro_data_test <- data.frame(
  periods = rep(10, ngames_prev),
  home_team = c("Italy", "Wales",
                     "Denmark", "Russia",
                     "England", "Austria",
                     "Netherlands", "Scotland",
                     "Poland", "Spain",
                     "Hungary", "Germany"),
  
  away_team = c("Turkey", "Switzerland",
                     "Finland", "Belgium",
                     "Croatia", "FYR Macedonia",
                     "Ukraine", "Czech Republic",
                     "Slovakia", "Sweden",
                     "Portugal", "France"),
  
  home_goals = c(3,1,0,0,1,3,3,0,1,0, 0,0),
  away_goals = c(0,1,1,3,0,1,2,2,2,0,3,1))

euro_data <-rbind(euro_data, euro_data_test)

# 2 groupstage
euro_data_test2 <- data.frame(
  periods = rep(11, ngames_prev),
  home_team = c("Russia", "Turkey",
                     "Italy", "Ukraine",
                     "Denmark", "Netherlands",
                     "Sweden", "Croatia",
                     "England", "Hungary", 
                     "Germany", "Spain"),
  away_team = c("Finland", "Wales",
                     "Switzerland", "FYR Macedonia",
                     "Belgium", "Austria",
                     "Slovakia",  "Czech Republic",
                     "Scotland", "France",
                     "Portugal", "Poland"),
  
  home_goals = c(1,0,3,2,1,2,1,1,0,1,4,1),
  away_goals = c(0,2,0,1,2,0,0,1,0,1,2,1))

euro_data <-rbind(euro_data, euro_data_test2)

# 3 groupstage

euro_data_test3 <- data.frame(
  periods = rep(12, ngames_prev),
  home_team = c( "Italy","Switzerland",  
                      "Ukraine", "Netherlands",
                      "Denmark", "Finland", 
                      "Scotland","England",
                      "Sweden", "Spain",
                      "Portugal", "Germany" ),
  away_team = c("Wales", "Turkey",
                     "Austria", "FYR Macedonia",
                     "Russia", "Belgium", 
                     "Croatia", "Czech Republic",
                     "Poland","Slovakia",  
                     "France","Hungary"),
  
  home_goals = c(1,3,0,3,4,0,1,1,3,5,2,2),
  away_goals = c(0,1,1,0,1,2,3,0,2,0,2,2))

euro_data <-rbind(euro_data, euro_data_test3)

# round of 16
ngames_prev <- 8
euro_data_test_round16 <- data.frame(
  periods = rep(13, ngames_prev),
  home_team = c("Wales", "Italy",
                     "Netherlands", "Belgium",
                     "Croatia", "France",
                     "England", "Sweden"),
  away_team = c( "Denmark", "Austria",
                      "Czech Republic", "Portugal",
                      "Spain", "Switzerland",
                      "Germany", "Ukraine"),
  home_goals = c(0,2,0,1,3,3,2,1),
  away_goals = c(4,1,2,0,5,3,0,2))

euro_data <-rbind(euro_data, euro_data_test_round16)

# quarter of finals
ngames_prev <- 4
euro_data_test_q <- data.frame(
  periods = rep(14, ngames_prev),
  home_team = c("Switzerland", "Belgium", 
                     "Czech Republic", "Ukraine"),
  away_team = c( "Spain", "Italy", "Denmark", "England"),
  home_goals = c(1,1,1,0),
  away_goals = c(1,2,2,4))

euro_data <-rbind(euro_data, euro_data_test_q)

# seminfinals
ngames_prev <- 2
euro_data_test_semi <- data.frame(
  periods = rep(15, ngames_prev),
  home_team = c("Italy", "England"),
  away_team = c( "Spain",  "Denmark" ),
  home_goals = c(1,2),
  away_goals = c(1,1))

euro_data <-rbind(euro_data, euro_data_test_semi)

fit_semi <- stan_foot(data = euro_data,
                      model = "diag_infl_biv_pois",
                      home_effect = FALSE,
                      dynamic_type = "seasonal",
                      predict = ngames_prev,
                      ranking = as.data.frame(rankings),
                      cores = 4)

foot_abilities(fit_semi, euro_data, 
               teams = c("Denmark", "England", "Italy", "Spain"))
ggsave("euro_abilities.pdf", width = 8, height = 6)

# finals
ngames_prev <- 2
euro_data_test_final <- data.frame(
  periods = rep(16, ngames_prev),
  home_team = c( "England", "Spain"  ),
  away_team = c( "Italy", "Denmark" ),
  home_goals = c(1, NA),
  away_goals = c(1, NA))

euro_data <-rbind(euro_data, euro_data_test_final)

fit_final <- stan_foot(data = euro_data,
                      model = "diag_infl_biv_pois",
                      home_effect = FALSE,
                      dynamic_type = "seasonal",
                      predict = ngames_prev,
                      ranking = as.data.frame(rankings),
                      cores = 4)

#### WORLD CUP 2022
### Data

wc_data_train <- read.csv("world.csv", sep=",")
wc_data_train <- wc_data_train[,-1]
colnames(wc_data_train) <- c("periods", "home_team", "away_team",
                             "home_goals", "away_goals",
                             "tournament")
rankings <- read.csv("ranking_world.csv", sep=",")
rankings <- rankings[,-1]

# 1 groupstage
ngames_matchday1 <- 16
wc_data_train_matchday_1 <- data.frame(
  periods = rep(length(unique(wc_data_train$periods))+1, ngames_matchday1),
  home_team = c("Qatar", "England" , "Senegal",
                "United States", "Argentina", "Denmark",
                "Mexico", "France", "Morocco", "Germany",
                "Spain","Belgium",
                "Switzerland","Uruguay",
                "Portugal","Brazil"),
  
  away_team = c("Ecuador", "Iran", "Netherlands",
                "Wales", "Saudi Arabia", "Tunisia",
                "Poland", "Australia", "Croatia",
                "Japan",
                "Costa Rica","Canada",
                "Cameroon","South Korea",
                "Ghana","Serbia"),
  
  home_goals = c(0,6,0,1,1,0,0,4,0,1,7,1,1,0,3,2),
  away_goals = c(2,2,2,1,2,0,0,1,0,2,0,0,0,0,2,0),
  tournament = rep("World Cup 2022", ngames_matchday1 ))

# 2 groupstage
ngames_matchday2 <- 16
wc_data_train_matchday2 <- data.frame(
  periods = rep(length(unique(wc_data_train$periods))+2, ngames_matchday2),
  home_team = c("Wales","Qatar","Netherlands",
                "England", "Tunisia","Poland",
                "France","Argentina", "Japan","Germany",
                "Belgium","Croatia","Cameroon","Brazil",
                "Portugal","South Korea"),
  
  away_team = c( "Iran",  "Senegal","Ecuador",
                 "United States", "Australia",
                 "Saudi Arabia",
                 "Denmark","Mexico","Costa Rica","Spain",
                 "Morocco","Canada","Serbia","Switzerland",
                 "Uruguay","Ghana"),
  
  home_goals = c(0,1,1,0,0,2,2,2,0,1,0,4,3,1,2,2),
  away_goals = c(2,3,1,0,1,0,1,0,1,1,2,1,3,0,0,3),
  tournament = rep("World Cup 2022",ngames_matchday2))

# groupstage 3
ngames_matchday3 <- 16
wc_data_train_matchday3 <- data.frame(
  periods = rep(length(unique(wc_data_train$periods))+3, ngames_matchday3),
  home_team = c("Ecuador", "Netherlands", 
                "Iran", "Wales",
                "Tunisia", "Australia",
                "Poland", "Saudi Arabia",
                "Croatia", "Canada",
                "Japan", "Costa Rica",
                "South Korea", "Ghana",
                "Serbia","Cameroon"),
  
  away_team = c( "Senegal", "Qatar",
                 "United States",  "England",
                 "France", "Denmark",
                 "Argentina", "Mexico",
                 "Belgium", "Morocco",
                 "Spain",  "Germany",
                 "Portugal", "Uruguay",
                 "Switzerland", "Brazil"),
  
  home_goals = c(1,2,0,0,1,1,0,1,0,1,2,2,2,0,2,1),
  away_goals = c(2,0,1,3,0,0,2,2,0,2,1,4,1,2,3,0),
  tournament = rep("World Cup 2022", ngames_matchday3))

wc_data_stan <-rbind(wc_data_train,
                     wc_data_train_matchday_1,
                     wc_data_train_matchday2,
                     wc_data_train_matchday3)

fit_group3 <- stan_foot(wc_data_stan[,-6],
                        model = "diag_infl_biv_pois",
                        home_effect = FALSE,
                        dynamic_type = "seasonal",
                        predict = ngames_matchday3,
                        ranking = as.data.frame(rankings),
                        cores = 4)

foot_prob(data = wc_data_stan[,-6], object = fit_group3)
ggsave("chessboard_3bis.pdf", width = 12, height = 10)
compare_foot(fit_group3, test_data = wc_data_train_matchday3)

# round of 16
ngames_matchday4 <- 8
wc_data_train_matchday4 <- data.frame(
  periods = rep(length(unique(wc_data_train$periods))+4, ngames_matchday4),
  home_team = c("Netherlands", "Argentina", 
                "France","England",
                "Japan","Brazil" ,
                "Morocco", "Portugal"),
  
  away_team = c( "United States", "Australia",
                 "Poland", "Senegal",
                 "Croatia", "South Korea",
                 "Spain", "Switzerland"),
  
  home_goals = c(3,2,3,3,1,4,0,6),
  away_goals = c(1,1,1,0,1,1,0,1),
  tournament = rep("World Cup 2022", ngames_matchday4))

# quarter of finals
ngames_matchday5 <- 4
wc_data_train_matchday5 <- data.frame(
  periods = rep(length(unique(wc_data_train$periods))+5, ngames_matchday5),
  home_team = c("Croatia", "Netherlands",
                "Morocco", "England"),
  
  away_team = c("Brazil", "Argentina", 
                "Portugal", "France"),
  
  home_goals = c(1,2,1,1),
  away_goals = c(1,2,0,2),
  tournament = rep("World Cup 2022", ngames_matchday5))

# semifinals
ngames_matchday6 <- 2        
wc_data_train_matchday6 <- data.frame(
  periods = rep(length(unique(wc_data_train$periods))+6, ngames_matchday6),
  home_team = c( "Argentina", "France"  ),
  away_team = c("Croatia" ,"Morocco"  ),
  home_goals = c(3,2),
  away_goals = c(0,0),
  tournament =  rep("World Cup 2022", ngames_matchday6))

# finals
ngames_matchday7 <- 2        
wc_data_test <- data.frame(
  periods = rep(length(unique(wc_data_train$periods))+7, ngames_matchday7),
  home_team = c( "Argentina", "Croatia"  ),
  away_team = c("France" ,"Morocco"  ),
  home_goals = c(3, 2),
  away_goals = c(3,1),     
  tournament = rep("World Cup 2022", ngames_matchday7))

wc_data_stan <-rbind(wc_data_train,
                     wc_data_train_matchday_1,
                     wc_data_train_matchday2,
                     wc_data_train_matchday3,
                     wc_data_train_matchday4,
                     wc_data_train_matchday5,
                     wc_data_train_matchday6,
                     wc_data_test
                     )

fit_final <- stan_foot(wc_data_stan[,-6],
                        model = "diag_infl_biv_pois",
                        home_effect = FALSE,
                        dynamic_type = "seasonal",
                        predict = ngames_matchday7,
                        ranking = as.data.frame(rankings),
                        cores = 4)
foot_prob(data = wc_data_stan[,-6], object = fit_final)
ggsave("chessboard_finaltris.pdf", width = 8, height = 8)
