
n<-nrow(chap07_ex2_soccer) 

all(levels(chap07_ex2_soccer$ht)==levels(chap07_ex2_soccer$at))


goals <-c(chap07_ex2_soccer$goals1,chap07_ex2_soccer$goals2) 
game <- c(1:n, 1:n ) 
home <- c( rep(1,n), rep(0,n)  ) 
att <- factor(c(chap07_ex2_soccer$ht, chap07_ex2_soccer$at) )
def <- factor(c(chap07_ex2_soccer$at, chap07_ex2_soccer$ht))

levels(att) <- levels(chap07_ex2_soccer$ht)
levels(def) <- levels(chap07_ex2_soccer$ht)

premier <- data.frame( game=game, att=att, def=def, home=home, goals=goals)
head(premier)
rm(goals)
rm(game)
rm(home)
rm(att)
rm(def) 


i<-order(game)
premier<-premier[i,] 
head(premier) 

contrasts(premier$att)<-contr.sum(20)
contrasts(premier$def)<-contr.sum(20)
model <- glm( goals~home+att+def, family=poisson, data=premier )
summary(model)
round(summary(model)$coef,3)

abilities <- matrix( nrow=20,ncol=4 )
abilities[1:19,1:2] <- summary(model)$coef[2+1:19,1:2]
abilities[1:19,3:4] <- summary(model)$coef[21+1:19,1:2]
abilities[20,1] <- -sum(summary(model)$coef[2+1:19,1])
abilities[20,3] <- -sum(summary(model)$coef[21+1:19,1])

rownames(abilities)<-levels(premier$att)
colnames(abilities)<-c( "Att", "SD-Att", "Def", "SD-Def" )
round(abilities,3)



