
i1<- premier$game<379 

premier2<-premier[i1,]
model2 <- glm( goals~home+att+def, family=poisson, data=premier2 )

lambda<-exp(predict(model2, premier[!i1,]))

names(lambda)<-levels(premier$att)[c(17,10,18,13)]
lambda

np <- nrow(premier[!i1,])
B<-10000
G<-matrix(nrow=B, ncol=np)
for (i in 1:B){
	G[i,]<-rpois(np, lambda)  
} 

i2<-seq(1,np,2)
diff <- G[,i2] - G[,i2+1] 

apply(diff,2,mean)
apply(diff,2,sd)
t(apply(diff,2,quantile, probs=c(0.025,0.975)))

tabres <- matrix("", 2,8) 

tabres[,1:2] <- matrix( names(lambda), 2,2, byrow=TRUE )
tabres[,3] <- paste(premier[!i1,]$goals[i2],premier[!i1,]$goals[i2+1],sep="-")
med <- apply(G,2,median)
tabres[,4] <- paste(med[i2],med[i2+1],sep="-")
l<-round(lambda,2)
tabres[,5] <- paste(l[i2],l[i2+1],sep="-")
tabres[,6] <- round(apply(diff,2,mean),2)
tabres[,7] <- round(apply(diff,2,sd),2) 
temp <- t(apply(diff,2,quantile, probs=c(0.025,0.975)))
tabres[,8] <-paste("(",apply(temp,1,paste,collapse=','),")", sep='')

rownames(tabres)<- premier[!i1,]$game[i2]
colnames(tabres)<- c("Home Team", "Away Team", "Score", "Median", "Expected", "Exp.Diff", "SD Diff", "95% CI Diff")
noquote(tabres)

probs<-rbind( 
	c(mean(diff[,1]>0), mean(diff[,1]==0), mean(diff[,1]<0)),
	c(mean(diff[,2]>0), mean(diff[,2]==0), mean(diff[,2]<0)) 
)
probs 

tabres2<-cbind(tabres[,1:3], round(probs,2))
colnames(tabres2)[4:6]<-c("HomeWins","Draw","AwayWins")
noquote(tabres2)

round(table(diff[,1])/B,3)
round(table(diff[,2])/B,3)
	