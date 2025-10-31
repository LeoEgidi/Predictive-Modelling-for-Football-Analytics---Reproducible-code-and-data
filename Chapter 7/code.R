 dum.cum <- function(x){
		a <- numeric(3)  
		if( x==1 ) a<-c(1,1,1)
		if( x==0 ) a<-c(0,1,1)
		if( x==-1 ) a<-c(0,0,1)
		return(a)
}

dum <- function(x){
		a <- numeric(3)  
		if( x==1 ) a<-c(1,0,0)
		if( x==0 ) a<-c(0,1,0)
		if( x==-1 ) a<-c(0,0,1)
		return(a)
}
x<-read.csv2("table3-2.csv")
x<-apply(x,2,as.numeric)


 diff<-x[,4]-x[,5]
 diff[diff>0] <-1 
 diff[diff==0] <- 0
 diff[diff<0] <- -1 
 probs <- as.matrix(x)[,1:3]
 cumprobs <- apply(probs, 1, cumsum)
 acum     <- apply(as.matrix(diff), 1, dum.cum)
 RPS      <- sum(apply((cumprobs -acum)^2,2,sum))/22
 RPS; 2*RPS

 eqprobs    <- matrix(1/3, 11, 3) 
 eqcumprobs <- apply(eqprobs, 1, cumsum)
 RPSrandom  <- sum(apply((eqcumprobs -acum)^2,2,sum))/22
 RPSrandom  ; 2*RPSrandom  
 
 a     <- apply(as.matrix(diff), 1, dum)
 BS <- sum(apply((t(probs) -a)^2,2,sum))/11
 BS

 BSrandom <- sum(apply((t(eqprobs) -a)^2,2,sum))/11
 BSrandom 


results <- c(RPS, RPSrandom, BS/2, BSrandom/2)
names(results) <- c("RPS", "RPSrandom", "BS/2", "BSrandom/2")
results


