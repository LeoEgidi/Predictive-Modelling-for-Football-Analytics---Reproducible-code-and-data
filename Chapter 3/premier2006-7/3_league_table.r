nteams<-length(levels(premier$att))
i3<-seq(1,2*n,2)
g1<-premier$goals[i3]
g2<-premier$goals[i3+1]
gd<-g1-g2 

p1<- 3*(gd>0)+(gd==0)
p2<- 3*(gd<0)+(gd==0)

points<-tapply(p1,premier$att[i3],sum)+tapply(p2,premier$def[i3],sum)
# Goals for 
GF <-tapply(g1,premier$att[i3],sum)+tapply(g2,premier$def[i3],sum)
# Goals against
GA <-tapply(g2,premier$att[i3],sum)+tapply(g1,premier$def[i3],sum) 

position <- nteams+1-rank(points, ties.method="max")

league<-function(goals1,goals2, HT, AT){
	
	nteams<-length(levels(HT))
	gd<-goals1-goals2 
	p1<- 3*(gd>0)+(gd==0)
	p2<- 3*(gd<0)+(gd==0)

	points<-tapply(p1,HT,sum)+tapply(p2,AT,sum)
	# Goals for 
	GF <-tapply(g1,HT,sum)+tapply(g2,AT,sum)
	# Goals against
	GA <-tapply(g2,HT,sum)+tapply(g1,AT,sum) 
	position <- nteams+1-rank(points, ties.method="max")
	
	tab <- matrix( "", nrow=nteams, ncol=6 )
	tab[,1]<-position 
	tab[,2]<-levels(HT)
	tab[,3]<-points 
	tab[,4]<-GF 
	tab[,5]<-GA 
	tab[,6]<-GF-GA 
	
	tab<-tab[order(points,decreasing = T),] 
	rownames(tab)<-1:nteams 
	colnames(tab)<-c("Rank","Team","Points","GF","GA","GD")
	
	results<-list( pos=position, points=points, GF=GF, GA=GA, GD=GF-GA, league.table=noquote(tab) )
	return(results)
} 

league(g1, g2, premier$att[i3], premier$def[i3]) 

lambda<-model$fit
B<-10000
nteams<-length(levels(premier$att))
P<-matrix(nrow=B,ncol=nteams)
R<-matrix(nrow=B,ncol=nteams)
GF<-matrix(nrow=B,ncol=nteams)
GA<-matrix(nrow=B,ncol=nteams)
GD<-matrix(nrow=B,ncol=nteams)
colnames(P)<-levels(premier$att)
colnames(R)<-levels(premier$att)
colnames(GF)<-levels(premier$att)
colnames(GA)<-levels(premier$att)
colnames(GD)<-levels(premier$att)

for (i in 1:B){
	g1gen<-rpois(n,lambda[i3])
	g2gen<-rpois(n,lambda[i3+1])
	res <-league(g1gen, g2gen, premier$att[i3], premier$def[i3]) 
	P[i,]<-res$points
	R[i,]<-res$pos 
	GF[i,]<-res$GF
	GA[i,]<-res$GA
	GD[i,]<-res$GD
}

apply(P,2,mean)
apply(P,2,sd)
apply(P,2,quantile,probs=c(0.025,0.5,0.975))
apply(R,2,quantile,probs=c(0.025,0.5,0.975))

summary.table<-matrix("",nrow=nteams, ncol=9)
colnames(summary.table)<-c("Team", "Exp-P","SD-P","95%LB-P","Med-P","95%UB-P","95%LB-R","Med.R","95%UB-R")
rownames(summary.table)<-1:nteams 
summary.table[,1]<- colnames(P)
summary.table[,2]<- round(apply(P,2,mean),1)
summary.table[,3]<- round(apply(P,2,sd),1)
summary.table[,4:6]<- t(apply(P,2,quantile,probs=c(0.025,0.5,0.975)))
summary.table[,7:9]<- t(round(apply(R,2,quantile,probs=c(0.025,0.5,0.975))))

noquote(summary.table)


summary.table2 <- summary.table[order(summary.table[,2],decreasing = T), ] 
rownames(summary.table2)<-1:nteams 
noquote(summary.table2)


res2<- apply(R,2,table)
tabrankings <- matrix(0,nteams,nteams)
rownames(tabrankings)<-levels(premier$att)
colnames(tabrankings)<-1:nteams 


for(i in 1:nteams){
	index<-as.numeric(names(res2[[i]])) 
	tabrankings[i,index]<-round(100*res2[[i]]/B,1)
}
tabrankings<-tabrankings[order(summary.table[,2],decreasing = T),]
tabrankings