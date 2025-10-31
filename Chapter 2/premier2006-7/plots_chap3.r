library(reactable)

stab2<-as.data.frame(summary.table2)
stab2[,-1]<-apply(stab2[,-1],2, as.numeric)
stab2

obspts<-league(g1, g2, premier$att[i3], premier$def[i3])$points
exppts<-apply(P,2,mean)

rownames(stab3)<-NULL


stab3<- data.frame( Team=names(obspts), Points=obspts, Expected_Points=round(exppts,1), Point_Difference=obspts-round(exppts,1) )

rownames(stab3)<-NULL

stab3<-stab3[ order( exppts,decreasing=TRUE), ]

#
# Table of points with bars
#
reactable(
  stab3,
  defaultColDef = colDef(
    cell = data_bars(stab3, text_position = "inside-base")
  )
  , pagination =F
)

#
# Table of points with bars
#
reactable(
  stab3,
  pagination = FALSE,
  defaultColDef = colDef(
    cell = data_bars(stab3,
                     bar_height = 3, 
                     text_position = "outside-end", 
                     background = "transparent", 
					 fill_color=c("red","blue"))
  )
)

#
# Simulated League Table 
#

reactable(
  stab2,
  pagination = FALSE
)

#install.packages("webshot")
#install.packages("htmlwidgets")

library(htmlwidgets)
library(webshot)

rtable <- reactable(stab2, pagination = F)
html <- "rtable.html"
saveWidget(rtable, html)
webshot::install_phantomjs()
webshot(html, "rtableSnapshot.png") # you can also export to pdf


#
# Histograms Points 
#
pdf("plot3_hist_points.pdf", height=8.3, width=11.7 )
P2<-P[,order( obspts,decreasing=TRUE)]
obspts2<-sort(obspts,decreasing=T)

par(mfrow=c(4,5)) 

for (k in 1:20){ 
	hist( P2[,k], col="skyblue4",border="gray", main=paste(k,". ",colnames(P2)[k]), ylab="", xlab="" )
	abline(v=obspts2[k], col="darkred", lwd=4)
}
graphics.off()

#
# Boxplots of Points
#

#
# Errobars of Points
#

#
# Histograms Rankings 
#
R2<-R[,order( obspts,decreasing=TRUE)]
R3 <-data.frame( teams=colnames(R2), ranks=as.vector(t(R2)) )
obsrankings <- 21-rank(obspts)

library(forcats)
library(ggridges)
library(ggplot2)
# install.packages("viridis")
# install.packages("hrbrthemes")
library(viridis)
library(hrbrthemes)

# Plot
pdf("plot4_hist_ranks.pdf", height=8.3, width=11.7 )
ggplot(data = R3, aes(y= teams, x = ranks, fill =teams)) +
  geom_density_ridges(alpha=0.5, stat="binline", bins=20, scale = 3, rel_min_height = 0.01)  +
  labs(title = 'Ranking Distribution') +
    theme(
      legend.position="none",
      panel.spacing = unit(0.1, "lines"),
      strip.text.x = element_text(size = 8)) + 
	  xlab("League Ranking") + ylab("Team")+scale_x_reverse() +
	  scale_y_discrete(limits = rev(colnames(R2)))
graphics.off()   
	
# --------------------------------------------- 
# Quantile Errobars of Rankings
#
R4<-as.data.frame(t(apply(R2,2,quantile,probs=c(0.25,0.5,0.75))))
R4<-cbind(rownames(R4),R4)
names(R4)<-c("Teams","low","median","high")

#pdf("plot5_error_ranks.pdf", height=8.3, width=11.7 )
 

ggplot(data = R4, aes(y= Teams, x = median)) +
  geom_pointrange(linewidth=1.5, color="blue",aes(xmin = low, x=median, xmax = high),alpha=0.5)+
  geom_point(size=2, color="blue")+
  labs(title = 'Ranking Distribution') +
    theme(
      legend.position="none",
      panel.spacing = unit(0.1, "lines"),
      strip.text.x = element_text(size = 8)) + 
	  xlab("League Ranking") + ylab("Team")+scale_x_reverse() +
	  scale_y_discrete(limits = rev(colnames(R2)))
graphics.off()   



#
# Other tests
#
reactable(
  stab2,
  defaultColDef = colDef(
    cell = data_bars(stab2, text_position = "outside-base")
  )
)


reactable(
  stab2,
  defaultColDef = colDef(
    cell = data_bars(stab2, text_position = "inside-base")
  )
  , pagination =F
)
