#library(plotrix)
#
#set.seed(1)
#sel = dat$TF_lab==1&dat$Enrolled_1yr==1&date>=ymd('2023-12-01')&(date<=(ymd('2024-01-30')-tmax))#&dat$OUTPT>0#&totprior>0#vaxvar%in%as.character(4:10)#&badEnd>0
#
#yBad = yHosp; badEnd = hospEnd; endFollow = 28
#yBad[yBad==1&badEnd>endFollow] = 0; badEnd[badEnd>endFollow] = endFollow
#
#kmHosp = kmHospAri = rep(1,31)
##kmHospCI = kmHospAriCI = array(NA,dim=c(31,2))
#kmEd = kmEdAri = rep(1,15)
##kmEdCI = kmEdAriCI = array(NA,dim=c(2,15))
#
#set.seed(1)
#ts = c(0.5,1:30)
#survHosp = survHospAri = kmHosp = kmHospAri = survEd = survEdAri = kmEd = kmEdAri = array(NA,dim=c(30,2,1e3))
#for (k in 1:1e3){
#  selA = sample(which(sel==1),sum(sel==1),replace=T)
#  for (j in 1:2){
#    for (i in 1:30){
#      survHosp[i,j,k] = 1-sum(yHosp[selA]==1&hospEnd[selA]==ts[i]&jn1[selA]==(j-1))/sum(hospEnd[selA]>=i&jn1[selA]==(j-1))
#      survHospAri[i,j,k] = 1-sum(yHospAri[selA]==1&hospAriEnd[selA]==ts[i]&jn1[selA]==(j-1))/sum(hospAriEnd[selA]>=i&jn1[selA]==(j-1))
#  
#      kmHosp[i,j,k] = prod(survHosp[1:i,j,k])
#      kmHospAri[i,j,k] = prod(survHospAri[1:i,j,k])
#    }
#    
#    for (i in 1:30){
#      survEd[i,j,k] = 1-sum(yEd[selA]==1&edEnd[selA]==ts[i]&jn1[selA]==(j-1))/sum(edEnd[selA]>=i&jn1[selA]==(j-1))
#      survEdAri[i,j,k] = 1-sum(yEdAri[selA]==1&edAriEnd[selA]==ts[i]&jn1[selA]==(j-1))/sum(edAriEnd[selA]>=i&jn1[selA]==(j-1))
#      
#      kmEd[i,j,k] = prod(survEd[1:i,j,k])
#      kmEdAri[i,j,k] = prod(survEdAri[1:i,j,k])
#    }
#  }  
#  print(k)
#}
#
#kmHospPlot = kmHospAriPlot = kmEdPlot = kmEdAriPlot = array(NA,dim=c(28,2,3))
#for (i in 1:28) for (j in 1:2){
#  kmHospPlot[i,j,] = c(mean(kmHosp[i,j,]),quantile(kmHosp[i,j,],c(0.025,0.975)))
#  kmHospAriPlot[i,j,] = c(mean(kmHospAri[i,j,]),quantile(kmHospAri[i,j,],c(0.025,0.975)))
#  
#  kmEdPlot[i,j,] = c(mean(kmEd[i,j,]),quantile(kmEd[i,j,],c(0.025,0.975)))
#  kmEdAriPlot[i,j,] = c(mean(kmEdAri[i,j,]),quantile(kmEdAri[i,j,],c(0.025,0.975)))
#}

#

plot.fn = function(obj,lab,tlim=28){
  plot(1,type='n',axes=F,ann=F,ylim=c(0.925,1),xlim=c(0,27))
  
  #if (is.na(tlim)==F){
  #  #polygon(x=c(0,0,tlim,tlim),y=c(-1e3,-1e3,1e3,1e3),col=rgb(0.7,0.5,0.05,0.1))
  #  abline(v=tlim,col='red')
  #}
  
  if (tlim<28){
    abline(v=14,col='darkgoldenrod2')
  }
  
  polygon(x=c(0,ts[1:tlim],rev(ts[1:tlim]),0),y=c(1,obj[1:tlim,1,2],rev(obj[1:tlim,1,3]),1),lty=0,col=rgb(0.5,0,0.5,0.1))
  polygon(x=c(0,ts[1:tlim],rev(ts[1:tlim]),0),y=c(1,obj[1:tlim,2,2],rev(obj[1:tlim,2,3]),1),lty=0,col=rgb(0,0,1,0.1))
  
  lines(y=c(1,obj[1:28,1,1]),x=c(0,ts[1:28]),col='darkorchid3',lwd=0.75)
  for (j in 1:2){
    lines(y=c(1,obj[1:28,1,1+j]),x=c(0,ts[1:28]),col='darkorchid3',lty='dotted',lwd=0.25)
  }
  lines(y=c(1,obj[1:28,2,1]),x=c(0,ts[1:28]),col='darkslategray4',lwd=0.75)
  for (j in 1:2){
    lines(y=c(1,obj[1:28,2,1+j]),x=c(0,ts[1:28]),col='darkslategray4',lty='dotted',lwd=0.25)
  }
  
  box(bty='l');
  axis(side=1,at=seq(0,28,7),labels=NA,lwd=0,lwd.ticks=0.5)
  axis(side=2,at=c(0.925,seq(0.94,1,0.02)),labels=c(0,seq(94,100,2)),lwd=0,lwd.ticks=0.5,las=1,cex.axis=0.65)
  text(x=seq(0,28,7),seq(0,28,7),y=0.925-0.09*0.075,srt=45,xpd=T,adj=1,cex=0.65)
  axis.break(axis=2,breakpos=0.9325,style='zigzag',brw=0.05)
  
  mtext(side=2,'Cases at risk (%)',cex=0.5,line=1.25)
  mtext(side=1,'Days from positive\noutpatient test',cex=0.5,line=1.5)
  mtext(side=3,adj=0,lab,cex=0.5,line=0.25,at=-7.25,font=2)
}

setwd("~/Library/CloudStorage/GoogleDrive-joelewnard@gmail.com/My Drive/CFA epi/JN1")

pdf(file='km plots.pdf',height=1.5,width=6)

layout(matrix(1:4,nrow=1))

par(mar=c(2.5,2,1,0.5)); par(lwd=0.5); par(mgp=c(3,0.3,0)); par(tck=-0.025)

plot.fn(obj=kmEdPlot,lab='A. ED presentation (any)',tlim=15)
plot.fn(obj=kmEdAriPlot,'B. ED presentation (ARI)',tlim=15)
plot.fn(obj=kmHospPlot,'C. Hospital admission (any)')
plot.fn(obj=kmHospAriPlot,'D. Hospital admission (ARI)')


polygon(y=c(0.97,0.97,0.9625,0.9625),x=c(2,8,8,2),col=rgb(0,0,1,0.1),lty=0)
polygon(y=c(0.96,0.96,0.9525,0.9525),x=c(2,8,8,2),col=rgb(0.5,0,0.5,0.1),lty=0)
lines(y=rep(0.97,2),x=c(2,8),col='darkslategray4',lwd=0.25,lty='dotted')
lines(y=rep(0.9625,2),x=c(2,8),col='darkslategray4',lwd=0.25,lty='dotted')
lines(y=rep(0.96,2),x=c(2,8),col='darkorchid3',lwd=0.25,lty='dotted')
lines(y=rep(0.9525,2),x=c(2,8),col='darkorchid3',lwd=0.25,lty='dotted')
lines(y=rep(0.96625,2),x=c(2,8),lwd=0.75,col='darkslategray4')
lines(y=rep(0.95625,2),x=c(2,8),lwd=0.75,col='darkorchid3')
text(x=9,y=c(0.96625,0.95625),c('BA.2.86 lineages','Non-BA.2.86 lineages'),cex=0.65,adj=0)
dev.off()




