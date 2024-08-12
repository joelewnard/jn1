c(31,9,57,120,61,27)/305


load('plotJn1.Rdata')
load('plotBa286.Rdata')

setwd("~/Library/CloudStorage/GoogleDrive-joelewnard@gmail.com/My Drive/CFA epi/JN1")
pdf(file='severity fig.pdf',width=6,height=2.5)



layout(matrix(c(1,2,7:8,
              3:6),nrow=2,byrow=T,ncol=4))

ylim=rep(1.5,5); by = rep(0.5,5)

par(mar=c(3,2.25,1,0.5));
par(mgp=c(3,0.3,0)); par(lwd=0.5); par(tck=-0.025)

plot(1,type='n',axes=F,ann=F,ylim=c(0,1),xlim=c(31,85))

#polygon(x=c(31,-1e3,-1e3,31),y=c(-1e3,-1e3,1e3,1e3),col=rgb(1,0,0,0.15),lty=0)
for (i in 31:85){
  polygon(x=i+c(-0.45,-0.45,0.45,0.45),y=c(0,rep(plotJn1[1,i],2),0),lty=0,col=rgb(0.29,0.5,0.29,0.25))
  lines(x=i+c(-0.45,0.45),y=rep(plotJn1[1,i],2),col='forestgreen',lwd=0.25)
  polygon(x=i+c(-0.45,-0.45,0.45,0.45),
          y=c(plotJn1[1,i],rep(plotBa286[1,i],2),plotJn1[1,i]),lty=0,col=rgb(0.95,0.7,0.1,0.25))
  lines(x=i+c(-0.45,0.45),y=rep(plotBa286[1,i],2),col='darkgoldenrod2',lwd=0.25)
  
  lines(y=plotsJN1[i,2:3],x=rep(i,2))
  points(y=plotsJN1[i,1],x=i,cex=0.5,pch=16)
}
text(x=c(31,41,51,62,72,82),y=-0.11,
     c('Dec 1','Dec 10','Dec 20','Jan 1','Jan 10','Jan 20'),cex=0.65,srt=45,xpd=T,adj=1)
mtext(side=1,'Date of test',cex=0.5,line=1.75)


box(bty='l')
axis(side=2,at=seq(0,1,0.2),labels=100*seq(0,1,0.2),cex.axis=0.65,lwd=0,lwd.ticks=0.5,las=1)
axis(side=1,at=c(1,10,20,31,41,51,62,72,82),labels=NA,lwd=0,lwd.ticks=0.5)
mtext(side=2,'Proportion (%)',cex=0.5,line=1.5)
mtext(side=3,adj=0,at=14,'A. Lineage frequency',cex=0.5,font=2,line=0.25)

plot(1,ylim=c(0.35,1.1),xlim=c(31,85),axes=F,ann=F)

polygon(x=c(31,38,38,31),y=c(1,1,0.95,0.95),col=rgb(0.95,0.7,0.1,0.25),border='darkgoldenrod2',lwd=0.25)
polygon(x=c(31,38,38,31),y=c(0.9,0.9,0.85,0.85),col=rgb(0.29,0.5,0.29,0.25),border='forestgreen',lwd=0.25)
lines(x=c(31,38),y=rep(0.775,2)); points(x=34.5,y=0.775,pch=16,cex=0.5)
text(x=40,y=c(0.975,0.875,0.775),c('BA.2.86 (all tests)','JN.1 (all tests)','SGTF (TaqPath tests)'),cex=0.65,font=3,adj=0,xpd=T)
text(x=31,y=c(1.075,0.5),c('(A)','(B-E)'),cex=0.65,font=3,adj=0)

lines(x=c(31,38),y=rep(0.4,2),col='black',xpd=T)
lines(x=c(31,38),y=rep(0.3,2),col='red',xpd=T)
points(x=rep(34.5,2),y=c(0.4,0.3),col=c('black','red'),cex=0.5,pch=16,xpd=T)
text(x=rep(40,2),y=c(0.4,0.3),c('Observed','Projected'),cex=0.5,font=3,adj=0,xpd=T)

labs = c('B. ED presentation (any)','C. ED presentation (ARI)','D. Hospital admission (any)','E. Hospital admission (ARI)')



for (i in c(1:4)){
  
  plot(1,type='n',axes=F,ann=F,ylim=c(0,ylim[i]),xlim=c(31,85))
  abline(h=1,col='blue')
  for (j in 1:55){
    lines(y=(plotsSGTF[i,j,2:3]),x=rep(j,2)+31+0.1,col='red')
    points(y=(plotsSGTF[i,j,1]),x=j+31+0.1,cex=0.5,pch=16,col='red')
  }
  
  
  #polygon(x=c(31,-1e3,-1e3,31),y=c(-1e3,-1e3,1e3,1e3),col=rgb(1,0,0,0.15),lty=0)
  for (j in 31:86){
    lines(y=ratios[i,j,2:3],x=rep(j,2)-0.1)
    points(y=ratios[i,j,1],x=j-0.1,cex=0.5,pch=16)
  }

  box(bty='l')
  axis(side=2,at=seq(0,ylim[i],by[i]),labels=seq(0,ylim[i],by[i]),cex.axis=0.65,lwd=0,lwd.ticks=0.5,las=1)
  axis(side=1,at=c(1,10,20,31,41,51,62,72,82),labels=NA,lwd=0,lwd.ticks=0.5)

  text(x=c(31,41,51,62,72,82),y=-ylim[i]*0.11,
       c('Dec 1','Dec 10','Dec 20','Jan 1','Jan 10','Jan 20'),cex=0.65,srt=45,xpd=T,adj=1)
  
  mtext(side=3,adj=0,at=14,labs[i],cex=0.5,font=2,line=0.25)
  #mtext(side=3,adj=0,at=-10,'(i) Observed frequency',cex=0.45,font=3,line=0.1)
  mtext(side=2,'Adjusted HR',cex=0.5,line=1.5)
  mtext(side=1,'Date of test',cex=0.5,line=1.75)
}



dev.off()



#
##
#
#sel = dat$Enrolled_1yr==1&date>=ymd('2023-11-01')&(date<=(ymd('2024-01-30')-tmax))#&badEnd>0
##
#totNon = totTf = totSGTF = totSGTD = totEdNon = totEdAri = totHospNon = totHospAri = totSevNon = totDeath = c()
#for (i in 1:length(dates)){
#  totNon[i] = sum(date==dates[i]&sel==1&dat$TF_lab==0)
#  totTf[i] = sum(date==dates[i]&sel==1&dat$TF_lab==1)
#
#  totSGTF[i] = sum(date==dates[i]&sel==1&dat$TF_lab==1&sgtf==1)
#  totSGTD[i] = sum(date==dates[i]&sel==1&dat$TF_lab==1&sgtf==0)
#
#  totEdNon[i] = sum(date==dates[i]&sel==1&yEd==1&yEdAri==0&edEnd<=14)
#  totEdAri[i] = sum(date==dates[i]&sel==1&yEdAri==1&edEnd<=14)
#  totHospNon[i] = sum(date==dates[i]&sel==1&yHosp==1&yHospAri==0&hospEnd<=28)
#  totHospAri[i] = sum(date==dates[i]&sel==1&yHospAri==1&hospEnd<=28)
#  totSevNon[i] = sum(date==dates[i]&sel==1&ySevere==1&yDeath==0&severeEnd<=14)
#  totDeath[i] = sum(date==dates[i]&sel==1&yDeath==1&tDeath<=90)
#}
#
#
#pdf(file='counts fig.pdf',width=6,height=1.25)
#
#par(mfrow=c(1,5))
#par(mar=c(2.5,2.25,1,0.5)); par(mgp=c(3,0.3,0)); par(lwd=0.5); par(tck=-0.025)
#
#plot(1,xlim=c(1,90),ylim=c(0,1.4e3),axes=F,ann=F,type='n')
#for (i in 1:90){
#  polygon(x=i+c(-0.5,0.5,0.5,-0.5),y=c(0,0,rep(totTf[i+1],2)),col=rgb(0,0,1,0.25),lty=0)
#  polygon(x=i+c(-0.5,0.5,0.5,-0.5),y=totTf[i+1]+c(0,0,rep(totNon[i+1],2)),col=rgb(1,0,0,0.25),lty=0)
#}
#lines(x=rep(1:90,each=2)+rep(c(-0.5,0.5),90),y=rep(totTf[2:91],each=2),col='dark blue')
#lines(x=rep(1:90,each=2)+rep(c(-0.5,0.5),90),y=rep(totNon[2:91],each=2)+rep(totTf[2:91],each=2),col='darkred')
#box(bty='l')
#axis(side=2,at=seq(0,1.4e3,350),cex.axis=0.65,lwd=0,lwd.ticks=0.5,las=1)
#axis(side=1,at=c(1,15,31,46,62,77,91),labels=NA,lwd=0,lwd.ticks=0.5)
#text(x=c(1,15,31,46,62,77,91),y=-1.4e3*0.1,
#     c('Nov 1','Nov 15','Dec 1','Dec 15','Jan 1','Jan 15','Jan 29'),cex=0.65,srt=45,xpd=T,adj=1)
#mtext(side=2,line=1.5,'Count',cex=0.5)
#mtext(side=3,line=0.25,'A. Cases diagnosed',cex=0.5,font=2,adj=0,at=-36)
#text(x=rep(0,2),y=c(1,0.9)*1400,c('Non-TF assay','TF assay'),col=c('darkred','darkblue'),adj=0,cex=0.65,font=3)
#
#plot(1,xlim=c(1,90),ylim=c(0,300),axes=F,ann=F,type='n')
#for (i in 1:90){
#  polygon(x=i+c(-0.5,0.5,0.5,-0.5),y=c(0,0,rep(totSGTF[i+1],2)),col=rgb(1,0.5,0,0.25),lty=0)
#  polygon(x=i+c(-0.5,0.5,0.5,-0.5),y=totSGTF[i+1]+c(0,0,rep(totSGTD[i+1],2)),col=rgb(0.25,0.85,0.25,0.25),lty=0)
#}
#lines(x=rep(1:90,each=2)+rep(c(-0.5,0.5),90),y=rep(totSGTF[2:91],each=2),col='brown')
#lines(x=rep(1:90,each=2)+rep(c(-0.5,0.5),90),y=rep(totSGTF[2:91],each=2)+rep(totSGTD[2:91],each=2),col='forestgreen')
#box(bty='l')
#axis(side=2,at=seq(0,300,60),cex.axis=0.65,lwd=0,lwd.ticks=0.5,las=1)
#axis(side=1,at=c(1,15,31,46,62,77,91),labels=NA,lwd=0,lwd.ticks=0.5)
#text(x=c(1,15,31,46,62,77,91),y=-300*0.1,
#     c('Nov 1','Nov 15','Dec 1','Dec 15','Jan 1','Jan 15','Jan 29'),cex=0.65,srt=45,xpd=T,adj=1)
#mtext(side=2,line=1.5,'Count',cex=0.5)
#mtext(side=3,line=0.25,'B. S gene detection (TF)',cex=0.5,font=2,adj=0,at=-36)
#text(x=rep(0,2),y=c(1,0.9)*300,c('Detected','Not detected'),col=c('forestgreen','brown'),adj=0,cex=0.65,font=3)
#
#plot(1,xlim=c(1,90),ylim=c(0,60),axes=F,ann=F,type='n')
#for (i in 1:90){
#  polygon(x=i+c(-0.5,0.5,0.5,-0.5),y=c(0,0,rep(totEdAri[i+1],2)),col=rgb(0.5,0,0.5,0.25),lty=0)
#  polygon(x=i+c(-0.5,0.5,0.5,-0.5),y=totEdAri[i+1]+c(0,0,rep(totEdNon[i+1],2)),col=rgb(0,0,1,0.25),lty=0)
#}
#lines(x=rep(1:90,each=2)+rep(c(-0.5,0.5),90),y=rep(totEdAri[2:91],each=2),col='darkorchid4')
#lines(x=rep(1:90,each=2)+rep(c(-0.5,0.5),90),y=rep(totEdNon[2:91],each=2)+rep(totEdAri[2:91],each=2),col='darkslategray4')
#box(bty='l')
#axis(side=2,at=seq(0,60,15),cex.axis=0.65,lwd=0,lwd.ticks=0.5,las=1)
#axis(side=1,at=c(1,15,31,46,62,77,91),labels=NA,lwd=0,lwd.ticks=0.5)
#text(x=c(1,15,31,46,62,77,91),y=-60*0.1,
#     c('Nov 1','Nov 15','Dec 1','Dec 15','Jan 1','Jan 15','Jan 29'),cex=0.65,srt=45,xpd=T,adj=1)
#mtext(side=2,line=1.5,'Count',cex=0.5)
#mtext(side=3,line=0.25,'C. ED presentations',cex=0.5,font=2,adj=0,at=-36)
#text(x=rep(0,2),y=c(1,0.9)*60,c('No ARI diagnosis','ARI diagnosis'),col=c('darkslategray4','darkorchid4'),adj=0,cex=0.65,font=3)
#
#plot(1,xlim=c(1,90),ylim=c(0,25),axes=F,ann=F,type='n')
#for (i in 1:90){
#  polygon(x=i+c(-0.5,0.5,0.5,-0.5),y=c(0,0,rep(totHospAri[i+1],2)),col=rgb(0.5,0,0.5,0.25),lty=0)
#  polygon(x=i+c(-0.5,0.5,0.5,-0.5),y=totHospAri[i+1]+c(0,0,rep(totHospNon[i+1],2)),col=rgb(0,0,1,0.25),lty=0)
#}
#lines(x=rep(1:90,each=2)+rep(c(-0.5,0.5),90),y=rep(totHospAri[2:91],each=2),col='darkorchid4')
#lines(x=rep(1:90,each=2)+rep(c(-0.5,0.5),90),y=rep(totHospAri[2:91],each=2)+rep(totHospNon[2:91],each=2),col='darkslategray4')
#box(bty='l')
#axis(side=2,at=seq(0,25,5),cex.axis=0.65,lwd=0,lwd.ticks=0.5,las=1)
#axis(side=1,at=c(1,15,31,46,62,77,91),labels=NA,lwd=0,lwd.ticks=0.5)
#text(x=c(1,15,31,46,62,77,91),y=-25*0.1,
#     c('Nov 1','Nov 15','Dec 1','Dec 15','Jan 1','Jan 15','Jan 29'),cex=0.65,srt=45,xpd=T,adj=1)
#mtext(side=2,line=1.5,'Count',cex=0.5)
#mtext(side=3,line=0.25,'D. Admissions',cex=0.5,font=2,adj=0,at=-36)
#text(x=rep(0,2),y=c(1,0.9)*25,c('No ARI diagnosis','ARI diagnosis'),col=c('darkslategray4','darkorchid4'),adj=0,cex=0.65,font=3)
#
#plot(1,xlim=c(1,90),ylim=c(0,12),axes=F,ann=F,type='n')
#for (i in 1:90){
#  polygon(x=i+c(-0.5,0.5,0.5,-0.5),y=c(0,0,rep(totSevNon[i+1],2)),col=rgb(1,0,0,1),lty=0)
#  polygon(x=i+c(-0.5,0.5,0.5,-0.5),y=totSevNon[i+1]+c(0,0,rep(totDeath[i+1],2)),col=rgb(0.5,0,0.33,0.05),lty=0)
#}
#lines(x=rep(1:90,each=2)+rep(c(-0.5,0.5),90),y=rep(totSevNon[2:91],each=2),col='darkred')
#lines(x=rep(1:90,each=2)+rep(c(-0.5,0.5),90),y=rep(totSevNon[2:91],each=2)+rep(totDeath[2:91],each=2),col='darkgoldenrod4')
#box(bty='l')
#axis(side=2,at=seq(0,12,3),cex.axis=0.65,lwd=0,lwd.ticks=0.5,las=1)
#axis(side=1,at=c(1,15,31,46,62,77,91),labels=NA,lwd=0,lwd.ticks=0.5)
#text(x=c(1,15,31,46,62,77,91),y=-12*0.1,
#     c('Nov 1','Nov 15','Dec 1','Dec 15','Jan 1','Jan 15','Jan 29'),cex=0.65,srt=45,xpd=T,adj=1)
#mtext(side=2,line=1.5,'Count',cex=0.5)
#mtext(side=3,line=0.25,'E. Severe outcomes',cex=0.5,font=2,adj=0,at=-36)
#text(x=rep(0,2),y=c(1,0.9)*12,c('Death','ICU or ventilation'),col=c('darkgoldenrod4','darkred'),adj=0,cex=0.65,font=3)
#
#dev.off()#
#
#
#
#