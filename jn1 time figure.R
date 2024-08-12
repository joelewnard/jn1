#library(plotrix)
#
#load(file='parsSev.Rdata')
#load(file='immPars.Rdata')
#load(file='vaxTypePars.Rdata')
#
##quantile(exp(parsSev[[4]][,4,]),c(0.5,0.025,0.975))
#

pdf(width=5,height=3,file='jn1sev.pdf')

layout(matrix(c(2,3,3,
                1,1,4),nrow=2,byrow=T),widths=c(1,0.2,0.8),
       heights=c(1,1.25))
par(mar=c(4,2,1.5,0.5)); par(mgp=c(3,0.3,0)); par(tck=-0.025); par(lwd=0.5)

xpos = c(1,3,2,4)

plot(1,type='n',axes=F,ann=F,xlim=c(1,22),ylim=c(-3.5,0.7))
abline(h=0,col='grey')
for (i in 1:4){
  #parsSev[[i]][parsSev[[i]]<(-3)] = -3
  for (j in 1:4){
   lines(y=quantile(parsSev[[j]][,i,],c(0.025,0.975)),x=rep(i,2)+(xpos[j]-1)*6)
    lines(y=quantile(parsSev[[j]][,i,],c(0.025,0.025)),x=i+(xpos[j]-1)*6+c(-0.1,0.1))
    lines(y=quantile(parsSev[[j]][,i,],c(0.975,0.975)),x=i+(xpos[j]-1)*6+c(-0.1,0.1))
    points(y=quantile(parsSev[[j]][,i,],0.5),x=i+(xpos[j]-1)*6,pch=16,cex=0.65)
  }
  axis(1,at=1:4+(i-1)*6,labels=NA,lwd=0.5,lwd.ticks=0.5)
  text(x=1:4+(i-1)*6,y=-3.5-0.42,c('A','B','C','D'),srt=45,cex=0.65,adj=1,xpd=T)
}
axis(side=2,at=c(log(seq(-0.03,0.09,0.01)),
                 log(seq(0.1,2,0.1))),labels=NA,lwd=0.5,lwd.ticks=0.5)
axis(side=2,at=c(log(c(0.03,0.05,0.1,0.2,0.3,0.5,1,2))),labels=c(0.03,0.05,0.1,0.2,0.3,0.5,1,2),
     las=1,cex.axis=0.65,lwd=0,lwd.tick=0)


text(x=2.5+c(0,6,12,18),y=-4.5,rep('Period',4),cex=0.65,xpd=T)

text(x=2.5+c(3,15),y=-5.75,
     c('ED presentation','Hospital admission'),
     xpd=T,cex=0.65,font=2)
text(x=2.5+c(0,6,12,18),y=-5.1,c('Any cause','ARI associated','Any cause','ARI associated'),
     cex=0.65,xpd=T,font=3)
mtext(side=2,'Adjusted HR',cex=0.5,line=1.25)
mtext(side=3,'C. Association of period with progression risk, by clinical outcome',cex=0.5,font=2,
      adj=0,at=-1.9,line=0.5)



par(mar=c(3.5,2,1.5,0.5)); 

addK = c(4,7)
lengthK = c(3,3,2)
labsK = list(c('5 doses','6 doses','7+ doses'),
             c('1 infection','2 infections','3+ infections'),
             c('XBB monovalent\nvaccine','BA.4/BA.5 bivalent\nvaccine'))
labK = c('A. Association of period with vaccine doses received',
         'B. Association of period with documented prior infection',
         'C. Association of period with updated vaccine receipt')

for (k in 1:2){
  
  if (k%in%c(1,2)){
    plot(1,type='n',axes=F,ann=F,xlim=c(1,18),ylim=c(-0.35,0.7))
    lines(x=c(-1000,17),y=rep(0,2),col='dark grey')
    for (i in 1:4){
      for (j in 1:3){
        lines(y=quantile(immPars[[i]][,j+addK[k],],c(0.025,0.975)),x=rep(i,2)+(j-1)*6)
        lines(y=quantile(immPars[[i]][,j+addK[k],],c(0.025,0.025)),x=i+(j-1)*6+c(-0.1,0.1))
        lines(y=quantile(immPars[[i]][,j+addK[k],],c(0.975,0.975)),x=i+(j-1)*6+c(-0.1,0.1))
        points(y=quantile(immPars[[i]][,j+addK[k],],0.5),x=i+(j-1)*6,pch=16,cex=0.65)
      }
    }   
    axis(side=2,at=log(seq(0.7,2,0.1)),labels=NA,lwd=0.5,lwd.ticks=0.5)
    axis(side=2,at=log(c(0.7,1,1.5,2)),labels=c(0.7,1,1.5,2),las=1,cex.axis=0.65,lwd=0,lwd.tick=0)
  }
  
  if (k==3){
    plot(1,type='n',axes=F,ann=F,xlim=c(1,22),ylim=c(-0.35,1.1))
    lines(x=c(-1000,11),y=rep(0,2),col='dark grey')
    for (i in 1:4){
      for (j in 1:2){
        lines(y=quantile(vaxTypePars[[i]][,j,],c(0.025,0.975)),x=rep(i,2)+(j-1)*6)
        lines(y=quantile(vaxTypePars[[i]][,j,],c(0.025,0.025)),x=i+(j-1)*6+c(-0.1,0.1))
        lines(y=quantile(vaxTypePars[[i]][,j,],c(0.975,0.975)),x=i+(j-1)*6+c(-0.1,0.1))
        points(y=quantile(vaxTypePars[[i]][,j,],0.5),x=i+(j-1)*6,pch=16,cex=0.65)
      }
    }   
    axis(side=2,at=log(c(0.7,1,2,3)),labels=c(0.7,1,2,3),lwd=0,lwd.tick=0,las=1,cex.axis=0.5)
    axis(side=2,at=log(seq(0.7,3,0.1)),labels=NA,lwd=0.5,lwd.tick=0.5)
  }
  
  for (i in 1:lengthK[k]){
    axis(1,at=1:4+(i-1)*6,labels=NA,lwd=0.5,lwd.ticks=0.5)
    text(x=1:4+(i-1)*6,y=-0.4-0.1*1.1,c('A','B','C','D'),srt=45,cex=0.65,adj=1,xpd=T)
  }
  
  mtext(side=2,'Adjusted OR',cex=0.5,line=1.25)
  text(x=2.5+seq(0,(lengthK[k]-1)*6,6),y=-0.75,rep('Period',lengthK[k]),cex=0.65,xpd=T)
  
  text(x=2.5+seq(0,(lengthK[k]-1)*6,6),y=ifelse(k<3,-1,-1.2),
       labsK[[k]],
       xpd=T,cex=0.65,font=2)
  mtext(side=3,labK[k],cex=0.5,font=2,adj=0,at=-1.8,line=0.5)
  

   
}


plot(1,type='n',axes=F,ann=F,xlim=c(0,1),ylim=c(0,1))
text(x=rep(0.1,4),y=c(0.75,0.65,0.55,0.45,0.35),c('A: 1-15 December, 2023',
                                             'B: 16-31 December, 2023',
                                             'C: 1-15 January, 2024',
                                             'D: 16-31 January, 2024',
                                             'Ref: 1-30 November, 2023'),
     font=3,cex=0.65,xpd=T,adj=0)
text(x=0.1,y=0.85,'Periods',cex=0.65,adj=0,font=2)


dev.off()

length(immPars)
dim(immPars[[1]])
quantile(exp(immPars[[4]][,5,]),c(0.5,0.025,0.975))
quantile(exp(immPars[[4]][,6,]),c(0.5,0.025,0.975))
quantile(exp(immPars[[4]][,7,]),c(0.5,0.025,0.975))

quantile(exp(immPars[[4]][,8,]),c(0.5,0.025,0.975))
quantile(exp(immPars[[4]][,9,]),c(0.5,0.025,0.975))
quantile(exp(immPars[[4]][,10,]),c(0.5,0.025,0.975))


quantile(exp(parsSev[[4]][,1,]),c(0.5,0.025,0.975))
quantile(exp(parsSev[[4]][,2,]),c(0.5,0.025,0.975))
quantile(exp(parsSev[[4]][,3,]),c(0.5,0.025,0.975))
quantile(exp(parsSev[[4]][,4,]),c(0.5,0.025,0.975))
