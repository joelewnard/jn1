######
#####
#####
#sensParsNumVax = array(NA,dim=c(2,5,5,5,1e4,5,7)); sensParsNumNat = array(NA,dim=c(2,5,5,5,1e4,5,3))
#sensParsTypeVax = array(NA,dim=c(2,5,5,5,1e4,5,2)); sensParsTypeNat = array(NA,dim=c(2,5,5,5,1e4,5,3))
#set.seed(1)
#for (i in 1:5){
#  
#  sel = dat$TF_lab==1&dat$Enrolled_1yr==1&date>=ymd('2023-12-01')&(date<=(ymd('2024-01-30')-tmax))#&badEnd>0
#
#  mod = glm(totprior~jn1+vaxvar+recomb+edPast+inptPast+char+outpt+sex+race+inc+as.factor(age)+smoke+inc+bmiCat+
#              paxPreEd+as.factor(week),
#            subset=sel==1,data=impDat[[i]],family='poisson'); #summary(mod)[[8]][1,c(1,3,4)]
#  summary(mod)
#  outbaseNum = exp(predict(mod))
#  
#  mod = glm(totprior~jn1+as.factor(vaxXbb)+as.factor(vax45)+vaxMono+recomb+edPast+inptPast+char+outpt+sex+race+inc+as.factor(age)+smoke+inc+bmiCat+
#              paxPreEd+as.factor(week),
#            subset=sel==1,data=impDat[[i]],family='poisson'); #summary(mod)[[8]][1,c(1,3,4)]
#  
#  outbaseType = exp(predict(mod))
#  
#  genMult = c(1,1.25,1.5,2,3)
#  protMult = c(1,1.25,1.5,2,3)
#  protJN1 = c(1,1.25,1.5,2,3)
#  
#  for (j in 1:length(genMult)) for (k in 1:length(protMult)) for (l in 1:length(protJN1)){
#
#    yBad = yEd[sel]; badEnd = edEnd[sel]; endFollow = 14
#    yBad[yBad==1&badEnd>endFollow] = 0; badEnd[badEnd>endFollow] = endFollow
#    
#    for (m in 1:10){
#    
#      totPriorModNum = genMult[j]*outbaseNum
#      totPriorModNum[yBad==0] = totPriorModNum[yBad==0]*protMult[k]
#      totPriorModNum[jn1[sel]==1] = totPriorModNum[jn1[sel]==1]*protJN1[l]
#      totPriorModNum = rpois(length(totPriorModNum),totPriorModNum)
#      
#      totPriorModType = genMult[j]*outbaseType
#      totPriorModType[yBad==0] = totPriorModType[yBad==0]*protMult[k]
#      totPriorModType[jn1[sel]==1] = totPriorModType[jn1[sel]==1]*protJN1[l]
#      totPriorModType = rpois(length(totPriorModType),totPriorModType)
#  
#
#      
#      # by simple number of doses
#      mod = (clogit(jn1[sel]~as.factor(vaxvar[sel])+as.factor(totPriorModNum)+edPast[sel]+inptPast[sel]+char[sel]+
#                      outpt[sel]+sex[sel]+race[sel]+inc[sel]+as.factor(age[sel])+smoke[sel]+inc[sel]+bmiCat[sel]+
#                      strata(week[sel]),data=impDat[[i]],
#                    method='approx')) 
#
#      vals = mvrnorm(1e3,coef(mod),vcov(mod))[,1:10]  
#      sensParsNumVax[1,j,k,l,(m-1)*1e3+1:1e3,i,] = vals[,1:7]
#      #sensParsNumNat[1,j,k,l,(m-1)*1e3+1:1e3,i,] = vals[,8:10]
#      
#      # by type received
#      mod = (clogit(jn1[sel]~as.factor(vaxXbb[sel])+as.factor(vax45[sel])+as.factor(totPriorModType)+vaxMono[sel]+
#                      edPast[sel]+inptPast[sel]+char[sel]+outpt[sel]+sex[sel]+race[sel]+inc[sel]+as.factor(age[sel])+
#                      smoke[sel]+inc[sel]+bmiCat[sel]+strata(week[sel]),data=impDat[[i]],
#                    method='approx')) 
#      
#      vals = mvrnorm(1e3,coef(mod),vcov(mod))[,1:5]  
#      sensParsTypeVax[1,j,k,l,(m-1)*1e3+1:1e3,i,] = vals[,1:2]
#      #sensParsTypeNat[1,j,k,l,(m-1)*1e3+1:1e3,i,] = vals[,3:5]
#      
#    }
#    
#    yBad = yHosp[sel]; badEnd = hospEnd[sel]; endFollow = 28
#    yBad[yBad==1&badEnd>endFollow] = 0; badEnd[badEnd>endFollow] = endFollow
#    for (m in 1:10){
#
#      totPriorModNum = genMult[j]*outbaseNum
#      totPriorModNum[yBad==0] = totPriorModNum[yBad==0]*protMult[k]
#      totPriorModNum[jn1[sel]==1] = totPriorModNum[jn1[sel]==1]*protJN1[l]
#      totPriorModNum = rpois(length(totPriorModNum),totPriorModNum)
#      
#      totPriorModType = genMult[j]*outbaseType
#      totPriorModType[yBad==0] = totPriorModType[yBad==0]*protMult[k]
#      totPriorModType[jn1[sel]==1] = totPriorModType[jn1[sel]==1]*protJN1[l]
#      totPriorModType = rpois(length(totPriorModType),totPriorModType)
#      
#      # by simple number of doses
#      mod = (clogit(jn1[sel]~as.factor(vaxvar[sel])+as.factor(totPriorModNum)+edPast[sel]+inptPast[sel]+char[sel]+
#                      outpt[sel]+sex[sel]+race[sel]+inc[sel]+as.factor(age[sel])+smoke[sel]+inc[sel]+bmiCat[sel]+
#                      strata(week[sel]),data=impDat[[i]],
#                    method='approx')) 
#      
#      vals = mvrnorm(1e3,coef(mod),vcov(mod))[,1:10]  
#      sensParsNumVax[2,j,k,l,(m-1)*1e3+1:1e3,i,] = vals[,1:7]
#      #sensParsNumNat[2,j,k,l,(m-1)*1e3+1:1e3,i,] = vals[,8:10]
#      
#      # by type received
#      mod = (clogit(jn1[sel]~as.factor(vaxXbb[sel])+as.factor(vax45[sel])+as.factor(totPriorModType)+vaxMono[sel]+
#                      edPast[sel]+inptPast[sel]+char[sel]+outpt[sel]+sex[sel]+race[sel]+inc[sel]+as.factor(age[sel])+
#                      smoke[sel]+inc[sel]+bmiCat[sel]+strata(week[sel]),data=impDat[[i]],
#                    method='approx')) 
#      
#      vals = mvrnorm(1e3,coef(mod),vcov(mod))[,1:5]  
#      sensParsTypeVax[2,j,k,l,(m-1)*1e3+1:1e3,i,] = vals[,1:2]
#      #sensParsTypeNat[2,j,k,l,(m-1)*1e3+1:1e3,i,] = vals[,3:5]
#      
#    }
#    
#    print(c(i,j,k,l))
#  }
#}
#
##save(sensParsNumVax,file='sensParsNumVax_dis.Rdata')
##save(sensParsTypeVax,file='sensParsTypeVax_dis.Rdata')
#
##hist(sensParsTypeVax[1,5,5,5,,,1],xlim=c(-1,1)); abline(v=0,col='red')
##
##
load('sensParsNumVax_dis.Rdata')
load('sensParsTypeVax_dis.Rdata')


cols = c('black','forestgreen','darkslategray4','darkorchid4','darkorange')

pdf(file='sens fig escape A.pdf',width=6,height=7)

par(mar=c(3,2.25,2,0.5)); par(mgp=c(3,0.3,0)); par(lwd=0.5); par(tck=-0.025)
layout(matrix(1:25,nrow=5,ncol=5,byrow=T))
for (i in 1:5){
  
  for (j in 1:5){
    plot(1,type='n',axes=F,ann=F,ylim=c(-0.1,0.5),xlim=c(0.5,5.5))
    if (i==1){
      polygon(x=c(-1e3,1e3,1e3,-1e3),y=log10(c(0.31,0.31,0.68,0.68)),col=rgb(0.5,0.49,0.51,0.25),lty=0)
      if (j==1){
        text(expression(theta==1),adj=0,y=0.5,x=2,cex=0.65,xpd=T)
        text(expression(theta==1.25),adj=0,y=0.43,x=2,cex=0.65,xpd=T)
        text(expression(theta==1.5),adj=0,y=0.36,x=2,cex=0.65,xpd=T)
        text(expression(theta==2),adj=0,y=0.29,x=2,cex=0.65,xpd=T)
        text(expression(theta==3),adj=0,y=0.22,x=2,cex=0.65,xpd=T)
        text('(for BA.2.86\nvs. others)',adj=0,y=0.45,x=3.7,cex=0.65,xpd=T)
        
        for (l in 1:5){
          lines(x=c(0.5,1.7),y=rep(0.5,2)-(l-1)*0.07,col=cols[l])
          points(x=1.1,y=0.5-(l-1)*0.07,col=cols[l],pch=16,cex=0.5)
        }
        
      }
    }
    
    if (i==1){
      sensPars = sensParsTypeVax[,,,,,,1]
      polygon(x=c(-1e3,1e3,1e3,-1e3),y=log10(c(1.01,1.01,1.28,1.28)),col=rgb(0.5,0.49,0.51,0.25),lty=0)
      if (j==1){  mtext(side=3,at=-1.5,'A. Association with previous receipt of XBB.1.5 (monovalent) vaccine',cex=0.5,font=2,adj=0,line=1) }
    } else{
      if (i==2){
        sensPars = sensParsTypeVax[,,,,,,2]
        polygon(x=c(-1e3,1e3,1e3,-1e3),y=log10(c(1.01,1.01,1.20,1.20)),col=rgb(0.5,0.49,0.51,0.25),lty=0)
        if (j==1){mtext(side=3,at=-1.5,'B. Association with previous receipt of BA.4/BA.5 (bivalent) vaccine',cex=0.5,font=2,adj=0,line=1)}
      } else{
        if (i==3){
          sensPars = sensParsNumVax[,,,,,,5]
          polygon(x=c(-1e3,1e3,1e3,-1e3),y=log10(c(1.20,1.20,1.71,1.71)),col=rgb(0.5,0.49,0.51,0.25),lty=0)
          if (j==1){mtext(side=3,at=-1.5,'C. Association with previous receipt of 5 COVID-19 vaccine doses (any type)',cex=0.5,font=2,adj=0,line=1)}
        } else{
          if (i==4){
            sensPars = sensParsNumVax[,,,,,,6]
            polygon(x=c(-1e3,1e3,1e3,-1e3),y=log10(c(1.28,1.28,1.91,1.91)),col=rgb(0.5,0.49,0.51,0.25),lty=0)
            if (j==1){mtext(side=3,at=-1.5,'D. Association with previous receipt of 6 COVID-19 vaccine doses (any type)',cex=0.5,font=2,adj=0,line=1)}
          } else{
            if (i==5){
              sensPars = sensParsNumVax[,,,,,,7]
              polygon(x=c(-1e3,1e3,1e3,-1e3),y=log10(c(1.16,1.16,2.45,2.45)),col=rgb(0.5,0.49,0.51,0.25),lty=0)
              if (j==1){mtext(side=3,at=-1.5,'E. Association with previous receipt of 7 or more COVID-19 vaccine doses (any type)',cex=0.5,font=2,adj=0,line=1)}
            }
          }
        }
      }
    }
    
    
    abline(h=0,col='red')
    count = 0
    for (k in c(1:5)){
      count = count+1
      for (l in 1:5){
        lines(x=rep(count,2)+(l-1)*0.15,y=log10(exp(quantile(sensPars[1,j,k,l,,],c(0.025,0.975)))),col=cols[l],xpd=T)
        points(x=count+(l-1)*0.15,y=log10(exp(quantile(sensPars[1,j,k,l,,],0.5))),pch=16,cex=0.5,col=cols[l],xpd=T)
      }
    }
    
    box(bty='l')
    axis(side=2,at=log10(c(0.8,0.9,1,2,3)),cex.axis=0.65,labels=c(0.8,0.9,1,2,3),lwd=0,lwd.ticks=0.5,las=1)
    axis(side=2,at=log10(c(seq(0.8,3,0.1))),labels=NA,lwd=0,lwd.ticks=0.5,tck=-0.02)
    #axis(side=2,at=log10(c(seq(2,9,1),seq(20,90,10))),labels=NA,lwd=0,lwd.ticks=0.5,tck=-0.02)
    axis(side=1,at=1:5,labels=NA,lwd=0,lwd.ticks=0.5)
    text(x=1:5,y=-0.17,c(1,1.25,1.5,2,3),adj=1,xpd=T,srt=45,cex=0.65)
    
    mtext(side=2,'Revised aOR',cex=0.5,line=1.25)
    mtext(side=1,expression(rho),cex=0.5,line=1.25,font=3)
    mtext(side=1,'(with outcome vs. without)',cex=0.45,line=1.85)
    
    if (j==1){
      mtext(side=3,at=0,expression(paste('(i) ',omega==1,' (for all)',sep='')),line=0.15,adj=0,cex=0.45)   
      
    }
    if (j==2){
      mtext(side=3,at=0,expression(paste('(ii) ',omega==1.25,' (for all)',sep='')),line=0.15,adj=0,cex=0.45)      
    }
    if (j==3){
      mtext(side=3,at=0,expression(paste('(iii) ',omega==1.5,' (for all)',sep='')),line=0.15,adj=0,cex=0.45)      
    }
    if (j==4){
      mtext(side=3,at=0,expression(paste('(iv) ',omega==2,' (for all)',sep='')),line=0.15,adj=0,cex=0.45)      
    }
    if (j==5){
      mtext(side=3,at=0,expression(paste('(v) ',omega==3,' (for all)',sep='')),line=0.15,adj=0,cex=0.45)      
    }
    
  }
}

dev.off()







cols = c('black','forestgreen','darkslategray4','darkorchid4','darkorange')

pdf(file='sens fig escape B.pdf',width=6,height=7)

par(mar=c(3,2.25,2,0.5)); par(mgp=c(3,0.3,0)); par(lwd=0.5); par(tck=-0.025)
layout(matrix(1:25,nrow=5,ncol=5,byrow=T))
for (i in 1:5){
  
  for (j in 1:5){
    plot(1,type='n',axes=F,ann=F,ylim=c(-0.1,0.5),xlim=c(0.5,5.5))
    if (i==1){
      polygon(x=c(-1e3,1e3,1e3,-1e3),y=log10(c(0.31,0.31,0.68,0.68)),col=rgb(0.5,0.49,0.51,0.25),lty=0)
      if (j==1){
        text(expression(theta==1),adj=0,y=0.5,x=2,cex=0.65,xpd=T)
        text(expression(theta==1.25),adj=0,y=0.43,x=2,cex=0.65,xpd=T)
        text(expression(theta==1.5),adj=0,y=0.36,x=2,cex=0.65,xpd=T)
        text(expression(theta==2),adj=0,y=0.29,x=2,cex=0.65,xpd=T)
        text(expression(theta==3),adj=0,y=0.22,x=2,cex=0.65,xpd=T)
        text('(for BA.2.86\nvs. others)',adj=0,y=0.45,x=3.7,cex=0.65,xpd=T)
        
        for (l in 1:5){
          lines(x=c(0.5,1.7),y=rep(0.5,2)-(l-1)*0.07,col=cols[l])
          points(x=1.1,y=0.5-(l-1)*0.07,col=cols[l],pch=16,cex=0.5)
        }
        
      }
    }
    
    if (i==1){
      sensPars = sensParsTypeVax[,,,,,,1]
      polygon(x=c(-1e3,1e3,1e3,-1e3),y=log10(c(1.01,1.01,1.28,1.28)),col=rgb(0.5,0.49,0.51,0.25),lty=0)
      if (j==1){  mtext(side=3,at=-1.5,'A. Association with previous receipt of XBB.1.5 (monovalent) vaccine',cex=0.5,font=2,adj=0,line=1) }
    } else{
      if (i==2){
        sensPars = sensParsTypeVax[,,,,,,2]
        polygon(x=c(-1e3,1e3,1e3,-1e3),y=log10(c(1.01,1.01,1.20,1.20)),col=rgb(0.5,0.49,0.51,0.25),lty=0)
        if (j==1){mtext(side=3,at=-1.5,'B. Association with previous receipt of BA.4/BA.5 (bivalent) vaccine',cex=0.5,font=2,adj=0,line=1)}
      } else{
        if (i==3){
          sensPars = sensParsNumVax[,,,,,,5]
          polygon(x=c(-1e3,1e3,1e3,-1e3),y=log10(c(1.20,1.20,1.71,1.71)),col=rgb(0.5,0.49,0.51,0.25),lty=0)
          if (j==1){mtext(side=3,at=-1.5,'C. Association with previous receipt of 5 COVID-19 vaccine doses (any type)',cex=0.5,font=2,adj=0,line=1)}
        } else{
          if (i==4){
            sensPars = sensParsNumVax[,,,,,,6]
            polygon(x=c(-1e3,1e3,1e3,-1e3),y=log10(c(1.28,1.28,1.91,1.91)),col=rgb(0.5,0.49,0.51,0.25),lty=0)
            if (j==1){mtext(side=3,at=-1.5,'D. Association with previous receipt of 6 COVID-19 vaccine doses (any type)',cex=0.5,font=2,adj=0,line=1)}
          } else{
            if (i==5){
              sensPars = sensParsNumVax[,,,,,,7]
              polygon(x=c(-1e3,1e3,1e3,-1e3),y=log10(c(1.16,1.16,2.45,2.45)),col=rgb(0.5,0.49,0.51,0.25),lty=0)
              if (j==1){mtext(side=3,at=-1.5,'E. Association with previous receipt of 7 or more COVID-19 vaccine doses (any type)',cex=0.5,font=2,adj=0,line=1)}
            }
          }
        }
      }
    }
    
    
    abline(h=0,col='red')
    count = 0
    for (k in c(1:5)){
      count = count+1
      for (l in 1:5){
        lines(x=rep(count,2)+(l-1)*0.15,y=log10(exp(quantile(sensPars[2,j,k,l,,],c(0.025,0.975)))),col=cols[l],xpd=T)
        points(x=count+(l-1)*0.15,y=log10(exp(quantile(sensPars[2,j,k,l,,],0.5))),pch=16,cex=0.5,col=cols[l],xpd=T)
      }
    }
    
    box(bty='l')
    axis(side=2,at=log10(c(0.8,0.9,1,2,3)),cex.axis=0.65,labels=c(0.8,0.9,1,2,3),lwd=0,lwd.ticks=0.5,las=1)
    axis(side=2,at=log10(c(seq(0.8,3,0.1))),labels=NA,lwd=0,lwd.ticks=0.5,tck=-0.02)
    #axis(side=2,at=log10(c(seq(2,9,1),seq(20,90,10))),labels=NA,lwd=0,lwd.ticks=0.5,tck=-0.02)
    axis(side=1,at=1:5,labels=NA,lwd=0,lwd.ticks=0.5)
    text(x=1:5,y=-0.17,c(1,1.25,1.5,2,3),adj=1,xpd=T,srt=45,cex=0.65)
    
    mtext(side=2,'Revised aOR',cex=0.5,line=1.25)
    mtext(side=1,expression(rho),cex=0.5,line=1.25,font=3)
    mtext(side=1,'(with outcome vs. without)',cex=0.45,line=1.85)
    
    if (j==1){
      mtext(side=3,at=0,expression(paste('(i) ',omega==1,' (for all)',sep='')),line=0.15,adj=0,cex=0.45)   
      
    }
    if (j==2){
      mtext(side=3,at=0,expression(paste('(ii) ',omega==1.25,' (for all)',sep='')),line=0.15,adj=0,cex=0.45)      
    }
    if (j==3){
      mtext(side=3,at=0,expression(paste('(iii) ',omega==1.5,' (for all)',sep='')),line=0.15,adj=0,cex=0.45)      
    }
    if (j==4){
      mtext(side=3,at=0,expression(paste('(iv) ',omega==2,' (for all)',sep='')),line=0.15,adj=0,cex=0.45)      
    }
    if (j==5){
      mtext(side=3,at=0,expression(paste('(v) ',omega==3,' (for all)',sep='')),line=0.15,adj=0,cex=0.45)      
    }
    
  }
}

dev.off()
