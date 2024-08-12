
setwd("~/Library/CloudStorage/GoogleDrive-joelewnard@gmail.com/My Drive/CFA epi/JN1")
#setwd('~/Google drive (jlewnard@berkeley.edu)/CAP KP study/ba4ba5')
#install.packages("survival")
#install.packages("haven");
#install.packages("gdata")
#install.packages("lubridate")
library(haven)
library(gdata)
library(lubridate)
library(survival)
library(MASS)


dat = read_sas("/Users/joelewnard/Library/CloudStorage/GoogleDrive-joelewnard@gmail.com/My Drive/CFA epi/JN1/jn1_20240130.sas7bdat")

tHosp1 = dat$hosp1_days
tHosp2 = dat$hosp2_days
tHosp3 = dat$hosp3_days

hospAri1 = dat$hosp_ari1
hospAri2 = dat$hosp_ari2
hospAri3 = dat$hosp_ari3

hospAri = rep(0,dim(dat)[1]); tHospAri = rep(28,dim(dat)[1])
sel = which(hospAri1==1); hospAri[sel] = 1; tHospAri[sel] = tHosp1[sel]
sel = which(hospAri2==1); hospAri[sel] = 1; tHospAri[sel] = tHosp2[sel]
sel = which(hospAri3==1); hospAri[sel] = 1; tHospAri[sel] = tHosp3[sel]
sel = which(tHospAri>28); hospAri[sel] = 0; tHospAri[sel] = 28

tEd1 = dat$ed1_days
tEd2 = dat$ed2_days
tEd3 = dat$ed3_days

edAri1 = dat$ed_ari1
edAri2 = dat$ed_ari2
edAri3 = dat$ed_ari3

edAri = rep(0,dim(dat)[1]); tEdAri = rep(28,dim(dat)[1])
sel = which(edAri1==1); edAri[sel] = 1; tEdAri[sel] = tEd1[sel]
sel = which(edAri2==1); edAri[sel] = 1; tEdAri[sel] = tEd2[sel]
sel = which(edAri3==1); edAri[sel] = 1; tEdAri[sel] = tEd3[sel]
sel = which(tEdAri>28); edAri[sel] = 0; tEdAri[sel] = 28

ed = dat$ed1_days<=28; tEd = dat$ed1_days; tEd[tEd>28] = 28; ed[is.na(ed)] = 0; tEd[is.na(tEd)] = 28
hosp = dat$hosp1_days<=28; tHosp = dat$hosp1_days; tHosp[tHosp>28] = 28; hosp[is.na(hosp)] = 0; tHosp[is.na(tHosp)] = 28

tDeath = dat$death_days
death = is.na(tDeath)==F

tDx = ymd(dat$index_date_s)
tEnd = as.numeric(ymd('2024-01-17')-tDx)

tEnd[tEnd<=0] = 1

tEnd[which(dat$disenroll_days<tEnd)] = dat$disenroll_days[which(dat$disenroll_days<tEnd)]
tEnd[which(tDeath<tEnd)] = tDeath[which(tDeath<tEnd)]

sel = which(tEnd<tEdAri);  tEdAri[sel] = tEnd[sel]; edAri[sel] = 0;
sel = which(tEnd<tEd); tEd[sel] = tEnd[sel]; ed[sel] = 0
sel = which(tEnd<tHospAri); tHospAri[sel] = tEnd[sel]; hospAri[sel] = 0
sel = which(tEnd<tHosp);  tHosp[sel] = tEnd[sel]; hosp[sel] = 0

sgtf = dat$SGTF; jn1 = sgtf==1
date = ymd(dat$index_date_s); week = floor(as.numeric(date)/7)

sel = dat$TF_lab==1&dat$Enrolled_1yr==1#&date>=ymd('2024-12-01')

dates = sort(unique(date))
prop = c()
for (i in 1:length(dates)){
  prop[i] = mean(sgtf[sel==1&date==dates[i]])
}

plot(prop[2:75],ylab='SGTF proportion',xlab='Days from Nov 1')

hist(as.numeric(date[sel]))

agelb = seq(0,80,10); ageub = seq(10,90,10); ageub[length(ageub)] = 1e3
age = rep(NA,dim(dat)[1])
for (i in 1:length(agelb)){
  age[dat$age>=agelb[i]&dat$age<ageub[i]] = i
}
age[age==6] = '0'

sex = dat$gender; sex[sex%in%c('','F')] = '0'
race = dat$RACE_ETH; race[race=='White'] = '0'; race[race%in%c('','Multiple','Native Am Alaskan','Other','Unknown')] = 'Oth'
totvax = (is.na(dat$covid_vac_days1)==F)+(is.na(dat$covid_vac_days2)==F)+(is.na(dat$covid_vac_days3)==F)+(is.na(dat$covid_vac_days4)==F)+(is.na(dat$covid_vac_days5)==F)+(is.na(dat$covid_vac_days6)==F)+(is.na(dat$covid_vac_days7)==F)+(is.na(dat$covid_vac_days8)==F)+(is.na(dat$covid_vac_days9)==F)+(is.na(dat$covid_vac_days10)==F)+(is.na(dat$covid_vac_days11)==F)+(is.na(dat$covid_vac_days12)==F)
recomb = dat$COVID_VAC_NAME1%in%c('Moderna bivalent','Moderna XBB','Novavax XBB','Pfizer bivalent','Pfizer XBB')
recomb = recomb+dat$COVID_VAC_NAME2%in%c('Moderna bivalent','Moderna XBB','Novavax XBB','Pfizer bivalent','Pfizer XBB')
recomb = recomb+dat$COVID_VAC_NAME3%in%c('Moderna bivalent','Moderna XBB','Novavax XBB','Pfizer bivalent','Pfizer XBB')
recomb = recomb+dat$COVID_VAC_NAME4%in%c('Moderna bivalent','Moderna XBB','Novavax XBB','Pfizer bivalent','Pfizer XBB')
recomb = recomb+dat$COVID_VAC_NAME5%in%c('Moderna bivalent','Moderna XBB','Novavax XBB','Pfizer bivalent','Pfizer XBB')
recomb = recomb+dat$COVID_VAC_NAME6%in%c('Moderna bivalent','Moderna XBB','Novavax XBB','Pfizer bivalent','Pfizer XBB')
recomb = recomb+dat$COVID_VAC_NAME7%in%c('Moderna bivalent','Moderna XBB','Novavax XBB','Pfizer bivalent','Pfizer XBB')
recomb = recomb+dat$COVID_VAC_NAME8%in%c('Moderna bivalent','Moderna XBB','Novavax XBB','Pfizer bivalent','Pfizer XBB')
recomb = recomb+dat$COVID_VAC_NAME9%in%c('Moderna bivalent','Moderna XBB','Novavax XBB','Pfizer bivalent','Pfizer XBB')
recomb = recomb+dat$COVID_VAC_NAME10%in%c('Moderna bivalent','Moderna XBB','Novavax XBB','Pfizer bivalent','Pfizer XBB')
recomb = recomb+dat$COVID_VAC_NAME11%in%c('Moderna bivalent','Moderna XBB','Novavax XBB','Pfizer bivalent','Pfizer XBB')
recomb = recomb+dat$COVID_VAC_NAME12%in%c('Moderna bivalent','Moderna XBB','Novavax XBB','Pfizer bivalent','Pfizer XBB')
recomb = recomb+dat$COVID_VAC_NAME13%in%c('Moderna bivalent','Moderna XBB','Novavax XBB','Pfizer bivalent','Pfizer XBB')
recomb = recomb>0

vax45 = dat$COVID_VAC_NAME1%in%c('Moderna bivalent','Pfizer bivalent')
vax45 = vax45 + dat$COVID_VAC_NAME2%in%c('Moderna bivalent','Pfizer bivalent')
vax45 = vax45 + dat$COVID_VAC_NAME3%in%c('Moderna bivalent','Pfizer bivalent')
vax45 = vax45 + dat$COVID_VAC_NAME4%in%c('Moderna bivalent','Pfizer bivalent')
vax45 = vax45 + dat$COVID_VAC_NAME5%in%c('Moderna bivalent','Pfizer bivalent')
vax45 = vax45 + dat$COVID_VAC_NAME6%in%c('Moderna bivalent','Pfizer bivalent')
vax45 = vax45 + dat$COVID_VAC_NAME7%in%c('Moderna bivalent','Pfizer bivalent')
vax45 = vax45 + dat$COVID_VAC_NAME8%in%c('Moderna bivalent','Pfizer bivalent')
vax45 = vax45 + dat$COVID_VAC_NAME9%in%c('Moderna bivalent','Pfizer bivalent')
vax45 = vax45 + dat$COVID_VAC_NAME10%in%c('Moderna bivalent','Pfizer bivalent')
vax45 = vax45 + dat$COVID_VAC_NAME11%in%c('Moderna bivalent','Pfizer bivalent')
vax45 = vax45 + dat$COVID_VAC_NAME12%in%c('Moderna bivalent','Pfizer bivalent')
vax45 = vax45 + dat$COVID_VAC_NAME13%in%c('Moderna bivalent','Pfizer bivalent')
vax45 = vax45>0

vaxXbb = dat$COVID_VAC_NAME1%in%c('Moderna XBB','Novavax XBB','Pfizer XBB')
vaxXbb = vaxXbb + dat$COVID_VAC_NAME2%in%c('Moderna XBB','Novavax XBB','Pfizer XBB')
vaxXbb = vaxXbb + dat$COVID_VAC_NAME3%in%c('Moderna XBB','Novavax XBB','Pfizer XBB')
vaxXbb = vaxXbb + dat$COVID_VAC_NAME4%in%c('Moderna XBB','Novavax XBB','Pfizer XBB')
vaxXbb = vaxXbb + dat$COVID_VAC_NAME5%in%c('Moderna XBB','Novavax XBB','Pfizer XBB')
vaxXbb = vaxXbb + dat$COVID_VAC_NAME6%in%c('Moderna XBB','Novavax XBB','Pfizer XBB')
vaxXbb = vaxXbb + dat$COVID_VAC_NAME7%in%c('Moderna XBB','Novavax XBB','Pfizer XBB')
vaxXbb = vaxXbb + dat$COVID_VAC_NAME8%in%c('Moderna XBB','Novavax XBB','Pfizer XBB')
vaxXbb = vaxXbb + dat$COVID_VAC_NAME9%in%c('Moderna XBB','Novavax XBB','Pfizer XBB')
vaxXbb = vaxXbb + dat$COVID_VAC_NAME10%in%c('Moderna XBB','Novavax XBB','Pfizer XBB')
vaxXbb = vaxXbb + dat$COVID_VAC_NAME11%in%c('Moderna XBB','Novavax XBB','Pfizer XBB')
vaxXbb = vaxXbb + dat$COVID_VAC_NAME12%in%c('Moderna XBB','Novavax XBB','Pfizer XBB')
vaxXbb = vaxXbb + dat$COVID_VAC_NAME13%in%c('Moderna XBB','Novavax XBB','Pfizer XBB')
vaxXbb = vaxXbb>0

vaxvar = totvax; vaxvar[totvax>=7] = '7'

prior = dat$prior_covid
totprior = (is.na(dat$covid_inf_days1)==F)+(is.na(dat$covid_inf_days2)==F)+(is.na(dat$covid_inf_days3)==F)+(is.na(dat$covid_inf_days4)==F)
totprior[totprior>3] = 3

char = rep(NA,dim(dat)[1])
char[dat$Charlson_Wt==0] = 0
char[dat$Charlson_Wt%in%1:2] = '1-2'
char[dat$Charlson_Wt%in%3:5] = '3-5'
char[dat$Charlson_Wt%in%6:1e3] = '6+'

outpt = rep(NA,dim(dat)[1])
outpt[dat$OUTPT%in%0:9] = '0: 0-9'
outpt[dat$OUTPT%in%10:19] = '1: 10-19'
outpt[dat$OUTPT%in%20:29] = '2: 20-29'
outpt[dat$OUTPT%in%30:1e3] = '3: 30+'

edPast = dat$ED>0; inptPast = dat$INPT>0; edInpt = edPast+inptPast; edInpt[edInpt>0] = 1

yEd = ed; edEnd = tEd
sel = which(yEd==0&hosp==1); yEd[sel] = 1; edEnd[sel] = tHosp[sel]
sel = which(yEd==0&death==1&tDeath<28); yEd[sel] = 1; edEnd[sel] = tDeath[sel]
edEnd[edEnd<=0] = 0.5

yEdAri = edAri; edAriEnd = tEdAri
sel = which(yEdAri==0&hospAri==1); yEdAri[sel] = 1; edAriEnd[sel] = tHospAri[sel]
sel = which(yEdAri==0&death==1&tDeath<28); yEdAri[sel] = 1; edAriEnd[sel] = tDeath[sel]
edAriEnd[edAriEnd<=0] = 0.5

yHosp = hosp; hospEnd = tHosp
sel = which(yHosp==0&death==1&tDeath<28); yHosp[sel] = 1; hospEnd[sel] = tDeath[sel]
hospEnd[hospEnd<=0] = 0.5

yHospAri = hospAri; hospAriEnd = tHospAri
sel = which(yHospAri==0&death==1&tDeath<28); yHospAri[sel] = 1; hospAriEnd[sel] = tDeath[sel]
hospAriEnd[hospAriEnd<=0] = 0.5

icu = rep(0,dim(dat)[1]); icu[which(dat$ed_icu1_days<=90|dat$hosp_icu1_days<=90)] = 1
tIcuA = dat$ed_icu1_days; tIcuB = dat$hosp_icu1_days; tIcuA[is.na(tIcuA)] = 1e9; tIcuB[is.na(tIcuB)] = 1e9
tIcu = tEnd; sel = which(icu==1); tIcu[sel] = tIcuA[sel]; sel = which(tIcuB<tIcuA); tIcu[sel] = tIcuB[sel]
tIcu[tIcu>90] = 90

vent = rep(0,dim(dat)[1]); vent[which(dat$ed_vent1_days<=90|dat$hosp_vent1_days<=90)] = 1
tVentA = dat$ed_vent1_days; tVentB = dat$hosp_vent1_days; tVentA[is.na(tVentA)] = 1e9; tVentB[is.na(tVentB)] = 1e9
tVent = tEnd; sel = which(vent==1); tVent[sel] = tVentA[sel]; sel = which(tVentB<tVentA); tVent[sel] = tVentB[sel]
tVent[tVent>90] = 90

tDeath = tEnd; yDeath = death

ySevere = icu|vent|death; tSevere = tEnd
for (i in which(ySevere==1)){
  tSevere[i] = min(c(tIcu[i],tVent[i],tDeath[i]))
}
severeEnd = tSevere



###### include if people have more severe outcome than that analyzed

sel = which(tSevere<tEd)
yEd[sel] = 1; edEnd[sel] = severeEnd[sel]
sel = which(tSevere<tEdAri)
yEdAri[sel] = 1; edAriEnd[sel] = severeEnd[sel]
sel = which(tSevere<tHosp)
yHosp[sel] = 1; hospEnd[sel] = severeEnd[sel]
sel = which(tSevere<tHospAri)
yHospAri[sel] = 1; hospAriEnd[sel] = severeEnd[sel]

sel = which(hospEnd<tEd)
yEd[sel] = 1; edEnd[sel] = hospEnd[sel]


###### update edARI and hospARI to censor at hosp for other cause, and include vent/icu/death

sel = which(hospEnd<tEdAri)
yEdAri[sel] = 0; edAriEnd[sel] = hospEnd[sel]

sel = which(hospEnd<tHospAri)
yHospAri[sel] = 0; hospAriEnd[sel] = hospEnd[sel]

######## MULTIPLE IMPUTATION for missing data
#install.packages("Amelia")
library(Amelia)

names(dat)

bmiCat = rep(NA,dim(dat)[1]); bmiCat[dat$BMI<18.5] = 'UW'; bmiCat[dat$BMI>=18.5&dat$BMI<25] = '0'; bmiCat[dat$BMI>=25&dat$BMI<30] = 'Over'; bmiCat[dat$BMI>=30] = 'Obese'
inc = rep(NA,dim(dat)[1])
incLb = c(0,40e3,80e3,120e3,160e3); incUb = c(40e3,80e3,120e3,160e3,Inf)
for (i in 1:length(incLb)){
  inc[dat$medhinc>=incLb[i]&dat$medhinc<incUb[i]] = i
}
smoke = dat$smoking; smoke[smoke=='Unknown'] = NA

tmax = 1; #edit = badEnd>tmax&yBad==1; yBad[edit] = 0; badEnd[badEnd>tmax] = tmax
selA = dat$TF_lab==1&dat$Enrolled_1yr==1&date>=ymd('2023-12-01')&(date<=(ymd('2024-01-30')-tmax))#&badEnd>0

mean(is.na(age))
try = apply(forImp,1,is.na)

sumNA = apply(try,1,sum)
sum(apply(try,2,sum)>0)
dim(forImp)

sumNA/dim(forImp)[1]

forImp = data.frame(date=as.numeric(date),tEd,tEdAri,tHosp,tHospAri,tSevere,tDeath,
                    sgtf,age,sex,race,vaxvar,recomb,totprior,char,outpt,edPast,inptPast,
                    paxPreEd=is.na(dat$paxlovid_days)==F&dat$paxlovid_days<tEd&dat$paxlovid_days<=5,
                    paxPreEdAri=is.na(dat$paxlovid_days)==F&dat$paxlovid_days<tEdAri&dat$paxlovid_days<=5,
                    paxPreHosp=is.na(dat$paxlovid_days)==F&dat$paxlovid_days<tHosp&dat$paxlovid_days<=5,
                    paxPreHospAri=is.na(dat$paxlovid_days)==F&dat$paxlovid_days<tHospAri&dat$paxlovid_days<=5,
                    paxPreSevere=is.na(dat$paxlovid_days)==F&dat$paxlovid_days<tSevere&dat$paxlovid_days<=5,
                    yEd,yEdAri,yHosp,yHospAri,ySevere,yDeath,
                    bmiCat,smoke,inc,week,selA)#[selA,]

set.seed(2); tryImp = amelia(forImp,noms=8:dim(forImp)[2])
impDat = tryImp[[1]]

tInfect1 = date-dat$covid_inf_days1
tInfect2 = date-dat$covid_inf_days2
tInfect3 = date-dat$covid_inf_days3
tInfect4 = date-dat$covid_inf_days4
tInfect5 = date-dat$covid_inf_days5
wuhan = dmy('1-1-2020')+0:535; priorWuhan = ((tInfect1%in%wuhan)|(tInfect2%in%wuhan)|(tInfect3%in%wuhan)|(tInfect4%in%wuhan)|(tInfect5%in%wuhan))
#alpha = dmy('1-4-2021')+0:79; priorAlpha = ((tInfect1%in%alpha)|(tInfect2%in%alpha)|(tInfect3%in%alpha)|(tInfect4%in%alpha)|(tInfect5%in%alpha))
delta = dmy('20-6-2021')+0:182; priorDelta = ((tInfect1%in%delta)|(tInfect2%in%delta)|(tInfect3%in%delta)|(tInfect4%in%delta)|(tInfect5%in%delta))
ba1 = dmy('20-12-2021')+0:44; priorBa1 = ((tInfect1%in%ba1)|(tInfect2%in%ba1)|(tInfect3%in%ba1)|(tInfect4%in%ba1)|(tInfect5%in%ba1))
ba2 = dmy('3-2-2022')+0:141; priorBa2 = ((tInfect1%in%ba2)|(tInfect2%in%ba2)|(tInfect3%in%ba2)|(tInfect4%in%ba2)|(tInfect5%in%ba2))
ba45 = dmy('25-6-2022')+0:158; priorBa45 = ((tInfect1%in%ba45)|(tInfect2%in%ba45)|(tInfect3%in%ba45)|(tInfect4%in%ba45)|(tInfect5%in%ba45))
xbb = dmy('1-12-2022')+0:334; priorXbb = ((tInfect1%in%xbb)|(tInfect2%in%xbb)|(tInfect3%in%xbb)|(tInfect4%in%xbb)|(tInfect5%in%xbb))

lastDose3 = (dat$covid_vac_days1<90)|(dat$covid_vac_days2<90)|(dat$covid_vac_days3<90)|(dat$covid_vac_days4<90)|(dat$covid_vac_days5<90)|(dat$covid_vac_days6<90)|(dat$covid_vac_days7<90)|(dat$covid_vac_days8<90)|(dat$covid_vac_days9<90)|(dat$covid_vac_days10<90)|(dat$covid_vac_days11<90)|(dat$covid_vac_days12<90)|(dat$covid_vac_days13<90)
lastDose6 = (dat$covid_vac_days1%in%91:180)|(dat$covid_vac_days2%in%91:180)|(dat$covid_vac_days3%in%91:180)|(dat$covid_vac_days4%in%91:180)|(dat$covid_vac_days5%in%91:180)|(dat$covid_vac_days6%in%91:180)|(dat$covid_vac_days7%in%91:180)|(dat$covid_vac_days8%in%91:180)|(dat$covid_vac_days9%in%91:180)|(dat$covid_vac_days10%in%91:180)|(dat$covid_vac_days11%in%91:180)|(dat$covid_vac_days12%in%91:180)|(dat$covid_vac_days13%in%91:180)
lastDoseLong = (dat$covid_vac_days1>180)|(dat$covid_vac_days2>180)|(dat$covid_vac_days3>180)|(dat$covid_vac_days4>180)|(dat$covid_vac_days5>180)|(dat$covid_vac_days6>180)|(dat$covid_vac_days7>180)|(dat$covid_vac_days8>180)|(dat$covid_vac_days9>180)|(dat$covid_vac_days10>180)|(dat$covid_vac_days11>180)|(dat$covid_vac_days12>180)|(dat$covid_vac_days13>180)

sel = dat$COVID_VAC_NAME6%in%c('Pfizer XBB','Moderna XBB','Novavax XBB')
table(dat$index_date_s[sel] - dat$covid_vac_days6[sel])



lastDose3[is.na(lastDose3)] = 0
lastDose6[is.na(lastDose6)|lastDose3==1] = 0
lastDoseLong[is.na(lastDoseLong)|lastDose3==1|lastDose6==1] = 0

infectFirst = (dat$covid_inf_days1>dat$covid_vac_days1)|(is.na(dat$covid_inf_days1)==F&is.na(dat$covid_vac_days1))
infectFirst[is.na(infectFirst)] = 0
#vaxVirst = dat$covid_vac_days2>dat$covid_inf_days1

tlastVax = rep(NA,dim(dat)[1])
sel = is.na(dat$covid_vac_days1)==F; tlastVax[sel] = as.character(dat$index_date_s[sel] - dat$covid_vac_days1[sel])
sel = is.na(dat$covid_vac_days2)==F; tlastVax[sel] = as.character(dat$index_date_s[sel] - dat$covid_vac_days2[sel])
sel = is.na(dat$covid_vac_days3)==F; tlastVax[sel] = as.character(dat$index_date_s[sel] - dat$covid_vac_days3[sel])
sel = is.na(dat$covid_vac_days4)==F; tlastVax[sel] = as.character(dat$index_date_s[sel] - dat$covid_vac_days4[sel])
sel = is.na(dat$covid_vac_days5)==F; tlastVax[sel] = as.character(dat$index_date_s[sel] - dat$covid_vac_days5[sel])
sel = is.na(dat$covid_vac_days6)==F; tlastVax[sel] = as.character(dat$index_date_s[sel] - dat$covid_vac_days6[sel])
sel = is.na(dat$covid_vac_days7)==F; tlastVax[sel] = as.character(dat$index_date_s[sel] - dat$covid_vac_days7[sel])
sel = is.na(dat$covid_vac_days8)==F; tlastVax[sel] = as.character(dat$index_date_s[sel] - dat$covid_vac_days8[sel])
sel = is.na(dat$covid_vac_days9)==F; tlastVax[sel] = as.character(dat$index_date_s[sel] - dat$covid_vac_days9[sel])
sel = is.na(dat$covid_vac_days10)==F; tlastVax[sel] = as.character(dat$index_date_s[sel] - dat$covid_vac_days10[sel])
sel = is.na(dat$covid_vac_days11)==F; tlastVax[sel] = as.character(dat$index_date_s[sel] - dat$covid_vac_days11[sel])
sel = is.na(dat$covid_vac_days12)==F; tlastVax[sel] = as.character(dat$index_date_s[sel] - dat$covid_vac_days12[sel])
sel = is.na(dat$covid_vac_days13)==F; tlastVax[sel] = as.character(dat$index_date_s[sel] - dat$covid_vac_days13[sel])
tlastVax = ymd(tlastVax)

lastVaxLate = tlastVax>=ymd('2023-12-01')
table(tlastVax[vaxXbb==0&vax45==1])

preNovDose = rep(0,dim(dat)[1])
preNovDose[((dat$index_date_s-dat$covid_vac_days1)<ymd('2023-10-16'))] = 1
preNovDose[((dat$index_date_s-dat$covid_vac_days2)<ymd('2023-10-16'))] = 2
preNovDose[((dat$index_date_s-dat$covid_vac_days3)<ymd('2023-10-16'))] = 3
preNovDose[((dat$index_date_s-dat$covid_vac_days4)<ymd('2023-10-16'))] = 4
preNovDose[((dat$index_date_s-dat$covid_vac_days5)<ymd('2023-10-16'))] = 5
preNovDose[((dat$index_date_s-dat$covid_vac_days6)<ymd('2023-10-16'))] = 6
preNovDose[((dat$index_date_s-dat$covid_vac_days7)<ymd('2023-10-16'))] = 7
preNovDose[((dat$index_date_s-dat$covid_vac_days8)<ymd('2023-10-16'))] = 8
preNovDose[((dat$index_date_s-dat$covid_vac_days9)<ymd('2023-10-16'))] = 9
preNovDose[((dat$index_date_s-dat$covid_vac_days10)<ymd('2023-10-16'))] = 10
preNovDose[((dat$index_date_s-dat$covid_vac_days11)<ymd('2023-10-16'))] = 11
preNovDose[((dat$index_date_s-dat$covid_vac_days12)<ymd('2023-10-16'))] = 12
preNovDose[((dat$index_date_s-dat$covid_vac_days13)<ymd('2023-10-16'))] = 13

preNovInfect = rep(0,dim(dat)[1])
preNovInfect[(dat$index_date_s-dat$covid_inf_days1)<('2023-10-16')] = 1
preNovInfect[(dat$index_date_s-dat$covid_inf_days2)<('2023-10-16')] = 2
preNovInfect[(dat$index_date_s-dat$covid_inf_days3)<('2023-10-16')] = 3
preNovInfect[(dat$index_date_s-dat$covid_inf_days4)<('2023-10-16')] = 4
preNovInfect[(dat$index_date_s-dat$covid_inf_days5)<('2023-10-16')] = 5


#######################################
######## SGTF Analysis: severity  #####
#######################################

set.seed(1)
sel = dat$TF_lab==1&dat$Enrolled_1yr==1&date>=ymd('2023-12-01')&(date<=(ymd('2024-01-30')-tmax))#&dat$OUTPT>0#&totprior>0#vaxvar%in%as.character(4:10)#&badEnd>0

yBad = yHosp; badEnd = hospEnd; endFollow = 28
yBad[yBad==1&badEnd>endFollow] = 0; badEnd[badEnd>endFollow] = endFollow

sum(yBad[jn1==0&sel==1]); sum(jn1==0&sel==1); 1e4*sum(yBad[jn1==0&sel==1])/sum(as.numeric(badEnd[jn1==0&sel==1]))
sum(yBad[jn1==1&sel==1]); sum(jn1==1&sel==1); 1e4*sum(yBad[jn1==1&sel==1])/sum(as.numeric(badEnd[jn1==1&sel==1]))

#library(survival)
mod = coxph(Surv(time=badEnd,event=yBad)~jn1+strata(week),subset=sel); summary(mod)[[8]][c(1,3,4)]

sel = dat$Enrolled_1yr==1&date>=ymd('2023-12-01')&(date<=(ymd('2024-01-30')-tmax))#&dat$OUTPT>0#&totprior>0#vaxvar%in%as.character(4:10)#&badEnd>0
mod = coxph(Surv(time=badEnd,event=yBad)~as.factor(times),subset=sel);

pars = c(); set.seed(1)
for (i in 1:5){
  mod = coxph(Surv(time=badEnd,event=yBad)~jn1+vaxvar+recomb+as.factor(totprior)+edPast+inptPast+char+outpt+sex+race+inc+as.factor(age)+smoke+inc+bmiCat+
                paxPreHosp+strata(week),
               subset=sel==1,data=impDat[[i]]); #summary(mod)[[8]][1,c(1,3,4)]
  pars = c(pars,rnorm(1e5,coef(mod)[1],sqrt(vcov(mod)[1,1])))
}


for (i in 1:5){print(exp(quantile(pars[(i-1)*1e5+1:1e5],c(0.5,0.025,0.975))))}
exp(quantile(pars,c(0.5,0.025,0.975)))

table(jn1[sel])

#######################################
######## SGTF Analysis: severity  #####
#######################################

sel = dat$Enrolled_1yr==1&(date<=(ymd('2024-01-30')-tmax))#&dat$OUTPT>0#&totprior>0#vaxvar%in%as.character(4:10)#&badEnd>0

yBad = yEd; badEnd = edEnd; endFollow = 14
yBad[yBad==1&badEnd>endFollow] = 0; badEnd[badEnd>endFollow] = endFollow

#library(survival)
mod = coxph(Surv(time=badEnd,event=yBad)~as.factor(times),subset=sel); summary(mod)[[8]][1:4,c(1,3,4)]

set.seed(1)
parsEd = array(NA,dim=c(1e4,4,5)); set.seed(1)
#parsEdAri = array(NA,dim=c(1e4,4,5)); set.seed(1)
#parsHosp = array(NA,dim=c(1e4,4,5)); set.seed(1)
#parsHospAri = array(NA,dim=c(1e4,4,5)); set.seed(1)
for (i in 1:5){
  mod = coxph(Surv(time=badEnd,event=yBad)~as.factor(times)+vaxvar+recomb+totprior+edPast+inptPast+char+outpt+sex+race+inc+as.factor(age)+smoke+inc+bmiCat+
                paxPreEd,
              subset=sel==1,data=impDat[[i]]); #summary(mod)[[8]][1,c(1,3,4)]
  parsEd[,,i] = mvrnorm(1e4,coef(mod),vcov(mod))[,1:4]
  #parsEdAri[,,i] = mvrnorm(1e4,coef(mod),vcov(mod))[,1:4]
  #parsHosp[,,i] = mvrnorm(1e4,coef(mod),vcov(mod))[,1:4]
  #parsHospAri[,,i] = mvrnorm(1e4,coef(mod),vcov(mod))[,1:4]
}


parsSev = list(parsEd,parsHosp,parsEdAri,parsHospAri)
parsSevSens = parsSev

#save(parsSev,file='parsSev.Rdata')
##########################################
######## SGTF Analysis: odds ratios  #####
##########################################

datemin = ymd('24-01-16'); datemax = ymd('24-01-31')
sum(jn1[sel&date>=datemin&date<=datemax])
sum(sel&date>=datemin&date<=datemax)
mean(jn1[sel&date>=datemin&date<=datemax])

tmax = 1; #edit = badEnd>tmax&yBad==1; yBad[edit] = 0; badEnd[badEnd>tmax] = tmax

vax2wk = rep(0,dim(dat)[1]); vax2wk[(dat$index_date_s-tlastVax)<=14] = 1
vaxLate = rep(0,dim(dat)[1]); vaxLate[tlastVax>ymd('2023-11-30')] = 1
sel = dat$TF_lab==1&dat$Enrolled_1yr==1&date>=ymd('2023-12-01')&(date<=(ymd('2024-01-30')-tmax))#&totprior>1#&badEnd>0

table(recomb=recomb[sel],jn1=jn1[sel])
#sel[vaxLate==1] = 0
sel[vax2wk==1] = 0

c(3096,1518)/4614; c(1927,1153)/3080
median(dat$age[sel==1&jn1==1])-median(dat$age[sel==1&jn1==0])

vaxvar5 = vaxvar; vaxvar5[vaxvar%in%c('5','6','7')] = '5'
vaxvar4 = vaxvar; vaxvar4[vaxvar%in%c('4','5','6','7')] = '4'
table(jn1[sel])
table(vaxvar5)
table(vaxXbb[sel],jn1[sel])
round(100*table(vaxXbb[sel],jn1[sel])[,1]/sum(jn1[sel]==0),1)
round(100*table(vaxXbb[sel],jn1[sel])[,2]/sum(jn1[sel]==1),1)


vaxXbbMod = vaxXbb; vaxXbbMod[recomb==1&vaxXbb==0] = NA
vax45Mod = vax45; vax45Mod[recomb==1&vax45==0] = NA

mod = (clogit(jn1~as.factor(vaxOmicron),subset=sel==1,
              method='approx'))
summary(mod)
table(vaxOmicron[sel],jn1[sel])
vaxOmicron = vaxXbb+vax45
vaxMono = totvax-(vaxXbb+vax45); vaxMono[vaxMono>7] = '7'
prevax = as.numeric(vaxvar)-1; prevax[prevax<0] = 0

vaxType = vaxOmicron; vaxType[vaxXbb==1&vax45==0] = 'xbb'; vaxType[vax45==1&vaxXbb==0] = '45'

set.seed(1); pars = array(NA,dim=c(1e5,7,5))
for (i in 1:5){

  ## simple number of doses
  #  mod = (clogit(jn1~as.factor(vaxvar)+as.factor(totprior)+edPast+inptPast+char+outpt+sex+race+inc+as.factor(age)+smoke+inc+bmiCat+strata(week),
  #                 subset=(sel==1),data=impDat[[i]],method='approx')) 

  ## by type received
 #     mod = (clogit(jn1~as.factor(vaxXbb)+as.factor(vax45)+vaxMono+as.factor(totprior)+edPast+inptPast+char+outpt+sex+race+inc+as.factor(age)+smoke+inc+bmiCat+strata(week),
 #                 subset=(sel==1),data=impDat[[i]],method='approx')) 

  ### by any omicron
 #   mod = (clogit(jn1~as.factor(vaxOmicron>0)+vaxMono+as.factor(totprior)+edPast+inptPast+char+outpt+sex+race+inc+as.factor(age)+smoke+inc+bmiCat+strata(week),
 #               subset=(sel==1),data=impDat[[i]],method='approx'))
  
  ## by num omicron
   #  mod = (clogit(jn1~as.factor(vaxOmicron)+vaxMono+as.factor(totprior)+edPast+inptPast+char+outpt+sex+race+inc+as.factor(age)+smoke+inc+bmiCat+strata(week),
   #              subset=(sel==1),data=impDat[[i]],method='approx'))
  
  
  ### by dose timing
  #mod = (clogit(jn1~lastDose3+lastDose6+lastDoseLong+as.numeric(vaxvar)+as.factor(totprior)+edPast+inptPast+char+outpt+sex+race+inc+as.factor(age)+smoke+inc+bmiCat+strata(week),
  #              subset=(sel==1),data=impDat[[i]],method='approx')) 
  
  ### any prior infect
  #     mod = (clogit(jn1~as.factor(totprior>0)+as.factor(vaxvar)+edPast+inptPast+char+outpt+sex+race+inc+as.factor(age)+smoke+inc+bmiCat+strata(week),
  #                    subset=(sel==1),data=impDat[[i]],method='approx')) 
  
  #### num prior infect
     mod = (clogit(jn1~as.factor(totprior)+as.factor(vaxvar)+edPast+inptPast+char+outpt+sex+race+inc+as.factor(age)+smoke+inc+bmiCat+strata(week),
                    subset=(sel==1),data=impDat[[i]],method='approx')) 
  
  
  pars[,,i] = mvrnorm(1e5,coef(mod),vcov(mod))[,1:7]
}

#for (i in 1:3){
#  print(exp(quantile(pars[,i,],c(0.5,0.025,0.975))))
#}


###### specific periods/variants analysis

tmax = 1; #edit = badEnd>tmax&yBad==1; yBad[edit] = 0; badEnd[badEnd>tmax] = tmax
sel = dat$TF_lab==1&dat$Enrolled_1yr==1&date>=ymd('2023-12-01')&(date<=(ymd('2024-01-30')-tmax))#&totprior>0#&badEnd>0

var = priorXbb
table(var[sel],jn1[sel])
round(100*table(var[sel],jn1[sel])[,1]/sum(jn1[sel]==0),1)
round(100*table(var[sel],jn1[sel])[,2]/sum(jn1[sel]==1),1)

othPrior = totprior-var

mod = (clogit(jn1~var+strata(week),subset=sel==1,
              method='approx'))
summary(mod)

set.seed(1); pars = c()
for (i in 1:5){
  mod = (clogit(jn1~var+vaxvar+recomb+as.factor(othPrior)+edPast+inptPast+char+outpt+sex+race+inc+as.factor(age)+smoke+inc+bmiCat+strata(week),
                subset=(sel==1),data=impDat[[i]],method='approx')) 
  pars = c(pars,mvrnorm(1e5,coef(mod),vcov(mod))[,1])
}

quantile(exp(pars),c(0.5,0.025,0.975))

c(378,102,465,267,414,264)/1574
#################################################################
######## SGTF Analysis: odds ratios BASED ON TIME not sgtf  #####
#################################################################

month = months(date)
month[month%in%c('October','November')] = '0'

tmax = 1; #edit = badEnd>tmax&yBad==1; yBad[edit] = 0; badEnd[badEnd>tmax] = tmax
sel = dat$Enrolled_1yr==1&(date<=(ymd('2024-01-30')-tmax))#


table(jn1[sel])
table(vaxvar5[sel],jn1[sel])
round(100*table(vaxvar5[sel],jn1[sel])[,1]/sum(jn1[sel]==0),1)
round(100*table(vaxvar5[sel],jn1[sel])[,2]/sum(jn1[sel]==1),1)

monthA = month; monthA[month=='January'] = NA; monthA[month=='December'] = '1'; monthA = as.numeric(monthA)
monthB = month; monthB[month=='December'] = NA; monthB[month=='January'] = '1'; monthB = as.numeric(monthB)

times = date>=ymd('2023-12-01')
times[date>=ymd('2023-12-01')&date<ymd('2023-12-16')] = 1
times[date>=ymd('2023-12-16')&date<ymd('2024-01-01')] = 2
times[date>=ymd('2024-01-01')&date<ymd('2024-01-16')] = 3
times[date>=ymd('2024-01-16')&date<ymd('2024-02-01')] = 4
times1 = times==1; times1[times>1] = NA
times2 = times==2; times2[times%in%c(1,3,4)] = NA
times3 = times==3; times3[times%in%c(1,2,4)] = NA
times4 = times==4; times4[times%in%c(1,3,2)] = NA

mod = (clogit(monthB~vaxvarNov,subset=(sel==1),
              method='approx'))
summary(mod)

vaxvarNov = preNovDose; vaxvarNov[vaxvarNov>7] = '7'; vaxvarNov = as.character(vaxvarNov)
infectNov = preNovInfect; infectNov[infectNov>3] = '3'; infectNov = as.character(infectNov)

vaxvarMonoNov = totvax-(vaxXbb+vax45); vaxvarMonoNov[vaxvarMonoNov%in%c('5','6','7','8','9','10')] = '5'
table(vaxvarMonoNov)


set.seed(1); pars1 = pars2 = pars3 = pars4 = array(NA,dim=c(1e5,10,5))
pars1type = pars2type = pars3type = pars4type = array(NA,dim=c(1e5,2,5))

periodA = times# rep(NA,length(times)); periodA[times==1] = 0; periodA[times==4] = 1
periodA[tlastVax>=ymd('2023-10-15')] = NA

periodA1 = periodA==1; periodA1[periodA>1] = NA
periodA2 = periodA==2; periodA2[periodA%in%c(1,3,4)] = NA
periodA3 = periodA==3; periodA3[periodA%in%c(1,2,4)] = NA
periodA4 = periodA==4; periodA4[periodA%in%c(1,2,3)] = NA

for (i in 1:5){
  mod = (clogit(times1~vaxvarNov+as.factor(infectNov)+edPast+inptPast+char+outpt+sex+race+inc+as.factor(age)+smoke+inc+bmiCat,#+strata(week),
                subset=(sel==1),data=impDat[[i]],method='approx')) 
  pars1[,,i] = mvrnorm(1e5,coef(mod),vcov(mod))[,1:10]

 #mod = (clogit(periodA1~vax45+vaxvarMonoNov+as.factor(infectNov)+edPast+inptPast+char+outpt+sex+race+inc+as.factor(age)+smoke+inc+bmiCat,#+strata(week),
 #              subset=(sel==1),data=impDat[[i]],method='approx')) 
 #pars1type[,,i] = mvrnorm(1e5,coef(mod),vcov(mod))[,1:2]
 #mod = (clogit(periodA1~vax45+vaxvar5+as.factor(totprior)+edPast+inptPast+char+outpt+sex+race+inc+as.factor(age)+smoke+inc+bmiCat,#+strata(week),
 #              subset=(sel==1),data=impDat[[i]],method='approx')) 
 #pars1type[,2,i] = mvrnorm(1e5,coef(mod),vcov(mod))[,2]
  
  
  
  mod = (clogit(times2~vaxvarNov+as.factor(infectNov)+edPast+inptPast+char+outpt+sex+race+inc+as.factor(age)+smoke+inc+bmiCat,#+strata(week),
                subset=(sel==1),data=impDat[[i]],method='approx')) 
  pars2[,,i] = mvrnorm(1e5,coef(mod),vcov(mod))[,1:10]
  
#mod = (clogit(periodA2~vaxXbb+vax45+vaxvarMonoNov+as.factor(infectNov)+edPast+inptPast+char+outpt+sex+race+inc+as.factor(age)+smoke+inc+bmiCat,#+strata(week),
#              subset=(sel==1),data=impDat[[i]],method='approx')) 
#pars2type[,,i] = mvrnorm(1e5,coef(mod),vcov(mod))[,1:2]
#mod = (clogit(periodA2~vax45+vaxvar5+as.factor(totprior)+edPast+inptPast+char+outpt+sex+race+inc+as.factor(age)+smoke+inc+bmiCat,#+strata(week),
#              subset=(sel==1),data=impDat[[i]],method='approx')) 
#pars2type[,2,i] = mvrnorm(1e5,coef(mod),vcov(mod))[,1]
  
  
  mod = (clogit(times3~vaxvarNov+as.factor(infectNov)+edPast+inptPast+char+outpt+sex+race+inc+as.factor(age)+smoke+inc+bmiCat,#+strata(week),
                subset=(sel==1),data=impDat[[i]],method='approx')) 
  pars3[,,i] = mvrnorm(1e5,coef(mod),vcov(mod))[,1:10]
  
  #mod = (clogit(periodA3~vaxXbb+vax45+vaxvarMonoNov+as.factor(infectNov)+edPast+inptPast+char+outpt+sex+race+inc+as.factor(age)+smoke+inc+bmiCat,#+strata(week),
  #              subset=(sel==1),data=impDat[[i]],method='approx')) 
  #pars3type[,,i] = mvrnorm(1e5,coef(mod),vcov(mod))[,1:2]
  #mod = (clogit(periodA3~vax45+vaxvar5+as.factor(totprior)+edPast+inptPast+char+outpt+sex+race+inc+as.factor(age)+smoke+inc+bmiCat,#+strata(week),
  #              subset=(sel==1),data=impDat[[i]],method='approx')) 
  #pars3type[,2,i] = mvrnorm(1e5,coef(mod),vcov(mod))[,1]

  
  mod = (clogit(times4~vaxvarNov+as.factor(infectNov)+edPast+inptPast+char+outpt+sex+race+inc+as.factor(age)+smoke+inc+bmiCat,#+strata(week),
                subset=(sel==1),data=impDat[[i]],method='approx')) 
  pars4[,,i] = mvrnorm(1e5,coef(mod),vcov(mod))[,1:10]
  
 #mod = (clogit(periodA4~vax45+vaxvarMonoNov+as.factor(infectNov)+edPast+inptPast+char+outpt+sex+race+inc+as.factor(age)+smoke+inc+bmiCat,#+strata(week),
 #              subset=(sel==1),data=impDat[[i]],method='approx')) 
 #pars4type[,,i] = mvrnorm(1e5,coef(mod),vcov(mod))[,1:2]
 #mod = (clogit(periodA4~vax45+vaxvar5+as.factor(totprior)+edPast+inptPast+char+outpt+sex+race+inc+as.factor(age)+smoke+inc+bmiCat,#+strata(week),
 #              subset=(sel==1),data=impDat[[i]],method='approx')) 
 #pars4type[,2,i] = mvrnorm(1e5,coef(mod),vcov(mod))[,1]
  
}

#for (i in 1:2){print(exp(quantile(pars4type[,i,],c(0.5,0.025,0.975))))}

immPars = list(pars1,pars2,pars3,pars4)
save(immPars,file='immPars.Rdata')

vaxTypePars = list(pars1type,pars2type,pars3type,pars4type)
save(vaxTypePars,file='vaxTypePars.Rdata')






