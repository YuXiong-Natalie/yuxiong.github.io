#20200511 PISA 57 countries
getwd()
#setwd("C:/Users/xiong/Box Sync/Pisa/PISA2015_2020analysis")
setwd("/Users/yuxiong/Box Sync/Pisa/PISA2015_2020analysis")
library(haven)
dat <- read_sav("PISA2015_interest_vars_nosysmis.sav")
View(dat)


for (i in 6:38) 
{
  print (i)
  print(min(dat[,i],na.rm = TRUE))
  print(max(dat[,i],na.rm = TRUE))
}

#merge ind and gdppp
install.packages('readxl')
library(readxl)
indiv<- read_excel("IND.xlsx")
c<-cbind.data.frame(indiv$CNTRYID,indiv$IND,indiv$GDPPP15,indiv$`GDPP15 growth%`)
colnames(c)<-c('CNTRYID','IND','GDPPP15','GDPP15 growth%')
c<-unique.data.frame(c)

data1<-merge(dat,c,by.x = 'CNTRYID',by.y = 'CNTRYID')
#another way of merging
#library(dplyr)
#data2<-left_join(dat,c,by='CNTRYID')


write.csv(data1,file = 'align_data.csv', na='NA',row.names = TRUE)

#mutilevel-modeling, #normalized weights in SPSS

library(lme4)
library(lmerTest)
attach(data1)
data1$sGDP<-scale(data1$GDPPP15,center = TRUE,scale=TRUE)
# do I need both school level and country level anxtest??

model1<-lmer(PV1MATH ~ ANXTEST+MOTIVAT+ESCS+female+
               XESCS+
               YESCS+sGDP+`GDPP15 growth%`+IND+(1|CNT)+(1|CNTSCHID), data=data1,
            weights = st_wgt57,REML = FALSE)
summary(model1)

model2<-lmer(PV1MATH ~ANXTEST+MOTIVAT+ANXTEST*MOTIVAT+ESCS+female+
               XESCS+
               YESCS+(1|CNT)+(1|CNTSCHID), data=data1,
             weights = st_wgt57,REML = FALSE)
summary(model2)
#model 2 needs to eliminate gdp for convergence

model3<-lmer(PV1MATH ~ANXTEST+MOTIVAT+ANXTEST*MOTIVAT+ANXTEST*IND+ESCS+female+
               XESCS+
               YESCS+(1|CNT)+(1|CNTSCHID), data=data1,
             weights = st_wgt57,REML = FALSE)
summary(model3)
#Correlation of YESCS AND GDP IS .7, may caused the convergence problem
#anx*mot*ind has convergence problem


#EMOTIONAL COST IDEA, the Negative interaction effect of motivation and anxiety, more evident in collectivistic country?

model2.1<-lmer(MOTIVAT ~ANXTEST+ESCS+PV1MATH+ANXTEST*PV1MATH+XESCS+female+
                 YESCS+`GDPP15 growth%`+IND+(1|CNT)+(1|CNTSCHID), data=data1,
               weights = st_wgt57,REML = FALSE)
summary(model2.1)

#substract escs from next level
data1$stuescs<-data1$ESCS-data1$XESCS
data1$schescs<-data1$XESCS-data1$YESCS

data1$mathstd<-scale(data1$PV1MATH)

model2.2<-lmer(MOTIVAT ~ANXTEST+stuescs+PV1MATH+ANXTEST*PV1MATH+ANXTEST*PV1MATH*IND+schescs+
                 YESCS+ANXTEST*IND+(1|CNT)+(1|CNTSCHID), data=data1,
               weights = st_wgt57,REML = FALSE)

model2.3<-lmer(MOTIVAT ~ANXTEST+stuescs+mathstd+ANXTEST*mathstd+ANXTEST*mathstd*IND+schescs+
                 YESCS+ANXTEST*IND+(1|CNT)+(1|CNTSCHID), data=data1,
               weights = st_wgt57,REML = FALSE)
#female variable cause non-convergence
summary(model2.3)


#multi-level with weights: check weights, whether it is normalized
sum(data1$W_FSTUWT) #not normalized given the weights exceeds total sample size, may need to normalizes
#two level normalized weights
n <- nrow(data1)
data1$W_HOUSEWHT <- n * data1$W_FSTUWT / sum(data1$W_FSTUWT)


#try subset china, japan, hongkong, us, cananda

china<-subset(data1, data1$CNTRYID==970)
canada<-subset(data1,data1$CNTRYID==124)
usa<-subset(data1,data1$CNTRYID==840)
japan<-subset(data1,data1$CNTRYID==392)
korea<-subset(data1,data1$CNTRYID==410)

modelcn<-lmer(MOTIVAT ~ 1+ANXTEST+mathstd+ ESCS+(1|CNTSCHID), data=china,
              weights = W_FSTUWT, REML = FALSE)

modelca<-lmer(MOTIVAT ~ 1+ANXTEST+mathstd+ ESCS+(1|CNTSCHID), data=canada,
              weights = W_FSTUWT, REML = FALSE)

modelus<-lmer(MOTIVAT ~ 1+ANXTEST+mathstd+ ESCS+(1|CNTSCHID), data=usa,
              weights = W_FSTUWT, REML = FALSE)
summary(modelca)
summary(modelus)

modeljp<-lmer(MOTIVAT ~ 1+ANXTEST+mathstd+ ESCS+(1|CNTSCHID), data=japan,
              weights = W_FSTUWT, REML = FALSE)
summary(modeljp)

modelkr<-lmer(MOTIVAT ~ 1+ANXTEST+mathstd+ ESCS+(1|CNTSCHID), data=korea,
              weights = W_FSTUWT, REML = FALSE)
summary(modelkr)

#20200515

#country level test anxiety, MOTIVATION
countrymean<-cbind(data1$CNTRYID,data1$ANXTEST,data1$MOTIVAT)
colnames(countrymean)<-c("CNTRYID","ANXTEST","MOTIVAT")

countrymean<-as.data.frame(countrymean)
manx<-aggregate(ANXTEST~CNTRYID, countrymean, FUN=mean)
mmot<-aggregate(MOTIVAT~CNTRYID, countrymean, FUN=mean)
mmath<-aggregate(PV1MATH~CNTRYID, data1, FUN=mean)

data2<-merge(manx,mmot,by.x = 'CNTRYID',by.y = 'CNTRYID')
colnames(data2)<-c("CNTRYID","manx","mmot")
data1<-merge(data1,data2,by.x = 'CNTRYID',by.y = 'CNTRYID',all.x = TRUE)
data1<-merge(data1,mmath,by.x = 'CNTRYID',by.y = 'CNTRYID',all.x = TRUE)


#female variable has problem because the 2012 syntax treat missing as female... need to rewrite the datset

#test anxiety and performance link
model<-lmer(PV1MATH ~ANXTEST+stuescs+schescs+
                 YESCS+GDPPP15+(1|CNT)+(1|CNTSCHID), data=data1,
                 weights = st_wgt57,REML = FALSE)
summary(model)

#export country level anxtest and pv1math to graph
out<-merge(data2,mmath,by.x = 'CNTRYID',by.y = 'CNTRYID',all.x = TRUE)
colnames(out)<-c("CNTRYID","CNT","manx","mmath")

countrylevel<-merge(out,c,by.x = 'CNTRYID',by.y = 'CNTRYID', all.x = TRUE)
yescs<-cbind(data1$CNTRYID,data1$YESCS)
yescs<-unique(yescs)
colnames(yescs)<-c("CNTRYID","YESCS")
countrylevel<-merge(countrylevel,yescs,by.x = 'CNTRYID',by.y = 'CNTRYID', all.x = TRUE)
write.csv(countrylevel,file = "country_level_anx_math.csv")

model<-lm(PV1MATH~1+manx+YESCS+GDPPP15, data=countrylevel)
summary(countylevel)
modelm<-lm(mmot~1+manx+YESCS+GDPPP15, data=countrylevel)
summary(modelm)

model1<-lmer(PV1MATH.x ~ PV1MATH.y+ANXTEST+manx+MOTIVAT+mmot+ESCS+
               YESCS+GDPPP15+(1|CNT), data=data1,
             weights = st_wgt57,REML = FALSE)
summary(model1)

cor(manx,mmot)
