#write out data file
#desktop
setwd('/Users/xiong/Box Sync/Pisa')
install.packages('haven')
library(haven)

PISA2015_full <- read_sav("PISA2015_full.sav")
#mac
data <- read_dta("/Users/xiong/Box Sync/Pisa/huyu-pisa-parent-0904.dta")
View(data)
write_sav(data, "huyu-pisa-parent-0904.sav")

#desktop
data <- read_dta("/Users/yuxiong/Box Sync/Pisa/huyu-pisa-parent-0904.dta")
View(data)
table(colnames(data))
cor(data$pqschool,data$cursupp)
x=data$pqschool
y=data$cursupp
a=lm(data$cursupp~1+data$pqschool,data = data)
summary(a)
library(lme4)
library(lmerTest)
b=lmer(cursupp~1+pqschool+(pqschool|cntryid),data)
summary(b)
c=lmer(cursupp~1+pqschool+(1|cntryid),data)
summary(c)

#CHECK COUNTRY ID
A<-table(PISA2015_full$CNTRYID,PISA2015_full$CNT)
A<-as.data.frame(A)
A[A==0] <- NA
A1<-A[complete.cases(A),]
A1<-A1[order(A1$Var1),]

library(foreign)
write.foreign(data, "C:/Users/xiong/Dropbox/Pisa/pisa.txt", "C:/Users/xiong/Dropbox/Pisa/pisa.sps",   package="SPSS")

# test path model with one country using three factor
install.packages('haven')
install.packages('lavaan')
install.packages("psych")
install.packages('foreign')
install.packages('GPArotation')
library(GPArotation)
library(haven)
#pisap <- read_dta("/Users/yuxiong/Box Sync/Pisa/huyu-pisa-parent-0904.dta")
#WINDOWS
pisap <- read_dta("/Users/xiong/Box Sync/Pisa/huyu-pisa-parent-0904.dta")
library(psych)
describe(pisap)
head(pisap)
pisap$escs

#explore factor structure for parental support(current support 8items,emotional support 4items)
#use HK as subsample to explore
data<-subset(pisap,cnt=='HKG', select=c(paste0(rep('PA003Q0',8),1:8,c(rep('TA',3),rep('NA',5))),
                                        paste0(rep('PA004Q0',4),1:4,rep('NA',4)),'cursupp','emosupp','pqschool','PV1MATH','escs'))

describe(data)
efadata<-fa(data[,1:12],nfactors = 3,rotate = 'oblimin')
print(efadata,cut=.3,sort=T)

efadata2<-fa(data[,1:12],nfactors = 2,rotate = 'oblimin')
print(efadata2,cut=.3,sort=T)

#confirmatory analysis

cfadata<-'
F1 =~ PA003Q06NA+PA003Q07NA+PA003Q04NA+PA003Q08NA+PA003Q05NA
F2=~ PA004Q03NA+PA004Q02NA+PA004Q04NA+PA004Q01NA
F3 =~ PA003Q01TA+PA003Q02TA+PA003Q03TA
'
#name F1 science support, F2emotional supp, F3talk with child in general
cfadata<-cfa(cfadata, data = data, missing = 'fiml')
summary(cfadata, fit.measures = T, standardized = T)

#modindices(cfadata, sort. = T, maximum.number = 5)

# tentative path analysis with HK data
data$scisup=1/5*(data$PA003Q06NA+data$PA003Q07NA+data$PA003Q04NA+data$PA003Q08NA+data$PA003Q05NA)
data$talkg=1/3*(data$PA003Q01TA+data$PA003Q02TA+data$PA003Q03TA)

#see which predictor is most related to math grades
attach(data)
a<-lm(PV1MATH~escs+scisup+emosupp+talkg+pqschool)
a
summary(a)
# check germany
data2<-subset(pisap,cnt=='DEU', select=c(paste0(rep('PA003Q0',8),1:8,c(rep('TA',3),rep('NA',5))),
                                        paste0(rep('PA004Q0',4),1:4,rep('NA',4)),'cursupp','emosupp','pqschool','PV1MATH','escs'))
detach(data)
attach(data2)
data2$scisup=1/5*(PA003Q06NA+PA003Q07NA+PA003Q04NA+PA003Q08NA+PA003Q05NA)
data2$talkg=1/3*(PA003Q01TA+PA003Q02TA+PA003Q03TA)

b<-lm(PV1MATH~escs+scisup+emosupp+talkg+pqschool)
b
summary(b)
# try path
detach(data2)
m1<-
  ' PV1MATH ~ scisup+emosupp+talkg
    talkg~pqschool
    scisup~pqschool
    emosupp~pqschool
'
m1.fit<-cfa(model = m1, data=data)
summary(m1.fit)

##descriptive statistics #Mianalysis
setwd('/Users/yuxiong/Box Sync/Pisa')
data<- read.csv('R_three_variables.csv',header = TRUE)
miss<-data[!complete.cases(data),]
a<-table(miss$CNT)
b<-table(data$CNT)
a-b
#ALB is systematically missing, delete this country
data2<-data[data$CNT!='ALB',]
#descriptive in each country
library(psych)
describeBy(cbind(data2$ANXTEST,data2$MOTIVAT,data2$EMOSUPS),data2$CNT)
#rm 97 99 which is unavailable/ they have item level value, may look back later
data2[data2$ANXTEST%in%c(97,99),]<-NA
data2[data2$MOTIVAT%in%c(97,99),]<-NA
data2[data2$EMOSUPS%in%c(97,99),]<-NA

describeBy(cbind(data2$ANXTEST,data2$MOTIVAT,data2$EMOSUPS),data2$CNT)
table(data2$CNT)
data3<-data2[complete.cases(data2),]#may need to consider further, because only need to delete systematic missing
describeBy(cbind(data3$ANXTEST,data3$MOTIVAT,data3$EMOSUPS),data3$CNT)
#delete not complete row
data3<-data3[!apply(data3 == "", 1, all),]
table(data3$CNT)
#delete unused levels
data3<-droplevels(data3)
table(data3$CNT)
describeBy(cbind(data3$ANXTEST,data3$MOTIVAT,data3$EMOSUPS),data3$CNT)

#correlation of emotional support and test anxiety by country
library(dplyr)
correlate1 <- data3 %>%
  group_by(CNT) %>%
  summarise(r = cor(EMOSUPS, ANXTEST))
correlate1
#correlation of emotional support and achivement motivation by country
correlate2 <- data3 %>%
  group_by(CNT) %>%
  summarise(r = cor(EMOSUPS, MOTIVAT))
correlate2


#compare factor structure of text anxiety,start by test it in one country
library(lavaan)
anx.model<-'f1=~NA*ST118Q01NA+ST118Q02NA+ST118Q03NA+ST118Q04NA+ST118Q05NA
            f1~~1*f1'
anx.fit<-cfa(anx.model,data = data3[data3$CNT=='HKG',])
summary(anx.fit,fit.measure=T)

#check modification
modificationindices(anx.fit)

##update model
anx.model.up.fit<-update(anx.fit,add='ST118Q01NA ~~ ST118Q02NA
                         ST118Q04NA ~~ ST118Q05NA')
summary(anx.model.up.fit,fit.measure=T)

#another country: Japan
anx.model2<-'f1=~NA*ST118Q01NA+ST118Q02NA+ST118Q03NA+ST118Q04NA+ST118Q05NA
            f1~~1*f1
            ST118Q01NA ~~ ST118Q02NA
            ST118Q04NA ~~ ST118Q05NA'
anx.fit2<-cfa(anx.model2,data = data3[data3$CNT=='JPN',])
summary(anx.fit2,fit.measure=T)



#compare between countries, the whole set seems problematic, try 2 countries first
data4<-data3[data3$CNT %in% c('JPN','HKG'),]

mi.configural<-'
f1=~NA*ST118Q01NA+ST118Q02NA+ST118Q03NA+ST118Q04NA+ST118Q05NA

ST118Q01NA ~ 1
ST118Q02NA ~ 1
ST118Q03NA ~ 1
ST118Q04NA ~ 1
ST118Q05NA ~ 1

#define variances
ST118Q01NA ~~ ST118Q01NA
ST118Q02NA ~~ ST118Q02NA
ST118Q03NA ~~ ST118Q03NA
ST118Q04NA ~~ ST118Q04NA
ST118Q05NA ~~ ST118Q05NA

#define factor mean
f1~0*1

#define factor variance
f1~~1*f1
'
mi.configural.fit<-lavaan(mi.configural,data=data4, missing="fiml", group = "CNT")
summary(mi.configural.fit, fit.measures=T, standardized=T)

mi.metric<-'
f1=~c(l1,l1)*ST118Q01NA+c(l2,l2)*ST118Q02NA+c(l3,l3)*ST118Q03NA+c(l4,l4)*ST118Q04NA+c(l5,l5)*ST118Q05NA

ST118Q01NA ~ 1
ST118Q02NA ~ 1
ST118Q03NA ~ 1
ST118Q04NA ~ 1
ST118Q05NA ~ 1

#define variances
ST118Q01NA ~~ ST118Q01NA
ST118Q02NA ~~ ST118Q02NA
ST118Q03NA ~~ ST118Q03NA
ST118Q04NA ~~ ST118Q04NA
ST118Q05NA ~~ ST118Q05NA

#define factor mean
f1~0*1

#define factor variance, for the other groups because use group 1 loading so not need to add identification constriant of variance
f1~~c(1,NA)*f1
'
mi.metric.fit<-lavaan(mi.metric,data=data4,missing='fiml',group='CNT')
summary(mi.metric.fit,fit.measures=T,standardized=T)
#?why metric model fit better in terms of tli and rmsea; it looks normal in as chi-square increased in metric model
anova(mi.configural.fit,mi.metric.fit)

mi.scalar<-'
f1=~c(l1,l1)*ST118Q01NA+c(l2,l2)*ST118Q02NA+c(l3,l3)*ST118Q03NA+c(l4,l4)*ST118Q04NA+c(l5,l5)*ST118Q05NA

ST118Q01NA ~ c(i1,i1)*1
ST118Q02NA ~ c(i2,i2)*1
ST118Q03NA ~ c(i3,i3)*1
ST118Q04NA ~ c(i4,i4)*1
ST118Q05NA ~ c(i5,i5)*1

#define variances
ST118Q01NA ~~ ST118Q01NA
ST118Q02NA ~~ ST118Q02NA
ST118Q03NA ~~ ST118Q03NA
ST118Q04NA ~~ ST118Q04NA
ST118Q05NA ~~ ST118Q05NA

#define factor mean,for the other groups because use group 1 intercept so not need identificaiton contrait for meanin other groups
f1~c(0,NA)*1

#define factor variance, 
f1~~c(1,NA)*f1
'
mi.scalar.fit<-lavaan(mi.scalar,data=data4,missing='fiml', group='CNT')
summary(mi.scalar.fit,fit.measures=T)


#fit factor analysis for child measures across groups to find a start model for measurement invariance
library(psych)
library(GPArotation)
library(nFactors)
PA<-parallel(subject = nrow(data3),var=ncol(data[,13:26]), rep=100, cent=.95,model='components')

cor<-cor(data3[,13:26],use='pairwise.complete.obs')
evdata<-eigen(cor)
evdata$values
plotnScree(nScree(x=evdata$values, aparallel = PA$eigen$qevpea,model = 'components'))

itemnames<-colnames(data3)
itemnames
efadata<-fa(data3[,13:26],nfactors = 3,rotate='oblimin')
print(efadata,cut=.3,sort = T)
#the structure looks clean, proceed to CFA, first anxiety measure
anx.model<-'f1=~NA*ST118Q01NA+ST118Q02NA+ST118Q03NA+ST118Q04NA+ST118Q05NA
            f1~~1*f1'
anx.fit.all<-cfa(anx.model,data = data3)
summary(anx.fit.all,fit.measure=T)
modindices(anx.fit.all)
#lookes like item1 & 2 high correlated.
anx.model.2<-'f1=~NA*ST118Q01NA+ST118Q02NA+ST118Q03NA+ST118Q04NA+ST118Q05NA
            f1~~1*f1
            ST118Q01NA ~~ ST118Q02NA'
anx.fit.all.2<-cfa(anx.model.2,data = data3)
summary(anx.fit.all.2,fit.measure=T)
# settle for anxiety model to do MI analysis
#second, motivation measure
mot.model<-'f1=~NA*ST119Q01NA+ST119Q02NA+ST119Q03NA+ST119Q04NA+ST119Q05NA
            f1~~1*f1'
mot.fit<-cfa(mot.model,data = data3)
summary(mot.fit,fit.measure=T)
modindices(mot.fit)
#lookes like item1 & 2 high correlated.
mot.model.2<-'f1=~NA*ST119Q01NA+ST119Q02NA+ST119Q03NA+ST119Q04NA+ST119Q05NA
            f1~~1*f1
ST119Q01NA ~~ ST119Q02NA'
mot.fit.2<-cfa(mot.model.2,data = data3)
summary(mot.fit.2,fit.measure=T)

#third, the support variable
sup.model.child<-'f1=~NA*ST123Q01NA+ST123Q02NA+ST123Q03NA+ST123Q04NA
            f1~~1*f1'
sup.model.child.fit<-cfa(sup.model.child,data = data3)
summary(sup.model.child.fit, fit.measure=T)
modindices(sup.model.child.fit)#looks fine. may consider to covariate first 2 items and last 2 items later..

sup.model.adult<-'f1=~NA*PA004Q01NA+PA004Q02NA+PA004Q03NA+PA004Q04NA
            f1~~1*f1'
sup.model.adult.fit<-cfa(sup.model.adult,data = data3)
summary(sup.model.adult.fit, fit.measure=T)
modindices(sup.model.adult.fit)#looks fine. may consider to covariate first 2 items and last 2 items later..

#measurement invariance across 57 groups
#configural model
mi.configural.all<-'
f1=~NA*ST118Q01NA+ST118Q02NA+ST118Q03NA+ST118Q04NA+ST118Q05NA

ST118Q01NA ~ 1
ST118Q02NA ~ 1
ST118Q03NA ~ 1
ST118Q04NA ~ 1
ST118Q05NA ~ 1

#define variances
ST118Q01NA ~~ ST118Q01NA
ST118Q02NA ~~ ST118Q02NA
ST118Q03NA ~~ ST118Q03NA
ST118Q04NA ~~ ST118Q04NA
ST118Q05NA ~~ ST118Q05NA

#residual correlation
ST118Q01NA ~ ST118Q02NA

#define factor mean
f1~0*1

#define factor variance
f1~~1*f1
'
mi.configural.all.fit<-lavaan(mi.configural.all,data=data3,missing='fiml',group = 'CNT')
fitMeasures(mi.configural.all.fit,fit.measures = c('cfi','tli','rmsea'))

#metric model
mi.metric.all<-'
f1=~c(l1,l1,l1,l1,l1,l1,l1,l1,l1,l1,
l1,l1,l1,l1,l1,l1,l1,l1,l1,l1,
l1,l1,l1,l1,l1,l1,l1,l1,l1,l1,
l1,l1,l1,l1,l1,l1,l1,l1,l1,l1,
l1,l1,l1,l1,l1,l1,l1,l1,l1,l1,
l1,l1,l1,l1,l1,l1,l1)*ST118Q01NA+
c(l2,l2,l2,l2,l2,l2,l2,l2,l2,l2,
l2,l2,l2,l2,l2,l2,l2,l2,l2,l2,
l2,l2,l2,l2,l2,l2,l2,l2,l2,l2,
l2,l2,l2,l2,l2,l2,l2,l2,l2,l2,
l2,l2,l2,l2,l2,l2,l2,l2,l2,l2,
l2,l2,l2,l2,l2,l2,l2)*ST118Q02NA+
c(l3,l3,l3,l3,l3,l3,l3,l3,l3,l3,
l3,l3,l3,l3,l3,l3,l3,l3,l3,l3,
l3,l3,l3,l3,l3,l3,l3,l3,l3,l3,
l3,l3,l3,l3,l3,l3,l3,l3,l3,l3,
l3,l3,l3,l3,l3,l3,l3,l3,l3,l3,
l3,l3,l3,l3,l3,l3,l3)*ST118Q03NA+
c(l4,l4,l4,l4,l4,l4,l4,l4,l4,l4,
l4,l4,l4,l4,l4,l4,l4,l4,l4,l4,
l4,l4,l4,l4,l4,l4,l4,l4,l4,l4,
l4,l4,l4,l4,l4,l4,l4,l4,l4,l4,
l4,l4,l4,l4,l4,l4,l4,l4,l4,l4,
l4,l4,l4,l4,l4,l4,l4)*ST118Q04NA+
c(l5,l5,l5,l5,l5,l5,l5,l5,l5,l5,
l5,l5,l5,l5,l5,l5,l5,l5,l5,l5,
l5,l5,l5,l5,l5,l5,l5,l5,l5,l5,
l5,l5,l5,l5,l5,l5,l5,l5,l5,l5,
l5,l5,l5,l5,l5,l5,l5,l5,l5,l5,
l5,l5,l5,l5,l5,l5,l5)*ST118Q05NA

ST118Q01NA ~ 1
ST118Q02NA ~ 1
ST118Q03NA ~ 1
ST118Q04NA ~ 1
ST118Q05NA ~ 1

#define variances
ST118Q01NA ~~ ST118Q01NA
ST118Q02NA ~~ ST118Q02NA
ST118Q03NA ~~ ST118Q03NA
ST118Q04NA ~~ ST118Q04NA
ST118Q05NA ~~ ST118Q05NA

#residual correlation
ST118Q01NA ~ ST118Q02NA

#define factor mean
f1~0*1

#define factor variance
f1~~c(1,NA,NA,NA,NA,NA,NA,NA,NA,NA,
NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
NA,NA,NA,NA,NA,NA,NA
)*f1
'
mi.metric.all.fit<-lavaan(mi.metric.all,data=data3,missing='fiml',group = 'CNT')
fitMeasures(mi.metric.all.fit,fit.measures = c('cfi','tli','rmsea'))
resid(mi.metric.all.fit,type='normalized')
inspect(mi.metric.all.fit,'coef')

#try use alignment to see pattern of non-invariance
install.packages('sirt')
library('sirt')
align.configural<-invariance_alignment_cfa_config(data3[,17:21],group = data3$CNT)
#where is the fit index of alignment?
al<-invariance.alignment(lambda = align.configural$lambda,nu=align.configural$nu,optimizer = 'nlminb')
summary(al)
#the lambda output from group 'ARE' greatly differ from other groups...why? 


# try metric model remove ARE
mi.configural.all.fit.2<-lavaan(mi.configural.all,data=data4,missing='fiml',group = 'CNT')
fitMeasures(mi.configural.all.fit.2,fit.measures = c('cfi','tli','rmsea'))

data4<-data3[!data3$CNT=='ARE',]
data4<-droplevels(data4)
mi.metric.all.2<-'
f1=~c(l1,l1,l1,l1,l1,l1,l1,l1,l1,l1,
l1,l1,l1,l1,l1,l1,l1,l1,l1,l1,
l1,l1,l1,l1,l1,l1,l1,l1,l1,l1,
l1,l1,l1,l1,l1,l1,l1,l1,l1,l1,
l1,l1,l1,l1,l1,l1,l1,l1,l1,l1,
l1,l1,l1,l1,l1,l1)*ST118Q01NA+
c(l2,l2,l2,l2,l2,l2,l2,l2,l2,l2,
l2,l2,l2,l2,l2,l2,l2,l2,l2,l2,
l2,l2,l2,l2,l2,l2,l2,l2,l2,l2,
l2,l2,l2,l2,l2,l2,l2,l2,l2,l2,
l2,l2,l2,l2,l2,l2,l2,l2,l2,l2,
l2,l2,l2,l2,l2,l2)*ST118Q02NA+
c(l3,l3,l3,l3,l3,l3,l3,l3,l3,l3,
l3,l3,l3,l3,l3,l3,l3,l3,l3,l3,
l3,l3,l3,l3,l3,l3,l3,l3,l3,l3,
l3,l3,l3,l3,l3,l3,l3,l3,l3,l3,
l3,l3,l3,l3,l3,l3,l3,l3,l3,l3,
l3,l3,l3,l3,l3,l3)*ST118Q03NA+
c(l4,l4,l4,l4,l4,l4,l4,l4,l4,l4,
l4,l4,l4,l4,l4,l4,l4,l4,l4,l4,
l4,l4,l4,l4,l4,l4,l4,l4,l4,l4,
l4,l4,l4,l4,l4,l4,l4,l4,l4,l4,
l4,l4,l4,l4,l4,l4,l4,l4,l4,l4,
l4,l4,l4,l4,l4,l4)*ST118Q04NA+
c(l5,l5,l5,l5,l5,l5,l5,l5,l5,l5,
l5,l5,l5,l5,l5,l5,l5,l5,l5,l5,
l5,l5,l5,l5,l5,l5,l5,l5,l5,l5,
l5,l5,l5,l5,l5,l5,l5,l5,l5,l5,
l5,l5,l5,l5,l5,l5,l5,l5,l5,l5,
l5,l5,l5,l5,l5,l5)*ST118Q05NA

ST118Q01NA ~ 1
ST118Q02NA ~ 1
ST118Q03NA ~ 1
ST118Q04NA ~ 1
ST118Q05NA ~ 1

#define variances
ST118Q01NA ~~ ST118Q01NA
ST118Q02NA ~~ ST118Q02NA
ST118Q03NA ~~ ST118Q03NA
ST118Q04NA ~~ ST118Q04NA
ST118Q05NA ~~ ST118Q05NA

#residual correlation
ST118Q01NA ~ ST118Q02NA

#define factor mean
f1~0*1

#define factor variance
f1~~c(1,NA,NA,NA,NA,NA,NA,NA,NA,NA,
NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
NA,NA,NA,NA,NA,NA)*f1
'
mi.metric.all.fit.2<-lavaan(mi.metric.all.2,data=data4,missing='fiml',group = 'CNT')
fitMeasures(mi.metric.all.fit.2,fit.measures = c('cfi','tli','rmsea'))
# not striking difference compared to not move ARE, there may be wrong use of alignment method functions

#eyeballing loadings in countries to find cases that is different may try partial metric?
resid(mi.metric.all.fit,type='normalized')
inspect(mi.metric.all.fit,'group.label')
modi<-modindices(mi.metric.all.fit,sort. = T)

#looks like group QES.group 55, in lavaan labels has the most problem;
#also, Fra item 3 (standard is >10 residual)
#bra item2, ESP item 2
#eval somethong to tell lavaan to run it rather than treat it as text


mi.metric.partial<-'
f1=~c(l1,l1,l1,l1,l1,l1,l1,l1,l1,l1,
l1,l1,l1,l1,l1,l1,l1,l1,l1,l1,
l1,l1,l1,l1,l1,l1,l1,l1,l1,l1,
l1,l1,l1,l1,l1,l1,l1,l1,l1,l1,
l1,l1,l1,l1,l1,l1,l1,l1,l1,l1,
l1,l1,l1,l1,l1,l1,l1)*ST118Q01NA+
c(l2,l2,l2,b2,l2,l2,l2,l2,l2,l2,
l2,l2,l2,l2,l2,l2,l2,l2,l2,l2,
l2,l2,l2,l2,l2,l2,l2,l2,l2,l2,
l2,l2,l2,l2,l2,l2,l2,l2,l2,l2,
l2,l2,l2,e2,l2,l2,l2,l2,l2,l2,
l2,l2,l2,l2,p2,l2,l2)*ST118Q02NA+
c(l3,l3,l3,l3,l3,l3,l3,l3,l3,l3,
l3,l3,l3,l3,l3,l3,f3,L3,l3,l3,
l3,l3,l3,l3,l3,l3,l3,l3,l3,l3,
l3,l3,l3,l3,l3,l3,l3,l3,l3,l3,
l3,l3,l3,l3,l3,l3,l3,l3,l3,l3,
l3,l3,l3,l3,l3,l3,l3)*ST118Q03NA+
c(l4,l4,l4,l4,l4,l4,l4,l4,l4,c4,
l4,l4,l4,l4,l4,l4,l4,l4,l4,l4,
l4,l4,l4,l4,l4,l4,l4,l4,l4,l4,
l4,l4,l4,l4,l4,l4,l4,l4,l4,l4,
l4,l4,l4,l4,l4,l4,l4,l4,l4,l4,
l4,l4,u4,l4,l4,l4,l4)*ST118Q04NA+
c(l5,l5,l5,b5,l5,l5,l5,l5,co5,c5,
l5,l5,l5,l5,l5,l5,l5,l5,l5,l5,
l5,l5,l5,l5,l5,l5,l5,l5,l5,l5,
l5,l5,l5,l5,l5,l5,l5,l5,l5,l5,
l5,l5,l5,l5,l5,l5,l5,l5,l5,l5,
l5,l5,u5,l5,l5,l5,l5)*ST118Q05NA

ST118Q01NA ~ 1
ST118Q02NA ~ 1
ST118Q03NA ~ 1
ST118Q04NA ~ 1
ST118Q05NA ~ 1

#define variances
ST118Q01NA ~~ ST118Q01NA
ST118Q02NA ~~ ST118Q02NA
ST118Q03NA ~~ ST118Q03NA
ST118Q04NA ~~ ST118Q04NA
ST118Q05NA ~~ ST118Q05NA

#residual correlation
ST118Q01NA ~ ST118Q02NA

#define factor mean
f1~0*1

#define factor variance
f1~~c(1,NA,NA,NA,NA,NA,NA,NA,NA,NA,
NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
NA,NA,NA,NA,NA,NA,NA)*f1
'
mi.metric.partial.fit<-lavaan(mi.metric.partial,data=data3,missing='fiml',group = 'CNTRYID')
fitMeasures(mi.metric.partial.fit,fit.measures = c('cfi','tli','rmsea'))

modindices(mi.metric.partial.fit,sort. = T) #check modification since we have good tli but not cfi, group 53, group10

resid(mi.metric.partial.fit,type='normalized')#check group 53,10,9（col, item 5） further free URY 53 item 4，5; CRI 10 item5，4,bra 4 item5
inspect(mi.metric.partial.fit,'group.label') 
#seems not extreme deviated residuals, settled for this partial metric model

#motivation measures
mot.mi.configural.all<-'
f1=~NA*ST119Q01NA+ST119Q02NA+ST119Q03NA+ST119Q04NA+ST119Q05NA

ST119Q01NA ~ 1
ST119Q02NA ~ 1
ST119Q03NA ~ 1
ST119Q04NA ~ 1
ST119Q05NA ~ 1

#define variances
ST119Q01NA ~~ ST119Q01NA
ST119Q02NA ~~ ST119Q02NA
ST119Q03NA ~~ ST119Q03NA
ST119Q04NA ~~ ST119Q04NA
ST119Q05NA ~~ ST119Q05NA

#residual correlation
ST119Q01NA ~ ST119Q02NA


#define factor mean
f1~0*1

#define factor variance
f1~~1*f1
'
mot.mi.configural.all.fit<-lavaan(mot.mi.configural.all,data=data3,missing='fiml',group = 'CNTRYID')
fitMeasures(mot.mi.configural.all.fit,fit.measures = c('cfi','tli','rmsea'))

#motivation metric model
mot.mi.metric.all<-'
f1=~c(l1,l1,l1,l1,l1,l1,l1,l1,l1,l1,
l1,l1,l1,l1,l1,l1,l1,l1,l1,l1,
l1,l1,l1,l1,l1,l1,l1,l1,l1,l1,
l1,l1,l1,l1,l1,l1,l1,l1,l1,l1,
l1,l1,l1,l1,l1,l1,l1,l1,l1,l1,
l1,l1,l1,l1,l1,l1,l1)*ST119Q01NA+
c(l2,l2,l2,l2,l2,l2,l2,l2,l2,l2,
l2,l2,l2,l2,l2,l2,l2,l2,l2,l2,
l2,l2,l2,l2,l2,l2,l2,l2,l2,l2,
l2,l2,l2,l2,l2,l2,l2,l2,l2,l2,
l2,l2,l2,l2,l2,l2,l2,l2,l2,l2,
l2,l2,l2,l2,l2,l2,l2)*ST119Q02NA+
c(l3,l3,l3,l3,l3,l3,l3,l3,l3,l3,
l3,l3,l3,l3,l3,l3,l3,l3,l3,l3,
l3,l3,l3,l3,l3,l3,l3,l3,l3,l3,
l3,l3,l3,l3,l3,l3,l3,l3,l3,l3,
l3,l3,l3,l3,l3,l3,l3,l3,l3,l3,
l3,l3,l3,l3,l3,l3,l3)*ST119Q03NA+
c(l4,l4,l4,l4,l4,l4,l4,l4,l4,l4,
l4,l4,l4,l4,l4,l4,l4,l4,l4,l4,
l4,l4,l4,l4,l4,l4,l4,l4,l4,l4,
l4,l4,l4,l4,l4,l4,l4,l4,l4,l4,
l4,l4,l4,l4,l4,l4,l4,l4,l4,l4,
l4,l4,l4,l4,l4,l4,l4)*ST119Q04NA+
c(l5,l5,l5,l5,l5,l5,l5,l5,l5,l5,
l5,l5,l5,l5,l5,l5,l5,l5,l5,l5,
l5,l5,l5,l5,l5,l5,l5,l5,l5,l5,
l5,l5,l5,l5,l5,l5,l5,l5,l5,l5,
l5,l5,l5,l5,l5,l5,l5,l5,l5,l5,
l5,l5,l5,l5,l5,l5,l5)*ST119Q05NA

ST119Q01NA ~ 1
ST119Q02NA ~ 1
ST119Q03NA ~ 1
ST119Q04NA ~ 1
ST119Q05NA ~ 1

#define variances
ST119Q01NA ~~ ST119Q01NA
ST119Q02NA ~~ ST119Q02NA
ST119Q03NA ~~ ST119Q03NA
ST119Q04NA ~~ ST119Q04NA
ST119Q05NA ~~ ST119Q05NA

#residual correlation
ST119Q01NA ~ ST119Q02NA


#define factor mean
f1~0*1

#define factor variance
f1~~c(1,NA,NA,NA,NA,NA,NA,NA,NA,NA,
NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
NA,NA,NA,NA,NA,NA,NA)*f1
'
mot.mi.metric.all.fit<-lavaan(mot.mi.metric.all,data=data3,missing='fiml',group = 'CNTRYID')
fitMeasures(mot.mi.metric.all.fit,fit.measures = c('cfi','tli','rmsea'))
modindices(mot.mi.metric.all.fit,sort. = T) # have a look to see which group deviates; 
#group 55QES Item 2, 4, 47THA, item3, 38PRT,19GRC
resid(mot.mi.metric.all.fit,type='normalized') #inspect resisual to decide which one to free
#55QES Item 2, 4, 47THA, item3,PER(36) Item4; MEX(31) 4, GRC(19) item 5; dom(14) item 4; col(9) item4, BRA(4) item 4
inspect(mot.mi.metric.all.fit,'group.label') #make sure group bumber



mot.mi.metric.partial<-'
f1=~c(l1,l1,l1,g1,l1,l1,l1,l1,X1,l1,
l1,l1,l1,l1,l1,l1,l1,l1,A1,l1,
l1,S1,l1,l1,l1,l1,l1,l1,l1,l1,
M1,l1,l1,l1,l1,l1,l1,B1,l1,l1,
l1,l1,l1,l1,l1,l1,l1,S1,l1,l1,
l1,l1,l1,l1,l1,l1,l1)*ST119Q01NA+
c(l2,l2,l2,B2,l2,l2,l2,l2,l2,l2,
l2,l2,l2,l2,l2,l2,l2,l2,l2,l2,
l2,l2,l2,y2,x2,l2,l2,l2,v2,l2,
l2,l2,l2,l2,l2,C2,l2,l2,Q2,l2,
l2,l2,l2,l2,l2,l2,l2,l2,l2,l2,
l2,l2,l2,l2,S2,l2,l2)*ST119Q02NA+
c(a3,l3,l3,g3,l3,l3,l3,l3,X3,l3,
l3,l3,l3,l3,l3,l3,l3,l3,l3,l3,
l3,l3,l3,l3,l3,l3,l3,l3,l3,l3,
l3,l3,l3,l3,l3,l3,l3,l3,l3,l3,
l3,l3,l3,l3,l3,l3,T3,l3,l3,l3,
l3,l3,l3,Y3,l3,l3,l3)*ST119Q03NA+
c(l4,l4,l4,v4,l4,l4,l4,l4,C4,l4,
l4,l4,l4,D4,l4,l4,l4,l4,A4,l4,
l4,l4,l4,l4,l4,l4,l4,l4,l4,l4,
M4,l4,l4,l4,l4,P4,l4,l4,b4,l4,
l4,l4,l4,l4,l4,l4,g4,S4,l4,l4,
l4,l4,l4,l4,Q4,l4,l4)*ST119Q04NA+
c(b5,l5,l5,z5,l5,l5,l5,l5,l5,l5,
l5,l5,l5,l5,l5,l5,l5,l5,A5,l5,
l5,l5,l5,l5,l5,l5,l5,l5,l5,l5,
l5,l5,N5,l5,l5,l5,l5,l5,l5,l5,
l5,l5,l5,l5,l5,V5,W2,S5,l5,l5,
l5,l5,l5,Y5,l5,l5,l5)*ST119Q05NA

ST119Q01NA ~ 1
ST119Q02NA ~ 1
ST119Q03NA ~ 1
ST119Q04NA ~ 1
ST119Q05NA ~ 1

#define variances
ST119Q01NA ~~ ST119Q01NA
ST119Q02NA ~~ ST119Q02NA
ST119Q03NA ~~ ST119Q03NA
ST119Q04NA ~~ ST119Q04NA
ST119Q05NA ~~ ST119Q05NA

#residual correlation
ST119Q01NA ~ ST119Q02NA

#define factor mean
f1~0*1

#define factor variance
f1~~c(1,NA,NA,1,NA,NA,NA,NA,NA,NA,
NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
NA,NA,NA,NA,NA,NA,NA)*f1
'
mot.mi.metric.partial.fit<-lavaan(mot.mi.metric.partial,data=data3,missing='fiml',group = 'CNTRYID')
fitMeasures(mot.mi.metric.partial.fit,fit.measures = c('cfi','tli','rmsea'))

modindices(mot.mi.metric.partial.fit,sort. = T) 
resid(mot.mi.metric.partial.fit,type='normalized') #39 QAT 2&4; NLD33, item 5; GRc 19 ITEM5 1,4  
inspect(mot.mi.metric.partial.fit,'group.label') 
# dealta cfi =.013 refer to alignment output for further refine partial model
#or could be error correlation of a specific group(group 55, 3~~4)
#although established partial, how come the tli improved?


#support measures
sup.mi.configural.all<-'
f1=~NA*ST123Q01NA+ST123Q02NA+ST123Q03NA+ST123Q04NA

ST123Q01NA ~ 1
ST123Q02NA ~ 1
ST123Q03NA ~ 1
ST123Q04NA ~ 1


#define variances
ST123Q01NA ~~ ST123Q01NA
ST123Q02NA ~~ ST123Q02NA
ST123Q03NA ~~ ST123Q03NA
ST123Q04NA ~~ ST123Q04NA


#residual correlation


#define factor mean
f1~0*1

#define factor variance
f1~~1*f1
'
sup.mi.configural.all.fit<-lavaan(sup.mi.configural.all,data=data3,missing='fiml',group = 'CNT')
fitMeasures(sup.mi.configural.all.fit,fit.measures = c('cfi','tli','rmsea'))

#sup metric model
sup.mi.metric.all<-'
f1=~c(l1,l1,l1,l1,l1,l1,l1,l1,l1,l1,
l1,l1,l1,l1,l1,l1,l1,l1,l1,l1,
l1,l1,l1,l1,l1,l1,l1,l1,l1,l1,
l1,l1,l1,l1,l1,l1,l1,l1,l1,l1,
l1,l1,l1,l1,l1,l1,l1,l1,l1,l1,
l1,l1,l1,l1,l1,l1,l1)*ST123Q01NA+
c(l2,l2,l2,l2,l2,l2,l2,l2,l2,l2,
l2,l2,l2,l2,l2,l2,l2,l2,l2,l2,
l2,l2,l2,l2,l2,l2,l2,l2,l2,l2,
l2,l2,l2,l2,l2,l2,l2,l2,l2,l2,
l2,l2,l2,l2,l2,l2,l2,l2,l2,l2,
l2,l2,l2,l2,l2,l2,l2)*ST123Q02NA+
c(l3,l3,l3,l3,l3,l3,l3,l3,l3,l3,
l3,l3,l3,l3,l3,l3,l3,l3,l3,l3,
l3,l3,l3,l3,l3,l3,l3,l3,l3,l3,
l3,l3,l3,l3,l3,l3,l3,l3,l3,l3,
l3,l3,l3,l3,l3,l3,l3,l3,l3,l3,
l3,l3,l3,l3,l3,l3,l3)*ST123Q03NA+
c(l4,l4,l4,l4,l4,l4,l4,l4,l4,l4,
l4,l4,l4,l4,l4,l4,l4,l4,l4,l4,
l4,l4,l4,l4,l4,l4,l4,l4,l4,l4,
l4,l4,l4,l4,l4,l4,l4,l4,l4,l4,
l4,l4,l4,l4,l4,l4,l4,l4,l4,l4,
l4,l4,l4,l4,l4,l4,l4)*ST123Q04NA

ST123Q01NA ~ 1
ST123Q02NA ~ 1
ST123Q03NA ~ 1
ST123Q04NA ~ 1


#define variances
ST123Q01NA ~~ ST123Q01NA
ST123Q02NA ~~ ST123Q02NA
ST123Q03NA ~~ ST123Q03NA
ST123Q04NA ~~ ST123Q04NA


#residual correlation


#define factor mean
f1~0*1

#define factor variance
f1~~c(1,NA,NA,NA,NA,NA,NA,NA,NA,NA,
NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
NA,NA,NA,NA,NA,NA,NA)*f1
'
sup.mi.metric.all.fit<-lavaan(sup.mi.metric.all,data=data3,missing='fiml',group = 'CNT')
fitMeasures(sup.mi.metric.all.fit,fit.measures = c('cfi','tli','rmsea'))
resid(sup.mi.metric.all.fit) 
#TLI, RMSEA even improved?!

#path model, given need to consider partial invariance, it will be necessary to include group in analysis at first, which seems to hard to get a fit lavaan project for lavaan.survey function
#will directly go to multiple group path analysis

path.all.3<-'
anx=~c(l1,l1,l1,l1,l1,l1,l1,l1,l1,l1,
l1,l1,l1,l1,l1,l1,l1,l1,l1,l1,
l1,l1,l1,l1,l1,l1,l1,l1,l1,l1,
l1,l1,l1,l1,l1,l1,l1,l1,l1,l1,
l1,l1,l1,l1,l1,l1,l1,l1,l1,l1,
l1,l1,l1,l1,l1,l1,l1)*ST118Q01NA+
c(l2,l2,l2,b2,l2,l2,l2,l2,l2,l2,
l2,l2,l2,l2,l2,l2,l2,l2,l2,l2,
l2,l2,l2,l2,l2,l2,l2,l2,l2,l2,
l2,l2,l2,l2,l2,l2,l2,l2,l2,l2,
l2,l2,l2,e2,l2,l2,l2,l2,l2,l2,
l2,l2,l2,l2,p2,l2,l2)*ST118Q02NA+
c(l3,l3,l3,l3,l3,l3,l3,l3,l3,l3,
l3,l3,l3,l3,l3,l3,f3,L3,l3,l3,
l3,l3,l3,l3,l3,l3,l3,l3,l3,l3,
l3,l3,l3,l3,l3,l3,l3,l3,l3,l3,
l3,l3,l3,l3,l3,l3,l3,l3,l3,l3,
l3,l3,l3,l3,l3,l3,l3)*ST118Q03NA+
c(l4,l4,l4,l4,l4,l4,l4,l4,l4,c4,
l4,l4,l4,l4,l4,l4,l4,l4,l4,l4,
l4,l4,l4,l4,l4,l4,l4,l4,l4,l4,
l4,l4,l4,l4,l4,l4,l4,l4,l4,l4,
l4,l4,l4,l4,l4,l4,l4,l4,l4,l4,
l4,l4,u4,l4,l4,l4,l4)*ST118Q04NA+
c(l5,l5,l5,b5,l5,l5,l5,l5,co5,c5,
l5,l5,l5,l5,l5,l5,l5,l5,l5,l5,
l5,l5,l5,l5,l5,l5,l5,l5,l5,l5,
l5,l5,l5,l5,l5,l5,l5,l5,l5,l5,
l5,l5,l5,l5,l5,l5,l5,l5,l5,l5,
l5,l5,u5,l5,l5,l5,l5)*ST118Q05NA

ST118Q01NA ~ 1
ST118Q02NA ~ 1
ST118Q03NA ~ 1
ST118Q04NA ~ 1
ST118Q05NA ~ 1

#define variances
ST118Q01NA ~~ ST118Q01NA
ST118Q02NA ~~ ST118Q02NA
ST118Q03NA ~~ ST118Q03NA
ST118Q04NA ~~ ST118Q04NA
ST118Q05NA ~~ ST118Q05NA

#residual correlation
ST118Q01NA ~ ST118Q02NA

#define factor mean
anx~0*1

#define factor variance
anx~~c(1,NA,NA,1,NA,NA,NA,NA,NA,NA,
NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
NA,NA,NA,NA,NA,NA,NA)*anx

mot=~c(l1,l1,l1,g1,l1,l1,l1,l1,X1,l1,
l1,l1,l1,l1,l1,l1,l1,l1,A1,l1,
l1,S1,l1,l1,l1,l1,l1,l1,l1,l1,
M1,l1,l1,l1,l1,l1,l1,B1,l1,l1,
l1,l1,l1,l1,l1,l1,l1,S1,l1,l1,
l1,l1,l1,l1,l1,l1,l1)*ST119Q01NA+
c(l2,l2,l2,B2,l2,l2,l2,l2,l2,l2,
l2,l2,l2,l2,l2,l2,l2,l2,l2,l2,
l2,l2,l2,y2,x2,l2,l2,l2,v2,l2,
l2,l2,l2,l2,l2,C2,l2,l2,Q2,l2,
l2,l2,l2,l2,l2,l2,l2,l2,l2,l2,
l2,l2,l2,l2,S2,l2,l2)*ST119Q02NA+
c(a3,l3,l3,g3,l3,l3,l3,l3,X3,l3,
l3,l3,l3,l3,l3,l3,l3,l3,l3,l3,
l3,l3,l3,l3,l3,l3,l3,l3,l3,l3,
l3,l3,l3,l3,l3,l3,l3,l3,l3,l3,
l3,l3,l3,l3,l3,l3,T3,l3,l3,l3,
l3,l3,l3,Y3,l3,l3,l3)*ST119Q03NA+
c(l4,l4,l4,v4,l4,l4,l4,l4,C4,l4,
l4,l4,l4,D4,l4,l4,l4,l4,A4,l4,
l4,l4,l4,l4,l4,l4,l4,l4,l4,l4,
M4,l4,l4,l4,l4,P4,l4,l4,b4,l4,
l4,l4,l4,l4,l4,l4,g4,S4,l4,l4,
l4,l4,l4,l4,Q4,l4,l4)*ST119Q04NA+
c(b5,l5,l5,z5,l5,l5,l5,l5,l5,l5,
l5,l5,l5,l5,l5,l5,l5,l5,A5,l5,
l5,l5,l5,l5,l5,l5,l5,l5,l5,l5,
l5,l5,N5,l5,l5,l5,l5,l5,l5,l5,
l5,l5,l5,l5,l5,V5,W2,S5,l5,l5,
l5,l5,l5,Y5,l5,l5,l5)*ST119Q05NA

ST119Q01NA ~ 1
ST119Q02NA ~ 1
ST119Q03NA ~ 1
ST119Q04NA ~ 1
ST119Q05NA ~ 1

#define variances
ST119Q01NA ~~ ST119Q01NA
ST119Q02NA ~~ ST119Q02NA
ST119Q03NA ~~ ST119Q03NA
ST119Q04NA ~~ ST119Q04NA
ST119Q05NA ~~ ST119Q05NA

#residual correlation
ST119Q01NA ~ ST119Q02NA

#define factor mean
mot~0*1

#define factor variance
mot~~c(1,NA,NA,NA,NA,NA,NA,NA,NA,NA,
NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
NA,NA,NA,NA,NA,NA,NA)*mot


sup=~c(l1,l1,l1,l1,l1,l1,l1,l1,l1,l1,
l1,l1,l1,l1,l1,l1,l1,l1,l1,l1,
l1,l1,l1,l1,l1,l1,l1,l1,l1,l1,
l1,l1,l1,l1,l1,l1,l1,l1,l1,l1,
l1,l1,l1,l1,l1,l1,l1,l1,l1,l1,
l1,l1,l1,l1,l1,l1,l1)*ST123Q01NA+
c(l2,l2,l2,l2,l2,l2,l2,l2,l2,l2,
l2,l2,l2,l2,l2,l2,l2,l2,l2,l2,
l2,l2,l2,l2,l2,l2,l2,l2,l2,l2,
l2,l2,l2,l2,l2,l2,l2,l2,l2,l2,
l2,l2,l2,l2,l2,l2,l2,l2,l2,l2,
l2,l2,l2,l2,l2,l2,l2)*ST123Q02NA+
c(l3,l3,l3,l3,l3,l3,l3,l3,l3,l3,
l3,l3,l3,l3,l3,l3,l3,l3,l3,l3,
l3,l3,l3,l3,l3,l3,l3,l3,l3,l3,
l3,l3,l3,l3,l3,l3,l3,l3,l3,l3,
l3,l3,l3,l3,l3,l3,l3,l3,l3,l3,
l3,l3,l3,l3,l3,l3,l3)*ST123Q03NA+
c(l4,l4,l4,l4,l4,l4,l4,l4,l4,l4,
l4,l4,l4,l4,l4,l4,l4,l4,l4,l4,
l4,l4,l4,l4,l4,l4,l4,l4,l4,l4,
l4,l4,l4,l4,l4,l4,l4,l4,l4,l4,
l4,l4,l4,l4,l4,l4,l4,l4,l4,l4,
l4,l4,l4,l4,l4,l4,l4)*ST123Q04NA

ST123Q01NA ~ 1
ST123Q02NA ~ 1
ST123Q03NA ~ 1
ST123Q04NA ~ 1


#define variances
ST123Q01NA ~~ ST123Q01NA
ST123Q02NA ~~ ST123Q02NA
ST123Q03NA ~~ ST123Q03NA
ST123Q04NA ~~ ST123Q04NA


#residual correlation


#define factor mean
sup~0*1

#define factor variance
sup~~c(1,NA,NA,NA,NA,NA,NA,NA,NA,NA,
NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
NA,NA,NA,NA,NA,NA,NA)*sup

anx~c(a,a,a,a,a,a,a,a,a,a,
a,a,a,a,a,a,a,a,a,a,
a,a,a,a,a,a,a,a,a,a,
a,a,a,a,a,a,a,a,a,a,
a,a,a,a,a,a,a,a,a,a,
a,a,a,a,a,a,a)*sup
mot~c(b,b,b,b,b,b,b,b,b,b,
b,b,b,b,b,b,b,b,b,b,
b,b,b,b,b,b,b,b,b,b,
b,b,b,b,b,b,b,b,b,b,
b,b,b,b,b,b,b,b,b,b,
b,b,b,b,b,b,b)*sup
anx~~mot
anx~ESCS
mot~ESCS
'
path.all.fit.2<-lavaan(path.all.2,data=data3,missing='fiml',group = 'CNTRYID')
#i had warning about positive definite with model that free estimate path in each group. but get rid of the warning by constrain it to be equal across groups
#anyway I can put in classes of country estimates?

fitMeasures(path.all.fit,fit.measures = c('cfi','tli','rmsea'))
fitMeasures(path.all.fit.2,fit.measures = c('cfi','tli','rmsea'))
lavInspect(path.all.fit.2, what='coef')

# add in country level variable-individalistic score
library(haven)
PISA2015_full <- read_sav("PISA2015_full.sav")

temp<-cbind(PISA2015_full$CNTSTUID,PISA2015_full$ESCS)
temp<-as.data.frame(temp)
colnames(temp)<-c('CNTSTUID','ESCS')
library(dplyr)
data4<-left_join(data3,temp,by='CNTSTUID')
path.all.fit.3<-lavaan(path.all.3,data=data4,missing='fiml',group = 'CNTRYID')
fitmeasures(path.all.fit.3, fit.measures = c('cfi','tli','rmsea'))
inspect(path.all.fit.3,'coef')


#addin individualistic
library(readxl)
indiv<- read_excel("INDIVIDUALISTIC.xlsx",col_types = c('text','text','numeric'))
temp2<-cbind.data.frame(indiv$CNT,indiv$individualism)
temp2<-as.data.frame(temp2)
colnames(temp2)<-c('CNT','INDI')
data5<-left_join(data4,temp2,by='CNT')

#regression by group
library(lme4)
install.packages('lmerTest')
library(lmerTest)
library(lattice)
View(data5)

manx<-aggregate(ANXTEST~CNT,data=data5, FUN=mean)
mescs<-aggregate(ESCS~CNT,data=data5, FUN=mean)
manx=as.data.frame(manx)
colnames(manx)<-c('CNT','manx')
mescs=as.data.frame(mescs)
colnames(mescs)<-c('CNT','mescs')
library(dplyr)
data5<-left_join(data5,manx,by='CNT')
data5<-left_join(data5,mescs,by='CNT')
data5$canx<-data5$ANXTEST -data5$manx.y
data5$cescs<-data5$ESCS-data5$mescs.y
#escs,anxtest,mot are already a standardized score

library(readxl)
gdp<- read_excel("gdp_percapita.xlsx",col_types = c('text','text','numeric','numeric'))
temp3<-cbind.data.frame(gdp$CNT,gdp$gdp)
colnames(temp3)<-c('CNT','GDPpp')
data5<-left_join(data5,temp3,by='CNT')

data5$INDI<-as.numeric(data5$INDI)

data5$GDPppstd<-scale(data5$GDPpp,center = TRUE,scale = TRUE)

model1<-lmer(MOTIVAT ~ANXTEST+ESCS+(1|CNT), data=data5,REML = FALSE)
summary(model1)

model2<-lmer(MOTIVAT ~ANXTEST+ESCS+GDPppstd+INDI*ANXTEST+(1|CNT), data=data5,REML = FALSE)
summary(model2)

model3<-lmer(MOTIVAT ~ANXTEST+ESCS+GDPppstd+INDI+GDPppstd*ANXTEST+INDI*ANXTEST+(1|CNT), data=data5,REML = FALSE)
summary(model3)

length(unique(data5$CNT))
#graph
cnt<-unique(data5$CNT)
cnt_color <- rainbow(57)
cnt2<-c('USA','GBR','CAN','AUS','ESP','IRL','JPN','KOR','MAC','HKG')
cnt_color2 <- rainbow(10)

# First make frame for plot
plot(data5$ANXTEST, data5$MOTIVAT,type = 'n', 
     ylim = c(-3, 3),
     xlim = c(-3, 3),
     cex.main = 1.5,
     xlab = 'Test anxiety', 
     ylab = "Achievement motivation",
     main = "Separate Regression for Each country "
)


# now fit and add regression lines
for(i in 1:10){
  sub <- data5[which(data5$CNT==cnt2[i]),]
  fitted <- fitted(lm(MOTIVAT~ANXTEST,data=sub))
  lines(sub$ANXTEST,fitted,col=cnt_color[i])
}



#library(lavaan.survey)
#pisa.svy<-svydesign(ids = data3$CNTSTUID, prob = ~1, data = data3)
#pisa.svy.fit<-lavaan.survey()


xyplot(anxtest ~ escs+emosupp|cnt, data=pisap,type ='l')
fits<-lmList(anxtest ~ escs+motivat|cnt, data=pisap)
fits

library(haven)
data <- read_sav("/Users/xiong/Box Sync/Pisa/PISA2015_full.sav")
fits2<-lmList(ANXTEST ~ ESCS+EMOSUPS|CNT, data=data)
fits3<-lmList(MOTIVAT ~ ESCS+EMOSUPS|CNT, data=data)
fits4<-lmList(MOTIVAT ~ ESCS+ANXTEST|CNT, data=data)
fits5<-lmList(ANXTEST ~ ESCS+MOTIVAT|CNT, data=data)
a<-coef(fits2)
b<-coef(fits3)
c<-coef(fits4)
d<-coef(fits5)
out<-cbind(a,b,c,d)
class(out)
write.csv(out,file = 'fit.csv')
wd()
#prepare data for alignment, delete country ARE for missing reasons
varname<-c('CNT','CNTRYID','CNTSCHID','CNTSTUID','OECD','ESCS','ST118Q01NA','ST118Q02NA','ST118Q03NA','ST118Q04NA','ST118Q05NA',
          'ST119Q01NA','ST119Q02NA','ST119Q03NA','ST119Q04NA','ST119Q05NA','ST123Q01NA','ST123Q02NA','ST123Q03NA','ST123Q04NA')


install.packages('dplyr')
library(dplyr)
a<-select(PISA2015_full,varname)
varname2<-c('W_FSTUWT','escs','ST004D01T')
b<-select(data,varname2) # this only has the countries with parenting measures, need to get a full dataset with there variables to combine


install.packages('readxl')
library(readxl)
indiv<- read_excel("INDIVIDUALISTIC.xlsx")
c<-cbind.data.frame(indiv$CNT,indiv$individualism)
colnames(c)<-c('CNT','INDI')
c<-unique.data.frame(c)
data1<-merge(a,c,by.x = 'CNT',by.y = 'CNT') #why by this method, I lose over 10,0000 observations?
data1.1<-left_join(a,c,by='CNT')

gdp<- read_excel("gdp_percapita.xlsx",col_types = c('text','text','numeric','numeric'))
temp3<-cbind.data.frame(gdp$CNT,gdp$gdp)
colnames(temp3)<-c('CNT','GDPpp')
temp3<-unique(temp3)
data2<-merge.data.frame(data1,temp3,by.x = 'CNT',by.y = 'CNT')
data2.1<-left_join(data1.1,temp3,by='CNT')
data3<-data2.1[data2.1$CNT!='ALB',]
#CHECK MISSING
miss<-data3[!complete.cases(data3),]
e<-table(miss$CNT)
f<-table(data3$CNT)
e-f

#drop missing countries
cntname<-c('DZA','GEO','IDN','ISR','JOR','KSV','LBN','MAC','MDA',
           'MKD','MLT','MNE','QAR','QES','QUC','QUD','QUE','ROU','TAP','TTO','TUN','VNM')
g<-data3[!data3$CNT %in% cntname,]
f<-data3[data3$CNT %in% cntname,]
write.csv(g,file = 'align_data.csv', na='NA',row.names = TRUE)
table(g$CNTSCHID)

data<-read.csv('align_data.csv')
table(data$AUS)


library(MplusAutomation)

#double check multilevel
data<-read.csv('align_data.csv')
colnames(data)<-c('CNT', 'CNTRYID', 'CNTSCHID', 'CNTSTUID', 'OECD', 'ESCS',
                  'st118q1n1', 'st118q1n2', 'st118q1n3', 'st118q1n4', 'st118q1n5',
                  'st119q1n1', 'st119q1n2', 'st119q1n3', 'st119q1n4', 'st119q1n5',
                  's123q1n1', 's123q1n2', 's123q1n3', 's123q1n4',
                  'INDI', 'GDPpp')

  
  
  