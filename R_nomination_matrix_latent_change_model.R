library(psych)
library(data.table)
library(Matrix)
install.packages('readr')
library(readr)
library(lme4)
library(lmerTest)
getwd()
setwd("C:/Users/xiong/Dropbox/peer2ndproject")
peer_cha<- read_csv("C:/Users/xiong/Dropbox/peer2ndproject/peer_characteristics.csv")
View(peer_cha)
#descriptives
col_index2<-grep('peermath', colnames(peer_wide))
describeBy(peer_wide[,col_index2],peer_wide$country)


w1fri=read.csv("w1fri_matrix.csv",row.names = 1)
w2fri=read.csv("w2fri_matrix.csv",row.names = 1)
w3fri=read.csv("w3fri_matrix.csv",row.names = 1)

#w1 peer antiso
w1fri=as.matrix(w1fri)
w1peer_anti=as.matrix(peer_cha$antisoW1)

w1peer_anti[is.na(w1peer_anti)] <- 0
w1peer_antiso=w1fri %*% w1peer_anti
w1peer_antiso[w1peer_antiso==0]<-NA
nomination=rowSums(w1fri)
nomination=as.numeric(nomination)
w1peer_antiso=cbind(w1peer_antiso,nomination)
w1peer_antiso=as.data.frame(w1peer_antiso)
w1peer_antiso$average=w1peer_antiso$V1/w1peer_antiso$V2

#w2 peer antiso
w2fri=as.matrix(w2fri)
w2peer_anti=as.matrix(peer_cha$antisoW2)
w2peer_anti[is.na(w2peer_anti)] <- 0
w2peer_antiso=w2fri %*% w2peer_anti
w2peer_antiso[w2peer_antiso==0]<-NA
nominationw2=rowSums(w2fri)
nominationw2=as.numeric(nominationw2)
w2peer_antiso=cbind(w2peer_antiso,nominationw2)
w2peer_antiso=as.data.frame(w2peer_antiso)
w2peer_antiso$average=w2peer_antiso$V1/w2peer_antiso$nominationw2

#3 peer antiso
w3fri=as.matrix(w3fri)
w3peer_anti=as.matrix(peer_cha$antisoW3)
w3peer_anti[is.na(w3peer_anti)] <- 0
w3peer_antiso=w3fri %*% w3peer_anti
w3peer_antiso[w3peer_antiso==0]<-NA
nominationw3=rowSums(w3fri)
nominationw3=as.numeric(nominationw3)
w3peer_antiso=cbind(w3peer_antiso,nominationw3)
w3peer_antiso=as.data.frame(w3peer_antiso)
w3peer_antiso$average=w3peer_antiso$V1/w3peer_antiso$nominationw3


#change peer antiso data to long to explore trajectory
dataw=cbind(peer_cha$ID,peer_cha$country,w1peer_antiso$average,w2peer_antiso$average,w3peer_antiso$average)

colnames(dataw)=c('ID','country','peerantiw1','peerantiw2','peerantiw3')
temp1=c('peerantiw1','peerantiw2','peerantiw3')
temp2='peeranti'
peer_long=l_to_w(temp1,temp2)

l_to_w <- function(x,y)
{
  a=melt(as.data.table(dataw), id.vars=c('ID','country'),measure.vars=x)
  names(a)[4]<-y
  names(a)[3]<-"wave"
  a$wave=str_sub(a$wave,-1)
  return(a)
}

dataw=as.data.table(dataw)
datawus=subset(dataw,dataw$country==1)
datawcn=subset(dataw,dataw$country==2)
describe(datawus)
peer_long$country=as.factor(peer_long$country)
peer_long$wave=as.numeric(peer_long$wave)
model.p=lmer(peeranti~1+wave+country+(1+wave|ID),data = peer_long, REML = FALSE)
summary(model.p)

#write peer_anti in to peer_wide dataset
peer_wide<-read.csv("C:/Users/xiong/Dropbox/peer2ndproject/peerprop_wide.csv",row.names = 1)
peer_wide=cbind(peer_wide,dataw$peerantiw1,dataw$peerantiw2,dataw$peerantiw3)
names(peer_wide)[36:38]=temp1
write.csv(peer_wide,file = 'peerprop_wide.csv')

#get academic achievement variable into dataset

peer_aci<-read.csv('aca_soc_LPA.csv')
View(peer_aci)
names(peer_aci[,1])<-'ID'
colnames(peer_aci)[1]<-'ID'
col_index<-grep('^autsup|psyctr|Zmath|Zlanguage',colnames(peer_aci))
col_index<-c(1,col_index)
achi<-subset(peer_aci[,col_index])
peer_wide<-merge(peer_wide,achi,by='ID')
write.csv(peer_wide,file = 'peerprop_wide.csv')

#get friends academic achievement
w1fri=read.csv("w1fri_matrix.csv",row.names = 1)
w2fri=read.csv("w2fri_matrix.csv",row.names = 1)
w3fri=read.csv("w3fri_matrix.csv",row.names = 1)

#w1 math
w1fri=as.matrix(w1fri)
w1peer_math=as.matrix(peer_wide$Zmathw1)
w1peer_math[is.na(w1peer_math)] <- 0
w1peer_zmath=w1fri %*% w1peer_math
w1peer_zmath[w1peer_zmath==0]<-NA
nomination=rowSums(w1fri)
nomination=as.numeric(nomination)
w1peer_zmath=cbind(w1peer_zmath,nomination)
w1peer_zmath=as.data.frame(w1peer_zmath)
w1peer_zmath$average=w1peer_zmath$V1/w1peer_zmath$nomination

#w2 math
w2fri=as.matrix(w2fri)
w2peer_math=as.matrix(peer_wide$Zmathw2)
w2peer_math[is.na(w2peer_math)] <- 0
w2peer_zmath=w2fri %*% w2peer_math
w2peer_zmath[w2peer_zmath==0]<-NA
nomination=rowSums(w2fri)
nomination=as.numeric(nomination)
w2peer_zmath=cbind(w2peer_zmath,nomination)
w2peer_zmath=as.data.frame(w2peer_zmath)
w2peer_zmath$average=w2peer_zmath$V1/w2peer_zmath$nomination

#w3 math
w3fri=as.matrix(w3fri)
w3peer_math=as.matrix(peer_wide$Zmathw3)
w3peer_math[is.na(w3peer_math)] <- 0
w3peer_zmath=w3fri %*% w3peer_math
w3peer_zmath[w3peer_zmath==0]<-NA
nomination=rowSums(w3fri)
nomination=as.numeric(nomination)
w3peer_zmath=cbind(w3peer_zmath,nomination)
w3peer_zmath=as.data.frame(w3peer_zmath)
w3peer_zmath$average=w3peer_zmath$V1/w3peer_zmath$nomination

peer_wide=cbind(peer_wide,w1peer_zmath$average,w2peer_zmath$average,w3peer_zmath$average)
names(peer_wide)[51:53]=c('peermathw1','peermathw2','peermathw3')
write.csv(peer_wide,file = 'peerprop_wide.csv')

#check country regression
dataus=subset(peer_wide,peer_wide$country==1)
datacn=subset(peer_wide,peer_wide$country==2)

col_index2<-grep('^peer',colnames(peer_wide))


cor(dataus[,col_index2],use = "pairwise.complete.obs")
cor(datacn[,col_index2],use = "pairwise.complete.obs")

m1<-lm(dataus$peercdW2~dataus$peercdW1+dataus$peermathw1,data = dataus)
summary(m1)

m1.1<-lm(dataus$peerrstW2~dataus$peerrstW1+dataus$peermathw1,data = dataus)
summary(m1.1)

m1.2<-lm(dataus$peerintW2~dataus$peerintW1+dataus$peermathw1,data = dataus)
summary(m1.2)


m2<-lm(datacn$peercdW2~datacn$peercdW1+datacn$peermathw1,data = datacn)
summary(m2)

m3<-lm(datacn$peerrstW2~datacn$peerrstW1+datacn$peermathw1,data = datacn)
summary(m3)

m1.4<-lm(datacn$peerintW2~datacn$peerintW1+datacn$peermathw1,data = dataus)
summary(m1.4)

#w1 lan
w1fri=as.matrix(w1fri)
w1peer_lan=as.matrix(peer_wide$Zlanguagew1)
w1peer_lan[is.na(w1peer_lan)] <- 0
w1peer_zlan=w1fri %*% w1peer_lan
w1peer_zlan[w1peer_zlan==0]<-NA
nominationw1=rowSums(w1fri)
nominationw1=as.numeric(nominationw1)
w1peer_zlan=cbind(w1peer_zlan,nominationw1)
w1peer_zlan=as.data.frame(w1peer_zlan)
w1peer_zlan$average=w1peer_zlan$V1/w1peer_zlan$nominationw1

#w2 lan
w2fri=as.matrix(w2fri)
w2peer_lan=as.matrix(peer_wide$Zlanguagew2)
w2peer_lan[is.na(w2peer_lan)] <- 0
w2peer_zlan=w2fri %*% w2peer_lan
w2peer_zlan[w2peer_zlan==0]<-NA
nominationw2=rowSums(w2fri)
nominationw2=as.numeric(nominationw2)
w2peer_zlan=cbind(w2peer_zlan,nominationw2)
w2peer_zlan=as.data.frame(w2peer_zlan)
w2peer_zlan$average=w2peer_zlan$V1/w2peer_zlan$nominationw2

#w3 lan
w3fri=as.matrix(w3fri)
w3peer_lan=as.matrix(peer_wide$Zlanguagew3)
w3peer_lan[is.na(w3peer_lan)] <- 0
w3peer_zlan=w3fri %*% w3peer_lan
w3peer_zlan[w3peer_zlan==0]<-NA
nominationw3=rowSums(w3fri)
nominationw3=as.numeric(nominationw3)
w3peer_zlan=cbind(w3peer_zlan,nominationw3)
w3peer_zlan=as.data.frame(w3peer_zlan)
w3peer_zlan$average=w3peer_zlan$V1/w3peer_zlan$nominationw3


peer_wide=cbind(peer_wide,w1peer_zlan$average,w2peer_zlan$average,w3peer_zlan$average,w1peer_zlan$nominationw1,
                w2peer_zlan$nominationw2,w3peer_zlan$nominationw3)
names(peer_wide)[54:59]=c('peerlanw1','peerlanw2','peerlanw3','frinomiw1','frinomiw2','frinomiw3')
peer_wide$frinomiw1[peer_wide$frinomiw1==0]<-NA
peer_wide$frinomiw2[peer_wide$frinomiw2==0]<-NA
peer_wide$frinomiw3[peer_wide$frinomiw3==0]<-NA
write.csv(peer_wide,file = 'peerprop_wide.csv')


#check country regression
dataus=subset(peer_wide,peer_wide$country==1)
datacn=subset(peer_wide,peer_wide$country==2)

col_index2<-grep('^peer|dpeer',colnames(peer_wide))


cor(dataus[,col_index2],use = "pairwise.complete.obs")
cor(datacn[,col_index2],use = "pairwise.complete.obs")

m1<-lm(dataus$peercdW2~dataus$peercdW1+dataus$peerlanw1+dataus$peerantiw1,data = dataus)
summary(m1)

m1.1<-lm(dataus$peerrstW2~dataus$peerrstW1+dataus$peerlanw1+dataus$peerantiw1,data = dataus)
summary(m1.1)

m1.1.1<-lm(dataus$peercdW2~dataus$peercdW1+dataus$peerlanw1+dataus$peerantiw1,data = dataus)
summary(m1.1.1)


m1.2<-lm(dataus$peerintW2~dataus$peerintW1+dataus$peerlanw1,data = dataus)
summary(m1.2)


m2<-lm(datacn$peercdW2~datacn$peercdW1+datacn$peerlanw1+datacn$peerantiw1,data = datacn)
summary(m2)

m3<-lm(datacn$peerrstW2~datacn$peerrstW1+datacn$peerlanw1+datacn$peerantiw1,data = datacn)
summary(m3)

m1.4<-lm(datacn$peerintW2~datacn$peerintW1+datacn$peerlanw1,data = dataus)
summary(m1.4)

#try change score
peer_wide$dpeercdw12<-peer_wide$peercdW2-peer_wide$peercdW1
peer_wide$dpeercdw23<-peer_wide$peercdW3-peer_wide$peercdW2
peer_wide$dpeerrstw12<-peer_wide$peerrstW2 -peer_wide$peerrstW1
peer_wide$dpeerrstw23<-peer_wide$peerrstW3 -peer_wide$peerrstW2
peer_wide$dpeerantiw12<-peer_wide$peerantiw2-peer_wide$peerantiw1
peer_wide$dpeermathw12<-peer_wide$peermathw2-peer_wide$peermathw1
peer_wide$dpeerlanw12<-peer_wide$peerlanw2-peer_wide$peerlanw1
peer_wide$dpeerantiw32<-peer_wide$peerantiw3-peer_wide$peerantiw2
peer_wide$dpeermathw32<-peer_wide$peermathw3-peer_wide$peermathw2
peer_wide$dpeerlanw32<-peer_wide$peerlanw3-peer_wide$peerlanw2


col_index2<-grep('^dpeer',colnames(peer_wide))


cor(dataus[,col_index2],use = "pairwise",method =  'pearson')
cor.test(dataus$dpeerantiw32,dataus$dpeerrstw23)

cor(datacn[,col_index2],use = "pairwise",method =  'pearson')
cor.test(datacn$dpeermathw12,datacn$dpeerrstw23)

m1.5<-lm(datacn$dpeercdw23~datacn$dpeercdw12+datacn$dpeerantiw12+datacn$dpeermathw12+datacn$dpeerlanw12,data = datacn)
summary(m1.5)
m1.7<-lm(peer_wide$dpeercdw23~peer_wide$dpeercdw12+
           peer_wide$dpeerantiw12+peer_wide$dpeermathw12+peer_wide$dpeerlanw12
         +peer_wide$country+peer_wide$country*peer_wide$dpeerantiw12
         +peer_wide$country*peer_wide$dpeermathw12,data = peer_wide)
summary(m1.7)

m1.5.2<-lm(datacn$dpeerrstw23~datacn$dpeerrstw12+datacn$dpeerantiw12+datacn$dpeermathw12+datacn$dpeerlanw12,data = datacn)
summary(m1.5.2)


m1.6<-lm(dataus$dpeercdw23 ~ dataus$dpeercdw12+dataus$dpeerantiw12+dataus$dpeermathw12+dataus$dpeerlanw12,data = dataus)
summary(m1.6)
m1.6.2<-lm(dataus$dpeerrstw23~dataus$dpeerrstw12+dataus$dpeerantiw12+dataus$dpeermathw12+dataus$dpeerlanw12,data = dataus)
summary(m1.6.2)

describeBy(peer_wide, group=peer_wide$country)

m1.8<-lm(datacn$dpeerantiw32~datacn$dpeercdw23+datacn$dpeerrstw23+datacn$dpeerantiw12,data = datacn)
summary(m1.8)

m1.9<-lm(dataus$dpeerantiw32~dataus$dpeercdw23+dataus$dpeerrstw23+dataus$dpeerantiw12,data = dataus)
summary(m1.9)

#try latent change score model
library(lavaan)
head(peer_wide)

#prcd
lcs.prcd<-'
#define time-specific latent variables
prcd1 =~ 1*peercdW1
prcd2 =~ 1*peercdW2
prcd3 =~ 1*peercdW3

#define structural relations

prcd2 ~ 1*prcd1
prcd3 ~ 1*prcd2

#define change factors

dprcd2 =~ 1*prcd2
dprcd3 =~ 1*prcd3

#specify latent means and variance

prcd1 ~ 1
prcd2 ~ (0)*1
prcd3 ~ (0)*1

dprcd2 ~ 1 # estimate mean of change scores
dprcd3 ~ 1

prcd1 ~~ prcd1 # estimate variance of start point
prcd2 ~~ (0)*prcd2 #fix variance estimate of other point to 0, since variance goes to change score
prcd3 ~~ (0)*prcd3 

dprcd2 ~~ dprcd2 #estimate variance of change score
dprcd3 ~~ dprcd3 

#specify intercept-change relations

dprcd2 ~ (a)*prcd1
dprcd3 ~ (a)*prcd2

#specify change-change relations

dprcd3 ~~0* dprcd2

#define indicator intercepts, fix to 0, mean goes to factor score, the read indicator has loading of 1

peercdW1 ~ 0*1 
peercdW2 ~ 0*1
peercdW3 ~ 0*1


#define indicator residual variance

peercdW1 ~~ (b)*peercdW1
peercdW2 ~~ (b)*peercdW2
peercdW3 ~~ (b)*peercdW3

#specify residual correlations
peercdW1 ~~ 0*peercdW3+0*peercdW2
peercdW2 ~~  0*peercdW3'


lcs.prcd.fit<-lavaan(lcs.prcd,data=dataus, missing="fiml")
summary(lcs.prcd.fit, fit.measures=T, standardized=T)

lcs.prcd.fit2<-lavaan(lcs.prcd,data=datacn, missing="fiml")
summary(lcs.prcd.fit2, fit.measures=T, standardized=T)


#prrst

lcs.prrst<-'
#define time-specific latent variables
prrst1 =~ 1*peerrstW1
prrst2 =~ 1*peerrstW2
prrst3 =~ 1*peerrstW3

#define structural relations

prrst2 ~ 1*prrst1
prrst3 ~ 1*prrst2

#define change factors

dprrst2 =~ 1*prrst2
dprrst3 =~ 1*prrst3

#specify latent means and variance

prrst1 ~ 1
prrst2 ~ (0)*1
prrst3 ~ (0)*1

dprrst2 ~ 1 # estimate mean of change scores
dprrst3 ~ 1

prrst1 ~~ prrst1 # estimate variance of start point
prrst2 ~~ (0)*prrst2 #fix variance estimate of other point to 0, since variance goes to change score
prrst3 ~~ (0)*prrst3 

dprrst2 ~~ dprrst2 #estimate variance of change score
dprrst3 ~~ dprrst3 

#specify intercept-change relations

dprrst2 ~ (a)*prrst1
dprrst3 ~ (a)*prrst2

#specify change-change relations

dprrst3 ~~0* dprrst2

#define indicator intercepts, fix to 0, mean goes to factor score, the read indicator has loading of 1

peerrstW1 ~ 0*1 
peerrstW2 ~ 0*1
peerrstW3 ~ 0*1


#define indicator residual variance

peerrstW1 ~~ (b)*peerrstW1
peerrstW2 ~~ (b)*peerrstW2
peerrstW3 ~~ (b)*peerrstW3

#specify residual correlations
peerrstW1 ~~ 0*peerrstW3+0*peerrstW2
peerrstW2 ~~  0*peerrstW3'


lcs.prrst.fit<-lavaan(lcs.prrst,data=dataus, missing="fiml")
summary(lcs.prrst.fit, fit.measures=T, standardized=T)

lcs.prrst.fit2<-lavaan(lcs.prrst,data=datacn, missing="fiml")
summary(lcs.prrst.fit2, fit.measures=T, standardized=T)

#antisocial behavior
lcs.peeranti<-'
#define time-specific latent variables
peeranti1 =~ 1*peerantiw1
peeranti2 =~ 1*peerantiw2
peeranti3 =~ 1*peerantiw3

#define structural relations

peeranti2 ~ 1*peeranti1
peeranti3 ~ 1*peeranti2

#define change factors

dpeeranti2 =~ 1*peeranti2
dpeeranti3 =~ 1*peeranti3

#specify latent means and variance

peeranti1 ~ 1
peeranti2 ~ (0)*1
peeranti3 ~ (0)*1

dpeeranti2 ~ 1 # estimate mean of change scores
dpeeranti3 ~ 1

peeranti1 ~~ peeranti1 # estimate variance of start point
peeranti2 ~~ (0)*peeranti2 #fix variance estimate of other point to 0, since variance goes to change score
peeranti3 ~~ (0)*peeranti3 

dpeeranti2 ~~ dpeeranti2 #estimate variance of change score
dpeeranti3 ~~ dpeeranti3 

#specify intercept-change relations

dpeeranti2 ~ (a)*peeranti1
dpeeranti3 ~ (b)*peeranti2

#specify change-change relations

dpeeranti3 ~~0* dpeeranti2

#define indicator intercepts, fix to 0, mean goes to factor score, the read indicator has loading of 1

peerantiw1 ~ 0*1 
peerantiw2 ~ 0*1
peerantiw3 ~ 0*1


#define indicator residual variance

peerantiw1 ~~ (0)*peerantiw1
peerantiw2 ~~ (0)*peerantiw2
peerantiw3 ~~ (0)*peerantiw3

#specify residual correlations
peerantiw1 ~~ 0*peerantiw3+0*peerantiw2
peerantiw2 ~~  0*peerantiw3'


lcs.peeranti.fit<-lavaan(lcs.peeranti,data=dataus, missing="fiml")
summary(lcs.peeranti.fit, fit.measures=T, standardized=T)

lcs.peeranti.fit2<-lavaan(lcs.peeranti,data=datacn, missing="fiml")
summary(lcs.peeranti.fit2, fit.measures=T, standardized=T)

#peerlan
lcs.peerlan<-'
#define time-specific latent variables
peerlan1 =~ 1*peerlanw1
peerlan2 =~ 1*peerlanw2
peerlan3 =~ 1*peerlanw3

#define structural relations

peerlan2 ~ 1*peerlan1
peerlan3 ~ 1*peerlan2

#define change factors

dpeerlan2 =~ 1*peerlan2
dpeerlan3 =~ 1*peerlan3

#specify latent means and variance

peerlan1 ~ 1
peerlan2 ~ (0)*1
peerlan3 ~ (0)*1

dpeerlan2 ~ 1 # estimate mean of change scores
dpeerlan3 ~ 1

peerlan1 ~~ peerlan1 # estimate variance of start point
peerlan2 ~~ (0)*peerlan2 #fix variance estimate of other point to 0, since variance goes to change score
peerlan3 ~~ (0)*peerlan3 

dpeerlan2 ~~ dpeerlan2 #estimate variance of change score
dpeerlan3 ~~ dpeerlan3 

#specify intercept-change relations

dpeerlan2 ~ (a)*peerlan1
dpeerlan3 ~ (a)*peerlan2

#specify change-change relations

dpeerlan3 ~~0* dpeerlan2

#define indicator intercepts, fix to 0, mean goes to factor score, the read indicator has loading of 1

peerlanw1 ~ 0*1 
peerlanw2 ~ 0*1
peerlanw3 ~ 0*1


#define indicator residual variance

peerlanw1 ~~ (c)*peerlanw1
peerlanw2 ~~ (c)*peerlanw2
peerlanw3 ~~ (c)*peerlanw3

#specify residual correlations
peerlanw1 ~~ 0*peerlanw3+0*peerlanw2
peerlanw2 ~~  0*peerlanw3'


lcs.peerlan.fit<-lavaan(lcs.peerlan,data=dataus, missing="fiml")
summary(lcs.peerlan.fit, fit.measures=T, standardized=T)

lcs.peerlan.fit2<-lavaan(lcs.peerlan,data=datacn, missing="fiml")
summary(lcs.peerlan.fit2, fit.measures=T, standardized=T)
#locally misfit for chinese data

#use peer anti to explain prcd

#define time-specific latent variables

lcs.crosslag<-
'peeranti1 =~ 1*peerantiw1
peeranti2 =~ 1*peerantiw2
peeranti3 =~ 1*peerantiw3

#define structural relations

peeranti2 ~ 1*peeranti1
peeranti3 ~ 1*peeranti2

#define change factors

dpeeranti2 =~ 1*peeranti2
dpeeranti3 =~ 1*peeranti3

#specify latent means and variance

peeranti1 ~ 1
peeranti2 ~ (0)*1
peeranti3 ~ (0)*1

dpeeranti2 ~ 1 # estimate mean of change scores
dpeeranti3 ~ 1

peeranti1 ~~ peeranti1 # estimate variance of start point
peeranti2 ~~ (0)*peeranti2 #fix variance estimate of other point to 0, since variance goes to change score
peeranti3 ~~ (0)*peeranti3 

dpeeranti2 ~~ dpeeranti2 #estimate variance of change score
dpeeranti3 ~~ dpeeranti3 

#specify intercept-change relations

dpeeranti2 ~ (g)*peeranti1
dpeeranti3 ~ (h)*peeranti2

#specify change-change relations

dpeeranti3 ~~0* dpeeranti2

#define indicator intercepts, fix to 0, mean goes to factor score, the read indicator has loading of 1

peerantiw1 ~ 0*1 
peerantiw2 ~ 0*1
peerantiw3 ~ 0*1


#define indicator residual variance

peerantiw1 ~~ (0)*peerantiw1
peerantiw2 ~~ (0)*peerantiw2
peerantiw3 ~~ (0)*peerantiw3

#specify residual correlations
peerantiw1 ~~ 0*peerantiw3+0*peerantiw2
peerantiw2 ~~  0*peerantiw3

#define time-specific latent variables
prcd1 =~ 1*peercdW1
prcd2 =~ 1*peercdW2
prcd3 =~ 1*peercdW3

#define structural relations

prcd2 ~ 1*prcd1
prcd3 ~ 1*prcd2

#define change factors

dprcd2 =~ 1*prcd2
dprcd3 =~ 1*prcd3

#specify latent means and variance

prcd1 ~ 1
prcd2 ~ (0)*1
prcd3 ~ (0)*1

dprcd2 ~ 1 # estimate mean of change scores
dprcd3 ~ 1

prcd1 ~~ prcd1 # estimate variance of start point
prcd2 ~~ (0)*prcd2 #fix variance estimate of other point to 0, since variance goes to change score
prcd3 ~~ (0)*prcd3 

dprcd2 ~~ dprcd2 #estimate variance of change score
dprcd3 ~~ dprcd3 

#specify intercept-change relations

dprcd2 ~ (e)*prcd1
dprcd3 ~ (e)*prcd2

#specify change-change relations

dprcd3 ~~0* dprcd2

#define indicator intercepts, fix to 0, mean goes to factor score, the read indicator has loading of 1

peercdW1 ~ 0*1 
peercdW2 ~ 0*1
peercdW3 ~ 0*1


#define indicator residual variance

peercdW1 ~~ (i)*peercdW1
peercdW2 ~~ (i)*peercdW2
peercdW3 ~~ (i)*peercdW3

#specify residual correlations
peercdW1 ~~ 0*peercdW3+0*peercdW2
peercdW2 ~~  0*peercdW3

dprcd3~dpeeranti2
dpeeranti3~dprcd2

prcd1~~0*peeranti1+0*peeranti2+0*peeranti3
prcd2~~0*peeranti1+0*peeranti2+0*peeranti3
prcd3~~0*peeranti1+0*peeranti2+0*peeranti3'

lcs.crosslag.fit<-lavaan(lcs.crosslag,data=dataus, missing="fiml")
summary(lcs.crosslag.fit, fit.measures=T, standardized=T)

lcs.crosslag.fit<-lavaan(lcs.crosslag,data=datacn, missing="fiml")
summary(lcs.crosslag.fit, fit.measures=T, standardized=T)


###use peer anti to explain prrst

#define time-specific latent variables

lcs.crosslag.prrst<-
  'peeranti1 =~ 1*peerantiw1
peeranti2 =~ 1*peerantiw2
peeranti3 =~ 1*peerantiw3

#define structural relations

peeranti2 ~ 1*peeranti1
peeranti3 ~ 1*peeranti2

#define change factors

dpeeranti2 =~ 1*peeranti2
dpeeranti3 =~ 1*peeranti3

#specify latent means and variance

peeranti1 ~ 1
peeranti2 ~ (0)*1
peeranti3 ~ (0)*1

dpeeranti2 ~ 1 # estimate mean of change scores
dpeeranti3 ~ 1

peeranti1 ~~ peeranti1 # estimate variance of start point
peeranti2 ~~ (0)*peeranti2 #fix variance estimate of other point to 0, since variance goes to change score
peeranti3 ~~ (0)*peeranti3 

dpeeranti2 ~~ dpeeranti2 #estimate variance of change score
dpeeranti3 ~~ dpeeranti3 

#specify intercept-change relations

dpeeranti2 ~ (g)*peeranti1
dpeeranti3 ~ (h)*peeranti2

#specify change-change relations

dpeeranti3 ~~0* dpeeranti2

#define indicator intercepts, fix to 0, mean goes to factor score, the read indicator has loading of 1

peerantiw1 ~ 0*1 
peerantiw2 ~ 0*1
peerantiw3 ~ 0*1


#define indicator residual variance

peerantiw1 ~~ (0)*peerantiw1
peerantiw2 ~~ (0)*peerantiw2
peerantiw3 ~~ (0)*peerantiw3

#specify residual correlations
peerantiw1 ~~ 0*peerantiw3+0*peerantiw2
peerantiw2 ~~  0*peerantiw3

#define time-specific latent variables
prrst1 =~ 1*peerrstW1
prrst2 =~ 1*peerrstW2
prrst3 =~ 1*peerrstW3

#define structural relations

prrst2 ~ 1*prrst1
prrst3 ~ 1*prrst2

#define change factors

dprrst2 =~ 1*prrst2
dprrst3 =~ 1*prrst3

#specify latent means and variance

prrst1 ~ 1
prrst2 ~ (0)*1
prrst3 ~ (0)*1

dprrst2 ~ 1 # estimate mean of change scores
dprrst3 ~ 1

prrst1 ~~ prrst1 # estimate variance of start point
prrst2 ~~ (0)*prrst2 #fix variance estimate of other point to 0, since variance goes to change score
prrst3 ~~ (0)*prrst3 

dprrst2 ~~ dprrst2 #estimate variance of change score
dprrst3 ~~ dprrst3 

#specify intercept-change relations

dprrst2 ~ (e)*prrst1
dprrst3 ~ (e)*prrst2

#specify change-change relations

dprrst3 ~~0* dprrst2

#define indicator intercepts, fix to 0, mean goes to factor score, the read indicator has loading of 1

peerrstW1 ~ 0*1 
peerrstW2 ~ 0*1
peerrstW3 ~ 0*1


#define indicator residual variance

peerrstW1 ~~ (i)*peerrstW1
peerrstW2 ~~ (i)*peerrstW2
peerrstW3 ~~ (i)*peerrstW3

#specify residual correlations
peerrstW1 ~~ 0*peerrstW3+0*peerrstW2
peerrstW2 ~~  0*peerrstW3

dprrst3~dpeeranti2
dpeeranti3~dprrst2

prrst1~~0*peeranti1+0*peeranti2+0*peeranti3
prrst2~~0*peeranti1+0*peeranti2+0*peeranti3
prrst3~~0*peeranti1+0*peeranti2+0*peeranti3'

lcs.crosslag.prrst.fit<-lavaan(lcs.crosslag.prrst,data=dataus, missing="fiml")
summary(lcs.crosslag.prrst.fit, fit.measures=T, standardized=T)

lcs.crosslag.prrst.fit<-lavaan(lcs.crosslag.prrst,data=datacn, missing="fiml")
summary(lcs.crosslag.prrst.fit, fit.measures=T, standardized=T)



#use peer lan to explain prcd

#define time-specific latent variables

lcs.crosslag.lan<-
'#define time-specific latent variables
peerlan1 =~ 1*peerlanw1
peerlan2 =~ 1*peerlanw2
peerlan3 =~ 1*peerlanw3

#define structural relations

peerlan2 ~ 1*peerlan1
peerlan3 ~ 1*peerlan2

#define change factors

dpeerlan2 =~ 1*peerlan2
dpeerlan3 =~ 1*peerlan3

#specify latent means and variance

peerlan1 ~ 1
peerlan2 ~ (0)*1
peerlan3 ~ (0)*1

dpeerlan2 ~ 1 # estimate mean of change scores
dpeerlan3 ~ 1

peerlan1 ~~ peerlan1 # estimate variance of start point
peerlan2 ~~ (0)*peerlan2 #fix variance estimate of other point to 0, since variance goes to change score
peerlan3 ~~ (0)*peerlan3 

dpeerlan2 ~~ dpeerlan2 #estimate variance of change score
dpeerlan3 ~~ dpeerlan3 

#specify intercept-change relations

dpeerlan2 ~ (a)*peerlan1
dpeerlan3 ~ (a)*peerlan2

#specify change-change relations

dpeerlan3 ~~0* dpeerlan2

#define indicator intercepts, fix to 0, mean goes to factor score, the read indicator has loading of 1

peerlanw1 ~ 0*1 
peerlanw2 ~ 0*1
peerlanw3 ~ 0*1


#define indicator residual variance

peerlanw1 ~~ (0)*peerlanw1
peerlanw2 ~~ (c)*peerlanw2
peerlanw3 ~~ (c)*peerlanw3

#specify residual correlations
peerlanw1 ~~ 0*peerlanw3+0*peerlanw2
peerlanw2 ~~  0*peerlanw3

#define time-specific latent variables
prcd1 =~ 1*peercdW1
prcd2 =~ 1*peercdW2
prcd3 =~ 1*peercdW3

#define structural relations

prcd2 ~ 1*prcd1
prcd3 ~ 1*prcd2

#define change factors

dprcd2 =~ 1*prcd2
dprcd3 =~ 1*prcd3

#specify latent means and variance

prcd1 ~ 1
prcd2 ~ (0)*1
prcd3 ~ (0)*1

dprcd2 ~ 1 # estimate mean of change scores
dprcd3 ~ 1

prcd1 ~~ prcd1 # estimate variance of start point
prcd2 ~~ (0)*prcd2 #fix variance estimate of other point to 0, since variance goes to change score
prcd3 ~~ (0)*prcd3 

dprcd2 ~~ dprcd2 #estimate variance of change score
dprcd3 ~~ dprcd3 

#specify intercept-change relations

dprcd2 ~ (e)*prcd1
dprcd3 ~ (e)*prcd2

#specify change-change relations

dprcd3 ~~0* dprcd2

#define indicator intercepts, fix to 0, mean goes to factor score, the read indicator has loading of 1

peercdW1 ~ 0*1 
peercdW2 ~ 0*1
peercdW3 ~ 0*1


#define indicator residual variance

peercdW1 ~~ (i)*peercdW1
peercdW2 ~~ (i)*peercdW2
peercdW3 ~~ (i)*peercdW3

#specify residual correlations
peercdW1 ~~ 0*peercdW3+0*peercdW2
peercdW2 ~~  0*peercdW3

dprcd3~dpeerlan2
dpeerlan3~dprcd2

prcd1~~0*peerlan1+0*peerlan2+0*peerlan3
prcd2~~0*peerlan1+0*peerlan2+0*peerlan3
prcd3~~0*peerlan1+0*peerlan2+0*peerlan3'


lcs.crosslag.lan.fit<-lavaan(lcs.crosslag.lan,data=dataus, missing="fiml")
summary(lcs.crosslag.lan.fit, fit.measures=T, standardized=T)

lcs.crosslag.lan.fit<-lavaan(lcs.crosslag.lan,data=datacn, missing="fiml")
summary(lcs.crosslag.lan.fit, fit.measures=T, standardized=T)

lcs.crosslag.prrst.lan<-

'#define time-specific latent variables
peerlan1 =~ 1*peerlanw1
peerlan2 =~ 1*peerlanw2
peerlan3 =~ 1*peerlanw3

#define structural relations

peerlan2 ~ 1*peerlan1
peerlan3 ~ 1*peerlan2

#define change factors

dpeerlan2 =~ 1*peerlan2
dpeerlan3 =~ 1*peerlan3

#specify latent means and variance

peerlan1 ~ 1
peerlan2 ~ (0)*1
peerlan3 ~ (0)*1

dpeerlan2 ~ 1 # estimate mean of change scores
dpeerlan3 ~ 1

peerlan1 ~~ peerlan1 # estimate variance of start point
peerlan2 ~~ (0)*peerlan2 #fix variance estimate of other point to 0, since variance goes to change score
peerlan3 ~~ (0)*peerlan3 

dpeerlan2 ~~ dpeerlan2 #estimate variance of change score
dpeerlan3 ~~ dpeerlan3 

#specify intercept-change relations

dpeerlan2 ~ (a)*peerlan1
dpeerlan3 ~ (a)*peerlan2

#specify change-change relations

dpeerlan3 ~~ 0* dpeerlan2

#define indicator intercepts, fix to 0, mean goes to factor score, the read indicator has loading of 1

peerlanw1 ~ 0*1 
peerlanw2 ~ 0*1
peerlanw3 ~ 0*1


#define indicator residual variance

peerlanw1 ~~ (c)*peerlanw1
peerlanw2 ~~ (c)*peerlanw2
peerlanw3 ~~ (c)*peerlanw3

#specify residual correlations
peerlanw1 ~~ 0*peerlanw3+0*peerlanw2
peerlanw2 ~~  0*peerlanw3

#define time-specific latent variables
prrst1 =~ 1*peerrstW1
prrst2 =~ 1*peerrstW2
prrst3 =~ 1*peerrstW3

#define structural relations

prrst2 ~ 1*prrst1
prrst3 ~ 1*prrst2

#define change factors

dprrst2 =~ 1*prrst2
dprrst3 =~ 1*prrst3

#specify latent means and variance

prrst1 ~ 1
prrst2 ~ (0)*1
prrst3 ~ (0)*1

dprrst2 ~ 1 # estimate mean of change scores
dprrst3 ~ 1

prrst1 ~~ prrst1 # estimate variance of start point
prrst2 ~~ (0)*prrst2 #fix variance estimate of other point to 0, since variance goes to change score
prrst3 ~~ (0)*prrst3 

dprrst2 ~~ dprrst2 #estimate variance of change score
dprrst3 ~~ dprrst3 

#specify intercept-change relations

dprrst2 ~ (e)*prrst1
dprrst3 ~ (e)*prrst2

#specify change-change relations

dprrst3 ~~0* dprrst2

#define indicator intercepts, fix to 0, mean goes to factor score, the read indicator has loading of 1

peerrstW1 ~ 0*1 
peerrstW2 ~ 0*1
peerrstW3 ~ 0*1


#define indicator residual variance

peerrstW1 ~~ (i)*peerrstW1
peerrstW2 ~~ (i)*peerrstW2
peerrstW3 ~~ (i)*peerrstW3

#specify residual correlations
peerrstW1 ~~ 0*peerrstW3+0*peerrstW2
peerrstW2 ~~  0*peerrstW3

dprrst3~dpeerlan2
dpeerlan3~dprrst2

prrst1~~0*peerlan1+0*peerlan2+0*peerlan3
prrst2~~0*peerlan1+0*peerlan2+0*peerlan3
prrst3~~0*peerlan1+0*peerlan2+0*peerlan3'


lcs.crosslag.prrst.lan.fit<-lavaan(lcs.crosslag.prrst.lan,data=dataus, missing="fiml")
summary(lcs.crosslag.prrst.lan.fit, fit.measures=T, standardized=T)

lcs.crosslag.prrst.lan.fit<-lavaan(lcs.crosslag.prrst.lan,data=datacn, missing="fiml")
summary(lcs.crosslag.prrst.lan.fit, fit.measures=T, standardized=T)

#prcd predicted by antiso lan

lcs.crosslag.lan.anti<-
  '
peeranti1 =~ 1*peerantiw1
peeranti2 =~ 1*peerantiw2
peeranti3 =~ 1*peerantiw3

#define structural relations

peeranti2 ~ 1*peeranti1
peeranti3 ~ 1*peeranti2

#define change factors

dpeeranti2 =~ 1*peeranti2
dpeeranti3 =~ 1*peeranti3

#specify latent means and variance

peeranti1 ~ 1
peeranti2 ~ (0)*1
peeranti3 ~ (0)*1

dpeeranti2 ~ 1 # estimate mean of change scores
dpeeranti3 ~ 1

peeranti1 ~~ peeranti1 # estimate variance of start point
peeranti2 ~~ (0)*peeranti2 #fix variance estimate of other point to 0, since variance goes to change score
peeranti3 ~~ (0)*peeranti3 

dpeeranti2 ~~ dpeeranti2 #estimate variance of change score
dpeeranti3 ~~ dpeeranti3 

#specify intercept-change relations

dpeeranti2 ~ (g)*peeranti1
dpeeranti3 ~ (h)*peeranti2

#specify change-change relations

dpeeranti3 ~~0* dpeeranti2

#define indicator intercepts, fix to 0, mean goes to factor score, the read indicator has loading of 1

peerantiw1 ~ 0*1 
peerantiw2 ~ 0*1
peerantiw3 ~ 0*1


#define indicator residual variance

peerantiw1 ~~ (0)*peerantiw1
peerantiw2 ~~ (0)*peerantiw2
peerantiw3 ~~ (0)*peerantiw3

#specify residual correlations
peerantiw1 ~~ 0*peerantiw3+0*peerantiw2
peerantiw2 ~~  0*peerantiw3

#define time-specific latent variables
peerlan1 =~ 1*peerlanw1
peerlan2 =~ 1*peerlanw2
peerlan3 =~ 1*peerlanw3

#define structural relations

peerlan2 ~ 1*peerlan1
peerlan3 ~ 1*peerlan2

#define change factors

dpeerlan2 =~ 1*peerlan2
dpeerlan3 =~ 1*peerlan3

#specify latent means and variance

peerlan1 ~ 1
peerlan2 ~ (0)*1
peerlan3 ~ (0)*1

dpeerlan2 ~ 1 # estimate mean of change scores
dpeerlan3 ~ 1

peerlan1 ~~ peerlan1 # estimate variance of start point
peerlan2 ~~ (0)*peerlan2 #fix variance estimate of other point to 0, since variance goes to change score
peerlan3 ~~ (0)*peerlan3 

dpeerlan2 ~~ dpeerlan2 #estimate variance of change score
dpeerlan3 ~~ dpeerlan3 

#specify intercept-change relations

dpeerlan2 ~ (a)*peerlan1
dpeerlan3 ~ (a)*peerlan2

#specify change-change relations

dpeerlan3 ~~0* dpeerlan2

#define indicator intercepts, fix to 0, mean goes to factor score, the read indicator has loading of 1

peerlanw1 ~ 0*1 
peerlanw2 ~ 0*1
peerlanw3 ~ 0*1


#define indicator residual variance

peerlanw1 ~~ (c)*peerlanw1
peerlanw2 ~~ (c)*peerlanw2
peerlanw3 ~~ (c)*peerlanw3

#specify residual correlations
peerlanw1 ~~ 0*peerlanw3+0*peerlanw2
peerlanw2 ~~  0*peerlanw3

#define time-specific latent variables
prcd1 =~ 1*peercdW1
prcd2 =~ 1*peercdW2
prcd3 =~ 1*peercdW3

#define structural relations

prcd2 ~ 1*prcd1
prcd3 ~ 1*prcd2

#define change factors

dprcd2 =~ 1*prcd2
dprcd3 =~ 1*prcd3

#specify latent means and variance

prcd1 ~ 1
prcd2 ~ (0)*1
prcd3 ~ (0)*1

dprcd2 ~ 1 # estimate mean of change scores
dprcd3 ~ 1

prcd1 ~~ prcd1 # estimate variance of start point
prcd2 ~~ (0)*prcd2 #fix variance estimate of other point to 0, since variance goes to change score
prcd3 ~~ (0)*prcd3 

dprcd2 ~~ dprcd2 #estimate variance of change score
dprcd3 ~~ dprcd3 

#specify intercept-change relations

dprcd2 ~ (e)*prcd1
dprcd3 ~ (e)*prcd2

#specify change-change relations

dprcd3 ~~0* dprcd2

#define indicator intercepts, fix to 0, mean goes to factor score, the read indicator has loading of 1

peercdW1 ~ 0*1 
peercdW2 ~ 0*1
peercdW3 ~ 0*1


#define indicator residual variance

peercdW1 ~~ (i)*peercdW1
peercdW2 ~~ (i)*peercdW2
peercdW3 ~~ (i)*peercdW3

#specify residual correlations
peercdW1 ~~ 0*peercdW3+0*peercdW2
peercdW2 ~~  0*peercdW3

dprcd3~dpeerlan2+dpeeranti2+dprcd2
dpeerlan3~dprcd2+dpeeranti2+dpeerlan2
dpeeranti3~dpeerlan2+dprcd2++dpeeranti2

prcd1~~0*peerlan1+0*peerlan2+0*peerlan3
prcd2~~0*peerlan1+0*peerlan2+0*peerlan3
prcd3~~0*peerlan1+0*peerlan2+0*peerlan3

prcd1~~0*peeranti1+0*peeranti2+0*peeranti3
prcd2~~0*peeranti1+0*peeranti2+0*peeranti3
prcd3~~0*peeranti1+0*peeranti2+0*peeranti3

peeranti1~~0*peerlan1+0*peerlan2+0*peerlan3
peeranti2~~0*peerlan1+0*peerlan2+0*peerlan3
peeranti3~~0*peerlan1+0*peerlan2+0*peerlan3
'
lcs.crosslag.lan.anti.fit2<-lavaan(lcs.crosslag.lan.anti,data=dataus, missing="fiml")
summary(lcs.crosslag.lan.anti.fit2, fit.measures=T, standardized=T)

lcs.crosslag.lan.anti.fit<-lavaan(lcs.crosslag.lan.anti,data=datacn, missing="fiml")
summary(lcs.crosslag.lan.anti.fit, fit.measures=T, standardized=T)

#model fit reall bad

#multi-group model prcd by anti
lcs.crosslag.mg<-
  'peeranti1 =~ 1*peerantiw1
peeranti2 =~ 1*peerantiw2
peeranti3 =~ 1*peerantiw3

#define structural relations

peeranti2 ~ 1*peeranti1
peeranti3 ~ 1*peeranti2

#define change factors

dpeeranti2 =~ 1*peeranti2
dpeeranti3 =~ 1*peeranti3

#specify latent means and variance

peeranti1 ~ 1
peeranti2 ~ (0)*1
peeranti3 ~ (0)*1

dpeeranti2 ~ 1 # estimate mean of change scores
dpeeranti3 ~ 1

peeranti1 ~~ peeranti1 # estimate variance of start point
peeranti2 ~~ (0)*peeranti2 #fix variance estimate of other point to 0, since variance goes to change score
peeranti3 ~~ (0)*peeranti3 

dpeeranti2 ~~ dpeeranti2 #estimate variance of change score
dpeeranti3 ~~ dpeeranti3 

#specify intercept-change relations

dpeeranti2 ~ c(a1,a2)*peeranti1
dpeeranti3 ~ c(a1,a2)*peeranti2

#specify change-change relations

dpeeranti3 ~~0* dpeeranti2

#define indicator intercepts, fix to 0, mean goes to factor score, the read indicator has loading of 1

peerantiw1 ~ 0*1 
peerantiw2 ~ 0*1
peerantiw3 ~ 0*1


#define indicator residual variance

peerantiw1 ~~ (0)*peerantiw1
peerantiw2 ~~ (0)*peerantiw2
peerantiw3 ~~ (0)*peerantiw3

#specify residual correlations
peerantiw1 ~~ 0*peerantiw3+0*peerantiw2
peerantiw2 ~~  0*peerantiw3

#define time-specific latent variables
prcd1 =~ 1*peercdW1
prcd2 =~ 1*peercdW2
prcd3 =~ 1*peercdW3

#define structural relations

prcd2 ~ 1*prcd1
prcd3 ~ 1*prcd2

#define change factors

dprcd2 =~ 1*prcd2
dprcd3 =~ 1*prcd3

#specify latent means and variance

prcd1 ~ 1
prcd2 ~ (0)*1
prcd3 ~ (0)*1

dprcd2 ~ 1 # estimate mean of change scores
dprcd3 ~ 1

prcd1 ~~ prcd1 # estimate variance of start point
prcd2 ~~ (0)*prcd2 #fix variance estimate of other point to 0, since variance goes to change score
prcd3 ~~ (0)*prcd3 

dprcd2 ~~ dprcd2 #estimate variance of change score
dprcd3 ~~ dprcd3 

#specify intercept-change relations

dprcd2 ~ c(e1,e1)*prcd1
dprcd3 ~ c(e1,e1)*prcd2

#specify change-change relations

dprcd3 ~~0* dprcd2

#define indicator intercepts, fix to 0, mean goes to factor score, the read indicator has loading of 1

peercdW1 ~ 0*1 
peercdW2 ~ 0*1
peercdW3 ~ 0*1


#define indicator residual variance

peercdW1 ~~ c(i1,i2)*peercdW1
peercdW2 ~~ c(i1,i2)*peercdW2
peercdW3 ~~ c(i1,i2)*peercdW3

#specify residual correlations
peercdW1 ~~ 0*peercdW3+0*peercdW2
peercdW2 ~~  0*peercdW3

dprcd3~c(k1,k1)*dpeeranti2+dprcd2
dpeeranti3~dprcd2+dpeeranti2

prcd1~~0*peeranti1+0*peeranti2+0*peeranti3
prcd2~~0*peeranti1+0*peeranti2+0*peeranti3
prcd3~~0*peeranti1+0*peeranti2+0*peeranti3'

lcs.crosslag.mg.fit<-lavaan(lcs.crosslag.mg,data=peer_wide,group='country', missing="fiml")
summary(lcs.crosslag.mg.fit, fit.measures=T, standardized=T)


#multi_group lcs prcd on lan

lcs.crosslag.lan<-
  '#define time-specific latent variables
peerlan1 =~ 1*peerlanw1
peerlan2 =~ 1*peerlanw2
peerlan3 =~ 1*peerlanw3

#define structural relations

peerlan2 ~ 1*peerlan1
peerlan3 ~ 1*peerlan2

#define change factors

dpeerlan2 =~ 1*peerlan2
dpeerlan3 =~ 1*peerlan3

#specify latent means and variance

peerlan1 ~ 1
peerlan2 ~ (0)*1
peerlan3 ~ (0)*1

dpeerlan2 ~ 1 # estimate mean of change scores
dpeerlan3 ~ 1

peerlan1 ~~ peerlan1 # estimate variance of start point
peerlan2 ~~ (0)*peerlan2 #fix variance estimate of other point to 0, since variance goes to change score
peerlan3 ~~ (0)*peerlan3 

dpeerlan2 ~~ dpeerlan2 #estimate variance of change score
dpeerlan3 ~~ dpeerlan3 

#specify intercept-change relations

dpeerlan2 ~ c(a1,a2)*peerlan1
dpeerlan3 ~ c(a1,a2)*peerlan2

#specify change-change relations

dpeerlan3 ~~0* dpeerlan2

#define indicator intercepts, fix to 0, mean goes to factor score, the read indicator has loading of 1

peerlanw1 ~ 0*1 
peerlanw2 ~ 0*1
peerlanw3 ~ 0*1


#define indicator residual variance

peerlanw1 ~~ (0)*peerlanw1
peerlanw2 ~~ c(0,0)*peerlanw2
peerlanw3 ~~ c(0,0)*peerlanw3

#specify residual correlations
peerlanw1 ~~ 0*peerlanw3+0*peerlanw2
peerlanw2 ~~  0*peerlanw3

#define time-specific latent variables
prcd1 =~ 1*peercdW1
prcd2 =~ 1*peercdW2
prcd3 =~ 1*peercdW3

#define structural relations

prcd2 ~ 1*prcd1
prcd3 ~ 1*prcd2

#define change factors

dprcd2 =~ 1*prcd2
dprcd3 =~ 1*prcd3

#specify latent means and variance

prcd1 ~ 1
prcd2 ~ (0)*1
prcd3 ~ (0)*1

dprcd2 ~ 1 # estimate mean of change scores
dprcd3 ~ 1

prcd1 ~~ prcd1 # estimate variance of start point
prcd2 ~~ (0)*prcd2 #fix variance estimate of other point to 0, since variance goes to change score
prcd3 ~~ (0)*prcd3 

dprcd2 ~~ dprcd2 #estimate variance of change score
dprcd3 ~~ dprcd3 

#specify intercept-change relations

dprcd2 ~ c(e1,e2)*prcd1
dprcd3 ~ c(e1,e2)*prcd2

#specify change-change relations

dprcd3 ~~0* dprcd2

#define indicator intercepts, fix to 0, mean goes to factor score, the read indicator has loading of 1

peercdW1 ~ 0*1 
peercdW2 ~ 0*1
peercdW3 ~ 0*1


#define indicator residual variance

peercdW1 ~~ c(i2,i2)*peercdW1
peercdW2 ~~ c(i2,i2)*peercdW2
peercdW3 ~~ c(i2,i2)*peercdW3

#specify residual correlations
peercdW1 ~~ 0*peercdW3+0*peercdW2
peercdW2 ~~  0*peercdW3

dprcd3~c(k1,k1)*dpeerlan2+dprcd2
dpeerlan3~dprcd2+dpeerlan2

prcd1~~0*peerlan1+0*peerlan2+0*peerlan3
prcd2~~0*peerlan1+0*peerlan2+0*peerlan3
prcd3~~0*peerlan1+0*peerlan2+0*peerlan3'

lcs.crosslag.lan.fit<-lavaan(lcs.crosslag.lan,data=peer_wide,group = 'country', missing="fiml")
summary(lcs.crosslag.lan.fit, fit.measures=T, standardized=T)


#multigroup prrst on lan
lcs.crosslag.prrst.lan.mg<-
  
  '#define time-specific latent variables
peerlan1 =~ 1*peerlanw1
peerlan2 =~ 1*peerlanw2
peerlan3 =~ 1*peerlanw3

#define structural relations

peerlan2 ~ 1*peerlan1
peerlan3 ~ 1*peerlan2

#define change factors

dpeerlan2 =~ 1*peerlan2
dpeerlan3 =~ 1*peerlan3

#specify latent means and variance

peerlan1 ~ 1
peerlan2 ~ (0)*1
peerlan3 ~ (0)*1

dpeerlan2 ~ 1 # estimate mean of change scores
dpeerlan3 ~ 1

peerlan1 ~~ peerlan1 # estimate variance of start point
peerlan2 ~~ (0)*peerlan2 #fix variance estimate of other point to 0, since variance goes to change score
peerlan3 ~~ (0)*peerlan3 

dpeerlan2 ~~ dpeerlan2 #estimate variance of change score
dpeerlan3 ~~ dpeerlan3 

#specify intercept-change relations

dpeerlan2 ~ c(a1,a2)*peerlan1
dpeerlan3 ~ c(a1,a2)*peerlan2

#specify change-change relations

dpeerlan3 ~~ 0* dpeerlan2

#define indicator intercepts, fix to 0, mean goes to factor score, the read indicator has loading of 1

peerlanw1 ~ 0*1 
peerlanw2 ~ 0*1
peerlanw3 ~ 0*1


#define indicator residual variance

peerlanw1 ~~ 0*peerlanw1
peerlanw2 ~~ c(c1,f2)*peerlanw2
peerlanw3 ~~ c(c1,f2)*peerlanw3

#specify residual correlations
peerlanw1 ~~ 0*peerlanw3+0*peerlanw2
peerlanw2 ~~  0*peerlanw3

#define time-specific latent variables
prrst1 =~ 1*peerrstW1
prrst2 =~ 1*peerrstW2
prrst3 =~ 1*peerrstW3

#define structural relations

prrst2 ~ 1*prrst1
prrst3 ~ 1*prrst2

#define change factors

dprrst2 =~ 1*prrst2
dprrst3 =~ 1*prrst3

#specify latent means and variance

prrst1 ~ 1
prrst2 ~ (0)*1
prrst3 ~ (0)*1

dprrst2 ~ 1 # estimate mean of change scores
dprrst3 ~ 1

prrst1 ~~ prrst1 # estimate variance of start point
prrst2 ~~ (0)*prrst2 #fix variance estimate of other point to 0, since variance goes to change score
prrst3 ~~ (0)*prrst3 

dprrst2 ~~ dprrst2 #estimate variance of change score
dprrst3 ~~ dprrst3 

#specify intercept-change relations

dprrst2 ~ c(e1,e2)*prrst1
dprrst3 ~ c(e1,e2)*prrst2

#specify change-change relations

dprrst3 ~~0* dprrst2

#define indicator intercepts, fix to 0, mean goes to factor score, the read indicator has loading of 1

peerrstW1 ~ 0*1 
peerrstW2 ~ 0*1
peerrstW3 ~ 0*1


#define indicator residual variance

peerrstW1 ~~ c(i1,i2)*peerrstW1
peerrstW2 ~~ c(i1,i2)*peerrstW2
peerrstW3 ~~ c(i1,i2)*peerrstW3

#specify residual correlations
peerrstW1 ~~ 0*peerrstW3+0*peerrstW2
peerrstW2 ~~  0*peerrstW3

dprrst3~c(k1,k2)*dpeerlan2
dpeerlan3~dprrst2

prrst1~~0*peerlan1+0*peerlan2+0*peerlan3
prrst2~~0*peerlan1+0*peerlan2+0*peerlan3
prrst3~~0*peerlan1+0*peerlan2+0*peerlan3'


lcs.crosslag.prrst.lan.mg.fit<-lavaan(lcs.crosslag.prrst.lan.mg,data=peer_wide,group = 'country', missing="fiml")
summary(lcs.crosslag.prrst.lan.mg.fit, fit.measures=T, standardized=T)








#multigroup lan and anti together
lcs.crosslag.lan.anti.mg<-
  '
peeranti1 =~ 1*peerantiw1
peeranti2 =~ 1*peerantiw2
peeranti3 =~ 1*peerantiw3

#define structural relations

peeranti2 ~ 1*peeranti1
peeranti3 ~ 1*peeranti2

#define change factors

dpeeranti2 =~ 1*peeranti2
dpeeranti3 =~ 1*peeranti3

#specify latent means and variance

peeranti1 ~ 1
peeranti2 ~ (0)*1
peeranti3 ~ (0)*1

dpeeranti2 ~ 1 # estimate mean of change scores
dpeeranti3 ~ 1

peeranti1 ~~ peeranti1 # estimate variance of start point
peeranti2 ~~ (0)*peeranti2 #fix variance estimate of other point to 0, since variance goes to change score
peeranti3 ~~ (0)*peeranti3 

dpeeranti2 ~~ dpeeranti2 #estimate variance of change score
dpeeranti3 ~~ dpeeranti3 

#specify intercept-change relations

dpeeranti2 ~ c(g1,g2)*peeranti1
dpeeranti3 ~ c(h1,h2)*peeranti2

#specify change-change relations

dpeeranti3 ~~0* dpeeranti2

#define indicator intercepts, fix to 0, mean goes to factor score, the read indicator has loading of 1

peerantiw1 ~ 0*1 
peerantiw2 ~ 0*1
peerantiw3 ~ 0*1


#define indicator residual variance

peerantiw1 ~~ (0)*peerantiw1
peerantiw2 ~~ (0)*peerantiw2
peerantiw3 ~~ (0)*peerantiw3

#specify residual correlations
peerantiw1 ~~ 0*peerantiw3+0*peerantiw2
peerantiw2 ~~  0*peerantiw3

#define time-specific latent variables
peerlan1 =~ 1*peerlanw1
peerlan2 =~ 1*peerlanw2
peerlan3 =~ 1*peerlanw3

#define structural relations

peerlan2 ~ 1*peerlan1
peerlan3 ~ 1*peerlan2

#define change factors

dpeerlan2 =~ 1*peerlan2
dpeerlan3 =~ 1*peerlan3

#specify latent means and variance

peerlan1 ~ 1
peerlan2 ~ (0)*1
peerlan3 ~ (0)*1

dpeerlan2 ~ 1 # estimate mean of change scores
dpeerlan3 ~ 1

peerlan1 ~~ peerlan1 # estimate variance of start point
peerlan2 ~~ (0)*peerlan2 #fix variance estimate of other point to 0, since variance goes to change score
peerlan3 ~~ (0)*peerlan3 

dpeerlan2 ~~ dpeerlan2 #estimate variance of change score
dpeerlan3 ~~ dpeerlan3 

#specify intercept-change relations

dpeerlan2 ~ c(a1,a2)*peerlan1
dpeerlan3 ~ c(a1,a2)*peerlan2

#specify change-change relations

dpeerlan3 ~~0* dpeerlan2

#define indicator intercepts, fix to 0, mean goes to factor score, the read indicator has loading of 1

peerlanw1 ~ 0*1 
peerlanw2 ~ 0*1
peerlanw3 ~ 0*1


#define indicator residual variance

peerlanw1 ~~ (0)*peerlanw1
peerlanw2 ~~ c(c1,c2)*peerlanw2
peerlanw3 ~~ c(c1,c2)*peerlanw3

#specify residual correlations
peerlanw1 ~~ 0*peerlanw3+0*peerlanw2
peerlanw2 ~~  0*peerlanw3

#define time-specific latent variables
prcd1 =~ 1*peercdW1
prcd2 =~ 1*peercdW2
prcd3 =~ 1*peercdW3

#define structural relations

prcd2 ~ 1*prcd1
prcd3 ~ 1*prcd2

#define change factors

dprcd2 =~ 1*prcd2
dprcd3 =~ 1*prcd3

#specify latent means and variance

prcd1 ~ 1
prcd2 ~ (0)*1
prcd3 ~ (0)*1

dprcd2 ~ 1 # estimate mean of change scores
dprcd3 ~ 1

prcd1 ~~ prcd1 # estimate variance of start point
prcd2 ~~ (0)*prcd2 #fix variance estimate of other point to 0, since variance goes to change score
prcd3 ~~ (0)*prcd3 

dprcd2 ~~ dprcd2 #estimate variance of change score
dprcd3 ~~ dprcd3 

#specify intercept-change relations

dprcd2 ~ c(e1,e2)*prcd1
dprcd3 ~ c(e2,e2)*prcd2

#specify change-change relations

dprcd3 ~~0* dprcd2

#define indicator intercepts, fix to 0, mean goes to factor score, the read indicator has loading of 1

peercdW1 ~ 0*1 
peercdW2 ~ 0*1
peercdW3 ~ 0*1


#define indicator residual variance

peercdW1 ~~ c(i1,i2)*peercdW1
peercdW2 ~~ c(i1,i2)*peercdW2
peercdW3 ~~ c(i1,i2)*peercdW3

#specify residual correlations
peercdW1 ~~ 0*peercdW3+0*peercdW2
peercdW2 ~~  0*peercdW3

dprcd3~dpeerlan2+dprcd2+dpeeranti2
#dpeerlan3~dpeerlan2+dprcd2+dpeeranti2
#dpeeranti3~dpeerlan2+dprcd2+dpeeranti2

prcd1~~0*peerlan1+0*peerlan2+0*peerlan3
prcd2~~0*peerlan1+0*peerlan2+0*peerlan3
prcd3~~0*peerlan1+0*peerlan2+0*peerlan3

prcd1~~0*peeranti1+0*peeranti2+0*peeranti3
prcd2~~0*peeranti1+0*peeranti2+0*peeranti3
prcd3~~0*peeranti1+0*peeranti2+0*peeranti3

peeranti1~~0*peerlan1+0*peerlan2+0*peerlan3
peeranti2~~0*peerlan1+0*peerlan2+0*peerlan3
peeranti3~~0*peerlan1+0*peerlan2+0*peerlan3
'

lcs.crosslag.lan.anti.mg.fit<-lavaan(lcs.crosslag.lan.anti.mg,data=peer_wide, group = 'country', missing="fiml")
summary(lcs.crosslag.lan.anti.mg.fit, fit.measures=T, standardized=T)

#multigroup prrst on anti and lan
lcs.crosslag.prrst.lan.anti.mg<-
  '
peeranti1 =~ 1*peerantiw1
peeranti2 =~ 1*peerantiw2
peeranti3 =~ 1*peerantiw3

#define structural relations

peeranti2 ~ 1*peeranti1
peeranti3 ~ 1*peeranti2

#define change factors

dpeeranti2 =~ 1*peeranti2
dpeeranti3 =~ 1*peeranti3

#specify latent means and variance

peeranti1 ~ 1
peeranti2 ~ (0)*1
peeranti3 ~ (0)*1

dpeeranti2 ~ 1 # estimate mean of change scores
dpeeranti3 ~ 1

peeranti1 ~~ peeranti1 # estimate variance of start point
peeranti2 ~~ (0)*peeranti2 #fix variance estimate of other point to 0, since variance goes to change score
peeranti3 ~~ (0)*peeranti3 

dpeeranti2 ~~ dpeeranti2 #estimate variance of change score
dpeeranti3 ~~ dpeeranti3 

#specify intercept-change relations

dpeeranti2 ~ c(g1,g2)*peeranti1
dpeeranti3 ~ c(h1,h2)*peeranti2

#specify change-change relations

dpeeranti3 ~~0* dpeeranti2

#define indicator intercepts, fix to 0, mean goes to factor score, the read indicator has loading of 1

peerantiw1 ~ 0*1 
peerantiw2 ~ 0*1
peerantiw3 ~ 0*1


#define indicator residual variance

peerantiw1 ~~ (0)*peerantiw1
peerantiw2 ~~ (0)*peerantiw2
peerantiw3 ~~ (0)*peerantiw3

#specify residual correlations
peerantiw1 ~~ 0*peerantiw3+0*peerantiw2
peerantiw2 ~~  0*peerantiw3

#define time-specific latent variables
peerlan1 =~ 1*peerlanw1
peerlan2 =~ 1*peerlanw2
peerlan3 =~ 1*peerlanw3

#define structural relations

peerlan2 ~ 1*peerlan1
peerlan3 ~ 1*peerlan2

#define change factors

dpeerlan2 =~ 1*peerlan2
dpeerlan3 =~ 1*peerlan3

#specify latent means and variance

peerlan1 ~ 1
peerlan2 ~ (0)*1
peerlan3 ~ (0)*1

dpeerlan2 ~ 1 # estimate mean of change scores
dpeerlan3 ~ 1

peerlan1 ~~ peerlan1 # estimate variance of start point
peerlan2 ~~ (0)*peerlan2 #fix variance estimate of other point to 0, since variance goes to change score
peerlan3 ~~ (0)*peerlan3 

dpeerlan2 ~~ dpeerlan2 #estimate variance of change score
dpeerlan3 ~~ dpeerlan3 

#specify intercept-change relations

dpeerlan2 ~ c(a1,a2)*peerlan1
dpeerlan3 ~ c(a1,a2)*peerlan2

#specify change-change relations

dpeerlan3 ~~0* dpeerlan2

#define indicator intercepts, fix to 0, mean goes to factor score, the read indicator has loading of 1

peerlanw1 ~ 0*1 
peerlanw2 ~ 0*1
peerlanw3 ~ 0*1


#define indicator residual variance

peerlanw1 ~~ c(0,c2)*peerlanw1
peerlanw2 ~~ c(c1,c2)*peerlanw2
peerlanw3 ~~ c(c1,c2)*peerlanw3

#specify residual correlations
peerlanw1 ~~ 0*peerlanw3+0*peerlanw2
peerlanw2 ~~  0*peerlanw3

#define time-specific latent variables
prrst1 =~ 1*peerrstW1
prrst2 =~ 1*peerrstW2
prrst3 =~ 1*peerrstW3

#define structural relations

prrst2 ~ 1*prrst1
prrst3 ~ 1*prrst2

#define change factors

dprrst2 =~ 1*prrst2
dprrst3 =~ 1*prrst3

#specify latent means and variance

prrst1 ~ 1
prrst2 ~ (0)*1
prrst3 ~ (0)*1

dprrst2 ~ 1 # estimate mean of change scores
dprrst3 ~ 1

prrst1 ~~ prrst1 # estimate variance of start point
prrst2 ~~ (0)*prrst2 #fix variance estimate of other point to 0, since variance goes to change score
prrst3 ~~ (0)*prrst3 

dprrst2 ~~ dprrst2 #estimate variance of change score
dprrst3 ~~ dprrst3 

#specify intercept-change relations

dprrst2 ~ c(x1,x2)*prrst1
dprrst3 ~ c(x1,x2)*prrst2

#specify change-change relations

dprrst3 ~~0* dprrst2

#define indicator intercepts, fix to 0, mean goes to factor score, the read indicator has loading of 1

peerrstW1 ~ 0*1 
peerrstW2 ~ 0*1
peerrstW3 ~ 0*1


#define indicator residual variance

peerrstW1 ~~ c(i1,i2)*peerrstW1
peerrstW2 ~~ c(i1,i2)*peerrstW2
peerrstW3 ~~ c(i1,i2)*peerrstW3

#specify residual correlations
peerrstW1 ~~ 0*peerrstW3+0*peerrstW2
peerrstW2 ~~  0*peerrstW3

dprrst3~c(k1,k1)*dpeerlan2+dprrst2+dpeeranti2
#dpeerlan3~dpeerlan2+dprrst2+dpeeranti2
#dpeeranti3~dpeerlan2+dprrst2+dpeeranti2

prrst1~~0*peerlan1+0*peerlan2+0*peerlan3
prrst2~~0*peerlan1+0*peerlan2+0*peerlan3
prrst3~~0*peerlan1+0*peerlan2+0*peerlan3

prrst1~~0*peeranti1+0*peeranti2+0*peeranti3
prrst2~~0*peeranti1+0*peeranti2+0*peeranti3
prrst3~~0*peeranti1+0*peeranti2+0*peeranti3

peeranti1~~0*peerlan1+0*peerlan2+0*peerlan3
peeranti2~~0*peerlan1+0*peerlan2+0*peerlan3
peeranti3~~0*peerlan1+0*peerlan2+0*peerlan3
'
#model needs refine
lcs.crosslag.prrst.lan.anti.mg.fit<-lavaan(lcs.crosslag.prrst.lan.anti.mg,data=peer_wide, group = 'country', missing="fiml")
summary(lcs.crosslag.prrst.lan.anti.mg.fit, fit.measures=T, standardized=T)

#########average language and math####for latent change score model
peer_wide$zachiw1=.5*peer_wide$Zmathw1+.5*peer_wide$Zlanguagew1
peer_wide$zachiw2=.5*peer_wide$Zmathw2+.5*peer_wide$Zlanguagew2
peer_wide$zachiw3=.5*peer_wide$Zmathw3+.5*peer_wide$Zlanguagew3

#w1 peerachi
w1fri=as.matrix(w1fri)
w1peer_achi=as.matrix(peer_wide$zachiw1)
w1peer_achi[is.na(w1peer_achi)] <- 0
w1peer_zachi=w1fri %*% w1peer_achi
w1peer_zachi[w1peer_zachi==0]<-NA
nomination=rowSums(w1fri)
nomination=as.numeric(nomination)
w1peer_zachi=cbind(w1peer_zachi,nomination)
w1peer_zachi=as.data.frame(w1peer_zachi)
w1peer_zachi$average=w1peer_zachi$V1/w1peer_zachi$nomination


#w2 peerachi
w2fri=as.matrix(w2fri)
w2peer_achi=as.matrix(peer_wide$zachiw2)
w2peer_achi[is.na(w2peer_achi)] <- 0
w2peer_zachi=w2fri %*% w2peer_achi
w2peer_zachi[w2peer_zachi==0]<-NA
nomination=rowSums(w2fri)
nomination=as.numeric(nomination)
w2peer_zachi=cbind(w2peer_zachi,nomination)
w2peer_zachi=as.data.frame(w2peer_zachi)
w2peer_zachi$average=w2peer_zachi$V1/w2peer_zachi$nomination

#w3 peerachi
w3fri=as.matrix(w3fri)
w3peer_achi=as.matrix(peer_wide$zachiw3)
w3peer_achi[is.na(w3peer_achi)] <- 0
w3peer_zachi=w3fri %*% w3peer_achi
w3peer_zachi[w3peer_zachi==0]<-NA
nomination=rowSums(w3fri)
nomination=as.numeric(nomination)
w3peer_zachi=cbind(w3peer_zachi,nomination)
w3peer_zachi=as.data.frame(w3peer_zachi)
w3peer_zachi$average=w3peer_zachi$V1/w3peer_zachi$nomination

peer_wide=cbind(peer_wide,w1peer_zachi$average,w2peer_zachi$average,w3peer_zachi$average)
names(peer_wide)[63:65]=c('peerachiw1','peerachiw2','peerachiw3')

write.csv(peer_wide,file = 'peerprop_wide.csv')

#latent change score peer achievement
#peerachi
lcs.peerachi<-'
#define time-specific latent variables
peerachi1 =~ 1*peerachiw1
peerachi2 =~ 1*peerachiw2
peerachi3 =~ 1*peerachiw3

#define structural relations

peerachi2 ~ 1*peerachi1
peerachi3 ~ 1*peerachi2

#define change factors

dpeerachi2 =~ 1*peerachi2
dpeerachi3 =~ 1*peerachi3

#specify latent means and variance

peerachi1 ~ 1
peerachi2 ~ (0)*1
peerachi3 ~ (0)*1

dpeerachi2 ~ 1 # estimate mean of change scores
dpeerachi3 ~ 1

peerachi1 ~~ peerachi1 # estimate variance of start point
peerachi2 ~~ (0)*peerachi2 #fix variance estimate of other point to 0, since variance goes to change score
peerachi3 ~~ (0)*peerachi3 

dpeerachi2 ~~ dpeerachi2 #estimate variance of change score
dpeerachi3 ~~ dpeerachi3 

#specify intercept-change relations

dpeerachi2 ~ (a)*peerachi1
dpeerachi3 ~ (b)*peerachi2

#specify change-change relations

dpeerachi3 ~~0* dpeerachi2

#define indicator intercepts, fix to 0, mean goes to factor score, the read indicator has loading of 1

peerachiw1 ~ 0*1 
peerachiw2 ~ 0*1
peerachiw3 ~ 0*1


#define indicator residual variance

peerachiw1 ~~ (0)*peerachiw1
peerachiw2 ~~ (0)*peerachiw2
peerachiw3 ~~ (0)*peerachiw3

#specify residual correlations
peerachiw1 ~~ 0*peerachiw3+0*peerachiw2
peerachiw2 ~~  0*peerachiw3'


lcs.peerachi.fit<-lavaan(lcs.peerachi,data=dataus, missing="fiml")
summary(lcs.peerachi.fit, fit.measures=T, standardized=T)

lcs.peerachi.fit2<-lavaan(lcs.peerachi,data=datacn, missing="fiml")
summary(lcs.peerachi.fit2, fit.measures=T, standardized=T)
# does not fit well


##try prrst &prcd with antiso

#selfantiso
lcs.antiso<-'
#define time-specific latent variables
antiso1 =~ 1*antisoW1
antiso2 =~ 1*antisoW2
antiso3 =~ 1*antisoW3

#define structural relations

antiso2 ~ 1*antiso1
antiso3 ~ 1*antiso2

#define change factors

dantiso2 =~ 1*antiso2
dantiso3 =~ 1*antiso3

#specify latent means and variance

antiso1 ~ 1
antiso2 ~ (0)*1
antiso3 ~ (0)*1

dantiso2 ~ 1 # estimate mean of change scores
dantiso3 ~ 1

antiso1 ~~ antiso1 # estimate variance of start point
antiso2 ~~ (0)*antiso2 #fix variance estimate of other point to 0, since variance goes to change score
antiso3 ~~ (0)*antiso3 

dantiso2 ~~ dantiso2 #estimate variance of change score
dantiso3 ~~ dantiso3 

#specify intercept-change relations

dantiso2 ~ (a)*antiso1
dantiso3 ~ (a)*antiso2

#specify change-change relations

dantiso3 ~~ dantiso2

#define indicator intercepts, fix to 0, mean goes to factor score, the read indicator has loading of 1

antisoW1 ~ 0*1 
antisoW2 ~ 0*1
antisoW3 ~ 0*1


#define indicator residual variance

antisoW1 ~~ (0)*antisoW1
antisoW2 ~~ (0)*antisoW2
antisoW3 ~~ (0)*antisoW3

#specify residual correlations
antisoW1 ~~ 0*antisoW3+0*antisoW2
antisoW2 ~~  0*antisoW3'

dataus2<-subset(peer_cha,peer_cha$country==1)
datacn2<-subset(peer_cha,peer_cha$country==2)

lcs.antiso.fit<-lavaan(lcs.antiso,data=dataus2, missing="fiml")
summary(lcs.antiso.fit, fit.measures=T, standardized=T)

lcs.antiso.fit2<-lavaan(lcs.antiso,data=datacn2, missing="fiml")
summary(lcs.antiso.fit2, fit.measures=T, standardized=T)


peer_wide$antisoW1<-peer_cha$antisoW1
peer_wide$antisoW2<-peer_cha$antisoW2
peer_wide$antisoW3<-peer_cha$antisoW3

#prrst by antiso bivariante latent change model
lcs.crosslag.prrst.antiso<-
  
  '#define time-specific latent variables
antiso1 =~ 1*antisoW1
antiso2 =~ 1*antisoW2
antiso3 =~ 1*antisoW3

#define structural relations

antiso2 ~ 1*antiso1
antiso3 ~ 1*antiso2

#define change factors

dantiso2 =~ 1*antiso2
dantiso3 =~ 1*antiso3

#specify latent means and variance

antiso1 ~ 1
antiso2 ~ (0)*1
antiso3 ~ (0)*1

dantiso2 ~ 1 # estimate mean of change scores
dantiso3 ~ 1

antiso1 ~~ antiso1 # estimate variance of start point
antiso2 ~~ (0)*antiso2 #fix variance estimate of other point to 0, since variance goes to change score
antiso3 ~~ (0)*antiso3 

dantiso2 ~~ dantiso2 #estimate variance of change score
dantiso3 ~~ dantiso3 

#specify intercept-change relations

dantiso2 ~ (a)*antiso1
dantiso3 ~ (a)*antiso2

#specify change-change relations

dantiso3 ~~ dantiso2

#define indicator intercepts, fix to 0, mean goes to factor score, the read indicator has loading of 1

antisoW1 ~ 0*1 
antisoW2 ~ 0*1
antisoW3 ~ 0*1


#define indicator residual variance

antisoW1 ~~ (0)*antisoW1
antisoW2 ~~ (0)*antisoW2
antisoW3 ~~ (0)*antisoW3

#specify residual correlations
antisoW1 ~~ 0*antisoW3+0*antisoW2
antisoW2 ~~  0*antisoW3

#define time-specific latent variables
prrst1 =~ 1*peerrstW1
prrst2 =~ 1*peerrstW2
prrst3 =~ 1*peerrstW3

#define structural relations

prrst2 ~ 1*prrst1
prrst3 ~ 1*prrst2

#define change factors

dprrst2 =~ 1*prrst2
dprrst3 =~ 1*prrst3

#specify latent means and variance

prrst1 ~ 1
prrst2 ~ (0)*1
prrst3 ~ (0)*1

dprrst2 ~ 1 # estimate mean of change scores
dprrst3 ~ 1

prrst1 ~~ prrst1 # estimate variance of start point
prrst2 ~~ (0)*prrst2 #fix variance estimate of other point to 0, since variance goes to change score
prrst3 ~~ (0)*prrst3 

dprrst2 ~~ dprrst2 #estimate variance of change score
dprrst3 ~~ dprrst3 

#specify intercept-change relations

dprrst2 ~ (e)*prrst1
dprrst3 ~ (e)*prrst2

#specify change-change relations

dprrst3 ~~ dprrst2

#define indicator intercepts, fix to 0, mean goes to factor score, the read indicator has loading of 1

peerrstW1 ~ 0*1 
peerrstW2 ~ 0*1
peerrstW3 ~ 0*1


#define indicator residual variance

peerrstW1 ~~ (i)*peerrstW1
peerrstW2 ~~ (i)*peerrstW2
peerrstW3 ~~ (i)*peerrstW3

#specify residual correlations
peerrstW1 ~~ 0*peerrstW3+0*peerrstW2
peerrstW2 ~~  0*peerrstW3

dprrst3~dantiso2+dprrst2
dantiso3~dprrst2+dantiso2

prrst1 ~~ 0*antiso1+0*antiso2+0*antiso3
prrst2 ~~ 0*antiso1+0*antiso2+0*antiso3
prrst3 ~~ 0*antiso1+0*antiso2+0*antiso3'


lcs.crosslag.prrst.antiso.fit<-lavaan(lcs.crosslag.prrst.antiso,data=dataus, missing="fiml")
summary(lcs.crosslag.prrst.antiso.fit, fit.measures=T, standardized=T)

lcs.crosslag.prrst.antiso.fit<-lavaan(lcs.crosslag.prrst.antiso,data=datacn, missing="fiml")
summary(lcs.crosslag.prrst.antiso.fit, fit.measures=T, standardized=T)

#try peer engagement
peer_wide$learnW1<-peer_cha$learnW1
peer_wide$learnW2<-peer_cha$learnW2
peer_wide$learnW3<-peer_cha$learnW3


#w1 peerengagment
w1fri=as.matrix(w1fri)
w1peer_learn=as.matrix(peer_wide$learnW1)
w1peer_learn[is.na(w1peer_learn)] <- 0
w1peer_zlearn=w1fri %*% w1peer_learn
w1peer_zlearn[w1peer_zlearn==0]<-NA
nomination=rowSums(w1fri)
nomination=as.numeric(nomination)
w1peer_zlearn=cbind(w1peer_zlearn,nomination)
w1peer_zlearn=as.data.frame(w1peer_zlearn)
w1peer_zlearn$average=w1peer_zlearn$V1/w1peer_zlearn$nomination


#w2 peerengagment
w2fri=as.matrix(w2fri)
w2peer_learn=as.matrix(peer_wide$learnW2)
w2peer_learn[is.na(w2peer_learn)] <- 0
w2peer_zlearn=w2fri %*% w2peer_learn
w2peer_zlearn[w2peer_zlearn==0]<-NA
nomination=rowSums(w2fri)
nomination=as.numeric(nomination)
w2peer_zlearn=cbind(w2peer_zlearn,nomination)
w2peer_zlearn=as.data.frame(w2peer_zlearn)
w2peer_zlearn$average=w2peer_zlearn$V1/w2peer_zlearn$nomination

#w3 peerlearn
w3fri=as.matrix(w3fri)
w3peer_learn=as.matrix(peer_wide$learnW3)
w3peer_learn[is.na(w3peer_learn)] <- 0
w3peer_zlearn=w3fri %*% w3peer_learn
w3peer_zlearn[w3peer_zlearn==0]<-NA
nomination=rowSums(w3fri)
nomination=as.numeric(nomination)
w3peer_zlearn=cbind(w3peer_zlearn,nomination)
w3peer_zlearn=as.data.frame(w3peer_zlearn)
w3peer_zlearn$average=w3peer_zlearn$V1/w3peer_zlearn$nomination

peer_wide=cbind(peer_wide,w1peer_zlearn$average,w2peer_zlearn$average,w3peer_zlearn$average)
names(peer_wide)[72:74]=c('peerlearnw1','peerlearnw2','peerlearnw3')

write.csv(peer_wide,file = 'peerprop_wide.csv')

#latent change score peer learnevement
#peerlearn
lcs.peerlearn<-'
#define time-specific latent variables
peerlearn1 =~ 1*peerlearnw1
peerlearn2 =~ 1*peerlearnw2
peerlearn3 =~ 1*peerlearnw3

#define structural relations

peerlearn2 ~ 1*peerlearn1
peerlearn3 ~ 1*peerlearn2

#define change factors

dpeerlearn2 =~ 1*peerlearn2
dpeerlearn3 =~ 1*peerlearn3

#specify latent means and variance

peerlearn1 ~ 1
peerlearn2 ~ (0)*1
peerlearn3 ~ (0)*1

dpeerlearn2 ~ 1 # estimate mean of change scores
dpeerlearn3 ~ 1

peerlearn1 ~~ peerlearn1 # estimate variance of start point
peerlearn2 ~~ (0)*peerlearn2 #fix variance estimate of other point to 0, since variance goes to change score
peerlearn3 ~~ (0)*peerlearn3 

dpeerlearn2 ~~ dpeerlearn2 #estimate variance of change score
dpeerlearn3 ~~ dpeerlearn3 

#specify intercept-change relations

dpeerlearn2 ~ (a)*peerlearn1
dpeerlearn3 ~ (a)*peerlearn2

#specify change-change relations

dpeerlearn3 ~~ dpeerlearn2

#define indicator intercepts, fix to 0, mean goes to factor score, the read indicator has loading of 1

peerlearnw1 ~ 0*1 
peerlearnw2 ~ 0*1
peerlearnw3 ~ 0*1


#define indicator residual variance

peerlearnw1 ~~ (c)*peerlearnw1
peerlearnw2 ~~ (c)*peerlearnw2
peerlearnw3 ~~ (c)*peerlearnw3

#specify residual correlations
peerlearnw1 ~~ 0*peerlearnw3+0*peerlearnw2
peerlearnw2 ~~  0*peerlearnw3'

lcs.peerlearn.fit<-lavaan(lcs.peerlearn,data=dataus, missing="fiml")
summary(lcs.peerlearn.fit, fit.measures=T, standardized=T)

lcs.peerlearn.fit<-lavaan(lcs.peerlearn,data=datacn, missing="fiml")
summary(lcs.peerlearn.fit, fit.measures=T, standardized=T)

#bivariant latent change peerlearn&prrst
lcs.crosslag.prrst.learn<-
  
  '
peerlearn1 =~ 1*peerlearnw1
peerlearn2 =~ 1*peerlearnw2
peerlearn3 =~ 1*peerlearnw3

#define structural relations

peerlearn2 ~ 1*peerlearn1
peerlearn3 ~ 1*peerlearn2

#define change factors

dpeerlearn2 =~ 1*peerlearn2
dpeerlearn3 =~ 1*peerlearn3

#specify latent means and variance

peerlearn1 ~ 1
peerlearn2 ~ (0)*1
peerlearn3 ~ (0)*1

dpeerlearn2 ~ 1 # estimate mean of change scores
dpeerlearn3 ~ 1

peerlearn1 ~~ peerlearn1 # estimate variance of start point
peerlearn2 ~~ (0)*peerlearn2 #fix variance estimate of other point to 0, since variance goes to change score
peerlearn3 ~~ (0)*peerlearn3 

dpeerlearn2 ~~ dpeerlearn2 #estimate variance of change score
dpeerlearn3 ~~ dpeerlearn3 

#specify intercept-change relations

dpeerlearn2 ~ (a)*peerlearn1
dpeerlearn3 ~ (a)*peerlearn2

#specify change-change relations

dpeerlearn3 ~~ dpeerlearn2

#define indicator intercepts, fix to 0, mean goes to factor score, the read indicator has loading of 1

peerlearnw1 ~ 0*1 
peerlearnw2 ~ 0*1
peerlearnw3 ~ 0*1


#define indicator residual variance

peerlearnw1 ~~ (c)*peerlearnw1
peerlearnw2 ~~ (c)*peerlearnw2
peerlearnw3 ~~ (c)*peerlearnw3

#specify residual correlations
peerlearnw1 ~~ 0*peerlearnw3+0*peerlearnw2
peerlearnw2 ~~  0*peerlearnw3

#define time-specific latent variables
prrst1 =~ 1*peerrstW1
prrst2 =~ 1*peerrstW2
prrst3 =~ 1*peerrstW3

#define structural relations

prrst2 ~ 1*prrst1
prrst3 ~ 1*prrst2

#define change factors

dprrst2 =~ 1*prrst2
dprrst3 =~ 1*prrst3

#specify latent means and variance

prrst1 ~ 1
prrst2 ~ (0)*1
prrst3 ~ (0)*1

dprrst2 ~ 1 # estimate mean of change scores
dprrst3 ~ 1

prrst1 ~~ prrst1 # estimate variance of start point
prrst2 ~~ (0)*prrst2 #fix variance estimate of other point to 0, since variance goes to change score
prrst3 ~~ (0)*prrst3 

dprrst2 ~~ dprrst2 #estimate variance of change score
dprrst3 ~~ dprrst3 

#specify intercept-change relations

dprrst2 ~ (e)*prrst1
dprrst3 ~ (e)*prrst2

#specify change-change relations

dprrst3 ~~ dprrst2

#define indicator intercepts, fix to 0, mean goes to factor score, the read indicator has loading of 1

peerrstW1 ~ 0*1 
peerrstW2 ~ 0*1
peerrstW3 ~ 0*1


#define indicator residual variance

peerrstW1 ~~ (i)*peerrstW1
peerrstW2 ~~ (i)*peerrstW2
peerrstW3 ~~ (i)*peerrstW3

#specify residual correlations
peerrstW1 ~~ 0*peerrstW3+0*peerrstW2
peerrstW2 ~~  0*peerrstW3

dprrst3~dpeerlearn2
dpeerlearn3~dprrst2

prrst1 ~~ 0*peerlearn1+0*peerlearn2+0*peerlearn3
prrst2 ~~ 0*peerlearn1+0*peerlearn2+0*peerlearn3
prrst3 ~~ 0*peerlearn1+0*peerlearn2+0*peerlearn3'


lcs.crosslag.prrst.learn.fit<-lavaan(lcs.crosslag.prrst.learn,data=dataus, missing="fiml")
summary(lcs.crosslag.prrst.learn.fit, fit.measures=T, standardized=T)

lcs.crosslag.prrst.learn.fit<-lavaan(lcs.crosslag.prrst.learn,data=datacn, missing="fiml")
summary(lcs.crosslag.prrst.learn.fit, fit.measures=T, standardized=T)
