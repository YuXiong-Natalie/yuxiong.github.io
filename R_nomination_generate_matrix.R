getwd()
setwd("C:/Users/xiong/Desktop/UIUC 2018 spring/peer/peer2ndproject")
library(readr)
library(data.table)
library(psych)
library(reshape2)
#install.packages('stringr')
library(stringr)
library(lme4)
library(lmerTest)
data <- read_csv("C:/Users/xiong/Desktop/UIUC 2018 spring/peer/woking data/peer2nd_project.csv")
dim(data)
View(data)

#20180826
#write matrix for nomination data-friends
d=subset(data,select=c(ID, country, Friends1W1:Friends93W1,Friends1W2:Friends93W2,Friends1W3:Friends93W3))
View(d)
d=as.matrix(d)
name=d[,1]
fri=matrix(data=0, nrow=934, ncol=934)
friw2=matrix(data=0, nrow=934, ncol=934)
friw3=matrix(data=0, nrow=934, ncol=934)
name=as.character(name)
colnames(fri)=name
rownames(fri)=name
colnames(friw2)=name
rownames(friw2)=name
colnames(friw3)=name
rownames(friw3)=name
View(fri)
View(data)

class(d)<-"numeric"

#wave 1 matrix
for (i in 1:934) {
  for (j in 1:93)
  {
    if (is.na(d[i,j+2]))
    {
      j=j+1
    }
    else
    {
      k=d[i,j+2]
      m=which(d[,1]==k,arr.ind = TRUE)
      fri[i,m]=1
    }
  }
  fri[i,i]=0 #do not count in self-nomination
}

write.csv(fri,file = 'w1fri_matrix.csv',col.names = TRUE)
#wave2 matrix
for (i in 1:934) {
  for (j in 1:93)
  {
    if (is.na(d[i,j+95]))
    {
      j=j+1
    }
    else
    {
      k=d[i,j+95]
      m=which(d[,1]==k,arr.ind = TRUE)
      friw2[i,m]=1
    }
  }
  friw2[i,i]=0 #do not count in self-nomination
}
View(friw2)
write.csv(friw2,file = 'w2fri_matrix.csv')

#wave3 matrix
for (i in 1:934) {
  for (j in 1:93)
  {
    if (is.na(d[i,j+188]))
    {
      j=j+1
    }
    else
    {
      k=d[i,j+188]
      m=which(d[,1]==k,arr.ind = TRUE)
      friw3[i,m]=1
    }
  }
  friw3[i,i]=0 #do not count in self-nomination
}
View(friw3)
write.csv(friw3,file = 'w3fri_matrix.csv')



#calculate friend nomination proportion for wave 1 & wave 2 and attach it to dataset for analysis
grp=table(data$ClassIDW1W2)
grp=as.numeric(grp)
grp=as.matrix(grp)

nomination=colSums(fri)
nomination=as.matrix(nomination)
nominationw2=colSums(friw2)
nominationw2=as.matrix(nominationw2)

nominationw3=colSums(friw3)
nominationw3=as.matrix(nominationw3)

prop=matrix(data=0,nrow=934,ncol=1)
propw2=matrix(data=0,nrow=934,ncol=1)
propw3=matrix(data=0,nrow=934,ncol=1)

#wave 1 proportion
start=0
for (p in 1:14)
{
  len=grp[p,1]
  en=len+start
  start=start+1
for (n in start:en)
{
  prop[n,1]=nomination[n,1]/len
}
  start=en
}
dataw=cbind(dataw,prop[,1])
head(dataw)
colnames(dataw)[36]='fri_propw1'

#wave2 proportion
start=0
for (p in 1:14)
{
  len=grp[p,1]
  en=len+start
  start=start+1
  for (n in start:en)
  {
    propw2[n,1]=nominationw2[n,1]/len
  }
  start=en
}


dataw=cbind(dataw,propw2[,1])
head(dataw)
colnames(dataw)[37]='fri_propw2'

#wave3 proportion-ignore change of class
start=0
for (p in 1:14)
{
  len=grp[p,1]
  en=len+start
  start=start+1
  for (n in start:en)
  {
    propw3[n,1]=nominationw3[n,1]/len
  }
  start=en
}

#wave3 proportion-include change of class
friw3=read.csv('w3fri_matrix.csv', row.names = 1,header = TRUE)

for (i in 1:934)
{
  if(is.na(data$ClassIDW3[i])==1)
  {
     data$group[i]=data$ClassIDW1W2[i]
  }
 else
 {
   data$group[i]=data$ClassIDW3[i]
 }
}


library(foreign)
test=aggregate(ID~group,data=data,FUN=length)
data=merge(data,test,by='group')
data=as.data.table(data)
names(data)[952]='groupsizew3'

nominationw3=colSums(friw3)
nominationw3=as.matrix(nominationw3)

propw3=matrix(data=0,nrow=934,ncol=1)


for (p in 1:934)
{
    propw3[p,1]=nominationw3[p,1]/data$groupsizew3[p]
}
propw3[propw3==0]<-NA
table(propw3)



#trajectory exploration of my proportion calculation
#change from wide to long
attach(dataw)
datas=cbind(ID, country,fri_propw1,fri_propw2,fri_propw3)
detach(dataw)

  a=melt(as.data.table(datas), id.vars=c('ID','country'),measure.vars=c('fri_propw1','fri_propw2','fri_propw3'))
  names(a)[4]<-'fri_prop'
  names(a)[3]<-"wave"
  a$wave=str_sub(a$wave,-1)
  
View(a)
a$country=as.factor(a$country)
a$wave=as.numeric(a$wave)
model.s=lmer(fri_prop ~ 1+wave+country+wave*country+(1+wave|ID),data=a,REML = FALSE)
summary(model.s)
#kind of similar to results used Emily's data


#20180825
#exploration
hist(data$admire_Prop_W1)
x=data$peerrstW1
y=data$admire_Prop_W1
z=data$liked_Prop_W1
o=data$friends_Prop_W1

y=asin(sqrt(y/100))
z=asin(sqrt(z/100))
o=asin(sqrt(o/100))
plot(x,y,type='p',main = 'peer interest to admire nomination')
abline(lm(y~x),col = 'blue')
       
summary(lm(z~x))

#change data wide to long for trajectory
attach(data)
dataw=cbind(ID, country, peercdW1,peercdW2,peercdW3,peerrstW1,peerrstW2,peerrstW3,
            peerintW1,peerintW2,peerintW3,peercpW1,peercpW2,peercpW3,peercsW1,
            peercsW2,peercsW3,admire_Prop_W1,admire_Prop_W2,Admire_Prop_W3,liked_Prop_W1,
            like_Prop_W2,Liked_Prop_W3,friends_Prop_W1,friends_Prop_W2,Friends_Prop_W3,
            bestFriends_Prop_W1,best_friends_Prop_W2,Bestfriends_Prop_W3,popular_Prop_W1,
            popular_Prop_W2,Popular_Prop_W3,veryBestFriend_Prop_W1,very_best_friend_Prop_W2,Verybestfriends_Prop_W3)
dim(dataw)
View(dataw)

#descriptive

dataw=as.data.table(dataw)
dataw_us=subset(dataw,dataw$country == 1)
dataw_cn=subset(dataw,dataw$country ==2)
usdes=describe(dataw_us[,3:35])
cndes=describe(dataw_cn[,3:35])
write.csv(usdes,file='usdes.csv')
write.csv(cndes,file = 'cndes.csv')


#change data wide to long, x is variables need to long, y is the new variable name


l_to_w <- function(x,y)
{
  a=melt(as.data.table(dataw), id.vars=c('ID','country'),measure.vars=x)
  names(a)[4]<-y
  names(a)[3]<-"wave"
  a$wave=str_sub(a$wave,-1)
    return(a)
  
}
temp1=c('peercdW1','peercdW2','peercdW3')
temp2=c('peerrstW1','peerrstW2','peerrstW3')
temp3=c('peerintW1','peerintW2','peerintW3')
temp4=c('peercpW1','peercpW2','peercpW3')
temp5=c('peercsW1','peercsW2','peercsW3')
temp6=c('admire_Prop_W1','admire_Prop_W2','Admire_Prop_W3')
temp7=c('liked_Prop_W1','like_Prop_W2','Liked_Prop_W3')
temp8=c('friends_Prop_W1','friends_Prop_W2','Friends_Prop_W3')
temp9=c('bestFriends_Prop_W1','best_friends_Prop_W2','Bestfriends_Prop_W3')
temp10=c('veryBestFriend_Prop_W1','very_best_friend_Prop_W2','Verybestfriends_Prop_W3')
temp11=c('popular_Prop_W1','popular_Prop_W2','Popular_Prop_W3')

peercd=l_to_w(temp1,'peercd')
peerrst=l_to_w(temp2,'peerrst')
peerint=l_to_w(temp3,'peerint')
peercp=l_to_w(temp4,'peercp')
peercs=l_to_w(temp5,'peercs')
admireprop=l_to_w(temp6,'admireprop')
likedprop=l_to_w(temp7,'likedprop')
friendsprop=l_to_w(temp8,'friendsprop')
bestfriprop=l_to_w(temp9,'bestfriprop')
verybestfriprop=l_to_w(temp10,'verybestfriprop')
popularprop=l_to_w(temp11,'popularprop')

datal=cbind(peercd,peerrst[,4,drop=FALSE],peerint[,4,drop=FALSE],
      peercp[,4,drop=FALSE],peercs[,4,drop=FALSE],admireprop[,4,drop=FALSE],
      likedprop[,4,drop=FALSE],friendsprop[,4,drop=FALSE],bestfriprop[,4,drop=FALSE],
      verybestfriprop[,4,drop=FALSE],popularprop[,4,drop=FALSE])

View(datal)
write.csv(datal,file = 'peerprop_long.csv')
write.csv(dataw,file = 'peerprop_wide.csv')

#trajectory analysis-multilevel method
datal <- read_csv("C:/Users/xiong/Desktop/UIUC 2018 spring/peer/woking data/peerprop_long.csv")

attach(datal)

datal$country=as.factor(country)

model.a=lmer(admireprop ~ 1+wave+country+(1|ID),data=datal,REML = FALSE)
summary(model.a)

model.b=lmer(likedprop ~ 1+wave+country+(1|ID),data=datal,REML = FALSE)
summary(model.b)
model.c=lmer(friendsprop ~ 1+wave+country+(1|ID),data=datal,REML = FALSE)
summary(model.c)
model.d=lmer(bestfriprop ~ 1+wave+country+(1|ID),data=datal,REML = FALSE)
summary(model.d)
model.e=lmer(verybestfriprop ~ 1+wave+country+(1|ID),data=datal,REML = FALSE)
summary(model.e)
model.f=lmer(popularprop ~ 1+wave+country+(1|ID),data=datal,REML = FALSE)
summary(model.f)


