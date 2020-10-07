getwd()
setwd("/Users/yuxiong/Dropbox/IRT Project")
library(haven)
dat <- read_sav("BFI2_Response styles.sav")
View(dat)
#check whether still have -9 because it should have been transformed to missing
min(dat$A59)
#check missing cases, all mising on age
a<-unique (unlist (lapply (dat, function (x) which (is.na (x)))))
View(dat[a,])

#organize items
p<-list(E=rep(1,12),A=rep(1,12),C=rep(1,12),N=rep(1,12),O=rep(1,12))
for (i in 1:5)
{
  p[[i]]<-c(paste0("A",seq(i,60,by=5)))
}

#calculate index together
mid_index<-matrix(0,9763,5)
ext_index<-matrix(0,9763,5)
for (i in 1:5)
{
 mid_index[,i]<-rowSums(subset(dat,select=colnames(dat) %in% p[[i]])==3)/12
 ext_index[,i]<-rowSums(subset(dat,select=colnames(dat) %in% p[[i]]) ==1|subset(dat,select=colnames(dat) %in% p[[i]]) ==5)/12
}

colnames(mid_index)<-c(paste0(c("E","A","C","N","O"),"MRS"))
colnames(ext_index)<-c(paste0(c("E","A","C","N","O"),"ERS"))
dat<-cbind(dat,mid_index,ext_index)

#write out data with index
write.csv(dat,"BFI_2.csv",col.names = TRUE)

#calculate mean and variance by country
m<-aggregate(dat[64:73],by=list(dat$Country),FUN='mean')
sd<-aggregate(dat[64:73],by=list(dat$Country),FUN='sd')
write.csv(m,"mean.csv")
write.csv(sd,"sd.csv")

table(dat$Country)

##irt response style
dat_irt<-dat[,4:63]

install.packages("ItemResponseTrees")
library(ItemResponseTrees)
