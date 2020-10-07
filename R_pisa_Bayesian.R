getwd()
setwd("C:/Users/xiong/Desktop/UIUC 2018 spring/2018 spring course work/psyc590/pisa")
pisa25=read.table('pisa25.txt',header = TRUE)
GDPgrowth=read.csv("gdp_g.csv",header=TRUE)
pisa25=merge(pisa25,GDPgrowth,by.x='CNT',by.y = 'CNT',all = TRUE)
View(pisa25)

#check result with controlling GDP
model.mean=lm(pisa25$meanPV1MATH ~ pisa25$meanANXMAT+pisa25$nGDP,data=pisa25)
summary(model.mean)
model.mean=lm(pisa25$meanPV1MATH ~ pisa25$meanANXMAT+pisa25$GDPgrowth,data=pisa25)
summary(model.mean)
model=lm(pisa25$PV1MATH ~ pisa25$ANXMAT+pisa25$GDPgrowth,data=pisa25)
summary(model)

#OLS regression-individual level
fit <- lm(pisa25$PV1MATH ~ pisa25$ANXMAT+pisa25$ESCS, data=pisa25 )
summary(fit)
par(mfrow=c(2,1))
plot(pisa25$PV1MATH ~ pisa25$ANXMAT)
abline(reg=lm(pisa25$PV1MATH ~ pisa25$ANXMAT),col = "blue")
plot(pisa25$PV1MATH ~ pisa25$ESCS)
abline(reg=lm(pisa25$PV1MATH ~ pisa25$ESCS),col = "blue")

#OLS regression-country mean level
par(mfrow=c(2,1))
plot(pisa25$meanPV1MATH ~ pisa25$meanANXMAT)
abline(reg=lm(pisa25$meanPV1MATH ~ pisa25$meanANXMAT),col = "blue")
plot(pisa25$meanPV1MATH ~ pisa25$meanESCS)
abline(reg=lm(pisa25$meanPV1MATH ~ pisa25$meanESCS),col = "blue")


#plot for each country 
#graphs
xyplot(pisa25$PV1MATH ~ pisa25$ANXMAT| CNT, data=pisa25, col.line='black', 
       type=c('p','l'),
       main='Math ~ anxiety relationship')

xyplot(pisa25$PV1MATH ~ pisa25$ESCS| CNT, data=pisa25, col.line='black', 
       type=c('p','l'),
       main='Math ~ ESCS relationship')

# Plot all country in one plot
par(mfrow=c(1,1))
plot(pisa25$ANXMAT,pisa25$PV1MATH,
     type="n", 
     xlab="math anxiety",
     ylab="math achievement",
     main="lines for each country: math~anxiety"
)
for (i in 1:25){
  g <- subset(pisa25,pisa25$a==i)
  abline(reg=lm(g$PV1MATH ~ g$ANXMAT),col = i,lwd=2)
}

par(mfrow=c(1,1))
plot(pisa25$ESCS,pisa25$PV1MATH,
     type="n", 
     xlab="ESCS",
     ylab="math achievement",
     main="lines for each country: math~ESCS"
)
for (i in 1:25){
  g <- subset(pisa25,pisa25$a==i)
  abline(reg=lm(g$PV1MATH ~ g$ESCS),col = i,lwd=2)
}


#rjags
library(rjags)
library(runjags)
library(coda)

# runjags: random intercept model

dataList <- list(
  y = pisa25$PV1MATH,
  ANX = pisa25$ANXMAT,
  ESCS = pisa25$ESCS,
  sdY = sd(pisa25$PV1MATH),
  n = length(pisa25$PV1MATH),
  ng= length(unique(pisa25$CNT)),
  CNT= pisa25$a
)

rmod1 <- "model { 
for (i in 1:n) {
y[i] ~ dnorm(mu[i],precision)
mu[i] <- b0j[CNT[i]] + g1*ANX[i] + g2*ESCS[i]
}

for (j in 1:ng) {
b0j[j] ~ dnorm(g0,ptau)
}

g0 ~ dnorm(0,1/(100*sdY^2))
g1 ~ dnorm(0,1/(100*sdY^2))
g2 ~ dnorm(0,1/(100*sdY^2))

tau ~ dunif(0.0001,200)
ptau <- 1/tau^2
sigma ~ dunif(0.0001,2000)
precision <- 1/sigma^2	
}"

writeLines(rmod1, con="rmod1.txt")

start1 = list("g0"=mean(pisa25$PV1MATH),"g1"=dnorm(1,0,3),  "g2"=dnorm(1,0,3),     
              "sigma"=sd(pisa25$PV1MATH), "tau"=.5,   .RNG.name="base::Wichmann-Hill", .RNG.seed=523) 

start2 = list("g0"=dnorm(1,0,3),    "g1"=dnorm(1,0,3), "g2"=dnorm(1,-1,3),      
              "sigma"=5,            "tau"=1,      .RNG.name="base::Marsaglia-Multicarry", .RNG.seed=57)

start3 = list("g0"=dnorm(1,3,4),    "g1"=dnorm(1,3,4),   "g2"=dnorm(1,3,4),  
              "sigma"=50,           "tau"=3,      .RNG.name="base::Super-Duper", .RNG.seed=24)

start4 = list("g0"=dnorm(1,-3,10),  "g1"=dnorm(1,-3,10),   "g2"=dnorm(1,10,5),
              "sigma"=10,          "tau"=10,     .RNG.name="base::Mersenne-Twister", .RNG.seed=72100)

start <- list(start1,start2,start3,start4)

rmod1.runjags <- run.jags(model=rmod1,      
                          method="parallel",  
                          monitor=c("g0", "g1", "g2","sigma", "tau"),
                          data=dataList,
                          sample=2000,				  
                          n.chains=4,
                          thin=5,
                          inits=start)

print(rmod1.runjags)
fit1<-lmer(pisa25$PV1MATH ~ 1+pisa25$ANXMAT+pisa25$ESCS+(1|pisa25$CNT),data=pisa25, REML = FALSE)
summary(fit1)
plot(rmod1.runjags)
extend.jags(rmod1.runjags)
gelman.plot(rmod1.runjags)
autocorr.plot(rmod1.runjags)

# runjags: random intercept & slope of anxiety model

dataList <- list(
  y = pisa25$PV1MATH,
  ANX = pisa25$ANXMAT,
  ESCS = pisa25$ESCS,
  sdY = sd(pisa25$PV1MATH),
  n = length(pisa25$PV1MATH),
  ng= length(unique(pisa25$CNT)),
  CNT= pisa25$a
)

rmod2 <- "model { 
for (i in 1:n) {
y[i] ~ dnorm(meanY[i],precision)
meanY[i] <-  betaj[CNT[i],1] + betaj[CNT[i],2]*ANX[i] + g2*ESCS[i]
}

for (j in 1:ng) {
betaj[j,1:2] ~ dmnorm(mu[1:2],Omega[1:2,1:2])
}

precision ~ dgamma(2,2)
sigma <- 1/sqrt(precision)
mu[1] ~ dnorm(0,1/(100*sdY^2))   
mu[2] ~ dnorm(0,1/(100*sdY^2))   

Omega[1:2,1:2] ~ dwish(R[,],2.1)
R[1,1] <- 1/2.1
R[1,2] <- 0                   
R[2,1] <- 0    
R[2,2] <- 1/2.1     
tau <- inverse(Omega)
g2 ~ dnorm(0,1/(100*sdY^2))
}"

writeLines(rmod2, con="model3.txt")

start1 = list("precision"=1, .RNG.name="base::Wichmann-Hill", .RNG.seed=523) 

start2 = list("precision"=1, .RNG.name="base::Marsaglia-Multicarry", .RNG.seed=57)

start3 = list("precision"=1, .RNG.name="base::Super-Duper", .RNG.seed=24)

start4 = list("precision"=1, .RNG.name="base::Mersenne-Twister", .RNG.seed=72100)

start <- list(start1,start2,start3,start4)

rmod2.runjags <- run.jags(model=rmod2,      
                          method="parallel",  
                          monitor=c("mu","g2","sigma", "tau"),
                          data=dataList,
                          sample=2000,				  
                          n.chains=4,
                          thin=10,
                          inits=start)

print(rmod2.runjags)
fit2<-lmer(pisa25$PV1MATH ~ 1+pisa25$ANXMAT+pisa25$ESCS+(1+ANXMAT|pisa25$CNT),data=pisa25, REML = FALSE)
summary(fit2)
plot(rmod2.runjags)
gelman.plot(rmod2.runjags)
autocorr.plot(rmod2.runjags)

# runjags: random intercept & slope for both anxiety and ESCS model
dataList <- list(
  y = pisa25$PV1MATH,
  ANX = pisa25$ANXMAT,
  ESCS = pisa25$ESCS,
  sdY = sd(pisa25$PV1MATH),
  n = length(pisa25$PV1MATH),
  ng= length(unique(pisa25$CNT)),
  CNT= pisa25$a
)

rmod3 <- "model { 
for (i in 1:n) {
y[i] ~ dnorm(meanY[i],precision)
meanY[i] <-  betaj[CNT[i],1] + betaj[CNT[i],2]*ANX[i] + betaj[CNT[i],3]*ESCS[i]
}

for (j in 1:ng) {
betaj[j,1:3] ~ dmnorm(mu[1:3],Omega[1:3,1:3])
}

precision ~ dgamma(3,3)
sigma <- 1/sqrt(precision)
mu[1] ~ dnorm(0,1/(100*sdY^2))   
mu[2] ~ dnorm(0,1/(100*sdY^2))   
mu[3] ~ dnorm(0,1/(100*sdY^2))

Omega[1:3,1:3] ~ dwish(R[,],3.1)
R[1,1] <- 1/2.1
R[1,2] <- 0   
R[1,3] <- 0 
R[2,1] <- 0    
R[2,2] <- 1/2.1
R[2,3] <- 0    
R[3,1] <- 0 
R[3,2] <- 0    
R[3,3] <- 1/2.1 
tau <- inverse(Omega)
}"

writeLines(rmod3, con="rmod3.txt")

start1 = list("precision"=1, .RNG.name="base::Wichmann-Hill", .RNG.seed=523) 

start2 = list("precision"=1, .RNG.name="base::Marsaglia-Multicarry", .RNG.seed=57)

start3 = list("precision"=1, .RNG.name="base::Super-Duper", .RNG.seed=24)

start4 = list("precision"=1, .RNG.name="base::Mersenne-Twister", .RNG.seed=72100)

start <- list(start1,start2,start3,start4)

rmod3.runjags <- run.jags(model=rmod3,      
                          method="parallel",  
                          monitor=c("mu","sigma", "tau"),
                          data=dataList,
                          sample=2000,				  
                          n.chains=4,
                          thin=10,
                          inits=start)

print(rmod3.runjags)
fit3<-lmer(pisa25$PV1MATH ~ 1+pisa25$ANXMAT+pisa25$ESCS+(1+ANXMAT+ESCS|pisa25$CNT),data=pisa25, REML = FALSE)
summary(fit3)
plot(rmod3.runjags)
gelman.plot(rmod3.runjags)
autocorr.plot(rmod3.runjags)

# runjags: random intercept & slope of anxiety model with interaction model
dataList <- list(
  y = pisa25$PV1MATH,
  ANX = pisa25$ANXMAT,
  ESCS = pisa25$ESCS,
  sdY = sd(pisa25$PV1MATH),
  n = length(pisa25$PV1MATH),
  ng= length(unique(pisa25$CNT)),
  CNT= pisa25$a
)

rmod4 <- "model { 
for (i in 1:n) {
y[i] ~ dnorm(meanY[i],precision)
meanY[i] <-  betaj[CNT[i],1] + betaj[CNT[i],2]*ANX[i] + betaj[CNT[i],3]*ESCS[i]+g0*ESCS[i]*ANX[i]
}

for (j in 1:ng) {
betaj[j,1:3] ~ dmnorm(mu[1:3],Omega[1:3,1:3])
}

precision ~ dgamma(3,3)
sigma <- 1/sqrt(precision)
mu[1] ~ dnorm(0,1/(100*sdY^2))   
mu[2] ~ dnorm(0,1/(100*sdY^2))   
mu[3] ~ dnorm(0,1/(100*sdY^2))

Omega[1:3,1:3] ~ dwish(R[,],3.1)
R[1,1] <- 1/2.1
R[1,2] <- 0   
R[1,3] <- 0 
R[2,1] <- 0    
R[2,2] <- 1/2.1
R[2,3] <- 0    
R[3,1] <- 0 
R[3,2] <- 0    
R[3,3] <- 1/2.1 
tau <- inverse(Omega)
g0 ~ dnorm(0,1/(100*sdY^2))
}"

writeLines(rmod4, con="rmod4.txt")

start1 = list("precision"=1, .RNG.name="base::Wichmann-Hill", .RNG.seed=523) 

start2 = list("precision"=1, .RNG.name="base::Marsaglia-Multicarry", .RNG.seed=57)

start3 = list("precision"=1, .RNG.name="base::Super-Duper", .RNG.seed=24)

start4 = list("precision"=1, .RNG.name="base::Mersenne-Twister", .RNG.seed=72100)

start <- list(start1,start2,start3,start4)

rmod4.runjags <- run.jags(model=rmod4,      
                          method="parallel",  
                          monitor=c("mu","sigma", "tau",'g0'),
                          data=dataList,
                          sample=2000,				  
                          n.chains=4,
                          thin=10,
                          inits=start)

print(rmod4.runjags)
fit4<-lmer(pisa25$PV1MATH ~ 1+pisa25$ANXMAT+pisa25$ESCS+pisa25$ANXMAT*pisa25$ESCS+(1+ANXMAT|pisa25$CNT),data=pisa25, REML = FALSE)
summary(fit4)

plot(rmod4.runjags)
gelman.plot(rmod4.runjags)
autocorr.plot(rmod4.runjags)
#may be need to give up random slope on ESCS

# three level model: random intercept in school and country level
dataList <- list(
  y = pisa25$PV1MATH,
  ANX = pisa25$ANXMAT,
  ESCS = pisa25$ESCS,
  idschool=pisa25$SCHOOLID2,
  sdY = sd(pisa25$PV1MATH),
  n = length(pisa25$PV1MATH),
  ng= length(unique(pisa25$CNT)),
  sg=length(unique(pisa25$SCHOOLID2)),
  CNT= pisa25$a
)

rmod5 <- "model { 
for (i in 1:n) {
y[i] ~ dnorm(mu[i],precision)
mu[i] <- b0+b1*ANX[i]+b2*ESCS[i]+U[idschool[i]]+W[CNT[i]]
}

for (j in 1:sg) {
U[j] ~ dnorm(0,uprec)
}

for (k in 1:ng)
{
  W[k]~dnorm(0,wprec)
}

b0 ~ dnorm(0,1/(100*sdY^2))
b1 ~ dnorm(0,1/(100*sdY^2))
b2 ~ dnorm(0,1/(100*sdY^2))

uprec~dgamma(1,1)
tau1 <- 1/sqrt(uprec)

wprec~dgamma(2,2)
tau2 <- 1/sqrt(wprec)

sigma ~ dunif(0.0001,2000)

precision <- 1/sigma^2	
}"

writeLines(rmod5, con="rmod5.txt")

start1 = list("b0"=mean(pisa25$PV1MATH),"b1"=dnorm(1,0,3),  "b2"=dnorm(1,0,3),     
              "sigma"=sd(pisa25$PV1MATH),  .RNG.name="base::Wichmann-Hill", .RNG.seed=523) 

start2 = list("b0"=dnorm(1,0,3),    "b1"=dnorm(1,0,3), "b2"=dnorm(1,-1,3),      
              "sigma"=5,           .RNG.name="base::Marsaglia-Multicarry", .RNG.seed=57)

start3 = list("b0"=dnorm(1,3,4),    "b1"=dnorm(1,3,4),   "b2"=dnorm(1,3,4),  
              "sigma"=50,            .RNG.name="base::Super-Duper", .RNG.seed=24)

start4 = list("b0"=dnorm(1,-3,10),  "b1"=dnorm(1,-3,10),   "b2"=dnorm(1,10,5),
              "sigma"=200,          .RNG.name="base::Mersenne-Twister", .RNG.seed=72100)

start <- list(start1,start2,start3,start4)

rmod5.runjags <- run.jags(model=rmod5,      
                          method="parallel",  
                          monitor=c("b0", "b1", "b2","sigma", "tau1","tau2"),
                          data=dataList,
                          sample=2000,				  
                          n.chains=4,
                          thin=10,
                          inits=start)

print(rmod5.runjags)
extend.jags(rmod5.runjags)
plot(rmod5.runjags)
gelman.plot(rmod5.runjags)
autocorr.plot(rmod5.runjags)

fit5=lmer(PV1MATH~1+ANXMAT+ESCS+(1|SCHOOLID2)+(1+ANXMAT|CNT),data=pisa25,REML=FALSE)
summary(fit5)

#play with variables
pisa25$groupid=as.factor(pisa25$groupid)
fit6=lmer(PV1MATH~1+ANXMAT+ESCS+nGDP+(1|SCHOOLID2)+(1+ANXMAT|CNT),data=pisa25,REML=FALSE)
summary(fit6)


fit7=lmer(PV1MATH~1+ANXMAT+ESCS+nGDP+groupid+(1|SCHOOLID2)+(1+ANXMAT|CNT),data=pisa25,REML=FALSE)
summary(fit7)

fit8=lmer(PV1MATH~1+ANXMAT+ESCS+(1|SCHOOLID2)+(1+ANXMAT|CNT),data=pisa25,REML=FALSE)
summary(fit8)

fit9=lmer(PV1MATH~1+ANXMAT+ESCS+INSTMOT+(1|SCHOOLID2)+(1+ANXMAT|CNT),data=pisa25,REML=FALSE)
summary(fit9)

fit10=lmer(PV1MATH~1+ANXMAT+ESCS+MMINS+groupid+MMINS*groupid+(1|SCHOOLID2)+(1+ANXMAT|CNT),data=pisa25,REML=FALSE)
summary(fit10)

fit11=lmer(PV1MATH~1+ANXMAT+ESCS+groupid*MMINS+(1|SCHOOLID2)+(1+ANXMAT|CNT),data=pisa25,REML=FALSE)
summary(fit11)

fit12=lmer(PV1MATH~1+ANXMAT+ESCS+GDPgrowth+(1|SCHOOLID2)+(1+ANXMAT|CNT),data=pisa25,REML=FALSE)
summary(fit12)

# tentative not working one
#three level model: random intercept in school and country level, random slope of anxiety in country level, 
#add in mmins to explain random slope & interceptof anxiety 


# MMINTS[i] ~ dnorm(239.6, 1/11239.26) deal with missing data
MMINS=pisa25$MMINS
MMINS=na.omit(MMINS)
sigma2=var(MMINS)
ybar=mean(MMINS)
mu0=239.6
tau02=20000
n=length(MMINS)

mu.n=((1/tau02)*mu0 + (n/sigma2)*ybar)/(1/tau02  +  n/sigma2)
tau2.n <- 1/(1/tau02 + n/sigma2)
mu.n
tau2.n
for(i in 1:5181)
{
  if(is.na(pisa25$MMINS[i])==TRUE)
    pisa25$MMINS[i]<- rnorm(1, mu.n,sqrt(tau2.n))
}
pisa25=pisa25
summary (pisa25$MMINS)


#### jags
dataList <- list(
  y = pisa25$PV1MATH,
  ANX = pisa25$ANXMAT,
  MMINS=pisa25$MMINS,
  ESCS = pisa25$ESCS,
  idschool=pisa25$SCHOOLID2,
  sdY = sd(pisa25$PV1MATH),
  n = length(pisa25$PV1MATH),
  ng= length(unique(pisa25$CNT)),
  sg=length(unique(pisa25$SCHOOLID2)),
  CNT= pisa25$a
)

rmod7 <- "model { 
for (i in 1:n) {
y[i] ~ dnorm(meany[i],precision)
meany[i] <- b0+b1*ANX[i]+b2*ESCS[i]+b3*MMINS[i]+b4*ANX[i]*MMINS[i]+U[idschool[i]]+W[CNT[i],1]+W[CNT[i],2]*ANX[i]
}

for (j in 1:sg) {
U[j] ~ dnorm(0,uprec)
}

for (k in 1:ng)
{
  W[k,1:2]~dmnorm(nu[1:2],Omega[1:2,1:2])
}


nu[1] ~ dnorm(0,1/(100*sdY^2))   
nu[2] ~ dnorm(0,1/(100*sdY^2))   

Omega[1:2,1:2] ~ dwish(R[,],2.1)
R[1,1] <- 1/2.1
R[1,2] <- 0                   
R[2,1] <- 0    
R[2,2] <- 1/2.1     
tau2 <- inverse(Omega)

b0 ~ dnorm(0,1/(100*sdY^2))
b1 ~ dnorm(0,1/(100*sdY^2))
b2 ~ dnorm(0,1/(100*sdY^2))
b3 ~ dnorm(0,1/(100*sdY^2))
b4 ~ dnorm(0,1/(100*sdY^2))

uprec~dgamma(1,1)
tau1 <- 1/sqrt(uprec)
precision ~ dgamma(2,2)
sigma <- 1/sqrt(precision)

}"

writeLines(rmod7, con="rmod7.txt")

start1 = list("b0"=mean(pisa25$PV1MATH),"b1"=dnorm(1,0,3),  "b2"=dnorm(1,0,3),     
               .RNG.name="base::Wichmann-Hill", .RNG.seed=523) 

start2 = list("b0"=dnorm(1,0,3),    "b1"=dnorm(1,0,3), "b2"=dnorm(1,-1,3),      
                     .RNG.name="base::Marsaglia-Multicarry", .RNG.seed=57)

start3 = list("b0"=dnorm(1,3,4),    "b1"=dnorm(1,3,4),   "b2"=dnorm(1,3,4),  
                        .RNG.name="base::Super-Duper", .RNG.seed=24)

start4 = list("b0"=dnorm(1,-3,10),  "b1"=dnorm(1,-3,10),   "b2"=dnorm(1,10,5),
                    .RNG.name="base::Mersenne-Twister", .RNG.seed=72100)

start <- list(start1,start2,start3,start4)

rmod7.runjags <- run.jags(model=rmod7,      
                          method="parallel",  
                          monitor=c("b0", "b1", "b2","b3","b4","sigma", "tau1","tau2"),
                          data=dataList,
                          sample=2000,				  
                          n.chains=4,
                          thin=10,
                          inits=start)

print(rmod7.runjags)
extend.jags(rmod7.runjags)
summary(fit11)

plot(rmod7.runjags)
gelman.plot(rmod7.runjags)
autocorr.plot(rmod7.runjags)


