getwd()
setwd("C:/Users/xy/Desktop/UIUC/2017fall course work/HLM course/final project")
library(foreign)
library(lme4)
library(lmerTest)
library(lattice)
library(ggplot2)
library(stringi)
install.packages("HLMdiag")
library(HLMdiag)


data=read.table("hlm-final project.csv",header=1,sep=",")
View(data)
data$boy=ifelse(data$gender==1,1,0)
data$country2=ifelse(data$country==1,1,0)
data$country=as.factor(data$country)
data$gender=as.factor(data$gender)
data$boy=as.factor(data$boy)
data$country2=as.factor(data$country2)
table(data$boy)
table(data$gender)
table(data$country)
table(data$country2)

class(data$country)
class(data$country2)
class(data$gender)
class(data$boy)

printm=as.data.frame(aggregate(print~ID,data=data,"mean"))
names(printm)=c('ID','printm')
data=merge(data,printm,by=c('ID'))

prcpm=as.data.frame(aggregate(prcp~ID,data=data,"mean"))
names(prcpm)=c('ID','prcpm')
data=merge(data,prcpm,by=c('ID'))

prcsm=as.data.frame(aggregate(prcs~ID,data=data,"mean"))
names(prcsm)=c('ID','prcsm')
data=merge(data,prcsm,by=c('ID'))

prrstm=as.data.frame(aggregate(prrst~ID,data=data,"mean"))
names(prrstm)=c('ID','prrstm')
data=merge(data,prrstm,by=c('ID'))

prcdm=as.data.frame(aggregate(prcd~ID,data=data,"mean"))
names(prcdm)=c('ID','prcdm')
data=merge(data,prcdm,by=c('ID'))

obligm=as.data.frame(aggregate(oblig~ID,data=data,"mean"))
names(obligm)=c('ID','obligm')
data=merge(data,obligm,by=c('ID'))

antisom=as.data.frame(aggregate(antiso~ID,data=data,"mean"))
names(antisom)=c('ID','antisom')
data=merge(data,antisom,by=c('ID'))

data$printc=data$print-data$printm
data$prcpc=data$prcp-data$prcpm
data$prcsc=data$prcs-data$prcsm
data$prcdc=data$prcd-data$prcdm
data$prrstc=data$prrst-data$prrstm
data$obligc=data$oblig-data$obligm
data$antisoc=data$antiso-data$antisom

attach(data)
model.a=lmer(disclosure ~ 1+wave+country+gender+printm+prrstm+prcdm
             +prcsm+prcpm+printc+prrstc+prcdc
             +prcsc+prcpc+antisom+obligm+antisoc+obligc+wave*gender+(1+wave|ID), data=data, REML = FALSE)
summary(model.a)
#different gender coding actually influence estimate of wave and intercept
model.a2=lmer(disclosure ~ 1+wave+country2+boy+printm+prrstm+prcdm
             +prcsm+prcpm+printc+prrstc+prcdc
             +prcsc+prcpc+antisom+obligm+antisoc+obligc+wave*boy+(1+wave|ID), data=data, REML = FALSE)
summary(model.a2)

model.b=lmer(disclosure ~ 1+country2+boy+printm+prrstm+prcdm
             +prcsm+prcpm+printc+prrstc+prcdc
             +prcsc+prcpc+antisom+obligm+antisoc+obligc+wave+wave*boy+(1|ID), data=data, REML = FALSE)
summary(model.b)

#check random slope for wave
lr.a=-2*logLik(model.a2)
lr.ranint=-2*logLik(model.b)
df=2
df1=1
lr=lr.ranint-lr.a
lr.a
lr.ranint
lr
p1=pchisq(lr,df1,lower.tail = FALSE)
p2=pchisq(lr,df,lower.tail = FALSE)
pvalue=0.5*(p1+p2)
pvalue
p1  
p2

#contrast for fixed effect
  L=matrix(0,nrow=3,ncol=19)
  L[1,6]=1#count column number for typecommunity you want to test
  L[2,7]=1#when change, need to change L as a whole
  L[3,9]=1
   contrast(model.a2,L)# the outcome suggest suburban and rural are similar, such that we will test interaction if the same we will recode

# delete three mean level peer management   
model.second=lmer(disclosure ~ 1+wave+country2+boy+printm
                +prcsm+printc+prrstc+prcdc
                +prcsc+prcpc+antisom+obligm+antisoc+obligc+wave*boy+(1+wave|ID), data=data, REML = FALSE)
summary(model.second) 

# contrast for fixed effect
L1=matrix(0,nrow=1, ncol = 16)
L1[1,14]=1
contrast(model.second, L1)

#robust function doesn't work
#r-square doesn't work might be related to the sequence of data(wave)

#check why country effect changes after introducing other variables
model.c=lmer(disclosure ~ 1+wave+country2+boy
                  +prcsm+prrstc+prcdc
                  +prcsc+wave*boy+(1+wave|ID), data=data, REML = FALSE)
summary(model.c)

#check why there is no gender intercept effect
model.d=lmer(disclosure ~ 1+wave+country2+boy+(1+wave|ID), data=data, REML = FALSE)
summary(model.d)

summary(model.second)

#graphs
groups=unique(data$ID)[sample(1:934,30)] 
subset=data[data$ID%in%groups,]

xyplot(disclosure ~ wave | ID, data=subset, col.line='black', 
       type=c('p','r'), 
       main='Variability in disclosure ~ wave relationship')

xyplot(disclosure ~ prrst | ID, data=subset2, col.line='black', 
       type=c('p','r'), 
       main='Variability in disclosure ~ peer restriction relationship')

xyplot(disclosure ~ oblig | ID, data=subset, col.line='black', 
       type=c('p','r'), 
       main='Variability in disclosure ~ family obligation relationship')

xyplot(disclosure ~ wave | gender, data=subset, col.line='black', 
       type=c('p','r'), 
       main='Variability in disclosure ~ family obligation relationship')


data$wave2=as.factor(data$wave)
groups=unique(data$ID)[sample(1:934,30)] 
subset2=data[data$ID%in%groups,]

xyplot(disclosure ~ wave2 | ID, data=subset2, col.line='black', 
       type=c('p','r'), 
       main='Variability in disclosure ~ wave relationship')
class(data$gender)
#model fit
model.a=lmer(disclosure ~ 1+country+gender+parchr+frq+autsup+psyctrl+print+prrst+prcd
             +prcs+prcp+antiso+learn+oblig+wave+(1|ID), data=data, REML = FALSE)

model.k=lmer(disclosure ~ 1+wave+country+gender+gender*wave+oblig
             +parchr+antiso+print+prrst+prcd
             +prcs+prcp+(1+wave|ID), data=data, REML = FALSE)
summary(model.k)



attach(data)

#random effect
dotplot(ranef(model.second,condVar=TRUE))

#residual


#hlm-residual
res1 <- HLMresid(model.second, level=1, type="EB",  standardize=TRUE)
plot(res1, xlab='Fitted Conditional', ylab='Pearson Residuals')

par(mfrow=c(2,2))
fit <- fitted(model.second)  

plot(fit,res1, 
     xlab='Conditional Fitted Values',
     ylab='Pearson Std Residuals',
     main='Conditional Residuals')


qqnorm(res1)                 
abline(a=0,b=0.45, col='blue')  

h<- hist(res1,breaks=15,density=20)      
xfit <- seq(-2, 2, length=50)  
yfit <- dnorm(xfit, mean=0, sd=0.3763)    
yfit <- yfit*diff(h$mids[1:2])*length(res1)
lines(xfit, yfit, col='darkblue', lwd=2)
plot.new( )                  
text(.5,1.0,'Model.preliminary')
text(.5,0.9,'Devience=4006.8')
text(.5,0.8,'AIC=4046.8')
text(.5,0.7,'BIC=4163.9')


#functions
contrast <- function(model,L) {
  
  gamma <- fixef(model)
  nfixed <- length(gamma)                          # number of fixed effects
  ftable <- summary(model)[10]                     # use this for df (satterthwaite)
  ftable <- matrix(unlist(ftable),nrow=nfixed)     # so I can put out df for fixed effects
  cov.gamma <- vcov(model)                         # covariance matrix of parameter estimates
  
  # check that number of columms of L = number of fixed effects
  nL <- ncol(L)
  if (nfixed != nL) { 
    print('The number of columns of L must equal number of fixed effects.')
    return( )
  }
  
  # check for linearly dependent rows of L
  ck <- L %*% t(L)
  det.ck <- det(ck)
  if (det.ck <= .0000001) {
    print('Check for linearly dependent rows of L')
    return()
  }
  
  estimate<- L %*% gamma                           # value of constrast(s)
  # F test statistic
  F <-  as.numeric((t(estimate) %*% solve(L %*% cov.gamma %*% t(L)) %*% estimate)/nrow(L))
  # Wald test statistic
  X2 <-  as.numeric((t(estimate) %*% solve(L %*% cov.gamma %*% t(L)) %*% estimate))
  
  
  which.df <- which(L !=0, arr.ind=TRUE)           # Find elements of L that are non-zero
  
  ddf <- ftable[which.df[1,2],3]                   # I don't know what these should be                
  ndf <- nrow(L)                                   # these are fine
  
  pvalue <- pf(F,ndf,ddf,lower.tail=FALSE)         # p-value of F
  pchi  <- pchisq(X2,ndf,lower.tail=FALSE)         # p-value for Wald
  # output results in nice table/format
  result <- c(F, ndf, ddf, pvalue, X2, ndf, pchi)
  names(result) <- c('F', 'num df', 'den df guess', 'p-value', 'X2', 'df', 'p-value')
  return(result)
  
}

#robust
robust <- function(inmodel,response,idvar,dftype) { 
  
  model0 <- inmodel
  y <- response
  id <- idvar
  ##########################
  # Set up                 #
  ##########################
  
  varcov <- as.data.frame(VarCorr(model0))[4]   # extract variance covariance as vector
  q <- -.5 + sqrt(1/4 + 2*(nrow(varcov)-1))     # number of random effects
  
  nclusters <-length(unique(id))                # number of clusters
  
  X <- model.matrix(model0)                     # Extract design matrix for fixed effects
  n <- nrow(X)                                  # total sample size
  p <- ncol(X)                                  # number of fixed effects
  ncov <- q*(q-1)/2                             # number of covariances 
  
  ############################################################################
  # This is general and works but perhaps not as efficient as could be       #
  ############################################################################
  if(q==1) { 
    T <- varcov[1,1] 
    Z <- X[,1] 
  } else {
    Z <- X[, 1:q]
    T <- diag(varcov[1:q,])                       
    ncov <- q*(q-1)/2
    justcov <- varcov[(q+1):(q+ncov),]
    
    T1 <- matrix(,(q), (q))
    T1[lower.tri(T1, diag=FALSE)] <- justcov
    T2 <- t(T1)
    T2[lower.tri(T2, diag=FALSE)] <- justcov
    T2 <- as.data.frame(T2)
    T2[is.na(T2)] <- 0
    
    T <- T + T2
  }
  T <- as.matrix(T)
  
  nj <- table(id)                              # number level 1 units per cluster
  csum <- cumsum(table(id))                    # cumulative frequencies
  
  cut2 <- csum                                 # end index
  cut1 <- c(0, csum) + 1                       # start index
  cut1 <- cut1[1:(nclusters)]
  
  sigma2 <- varcov[(q+ncov+1),]                 # put within variance estimated into sigma2
  
  gamma <- as.matrix(fixef(model0), nrow=q, ncol=1)  # extract fixed effects and put into column vector
  yhat <-  X %*% gamma                           # model based value of response variable)
  yvec <- as.matrix(y, n)                        # turn y into a matrix (vector actually)
  
  model.cov <-  vcov(model0)                     # These are model based ses from lmer
  Robust.cov <- matrix(0,nrow=p, ncol=p)
  
  # loop throught to get robust.cov
  
  for (i in 1:nclusters){         
    # This is for getting model based covariance matrix              
    Zj <- X[cut1[i]:cut2[i],1:q]              # extract columns of X for group j  ***********
    Zj <- as.matrix(Zj)
    I <-diag(nrow(Zj))                        # create identity matirx of appropirate size
    Vj <- Zj %*% T %*% t(Zj) + sigma2*I       # compute V_j
    
    Xj <- X[cut1[i]:cut2[i],]
    iVj <- solve(Vj)
    A  <- model.cov %*% t(Xj) %*% iVj
    
    # This is for getting data based covaraiance matrix
    yj     <- yvec[cut1[i]:cut2[i],1]          # extract columns of y for group j
    yhatj <- yhat[cut1[i]:cut2[i],1]           # extract columns of yhat for group j
    ssresj <- (yj-yhatj) %*% t(yj - yhatj)     # compute sum sq res for group j
    
    Robust.cov <- Robust.cov + A %*% ssresj %*% t(A)
  }
  
  #################################################################
  # Compute test statistics                                       #
  #################################################################
  
  model.se <-sqrt(diag(model.cov))
  model.se
  model.t <- gamma/model.se                      
  
  robust.se <- sqrt(diag(Robust.cov))
  robust.se
  robust.t <- gamma/robust.se
  
  ################################################################
  # Compute chosen type of (denominator) df                      #
  #   if (var(X[cut1[1]:cut2[1],i]) < .000000001 (i.e., zero)    #
  ################################################################
  rank.design <- rankMatrix(X)
  df.residual = n -rank.design[1]
  
  if (dftype=="residual"){ df <- df.residual }        # for residual df 
  if (dftype=="between/within"){                      # for between/within df
    pbetween <- 0                                   # find number of between variables
    for (i in 1:p){
      if (var(X[cut1[i]:cut2[i],i]) < .0000001)       # if variance w/in=0, then it's a 
        pbetween <- pbetween + 1                    #   between cluster variable         
    }
    
    df <- matrix(, nrow = p, ncol = 1)              # initalize matrix
    for (i in 1:p){
      if (var(X[cut1[1]:cut2[1],i]) >  0.0000){     # checking to see if variance >0
        tmp <- df.residual - nclusters + pbetween     # then this is a within cluster fixed effect
      }   else { tmp <- nclusters - pbetween         # else this is a between cluster fixed effect
      } 
      df[i] <- tmp                          
    } 
  }
  
  ################################################
  # Compute p-values                             #
  ################################################
  p.valueR <- 2*(1-pt(abs(robust.t),df))
  p.valueM <- 2*(1-pt(abs(model.t),df))
  
  ################################################
  # Output table of results                      #
  ################################################
  fixed.table <- cbind(gamma, df, model.se, model.t, p.valueM, robust.se, robust.t, p.valueR)
  colnames(fixed.table) <- c('Fixed Est.', dftype, 'Model se.','Model t', 'p-value', 'Robust se', 'Robust t', 'p-value')
  
  return(fixed.table)
}

#R-square
hlmRsq <- function(dataset,model1,r.effects,id.name) {
  
  id.name <- as.character(id.name)
  id <- dataset[id.name]                             # get the id numbers
  
  xtmp <- model.matrix(model1)                       # extracts fixed effects design matrix
  xtmp <- as.data.frame(xtmp)                        # turns fixed effects design matirx into a data fram
  X <- as.matrix(xtmp[,r.effects])                   # takes only the fixed effects that are also random effects
  
  n <- nrow(X)                                       # total sample size
  varcov <- as.data.frame(VarCorr(model1))[4]        # extract variance covariance of Random effects as vector
  q <- -.5 + sqrt(1/4 + 2*(nrow(varcov)-1))          # number of random effects (solution to quadratic eq)
  ncov <- q*(q-1)/2                                  # number of covariances 
  justcov <- varcov[(q+1):(q+ncov),]                 # vector of just the covariances
  nclusters <- length(unique(id))                      # number of clusters
  
  
  ##### May change because now Z=X and probably can get rid of "if"
  T <- if(q==1) { 
    Z <- X[,1]
    zbar <- mean(Z)
    T <- varcov[1,1]
  } else{ 
    Z <- X[, 1:q]
    zbar <- colMeans(Z)
    T <- diag(varcov[1:q,])                       
    T1 <- matrix(,(q), (q))
    T1[lower.tri(T1, diag=FALSE)] <- justcov
    T2 <- t(T1)
    T2[lower.tri(T2, diag=FALSE)] <- justcov
    T2 <- as.data.frame(T2)
    T2[is.na(T2)] <- 0
    T <- T + T2
  }
  T <- as.matrix(T)                            # Known at Psi in my glmm book
  
  # Set up for loop
  
  nj <- table(id)                              # number level 1 units per cluster
  csum <- cumsum(table(id))                    # cumulative frequencies
  cut2 <- csum                                 # end index
  cut1 <- c(0, csum) + 1                       # start index
  cut1 <- cut1[1:(nclusters)]
  
  Sw <- matrix(0, nrow=q, ncol=q)              # initial Sum of Square between
  Sb <- matrix(0, nrow=q, ncol=q)              # initial sum of squares within
  
  # loop throught to get Sb and Sw
  
  for (i in 1:nclusters){         
    # This is for getting model based covariance matrix              
    Zj <- X[cut1[i]:cut2[i],1:q]                      # extract columns of X for random design for group j
    Zj <- as.matrix(Zj)
    onej <- matrix(1, nrow=1, ncol=nj[i])
    zbarj <- (t(onej %*% Zj)/nj[i])
    zbar  <- matrix(zbar, nrow=q, ncol=1)
    Sb <- Sb + nj[i] * (zbarj - zbar) %*% t(zbarj - zbar) 
    
    zbarj <- t(matrix(zbarj, nrow= q, ncol=nj[i]))
    Sw <- Sw +  t(Zj - zbarj) %*% (Zj - zbarj)
  }
  Sb <- Sb/(n-1)
  Sw <- Sw/(n-1)
  
  sigma2 <- varcov[(q+ncov+1),]                # put within variance estimated into sigma2
  
  #
  # Fit the null model using same estimation method at model input
  #
  
  # Determines whether ML or REML was used to fit the model
  
  sum <- summary(model1)
  if (sum[1]=="Linear mixed model fit by maximum likelihood "){
    method="FALSE"
  } else {
    method='TRUE'
  }
  
  # Fits the null model by updating formula from model input to functon
  
  f <- formula(model1)
  all <- all.vars(f)
  null <- update(f, .~ 1 -all + (1 | id))
  model0 <- lmer(null, data=dataset, REML=method)
  varcov0 <- as.data.frame(VarCorr(model0))[4] 
  
  ####################################################################
  #  R1sq    & R2sq                                                  #
  ####################################################################
  
  numerator1 <- t(zbar) %*% T %*% zbar + sum(diag( (T %*% (Sb + Sw)) )) + sigma2
  denominator1 <- sum(varcov0)
  R1sq <- 1 -numerator1/denominator1
  
  harmonic.mean <- nclusters/sum(1/nj)
  
  numerator2 <- t(zbar) %*% T %*% zbar + sum(diag((T %*% (Sb + Sw/harmonic.mean)))) + sigma2/harmonic.mean
  denominator2 <- sum(varcov0[1,1]) + varcov0[2,1]/harmonic.mean
  
  R2sq <- 1 - numerator2/denominator2
  
  Rsq.values <- cbind(harmonic.mean, R1sq,R2sq)
  colnames(Rsq.values) <- c('harmonic.mean', 'R1sq', 'R2sq')
  return(Rsq.values)
}
