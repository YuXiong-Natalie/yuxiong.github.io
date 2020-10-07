getwd()
#setwd("Box Sync/peer project/Data, codebook and syntax")
setwd('/Users/xiong/Dropbox/EMLP/response measure/R')
#setwd('/Users/yuxiong/Dropbox/DP revision/revision analysis')

rm(list = ls())

library(haven)
efa <- read_sav("C:/Users/xiong/Dropbox/EMLP/response measure/R/response_items.sav")
View(efa)

library(psych)
library(nFactors)
library(lavaan)
library(GPArotation)
install.packages('semTools')
library(semTools)

colindex<-grep('^P3', colnames(efa),value = F)
P3efa<-efa[,colindex]

colindex<-grep('^P4', colnames(efa),value = F)
P4efa<-efa[,colindex]

#cor(P3efapro_per,use="pairwise")
cor<-lavCor(P3efa, missing="fiml")
cor.ev<-eigen(cor[1:24,1:24])
cor.PA<-parallel(subject = dim(P3efa)[1], var = dim(P3efa)[2], rep = 100, cent = .95, model = "components")
which(cor.ev$values > cor.PA$eigen$qevpea)
plotnScree(nScree(x = cor.ev$values, aparallel = cor.PA$eigen$qevpea, model = "components"))

#sem efa
p3<- efaUnrotate(P3efa, nf = 4, varList = colnames(P3efa), missing='fiml', estimator = "mlr")
summary(p3,fit.measures = T, standardized = T)
p3_rotate <- oblqRotate(p3, method = "quartimin")
summary(p3_rotate)

p4<- efaUnrotate(P4efa, nf = 4, varList = colnames(P4efa), missing='fiml', estimator = "mlr")
summary(p4,fit.measures = T, standardized = T)
p4_rotate <- oblqRotate(p4, method = "quartimin")
summary(p4_rotate)

#reliability
for (i in 0:3)
{
  a<-alpha(P3efa[,(6*i+1):(6*(i+1))])
  print(a)
}

for (i in 0:3)
{
  b<-alpha(P4efa[,(6*i+1):(6*(i+1))])
  print(b)
}


#fa from psych package, not sure whether dealt by fiml or listwise

efa.3<-fa(P3efa, nfactors = 3, rotate = "oblimin")
print(efa.3, cut = .0)

efa.4<-fa(P3efa, nfactors = 4, rotate = "oblimin")
print(efa.4, cut = .0)

fa.CFI<-function(x){
  nombre<-paste(x,"CFI",sep = ".")
  nombre<-
    ((x$null.chisq-x$null.dof)-(x$STATISTIC-x$dof))/(x$null.chisq-x$null.dof)
  return(nombre)
}
fa.CFI(efa.3)

# CFA
p3cfa.model1<-'
pros =~ P3Succ01+P3Succ02+P3Succ03+P3Succ04+P3Succ05+P3Succ06
pers =~ P3Succ07+P3Succ08+P3Succ09+P3Succ10+P3Succ11+P3Succ12
prof =~ P3Fail01+P3Fail02+P3Fail03+P3Fail04+P3Fail05+P3Fail06
perf =~ P3Fail07+P3Fail08+P3Fail09+P3Fail10+P3Fail11+P3Fail12
pros~~pers+perf+prof
pers~~perf+prof
prof~~perf'

cfa.model1.fit<-cfa(p3cfa.model1, data = P3efa, missing = 'fiml')
summary(cfa.model1.fit, fit.measures = T, standardized = T)


p3cfa.model2<-'
pros =~ P3Succ01+P3Succ02+P3Succ03+P3Succ04+P3Succ05+P3Succ06+P3Fail01+P3Fail02+P3Fail03+P3Fail04+P3Fail05+P3Fail06
pers =~ P3Succ07+P3Succ08+P3Succ09+P3Succ10+P3Succ11+P3Succ12
perf =~ P3Fail07+P3Fail08+P3Fail09+P3Fail10+P3Fail11+P3Fail12
pros~~pers+perf
pers~~perf'

cfa.model2.fit<-cfa(p3cfa.model2, data = P3efapro_per, missing = 'fiml')
summary(cfa.model2.fit, fit.measures = T, standardized = T)

p3cfa.model3<-'
pros =~ P3Succ01+P3Succ02+P3Succ03+P3Succ04+P3Succ05+P3Succ06
pers =~ P3Succ07+P3Succ08+P3Succ09+P3Succ10+P3Succ11+P3Succ12
prof =~ P3Fail01+P3Fail02+P3Fail03+P3Fail04+P3Fail05+P3Fail06
perf =~ P3Fail07+P3Fail08+P3Fail09+P3Fail10+P3Fail11+P3Fail12
suc=~pros+pers
fail=~prof+perf
pros~~pers+perf
pers~~perf
suc~~fail
suc~~0*prof+0*pers+0*perf+0*pros
fail~~0*prof+0*pers+0*perf+0*pros'

cfa.model3.fit<-cfa(p3cfa.model3, data = P3efapro_per, missing = 'fiml')
summary(cfa.model2.fit, fit.measures = T, standardized = T)


# correlation visualization
P3efapro_per$process_success<-rowMeans(P3efapro_per[,1:6])
P3efapro_per$person_success<-rowMeans(P3efapro_per[,7:12])
P3efapro_per$process_failure<-rowMeans(P3efapro_per[,13:18])
P3efapro_per$person_failure<-rowMeans(P3efapro_per[,19:24])

facor<-lavCor(P3efapro_per[,25:28],missing="fiml")

#visualization
#http://www.sthda.com/english/wiki/ggcorrplot-visualization-of-a-correlation-matrix-using-ggplot2
#https://jamesmarquezportfolio.com/correlation_matrices_in_r.html

install.packages("ggcorrplot")
library(ggcorrplot)
facor<-round(cor(P3efapro_per[,25:28], use="pairwise"), 1)
p.mat <- cor_pmat(P3efapro_per[,25:28])
ggcorrplot(facor,lab=TRUE, type =  "lower",tl.cex = 13,tl.srt = 0,hc.order= TRUE)


#correlation with parental belief, goal, autonomy support, control
library(haven)
full <- read_sav("C:/Users/xiong/Dropbox/EMLP/response measure/R/Cohort 1, 2, 3 Phases 0, 1, 3,4 EMLP Data finalized gender race WJ_1.sav")
pcorindex<-grep('^T3', colnames(full),value = F)
varname<-c("T3_PGM","T3_PFM","T3_PMasteryGoal","T3_PPerfGoal","T3_PAS","T3_PCon")
resname<-c("T3_PProcS","T3_PPersS","T3_PProcF","T3_PPersF")
var<-subset(full, select = colnames(full) %in% varname)
res<-subset(full, select = colnames(full) %in% resname)

for(i in 1:4)
{
  a<-cbind(res[,i],var)
  acor<-lavCor(a,missing="fiml")
  acor.mat<-cor_pmat(a)
  plot<-ggcorrplot(acor,lab=TRUE,p.mat = acor.mat,type = "lower")
  print(plot)
}

pcor_mat<-lavCor(pcor[,8:18],missing="fiml")
ppcor.mat <- cor_pmat(pcor[,8:18])
ggcorrplot(pcor_mat,lab=TRUE)

#test correlation difference
facor<-cor(P3efapro_per[,25:28],use="pairwise")
describe(P3efapro_per[,25:28])

#correlation between types and scenarios of responses
#x: process_S, y: process_F Z: person_S
diff<-paired.r(.63,.42,.27,546,twotailed = TRUE)
#x: person_F, y: person_S Z: process_F
paired.r(.52,.26,.27,546,twotailed = TRUE)
#x: person_S, y: person_F Z: process_S
paired.r(.52,.42,.22,546,twotailed = TRUE)

#correlations between self-report and daily checklist
library(psych)
#x: self-report, y: process_F Z: person_S
paired.r(.314,.171,NULL,435,203,twotailed = TRUE)
paired.r(.314,.590,NULL,435,twotailed = TRUE)
paired.r(.314,.233,NULL,435,203,twotailed = TRUE)
paired.r(.171,.233,NULL,203,twotailed = TRUE)

paired.r(.314,.211,.63,435,435,twotailed = TRUE)
paired.r(.194,.171,.63,203,203,twotailed = TRUE)
paired.r(.590,.370,.52,435,435,twotailed = TRUE)
paired.r(.25,.233,.52,203,203,twotailed = TRUE)

paired.r(.157,.165,NULL,435,203,twotailed = TRUE)
paired.r(.565,.279,NULL,435,203,twotailed = TRUE)
paired.r(.279,.165,NULL,203,twotailed = TRUE)
paired.r(.279,.157,NULL,203,435,twotailed = TRUE)
paired.r(.370,.225,.22,435,twotailed = TRUE)

paired.r(.157,.125,.63,435,twotailed = TRUE)
paired.r(.168,.165,.63,203,twotailed = TRUE)
paired.r(.565,.302,.52,435,twotailed = TRUE)
paired.r(.302,.148,.22,435,twotailed = TRUE)
paired.r(.233,.279,.52,203,twotailed = TRUE)

paired.r(.47,.26,.42,537,twotailed = TRUE)
