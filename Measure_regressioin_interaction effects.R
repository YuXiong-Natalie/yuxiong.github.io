getwd()
setwd('C:/Users/xiong/Dropbox/EMLP/response measure')
library(haven)
mod_ana<-read_sav("Cohort 1, 2, 3 Phases 0, 1, 3,4 EMLP Data finalized gender race WJ_1.sav")
attach(mod_ana)


fit<-lm(C4_Anx~1+C3_Anx+Pedu+T3_PPersS+T3_PCon, data = mod_ana)
summary(fit)

fit<-lm(C4_Anx~1+C3_Anx+Pedu+T3_PPersF+T3_PCon, data = mod_ana)
summary(fit)

fit<-lm(C4WJA_W_Final~1+C3WJA_W_Final+Pedu+T3_PPers+T3_PCon, data = mod_ana)
summary(fit)

fit<-lm(C4WJA_W_Final~1+C3WJA_W_Final+Pedu+T3_PPersF+T3_PCon, data = mod_ana)
summary(fit)

layout(matrix(c(1,2,3,4),2,2))
plot(fit)
pro_name<-c("T3_PProcS","T3_PPersS","T3_PProcF","T3_PPersF","P3PRO_PER_S","P3PRO_PER_F")
pro_name[1]
chi_name_3<-c("C3_Fixed","C3_Anx","C3WJA_W_Final","C3_Challenge","C3ChallengePro")
chi_name_4<-c("C4_Fixed","C4_Anx","C4WJA_W_Final","C4_Challenge","C4ChallengePro")
for (i in 1:5)
{
  for(j in 1:6)
  {
    equa<-as.formula(paste("scale(",chi_name_4[i],")~ 1+Pedu+","scale(",chi_name_3[i],")+","scale(",pro_name[j],")+",
                           "scale(",chi_name_3[i],")*scale(",pro_name[j],")"))
    fit<-lm(equa,data=mod_ana)
    print(summary(fit))
  }
}

#explore interaction effects with WJK
#http://www.alexanderdemos.org/Class6.html

subset<-c("T3_PProcS","T3_PPersS","T3_PProcF","T3_PPersF","P3PRO_PER_S","P3PRO_PER_F","C3WJA_W_Final","C4WJA_W_Final")

m2<-subset(mod_ana, select = which(colnames(mod_ana) %in% subset))
m2s<-scale(m2,center=TRUE, scale=FALSE)
library(psych)
describe(m2s)
m2d<-cbind(mod_ana[,1:7],m2s,mod_ana[,1357])

install.packages("effects")
library(effects)

mjk1<-lm(C4WJA_W_Final ~ 1 + C3WJA_W_Final +Pedu+ T3_PProcS + C3WJA_W_Final * T3_PProcS,data=m2d)

#1+-sd
Inter.1a<-effect(c("C3WJA_W_Final * T3_PProcS"), mjk1,
                 xlevels=list(C3WJA_W_Final=seq(-22.27,0,22.27), 
                              T3_PProcS=c(-.70,0,.70)))
knitr::kable(summary(Inter.1a)$effect, digits=4)
plot(Inter.1a, multiline = TRUE)

#test simple slope
install.packages("rockchalk")
library("rockchalk")
m1ps <- plotSlopes(mjk1, modx = "C3WJA_W_Final", plotx = "T3_PProcS", n=3, modxVals="std.dev")
m1psts <- testSlopes(m1ps)
round(m1psts$hypotests,4)

mjk1<-lm(C4WJA_W_Final ~ 1 + C3WJA_W_Final + T3_PProcS + C3WJA_W_Final * T3_PProcS,data=m2d)
vcov(mjk1)

mjk2<-lm(C4WJA_W_Final ~ 1 + Pedu + C3WJA_W_Final + T3_PPersS + C3WJA_W_Final * T3_PPersS,data=m2d)
mjk3<-lm(C4WJA_W_Final ~ 1 + Pedu + C3WJA_W_Final + T3_PProcF + C3WJA_W_Final * T3_PProcF,data=m2d)
mjk4<-lm(C4WJA_W_Final ~ 1 + Pedu + C3WJA_W_Final + T3_PPersF + C3WJA_W_Final * T3_PPersF,data=m2d)
interplot(m = mjk1, var1 = "T3_PProcS", var2 = "C3WJA_W_Final")
interplot(m = mjk2, var1 = "T3_PPersS", var2 = "C3WJA_W_Final")
interplot(m = mjk3, var1 = "T3_PProcF", var2 = "C3WJA_W_Final")
interplot(m = mjk4, var1 = "T3_PPersF", var2 = "C3WJA_W_Final")

m2ps<-plotSlopes(mjk2, modx = "C3WJA_W_Final", plotx = "T3_PPersS", n=3, modxVals="std.dev")
m2psts<-testSlopes(m2ps)
print(m2psts$hypotests)


m3ps<-plotSlopes(mjk3, modx = "C3WJA_W_Final", plotx = "T3_PProcF", n=3, modxVals="std.dev")
m3psts<-testSlopes(m3ps)
print(m3psts$hypotests)

m4ps<-plotSlopes(mjk4, modx = "C3WJA_W_Final", plotx = "T3_PPersF", n=3, modxVals="std.dev")
m4psts<-testSlopes(m4ps)
print(m4psts$hypotests)

#explore interaction effects with math anxiety

subset<-c("T3_PProcS","T3_PPersS","T3_PProcF","T3_PPersF","P3PRO_PER_S","P3PRO_PER_F","C3WJA_W_Final","C3_Anx","C4_Anx")

anx<-subset(mod_ana, select = which(colnames(mod_ana) %in% subset))
anxs<-scale(anx,center=TRUE, scale=FALSE)
anxsd<-cbind(mod_ana[,1:7],anxs,mod_ana[,1357])

anx1<-lm(C4_Anx ~ 1 +C3_Anx+Pedu+ T3_PProcS + C3WJA_W_Final * T3_PProcS,data=anxsd)
anx2<-lm(C4_Anx ~ 1 +C3_Anx+Pedu+ T3_PPersS + C3WJA_W_Final * T3_PPersS,data=anxsd)
anx3<-lm(C4_Anx ~ 1 +C3_Anx+Pedu+ T3_PProcF + C3WJA_W_Final * T3_PProcF,data=anxsd)
anx4<-lm(C4_Anx ~ 1 +C3_Anx+Pedu+ T3_PPersF + C3WJA_W_Final * T3_PPersF,data=anxsd)

#1+-sd

#test simple slope
anx1ps <- plotSlopes(anx1, modx = "C3WJA_W_Final", plotx = "T3_PProcS", n=3, modxVals="std.dev")
anx1psts <- testSlopes(anx1ps)
round(anx1psts$hypotests,4)

anx2ps <- plotSlopes(anx2, modx = "C3WJA_W_Final", plotx = "T3_PPersS", n=3, modxVals="std.dev")
anx2psts <- testSlopes(anx2ps)
round(anx2psts$hypotests,4)

anx3ps <- plotSlopes(anx3, modx = "C3WJA_W_Final", plotx = "T3_PProcF", n=3, modxVals="std.dev")
anx3psts <- testSlopes(anx3ps)
round(anx3psts$hypotests,4)

anx4ps <- plotSlopes(anx4, modx = "C3WJA_W_Final", plotx = "T3_PPersF", n=3, modxVals="std.dev")
anx4psts <- testSlopes(anx4ps)
round(anx4psts$hypotests,4)

#explore interaction effects with fixed mindset

subset<-c("T3_PProcS","T3_PPersS","T3_PProcF","T3_PPersF","P3PRO_PER_S","P3PRO_PER_F","C3WJA_W_Final","C4_Fixed","C3_Fixed")

fix<-subset(mod_ana, select = which(colnames(mod_ana) %in% subset))
fixs<-scale(fix,center=TRUE, scale=FALSE)
fixsd<-cbind(mod_ana[,1:7],fixs,mod_ana[,1357])

fix1<-lm(C4_Fixed ~ 1 +C3_Fixed+Pedu+ T3_PProcS + C3WJA_W_Final * T3_PProcS,data=fixsd)
fix2<-lm(C4_Fixed ~ 1 +C3_Fixed+Pedu+ T3_PPersS + C3WJA_W_Final * T3_PPersS,data=fixsd)
fix3<-lm(C4_Fixed ~ 1 +C3_Fixed+Pedu+ T3_PProcF + C3WJA_W_Final * T3_PProcF,data=fixsd)
fix4<-lm(C4_Fixed ~ 1 +C3_Fixed+Pedu+ T3_PPersF + C3WJA_W_Final * T3_PPersF,data=fixsd)

#1+-sd

#test simple slope

fix2ps <- plotSlopes(fix2, modx = "C3WJA_W_Final", plotx = "T3_PPersS", n=3, modxVals="std.dev")
fix2psts <- testSlopes(fix2ps)
round(fix2psts$hypotests,4)


#control parental control

subset<-c("T3_PProcS","T3_PPersS","T3_PProcF","T3_PPersF","P3PRO_PER_S","P3PRO_PER_F",'T3_PCon',"C3_Anx","C4_Anx")
anx<-subset(mod_ana, select = which(colnames(mod_ana) %in% subset))
anxs<-scale(anx,center=TRUE, scale=TRUE)
anxsd<-cbind(mod_ana[,1:7],anxs,mod_ana[,1357])
fit<-lm(C4_Anx~1+C3_Anx+Pedu+T3_PPersS+T3_PCon, data = anxsd)
summary(fit)

fit<-lm(C4_Anx~1+C3_Anx+Pedu+T3_PPersF+T3_PCon, data = anxsd)
summary(fit)

subset<-c("T3_PProcS","T3_PPersS","T3_PProcF","T3_PPersF","P3PRO_PER_S","P3PRO_PER_F","C3WJA_W_Final","C4WJA_W_Final",'T3_PCon')

m2<-subset(mod_ana, select = which(colnames(mod_ana) %in% subset))
m2s<-scale(m2,center=TRUE, scale=TRUE)
m2d<-cbind(mod_ana[,1:7],m2s,mod_ana[,1357])

fit<-lm(C4WJA_W_Final~1+C3WJA_W_Final+Pedu+T3_PPersS+T3_PCon, data = m2d)
summary(fit)

fit<-lm(C4WJA_W_Final~1+C3WJA_W_Final+Pedu+T3_PPersF+T3_PCon, data = m2d)
summary(fit)
