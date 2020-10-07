setwd('c:/Users/xiong/Dropbox/EMLP/response measure/R')
library(lavaan)
library(psych)
library(haven)
dat <- read_sav("Cohort 1, 2, 3 Phases 0, 1, 3,4 EMLP Data finalized gender race WJ_1.sav")
View(dat)

varnames<-c("ID","Parent_Gender","Pedu","C3_Fixed","C4_Fixed","C3_Anx","C4_Anx",
"C3ChallengePro","C4ChallengePro","C3WJA_W_Final","C4WJA_W_Final","C3_MIC","C4_MIC","C3_ME","C4_ME",
"C3_Liking","C4_Liking",
"T3_PProcS","T3_PPersS","T3_PProcF","T3_PPersF","T3_PGM","T3_PFM","T3_PMasteryGoal","T3_PPerfGoal")
 
test<-subset(dat, select = colnames(dat) %in% varnames)
View(test)
model.1<-'
C4_Fixed~Parent_Gender+Pedu+C3_Fixed+T3_PProcS
'
model.2<-'
C4_Fixed~Parent_Gender+Pedu+C3_Fixed+T3_PPersS
'
model.3<-'
C4_Fixed~Parent_Gender+Pedu+C3_Fixed+T3_PProcF
'
model.4<-'
C4_Fixed~Parent_Gender+Pedu+C3_Fixed+T3_PPersF
'

m1<-sem(model=model.1, data =test, missing='fiml',meanstructure=TRUE, fixed.x =FALSE)
summary(m1,fit.measures=TRUE, rsquare=TRUE,standardize =TRUE)
m2<-sem(model=model.2, data =test, missing='fiml',meanstructure=TRUE, fixed.x =FALSE)
summary(m2,fit.measures=TRUE, rsquare=TRUE,standardize =TRUE)
m3<-sem(model=model.3, data =test, missing='fiml',meanstructure=TRUE, fixed.x =FALSE)
summary(m3,fit.measures=TRUE, rsquare=TRUE,standardize =TRUE)
m4<-sem(model=model.4, data =test, missing='fiml',meanstructure=TRUE, fixed.x =FALSE)
summary(m4,fit.measures=TRUE, rsquare=TRUE,standardize =TRUE)


model.5<-'
C4_Anx~Parent_Gender+Pedu+C3_Anx+T3_PProcS
'
model.6<-'
C4_Anx~Parent_Gender+Pedu+C3_Anx+T3_PPersS
'
model.7<-'
C4_Anx~Parent_Gender+Pedu+C3_Anx+T3_PProcF
'
model.8<-'
C4_Anx~Parent_Gender+Pedu+C3_Anx+T3_PPersF
'

m5<-sem(model=model.5, data =test, missing='fiml',meanstructure=TRUE, fixed.x =FALSE)
summary(m5,fit.measures=TRUE, rsquare=TRUE,standardize =TRUE)
m6<-sem(model=model.6, data =test, missing='fiml',meanstructure=TRUE, fixed.x =FALSE)
summary(m6,fit.measures=TRUE, rsquare=TRUE,standardize =TRUE)
m7<-sem(model=model.7, data =test, missing='fiml',meanstructure=TRUE, fixed.x =FALSE)
summary(m7,fit.measures=TRUE, rsquare=TRUE,standardize =TRUE)
m8<-sem(model=model.8, data =test, missing='fiml',meanstructure=TRUE, fixed.x =FALSE)
summary(m8,fit.measures=TRUE, rsquare=TRUE,standardize =TRUE)


model.9<-'
C4ChallengePro~Parent_Gender+Pedu+C3ChallengePro+T3_PProcS
'
model.10<-'
C4ChallengePro~Parent_Gender+Pedu+C3ChallengePro+T3_PPersS
'
model.11<-'
C4ChallengePro~Parent_Gender+Pedu+C3ChallengePro+T3_PProcF
'
model.12<-'
C4ChallengePro~Parent_Gender+Pedu+C3ChallengePro+T3_PPersF
'

m9<-sem(model=model.9, data =test, missing='fiml',meanstructure=TRUE, fixed.x =FALSE)
summary(m9,fit.measures=TRUE, rsquare=TRUE,standardize =TRUE)
m10<-sem(model=model.10, data =test, missing='fiml',meanstructure=TRUE, fixed.x =FALSE)
summary(m10,fit.measures=TRUE, rsquare=TRUE,standardize =TRUE)
m11<-sem(model=model.11, data =test, missing='fiml',meanstructure=TRUE, fixed.x =FALSE)
summary(m11,fit.measures=TRUE, rsquare=TRUE,standardize =TRUE)
m12<-sem(model=model.12, data =test, missing='fiml',meanstructure=TRUE, fixed.x =FALSE)
summary(m12,fit.measures=TRUE, rsquare=TRUE,standardize =TRUE)


model.13<-'
C4WJA_W_Final~Parent_Gender+Pedu+C3WJA_W_Final+T3_PProcS
'
model.14<-'
C4WJA_W_Final~Parent_Gender+Pedu+C3WJA_W_Final+T3_PPersS
'
model.15<-'
C4WJA_W_Final~Parent_Gender+Pedu+C3WJA_W_Final+T3_PProcF
'
model.16<-'
C4WJA_W_Final~Parent_Gender+Pedu+C3WJA_W_Final+T3_PPersF
'

m13<-sem(model=model.13, data =test, missing='fiml',meanstructure=TRUE, fixed.x =FALSE)
summary(m13,fit.measures=TRUE, rsquare=TRUE,standardize =TRUE)
m14<-sem(model=model.14, data =test, missing='fiml',meanstructure=TRUE, fixed.x =FALSE)
summary(m14,fit.measures=TRUE, rsquare=TRUE,standardize =TRUE)
m15<-sem(model=model.15, data =test, missing='fiml',meanstructure=TRUE, fixed.x =FALSE)
summary(m15,fit.measures=TRUE, rsquare=TRUE,standardize =TRUE)
m16<-sem(model=model.16, data =test, missing='fiml',meanstructure=TRUE, fixed.x =FALSE)
summary(m16,fit.measures=TRUE, rsquare=TRUE,standardize =TRUE)


model.41<-'
C4_MIC~Parent_Gender+Pedu+C3_MIC+T3_PProcS
'
model.42<-'
C4_MIC~Parent_Gender+Pedu+C3_MIC+T3_PPersS
'
model.43<-'
C4_MIC~Parent_Gender+Pedu+C3_MIC+T3_PProcF
'
model.44<-'
C4_MIC~Parent_Gender+Pedu+C3_MIC+T3_PPersF
'

m41<-sem(model=model.41, data =test, missing='fiml',meanstructure=TRUE, fixed.x =FALSE)
summary(m41,fit.measures=TRUE, rsquare=TRUE,standardize =TRUE)
m42<-sem(model=model.42, data =test, missing='fiml',meanstructure=TRUE, fixed.x =FALSE)
summary(m42,fit.measures=TRUE, rsquare=TRUE,standardize =TRUE)
m43<-sem(model=model.43, data =test, missing='fiml',meanstructure=TRUE, fixed.x =FALSE)
summary(m43,fit.measures=TRUE, rsquare=TRUE,standardize =TRUE)
m44<-sem(model=model.44, data =test, missing='fiml',meanstructure=TRUE, fixed.x =FALSE)
summary(m44,fit.measures=TRUE, rsquare=TRUE,standardize =TRUE)




model.51<-'
C4_ME~Parent_Gender+Pedu+C3_ME+T3_PProcS
'
model.52<-'
C4_ME~Parent_Gender+Pedu+C3_ME+T3_PPersS
'
model.53<-'
C4_ME~Parent_Gender+Pedu+C3_ME+T3_PProcF
'
model.54<-'
C4_ME~Parent_Gender+Pedu+C3_ME+T3_PPersF
'

m51<-sem(model=model.51, data =test, missing='fiml',meanstructure=TRUE, fixed.x =FALSE)
summary(m51,fit.measures=TRUE, rsquare=TRUE,standardize =TRUE)
m52<-sem(model=model.52, data =test, missing='fiml',meanstructure=TRUE, fixed.x =FALSE)
summary(m52,fit.measures=TRUE, rsquare=TRUE,standardize =TRUE)
m53<-sem(model=model.53, data =test, missing='fiml',meanstructure=TRUE, fixed.x =FALSE)
summary(m53,fit.measures=TRUE, rsquare=TRUE,standardize =TRUE)
m54<-sem(model=model.54, data =test, missing='fiml',meanstructure=TRUE, fixed.x =FALSE)
summary(m54,fit.measures=TRUE, rsquare=TRUE,standardize =TRUE)

model.61<-'
C4_Liking~Parent_Gender+Pedu+C3_Liking+T3_PProcS
'
model.62<-'
C4_Liking~Parent_Gender+Pedu+C3_Liking+T3_PPersS
'
model.63<-'
C4_Liking~Parent_Gender+Pedu+C3_Liking+T3_PProcF
'
model.64<-'
C4_Liking~Parent_Gender+Pedu+C3_Liking+T3_PPersF
'

m61<-sem(model=model.61, data =test, missing='fiml',meanstructure=TRUE, fixed.x =FALSE)
summary(m61,fit.measures=TRUE, rsquare=TRUE,standardize =TRUE)
m62<-sem(model=model.62, data =test, missing='fiml',meanstructure=TRUE, fixed.x =FALSE)
summary(m62,fit.measures=TRUE, rsquare=TRUE,standardize =TRUE)
m63<-sem(model=model.63, data =test, missing='fiml',meanstructure=TRUE, fixed.x =FALSE)
summary(m63,fit.measures=TRUE, rsquare=TRUE,standardize =TRUE)
m64<-sem(model=model.64, data =test, missing='fiml',meanstructure=TRUE, fixed.x =FALSE)
summary(m64,fit.measures=TRUE, rsquare=TRUE,standardize =TRUE)

#predict response from parents' beliefs

model.17<-'
T3_PProcS~Parent_Gender+Pedu+T3_PGM
'
model.18<-'
T3_PPersS~Parent_Gender+Pedu+T3_PGM
'
model.19<-'
T3_PProcF~Parent_Gender+Pedu+T3_PGM
'
model.20<-'
T3_PPersF~Parent_Gender+Pedu+T3_PGM
'

m17<-sem(model=model.17, data =test, missing='fiml',meanstructure=TRUE, fixed.x =FALSE)
summary(m17,fit.measures=TRUE, rsquare=TRUE,standardize =TRUE)
m18<-sem(model=model.18, data =test, missing='fiml',meanstructure=TRUE, fixed.x =FALSE)
summary(m18,fit.measures=TRUE, rsquare=TRUE,standardize =TRUE)
m19<-sem(model=model.19, data =test, missing='fiml',meanstructure=TRUE, fixed.x =FALSE)
summary(m19,fit.measures=TRUE, rsquare=TRUE,standardize =TRUE)
m20<-sem(model=model.20, data =test, missing='fiml',meanstructure=TRUE, fixed.x =FALSE)
summary(m20,fit.measures=TRUE, rsquare=TRUE,standardize =TRUE)


model.21<-'
T3_PProcS~Parent_Gender+Pedu+T3_PFM
'
model.22<-'
T3_PPersS~Parent_Gender+Pedu+T3_PFM
'
model.23<-'
T3_PProcF~Parent_Gender+Pedu+T3_PFM
'
model.24<-'
T3_PPersF~Parent_Gender+Pedu+T3_PFM
'

m21<-sem(model=model.21, data =test, missing='fiml',meanstructure=TRUE, fixed.x =FALSE)
summary(m21,fit.measures=TRUE, rsquare=TRUE,standardize =TRUE)
m22<-sem(model=model.22, data =test, missing='fiml',meanstructure=TRUE, fixed.x =FALSE)
summary(m22,fit.measures=TRUE, rsquare=TRUE,standardize =TRUE)
m23<-sem(model=model.23, data =test, missing='fiml',meanstructure=TRUE, fixed.x =FALSE)
summary(m23,fit.measures=TRUE, rsquare=TRUE,standardize =TRUE)
m24<-sem(model=model.24, data =test, missing='fiml',meanstructure=TRUE, fixed.x =FALSE)
summary(m24,fit.measures=TRUE, rsquare=TRUE,standardize =TRUE)

model.25<-'
T3_PProcS~Parent_Gender+Pedu+T3_PMasteryGoal
'
model.26<-'
T3_PPersS~Parent_Gender+Pedu+T3_PMasteryGoal
'
model.27<-'
T3_PProcF~Parent_Gender+Pedu+T3_PMasteryGoal
'
model.28<-'
T3_PPersF~Parent_Gender+Pedu+T3_PMasteryGoal
'

m25<-sem(model=model.25, data =test, missing='fiml',meanstructure=TRUE, fixed.x =FALSE)
summary(m25,fit.measures=TRUE, rsquare=TRUE,standardize =TRUE)
m26<-sem(model=model.26, data =test, missing='fiml',meanstructure=TRUE, fixed.x =FALSE)
summary(m26,fit.measures=TRUE, rsquare=TRUE,standardize =TRUE)
m27<-sem(model=model.27, data =test, missing='fiml',meanstructure=TRUE, fixed.x =FALSE)
summary(m27,fit.measures=TRUE, rsquare=TRUE,standardize =TRUE)
m28<-sem(model=model.28, data =test, missing='fiml',meanstructure=TRUE, fixed.x =FALSE)
summary(m28,fit.measures=TRUE, rsquare=TRUE,standardize =TRUE)

model.29<-'
T3_PProcS~Parent_Gender+Pedu+T3_PPerfGoal
'
model.30<-'
T3_PPersS~Parent_Gender+Pedu+T3_PPerfGoal
'
model.31<-'
T3_PProcF~Parent_Gender+Pedu+T3_PPerfGoal
'
model.32<-'
T3_PPersF~Parent_Gender+Pedu+T3_PPerfGoal
'

m29<-sem(model=model.29, data =test, missing='fiml',meanstructure=TRUE, fixed.x =FALSE)
summary(m29,fit.measures=TRUE, rsquare=TRUE,standardize =TRUE)
m30<-sem(model=model.30, data =test, missing='fiml',meanstructure=TRUE, fixed.x =FALSE)
summary(m30,fit.measures=TRUE, rsquare=TRUE,standardize =TRUE)
m31<-sem(model=model.31, data =test, missing='fiml',meanstructure=TRUE, fixed.x =FALSE)
summary(m31,fit.measures=TRUE, rsquare=TRUE,standardize =TRUE)
m32<-sem(model=model.32, data =test, missing='fiml',meanstructure=TRUE, fixed.x =FALSE)
summary(m32,fit.measures=TRUE, rsquare=TRUE,standardize =TRUE)
