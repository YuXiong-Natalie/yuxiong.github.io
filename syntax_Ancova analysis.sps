﻿*Ancova analysis*

UNIANOVA T3_PGM BY class WITH Pedu 
  /METHOD=SSTYPE(3) 
  /INTERCEPT=INCLUDE 
  /SAVE=ZRESID 
  /EMMEANS=TABLES(class) WITH(Pedu=MEAN) COMPARE ADJ(BONFERRONI) 
  /PRINT=ETASQ HOMOGENEITY DESCRIPTIVE 
  /PLOT=RESIDUALS 
  /CRITERIA=ALPHA(.05) 
  /DESIGN=Pedu class.



UNIANOVA T3_PFM BY class WITH Pedu 
  /METHOD=SSTYPE(3) 
  /INTERCEPT=INCLUDE 
  /SAVE=ZRESID 
  /EMMEANS=TABLES(class) WITH(Pedu=MEAN) COMPARE ADJ(BONFERRONI) 
  /PRINT=ETASQ HOMOGENEITY DESCRIPTIVE 
  /PLOT=RESIDUALS 
  /CRITERIA=ALPHA(.05) 
  /DESIGN=Pedu class.



UNIANOVA T3_PMasteryGoal BY class WITH Pedu 
  /METHOD=SSTYPE(3) 
  /INTERCEPT=INCLUDE 
  /SAVE=ZRESID 
  /EMMEANS=TABLES(class) WITH(Pedu=MEAN) COMPARE ADJ(BONFERRONI) 
  /PRINT=ETASQ HOMOGENEITY DESCRIPTIVE 
  /PLOT=RESIDUALS 
  /CRITERIA=ALPHA(.05) 
  /DESIGN=Pedu class.

UNIANOVA T3_PPerfGoal BY class WITH Pedu 
  /METHOD=SSTYPE(3) 
  /INTERCEPT=INCLUDE 
  /SAVE=ZRESID 
  /EMMEANS=TABLES(class) WITH(Pedu=MEAN) COMPARE ADJ(BONFERRONI) 
  /PRINT=ETASQ HOMOGENEITY DESCRIPTIVE 
  /PLOT=RESIDUALS 
  /CRITERIA=ALPHA(.05) 
  /DESIGN=Pedu class.

UNIANOVA C4_Fixed BY class WITH Pedu 
  /METHOD=SSTYPE(3) 
  /INTERCEPT=INCLUDE 
  /SAVE=ZRESID 
  /EMMEANS=TABLES(class) WITH(Pedu=MEAN) COMPARE ADJ(BONFERRONI) 
  /PRINT=ETASQ HOMOGENEITY DESCRIPTIVE 
  /PLOT=RESIDUALS 
  /CRITERIA=ALPHA(.05) 
  /DESIGN=Pedu class.

UNIANOVA C4_Anx BY class WITH Pedu 
  /METHOD=SSTYPE(3) 
  /INTERCEPT=INCLUDE 
  /SAVE=ZRESID 
  /EMMEANS=TABLES(class) WITH(Pedu=MEAN) COMPARE ADJ(BONFERRONI) 
  /PRINT=ETASQ HOMOGENEITY DESCRIPTIVE 
  /PLOT=RESIDUALS 
  /CRITERIA=ALPHA(.05) 
  /DESIGN=Pedu class.

UNIANOVA C4ChallengePro BY class WITH Pedu 
  /METHOD=SSTYPE(3) 
  /INTERCEPT=INCLUDE 
  /SAVE=ZRESID 
  /EMMEANS=TABLES(class) WITH(Pedu=MEAN) COMPARE ADJ(BONFERRONI) 
  /PRINT=ETASQ HOMOGENEITY DESCRIPTIVE 
  /PLOT=RESIDUALS 
  /CRITERIA=ALPHA(.05) 
  /DESIGN=Pedu class.

UNIANOVA C4WJA_W_Final BY class WITH Pedu 
  /METHOD=SSTYPE(3) 
  /INTERCEPT=INCLUDE 
  /SAVE=ZRESID 
  /EMMEANS=TABLES(class) WITH(Pedu=MEAN) COMPARE ADJ(BONFERRONI) 
  /PRINT=ETASQ HOMOGENEITY DESCRIPTIVE 
  /PLOT=RESIDUALS 
  /CRITERIA=ALPHA(.05) 
  /DESIGN=Pedu class.

*adding wave 1 adjustmen as covariante*

UNIANOVA C4_Fixed BY class WITH Pedu C3_Fixed 
  /METHOD=SSTYPE(3) 
  /INTERCEPT=INCLUDE 
  /SAVE=ZRESID 
  /EMMEANS=TABLES(class) WITH(Pedu=MEAN C3_Fixed=MEAN) COMPARE ADJ(BONFERRONI) 
  /PRINT=ETASQ HOMOGENEITY DESCRIPTIVE 
  /PLOT=RESIDUALS 
  /CRITERIA=ALPHA(.05) 
  /DESIGN=Pedu C3_Fixed class.

UNIANOVA C4_Anx BY class WITH Pedu C3_Anx 
  /METHOD=SSTYPE(3) 
  /INTERCEPT=INCLUDE 
  /SAVE=ZRESID 
  /EMMEANS=TABLES(class) WITH(Pedu=MEAN C3_Anx=MEAN) COMPARE ADJ(BONFERRONI) 
  /PRINT=ETASQ HOMOGENEITY DESCRIPTIVE 
  /PLOT=RESIDUALS 
  /CRITERIA=ALPHA(.05) 
  /DESIGN=Pedu C3_Anx class.

UNIANOVA C4ChallengePro BY class WITH Pedu C3ChallengePro 
  /METHOD=SSTYPE(3) 
  /INTERCEPT=INCLUDE 
  /SAVE=ZRESID 
  /EMMEANS=TABLES(class) WITH(Pedu=MEAN C3ChallengePro=MEAN) COMPARE ADJ(BONFERRONI) 
  /PRINT=ETASQ HOMOGENEITY DESCRIPTIVE 
  /PLOT=RESIDUALS 
  /CRITERIA=ALPHA(.05) 
  /DESIGN=Pedu C3ChallengePro class.

UNIANOVA C4WJA_W_Final BY class WITH Pedu C3WJA_W_Final
  /METHOD=SSTYPE(3) 
  /INTERCEPT=INCLUDE 
  /SAVE=ZRESID 
  /EMMEANS=TABLES(class) WITH(Pedu=MEAN C3WJA_W_Final=MEAN) COMPARE ADJ(BONFERRONI) 
  /PRINT=ETASQ HOMOGENEITY DESCRIPTIVE 
  /PLOT=RESIDUALS 
  /CRITERIA=ALPHA(.05) 
  /DESIGN=Pedu C3WJA_W_Final class.

