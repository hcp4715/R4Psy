
****************** PART 1. Regression Model Summary ******************

PROCESS Model Code : 1 (Hayes, 2018; www.guilford.com/p/hayes3)
PROCESS Model Type : Simple Moderation
-    Outcome (Y) : ECR_mean
-  Predictor (X) : ALEX_mean
-  Mediators (M) : -
- Moderators (W) : KAMF_mean
- Covariates (C) : age, language, avgtemp
-   HLM Clusters : -

Formula of Outcome:
-    ECR_mean ~ age + language + avgtemp + ALEX_mean*KAMF_mean

CAUTION:
  Fixed effect (coef.) of a predictor involved in an interaction
  denotes its "simple effect/slope" at the other predictor = 0.
  Only when all predictors in an interaction are mean-centered
  can the fixed effect denote the "main effect"!
  
Model Summary

───────────────────────────────────────────────
                     (1) ECR_mean  (2) ECR_mean
───────────────────────────────────────────────
(Intercept)            -1.443        -1.187    
                       (5.194)       (5.192)   
age                     0.002         0.002    
                       (0.003)       (0.003)   
language                0.018         0.018    
                       (0.010)       (0.010)   
avgtemp                -0.048        -0.052    
                       (0.048)       (0.048)   
ALEX_mean               0.733 ***     0.993 ***
                       (0.040)       (0.125)   
KAMF_mean                             0.170 *  
                                     (0.079)   
ALEX_mean:KAMF_mean                  -0.069 *  
                                     (0.031)   
───────────────────────────────────────────────
R^2                     0.217         0.219    
Adj. R^2                0.214         0.216    
Num. obs.            1344          1344        
───────────────────────────────────────────────
Note. * p < .05, ** p < .01, *** p < .001.

************ PART 2. Mediation/Moderation Effect Estimate ************

Package Use : ‘interactions’ (v1.1.5)
Effect Type : Simple Moderation (Model 1)
Sample Size : 1344 (179 missing observations deleted)
Random Seed : -
Simulations : -

Interaction Effect on "ECR_mean" (Y)
──────────────────────────────────────────────
                          F df1  df2     p    
──────────────────────────────────────────────
ALEX_mean * KAMF_mean  4.86   1 1337  .028 *  
──────────────────────────────────────────────

Simple Slopes: "ALEX_mean" (X) ==> "ECR_mean" (Y)
────────────────────────────────────────────────────────────
 "KAMF_mean"  Effect    S.E.      t     p           [95% CI]
────────────────────────────────────────────────────────────
 2.471 (- SD)  0.822 (0.057) 14.460 <.001 *** [0.711, 0.934]
 3.650 (Mean)  0.740 (0.040) 18.538 <.001 *** [0.662, 0.819]
 4.829 (+ SD)  0.659 (0.052) 12.682 <.001 *** [0.557, 0.761]
────────────────────────────────────────────────────────────

