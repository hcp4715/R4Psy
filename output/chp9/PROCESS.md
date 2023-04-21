
****************** PART 1. Regression Model Summary ******************

PROCESS Model Code : 7 (Hayes, 2018; www.guilford.com/p/hayes3)
PROCESS Model Type : Moderated Mediation
-    Outcome (Y) : HOME_mean
-  Predictor (X) : ALEX_mean
-  Mediators (M) : ECR_mean
- Moderators (W) : KAMF_mean
- Covariates (C) : age, language, avgtemp
-   HLM Clusters : -

Formula of Mediator:
-    ECR_mean ~ age + language + avgtemp + ALEX_mean*KAMF_mean
Formula of Outcome:
-    HOME_mean ~ age + language + avgtemp + ALEX_mean + KAMF_mean + ECR_mean

CAUTION:
  Fixed effect (coef.) of a predictor involved in an interaction
  denotes its "simple effect/slope" at the other predictor = 0.
  Only when all predictors in an interaction are mean-centered
  can the fixed effect denote the "main effect"!
  
Model Summary

───────────────────────────────────────────────────────────────
                     (1) HOME_mean  (2) ECR_mean  (3) HOME_mean
───────────────────────────────────────────────────────────────
(Intercept)             0.431         -1.187        -0.100     
                       (4.828)        (5.192)       (4.775)    
age                     0.000          0.002         0.001     
                       (0.002)        (0.003)       (0.002)    
language                0.015          0.018         0.019 *   
                       (0.009)        (0.010)       (0.009)    
avgtemp                 0.097 *       -0.052         0.081     
                       (0.045)        (0.048)       (0.044)    
ALEX_mean              -0.138 ***      0.993 ***    -0.039     
                       (0.037)        (0.125)       (0.041)    
KAMF_mean                              0.170 *       0.039 *   
                                      (0.079)       (0.017)    
ALEX_mean:KAMF_mean                   -0.069 *                 
                                      (0.031)                  
ECR_mean                                            -0.132 *** 
                                                    (0.025)    
───────────────────────────────────────────────────────────────
R^2                     0.017          0.219         0.040     
Adj. R^2                0.014          0.216         0.036     
Num. obs.            1344           1344          1344         
───────────────────────────────────────────────────────────────
Note. * p < .05, ** p < .01, *** p < .001.

************ PART 2. Mediation/Moderation Effect Estimate ************

Package Use : ‘mediation’ (v4.5.0), ‘interactions’ (v1.1.5)
Effect Type : Moderated Mediation (Model 7)
Sample Size : 1344 (179 missing observations deleted)
Random Seed : set.seed()
Simulations : 100 (Bootstrap)

Direct Effect: "ALEX_mean" (X) ==> "HOME_mean" (Y)
─────────────────────────────────────────────────────────────
             Effect    S.E.      t     p             [95% CI]
─────────────────────────────────────────────────────────────
Direct (c')  -0.039 (0.041) -0.951  .342     [-0.119,  0.041]
─────────────────────────────────────────────────────────────

Interaction Effect on "ECR_mean" (M)
──────────────────────────────────────────────
                          F df1  df2     p    
──────────────────────────────────────────────
ALEX_mean * KAMF_mean  4.86   1 1337  .028 *  
──────────────────────────────────────────────

Simple Slopes: "ALEX_mean" (X) ==> "ECR_mean" (M)
(Conditional Effects [a] of X on M)
────────────────────────────────────────────────────────────
 "KAMF_mean"  Effect    S.E.      t     p           [95% CI]
────────────────────────────────────────────────────────────
 2.471 (- SD)  0.822 (0.057) 14.460 <.001 *** [0.711, 0.934]
 3.650 (Mean)  0.740 (0.040) 18.538 <.001 *** [0.662, 0.819]
 4.829 (+ SD)  0.659 (0.052) 12.682 <.001 *** [0.557, 0.761]
────────────────────────────────────────────────────────────

Running 100 * 3 simulations...
Indirect Path: "ALEX_mean" (X) ==> "ECR_mean" (M) ==> "HOME_mean" (Y)
(Conditional Indirect Effects [ab] of X through M on Y)
──────────────────────────────────────────────────────────────
 "KAMF_mean"  Effect    S.E.      z     p        [Boot 95% CI]
──────────────────────────────────────────────────────────────
 2.471 (- SD) -0.108 (0.024) -4.468 <.001 *** [-0.153, -0.065]
 3.650 (Mean) -0.097 (0.021) -4.677 <.001 *** [-0.138, -0.060]
 4.829 (+ SD) -0.087 (0.021) -4.198 <.001 *** [-0.127, -0.050]
──────────────────────────────────────────────────────────────
Percentile Bootstrap Confidence Interval
(SE and CI are estimated based on 100 Bootstrap samples.)

Note. The results based on bootstrapping or other random processes
are unlikely identical to other statistical software (e.g., SPSS).
To make results reproducible, you need to set a seed (any number).
Please see the help page for details: help(PROCESS)
Ignore this note if you have already set a seed. :)

