
****************** PART 1. Regression Model Summary ******************

PROCESS Model Code : 4 (Hayes, 2018; www.guilford.com/p/hayes3)
PROCESS Model Type : Simple Mediation
-    Outcome (Y) : HOME_mean
-  Predictor (X) : ALEX_mean
-  Mediators (M) : ECR_mean
- Moderators (W) : -
- Covariates (C) : age, language, avgtemp
-   HLM Clusters : -

Formula of Mediator:
-    ECR_mean ~ age + language + avgtemp + ALEX_mean
Formula of Outcome:
-    HOME_mean ~ age + language + avgtemp + ALEX_mean + ECR_mean

CAUTION:
  Fixed effect (coef.) of a predictor involved in an interaction
  denotes its "simple effect/slope" at the other predictor = 0.
  Only when all predictors in an interaction are mean-centered
  can the fixed effect denote the "main effect"!
  
Model Summary

───────────────────────────────────────────────────────
             (1) HOME_mean  (2) ECR_mean  (3) HOME_mean
───────────────────────────────────────────────────────
(Intercept)     0.431         -1.443         0.241     
               (4.828)        (5.194)       (4.781)    
age             0.000          0.002         0.000     
               (0.002)        (0.003)       (0.002)    
language        0.015          0.018         0.018     
               (0.009)        (0.010)       (0.009)    
avgtemp         0.097 *       -0.048         0.091 *   
               (0.045)        (0.048)       (0.044)    
ALEX_mean      -0.138 ***      0.733 ***    -0.042     
               (0.037)        (0.040)       (0.041)    
ECR_mean                                    -0.132 *** 
                                            (0.025)    
───────────────────────────────────────────────────────
R^2             0.017          0.217         0.037     
Adj. R^2        0.014          0.214         0.033     
Num. obs.    1344           1344          1344         
───────────────────────────────────────────────────────
Note. * p < .05, ** p < .01, *** p < .001.

************ PART 2. Mediation/Moderation Effect Estimate ************

Package Use : ‘mediation’ (v4.5.0)
Effect Type : Simple Mediation (Model 4)
Sample Size : 1344 (179 missing observations deleted)
Random Seed : set.seed()
Simulations : 1000 (Bootstrap)

Running 1000 simulations...
Indirect Path: "ALEX_mean" (X) ==> "ECR_mean" (M) ==> "HOME_mean" (Y)
───────────────────────────────────────────────────────────────
               Effect    S.E.      z     p        [Boot 95% CI]
───────────────────────────────────────────────────────────────
Indirect (ab)  -0.096 (0.020) -4.891 <.001 *** [-0.139, -0.061]
Direct (c')    -0.042 (0.041) -1.010  .312     [-0.118,  0.037]
Total (c)      -0.138 (0.037) -3.687 <.001 *** [-0.210, -0.064]
───────────────────────────────────────────────────────────────
Percentile Bootstrap Confidence Interval
(SE and CI are estimated based on 1000 Bootstrap samples.)

Note. The results based on bootstrapping or other random processes
are unlikely identical to other statistical software (e.g., SPSS).
To make results reproducible, you need to set a seed (any number).
Please see the help page for details: help(PROCESS)
Ignore this note if you have already set a seed. :)

