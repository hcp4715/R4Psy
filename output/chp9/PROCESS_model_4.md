
****************** PART 1. Regression Model Summary ******************

PROCESS Model Code : 4 (Hayes, 2018; www.guilford.com/p/hayes3)
PROCESS Model Type : Simple Mediation
-    Outcome (Y) : avgtemp
-  Predictor (X) : DEQ
-  Mediators (M) : socialdiversity
- Moderators (W) : -
- Covariates (C) : -
-   HLM Clusters : -

Formula of Mediator:
-    socialdiversity ~ DEQ
Formula of Outcome:
-    avgtemp ~ DEQ + socialdiversity

CAUTION:
  Fixed effect (coef.) of a predictor involved in an interaction
  denotes its "simple effect/slope" at the other predictor = 0.
  Only when all predictors in an interaction are mean-centered
  can the fixed effect denote the "main effect"!
  
Model Summary

────────────────────────────────────────────────────────────────
                 (1) avgtemp   (2) socialdiversity  (3) avgtemp 
────────────────────────────────────────────────────────────────
(Intercept)        36.564 ***     6.134 ***           36.157 ***
                   (0.035)       (0.114)              (0.059)   
DEQ                -0.004 ***     0.014 ***           -0.005 ***
                   (0.001)       (0.003)              (0.001)   
socialdiversity                                        0.066 ***
                                                      (0.008)   
────────────────────────────────────────────────────────────────
R^2                 0.020         0.018                0.066    
Adj. R^2            0.019         0.017                0.064    
Num. obs.        1434          1434                 1434        
────────────────────────────────────────────────────────────────
Note. * p < .05, ** p < .01, *** p < .001.

************ PART 2. Mediation/Moderation Effect Estimate ************

Package Use : ‘mediation’ (v4.5.0)
Effect Type : Simple Mediation (Model 4)
Sample Size : 1434 (89 missing observations deleted)
Random Seed : set.seed()
Simulations : 1000 (Bootstrap)

Running 1000 simulations...
Indirect Path: "DEQ" (X) ==> "socialdiversity" (M) ==> "avgtemp" (Y)
───────────────────────────────────────────────────────────────
               Effect    S.E.      z     p        [Boot 95% CI]
───────────────────────────────────────────────────────────────
Indirect (ab)   0.001 (0.000)  4.832 <.001 *** [ 0.001,  0.001]
Direct (c')    -0.005 (0.001) -5.844 <.001 *** [-0.007, -0.004]
Total (c)      -0.004 (0.001) -4.857 <.001 *** [-0.006, -0.003]
───────────────────────────────────────────────────────────────
Percentile Bootstrap Confidence Interval
(SE and CI are estimated based on 1000 Bootstrap samples.)

Note. The results based on bootstrapping or other random processes
are unlikely identical to other statistical software (e.g., SPSS).
To make results reproducible, you need to set a seed (any number).
Please see the help page for details: help(PROCESS)
Ignore this note if you have already set a seed. :)

