------ EMMEANS (effect = "A_") ------

Joint Tests of "A_":
──────────────────────────────────────────────────────────
 Effect  "B_" df1 df2      F     p     η²p [90% CI of η²p]
──────────────────────────────────────────────────────────
     A_ Other   1  43  2.801  .101       .061 [.000, .206]
     A_ Self    1  43 56.904 <.001 ***   .570 [.403, .683]
──────────────────────────────────────────────────────────
Note. Simple effects of repeated measures with 3 or more levels
are different from the results obtained with SPSS MANOVA syntax.

Estimated Marginal Means of "A_":
──────────────────────────────────────────
  "A_"  "B_" Mean [95% CI of Mean]    S.E.
──────────────────────────────────────────
 Bad.  Other  0.719 [0.701, 0.737] (0.009)
 Good. Other  0.707 [0.687, 0.727] (0.010)
 Bad.  Self   0.734 [0.715, 0.753] (0.009)
 Good. Self   0.682 [0.665, 0.699] (0.008)
──────────────────────────────────────────

Pairwise Comparisons of "A_":
────────────────────────────────────────────────────────────────────────────────
     Contrast  "B_" Estimate    S.E. df      t     p     Cohen’s d [95% CI of d]
────────────────────────────────────────────────────────────────────────────────
 Good. - Bad. Other   -0.012 (0.007) 43 -1.674  .101     -0.248 [-0.548,  0.051]
 Good. - Bad. Self    -0.052 (0.007) 43 -7.544 <.001 *** -1.093 [-1.386, -0.801]
────────────────────────────────────────────────────────────────────────────────
Pooled SD for computing Cohen’s d: 0.047
No need to adjust p values.

Disclaimer:
By default, pooled SD is Root Mean Square Error (RMSE).
There is much disagreement on how to compute Cohen’s d.
You are completely responsible for setting `sd.pooled`.
You might also use `effectsize::t_to_d()` to compute d.

