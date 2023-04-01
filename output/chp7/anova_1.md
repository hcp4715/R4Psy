
====== ANOVA (Within-Subjects Design) ======

Descriptives:
──────────────────────────
  "Match"  Mean    S.D.  n
──────────────────────────
 match    0.682 (0.054) 44
 mismatch 0.737 (0.056) 44
──────────────────────────
Total sample size: N = 44

ANOVA Table:
Dependent variable(s):      rt_mean
Between-subjects factor(s): –
Within-subjects factor(s):  Match
Covariate(s):               –
─────────────────────────────────────────────────────────────────────
          MS   MSE df1 df2       F     p     η²p [90% CI of η²p]  η²G
─────────────────────────────────────────────────────────────────────
Match  0.067 0.000   1  43 267.062 <.001 ***   .861 [.796, .899] .206
─────────────────────────────────────────────────────────────────────
MSE = mean square error (the residual variance of the linear model)
η²p = partial eta-squared = SS / (SS + SSE) = F * df1 / (F * df1 + df2)
ω²p = partial omega-squared = (F - 1) * df1 / (F * df1 + df2 + 1)
η²G = generalized eta-squared (see Olejnik & Algina, 2003)
Cohen’s f² = η²p / (1 - η²p)

Levene’s Test for Homogeneity of Variance:
No between-subjects factors. No need to do the Levene’s test.

Mauchly’s Test of Sphericity:
The repeated measures have only two levels. The assumption of sphericity is always met.

