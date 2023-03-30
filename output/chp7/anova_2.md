
====== ANOVA (Within-Subjects Design) ======

Descriptives:
────────────────────────────────
     "A_"  "B_"  Mean    S.D.  n
────────────────────────────────
 immoral. Other 0.719 (0.059) 44
 immoral. Self  0.734 (0.062) 44
 moral.   Other 0.707 (0.067) 44
 moral.   Self  0.682 (0.056) 44
────────────────────────────────
Total sample size: N = 44

ANOVA Table:
Dependent variable(s):      A_immoral&B_Other, A_immoral&B_Self, A_moral&B_Other, A_moral&B_Self
Between-subjects factor(s): –
Within-subjects factor(s):  A_, B_
Covariate(s):               –
──────────────────────────────────────────────────────────────────────
            MS   MSE df1 df2      F     p     η²p [90% CI of η²p]  η²G
──────────────────────────────────────────────────────────────────────
A_       0.045 0.001   1  43 49.778 <.001 ***   .537 [.364, .658] .065
B_       0.001 0.001   1  43  0.954  .334       .022 [.000, .138] .002
A_ * B_  0.018 0.001   1  43 14.296 <.001 ***   .250 [.084, .416] .027
──────────────────────────────────────────────────────────────────────
MSE = mean square error (the residual variance of the linear model)
η²p = partial eta-squared = SS / (SS + SSE) = F * df1 / (F * df1 + df2)
ω²p = partial omega-squared = (F - 1) * df1 / (F * df1 + df2 + 1)
η²G = generalized eta-squared (see Olejnik & Algina, 2003)
Cohen’s f² = η²p / (1 - η²p)

Levene’s Test for Homogeneity of Variance:
No between-subjects factors. No need to do the Levene’s test.

Mauchly’s Test of Sphericity:
The repeated measures have only two levels. The assumption of sphericity is always met.

