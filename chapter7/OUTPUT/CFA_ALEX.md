lavaan 0.6.14 ended normally after 68 iterations

  Estimator                                         ML
  Optimization method                           NLMINB
  Number of model parameters                        35

                                                  Used       Total
  Number of observations                          1508        1523

Model Test User Model:
                                                      
  Test statistic                               792.150
  Degrees of freedom                               101
  P-value (Chi-square)                           0.000

Model Test Baseline Model:

  Test statistic                              7485.451
  Degrees of freedom                               120
  P-value                                        0.000

User Model versus Baseline Model:

  Comparative Fit Index (CFI)                    0.906
  Tucker-Lewis Index (TLI)                       0.889

Loglikelihood and Information Criteria:

  Loglikelihood user model (H0)             -31297.642
  Loglikelihood unrestricted model (H1)     -30901.566
                                                      
  Akaike (AIC)                               62665.283
  Bayesian (BIC)                             62851.432
  Sample-size adjusted Bayesian (SABIC)      62740.247

Root Mean Square Error of Approximation:

  RMSEA                                          0.067
  90 Percent confidence interval - lower         0.063
  90 Percent confidence interval - upper         0.072
  P-value H_0: RMSEA <= 0.050                    0.000
  P-value H_0: RMSEA >= 0.080                    0.000

Standardized Root Mean Square Residual:

  SRMR                                           0.069

Parameter Estimates:

  Standard errors                             Standard
  Information                                 Expected
  Information saturated (h1) model          Structured

Latent Variables:
                   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
  DIF =~                                                                
    ALEX1             1.000                               0.807    0.775
    ALEX3             0.602    0.035   17.198    0.000    0.486    0.459
    ALEX5             0.914    0.034   26.528    0.000    0.738    0.685
    ALEX6             0.861    0.034   25.410    0.000    0.695    0.659
    ALEX7             1.003    0.035   28.383    0.000    0.809    0.728
    ALEX10            0.948    0.033   28.874    0.000    0.765    0.739
    ALEX11            0.787    0.034   23.449    0.000    0.635    0.613
  DDF =~                                                                
    ALEX2             1.000                               0.887    0.802
    ALEX4             0.781    0.029   26.632    0.000    0.693    0.698
    ALEX8             0.880    0.031   28.210    0.000    0.781    0.737
    ALEX9             0.611    0.033   18.609    0.000    0.542    0.501
  EOF =~                                                                
    ALEX12            1.000                               0.163    0.192
    ALEX13            2.147    0.425    5.056    0.000    0.349    0.335
    ALEX14            2.856    0.523    5.463    0.000    0.464    0.542
    ALEX15            2.776    0.520    5.342    0.000    0.451    0.443
    ALEX16            3.185    0.581    5.479    0.000    0.518    0.571
  ALEX =~                                                               
    DIF               1.000                               0.560    0.560
    DDF               2.790    0.521    5.357    0.000    1.421    1.421
    EOF               0.101    0.020    5.055    0.000    0.281    0.281

Variances:
                   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
   .ALEX1             0.433    0.020   21.811    0.000    0.433    0.399
   .ALEX3             0.883    0.033   26.470    0.000    0.883    0.789
   .ALEX5             0.616    0.025   24.158    0.000    0.616    0.531
   .ALEX6             0.628    0.026   24.598    0.000    0.628    0.566
   .ALEX7             0.581    0.025   23.243    0.000    0.581    0.470
   .ALEX10            0.486    0.021   22.949    0.000    0.486    0.454
   .ALEX11            0.671    0.027   25.224    0.000    0.671    0.625
   .ALEX2             0.435    0.024   18.424    0.000    0.435    0.356
   .ALEX4             0.507    0.022   22.788    0.000    0.507    0.513
   .ALEX8             0.514    0.024   21.569    0.000    0.514    0.457
   .ALEX9             0.875    0.034   25.840    0.000    0.875    0.749
   .ALEX12            0.688    0.026   26.742    0.000    0.688    0.963
   .ALEX13            0.965    0.038   25.073    0.000    0.965    0.888
   .ALEX14            0.519    0.027   19.320    0.000    0.519    0.707
   .ALEX15            0.836    0.037   22.779    0.000    0.836    0.804
   .ALEX16            0.553    0.031   18.008    0.000    0.553    0.674
   .DIF               0.447    0.045    9.837    0.000    0.686    0.686
   .DDF              -0.803    0.286   -2.804    0.005   -1.019   -1.019
   .EOF               0.024    0.008    2.890    0.004    0.921    0.921
    ALEX              0.204    0.042    4.878    0.000    1.000    1.000

