# Priors in Confidence

## Using the code
This repository includes the code for the model-free analyses, the modelling analyses, the model code in the form of Stan files, and all the data needed to run the scripts. It also includes the saved results files needed to produce the plots in the manuscript. Main scripts include an option to rerun the modelling and/or analyses, as well as an option to load in the results for further analysis and visualization. Because the full result files for the model fits are so large, the results provided for plotting are smaller summary files. Full stanfit objects will be saved if the modelling is rerun.  

Note that variables "m1" and "m2" often appear in the code. These correspond to _w_choice_ and _w_conf_, respectively. 

### Scripts 

 
| Script | Description |
| ------ | ------ |
| modelFreeAnalyses.Rmd | All the behavioural analyses and plotting, including regression analyses. |
| calcMRatios.R | Estimates M-Ratios per subject and plots. |
| estimate_noise.R | Estimates internal noise and decision bias per subject and adds these columns to data. |
| plotModelPredictions.R | Simulate from the model and plot the predictions. |
| fit_hierarchical.R | Fits full Flexible model hierarchically. Also fits other versions of the model. Plots posteriors. |
| fit_subj.R | Fits the full Flexible model individually. (Not used in analyses for manuscript.) |
| kfold_h.R | Runs the 10-fold LOGO-CV analysis and plots the results. |
| simsFromFits.R | Runs and plots the posterior predictive checks that sample from model fits and simulate internal signals and then full trials. |
| postpredchecks.R | Runs and plots the posterior predictive checks that just sample and compute choice probabilities and confidence without internal signals. |
| paramRecovery.R | Runs the parameter recovery analyses and plots. |
| modelRecovery.R | Runs the model recovery analysis and plots. |
| ind_analysis_exp3.R | Estimates internal noise, decision bias, and metacognitive noise per subject and adds these columns to data for Exp. 3. |
| fit_hierarchical_exp3.R | Fits full Flexible model hierarchically for the probability task in Exp. 3. Plots posteriors. |
| plotGroup_exp3.R | Plots the results from model fits against simulations for Exp. 3. |
| paramRecovery_exp3.R | Runs the parameter recovery analyses and plots for Exp. 3. |

## Installation Note
This code uses RStan and CmdStanR for interfacing with Stan. In order to install them, a properly configurated C++ toolchain is needed. See for details: 
[RStan](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started)
[CmdStanR](https://mc-stan.org/cmdstanr/articles/cmdstanr.html)
