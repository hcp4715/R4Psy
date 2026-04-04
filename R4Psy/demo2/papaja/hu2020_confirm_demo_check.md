# Method

Describe participants, tasks, and procedure for the confirmatory study.
Use the official article and supplementary material for exact details.

# Results

    data_dir <- file.path("..", "data", "processed")
    match_wide <- read.csv(file.path(data_dir, "MS_match_behav_wide.csv"), stringsAsFactors = FALSE)
    categ_wide <- read.csv(file.path(data_dir, "MS_categ_behav_wide.csv"), stringsAsFactors = FALSE)
    categ_notask <- read.csv(file.path(data_dir, "MS_categ_behav_noTask_wide.csv"), stringsAsFactors = FALSE)
    cross_task <- read.csv(file.path(data_dir, "MS_cross_taskeffect_wide.csv"), stringsAsFactors = FALSE)
    jasp_results <- readRDS(file.path(data_dir, "jasp_reproduction", "jasp_reproduction_results.rds"))

Summarize the confirmatory study analyses here, mirroring reported
results.

    compare_dir <- file.path(data_dir, "jasp_reproduction")
    compare_ttests <- file.path(compare_dir, "compare_ttests.csv")
    compare_anova <- file.path(compare_dir, "compare_anova.csv")
    compare_cor <- file.path(compare_dir, "compare_cor.csv")

    safe_read_csv <- function(path) {
      if (!file.exists(path) || file.info(path)$size <= 3) {
        return(NULL)
      }
      out <- tryCatch(read.csv(path, stringsAsFactors = FALSE), error = function(e) NULL)
      if (is.null(out) || nrow(out) == 0) {
        return(NULL)
      }
      out
    }

    anova_df <- safe_read_csv(compare_anova)
    ttest_df <- safe_read_csv(compare_ttests)
    cor_df <- safe_read_csv(compare_cor)

    if (!is.null(anova_df)) {
      cat("## JASP comparison (ANOVA)\n")
      print(utils::head(anova_df))
    }
    if (!is.null(ttest_df)) {
      cat("## JASP comparison (T-tests)\n")
      print(utils::head(ttest_df))
    }
    if (!is.null(cor_df)) {
      cat("## JASP comparison (Correlations)\n")
      print(utils::head(cor_df))
    }

## JASP comparison (Correlations)

                        analysis                    var1

1 MS\_cross\_taskeffect\_wide:0:cor d\_goodslf\_goodoth 2
MS\_cross\_taskeffect\_wide:0:cor d\_goodslf\_goodoth 3
MS\_cross\_taskeffect\_wide:0:cor Val\_ACC\_goodslf\_goodoth 4
MS\_cross\_taskeffect\_wide:2:cor d\_goodslf\_badslf 5
MS\_cross\_taskeffect\_wide:2:cor d\_goodslf\_badslf 6
MS\_cross\_taskeffect\_wide:2:cor Val\_ACC\_goodslf\_badslf var2 r\_jasp
r\_r r\_diff p\_jasp 1 Val\_ACC\_goodslf\_goodoth 0.20514179 0.20514179
-8.326673e-17 0.1982124 2 Id\_ACC\_goodslf\_goodoth 0.13063634
0.13063634 4.163336e-16 0.4155821 3 Id\_ACC\_goodslf\_goodoth 0.14413807
0.14413807 -2.220446e-16 0.3685996 4 Val\_ACC\_goodslf\_badslf
0.24164200 0.24164200 -3.885781e-16 0.1279914 5 Id\_ACC\_goodslf\_badslf
0.06007816 0.06007816 -4.163336e-17 0.7090527 6 Id\_ACC\_goodslf\_badslf
0.18571572 0.18571572 0.000000e+00 0.2450190 p\_r p\_diff 1 0.1982124
-2.775558e-17 2 0.4155821 5.551115e-16 3 0.3685996 -1.665335e-16 4
0.1279914 5.273559e-16 5 0.7090527 -2.220446e-16 6 0.2450190
-1.387779e-16

    if (!is.null(jasp_results$r$MS_rep_match_behav)) {
      cat("## Matching task (JASP-matched outputs)\n")
      print(jasp_results$r$MS_rep_match_behav[[1]])
    }

## Matching task (JASP-matched outputs)

## Bayes factor analysis

\[1\] MValence + Subject : 8308161 ±0.77% \[2\] ID + Subject : 0.5847399
±4.84% \[3\] MValence + ID + Subject : 7105599 ±1.7% \[4\] MValence +
ID + MValence:ID + Subject : 514807069 ±2.72%

Against denominator: value ~ Subject — Bayes factor type: BFlinearModel,
JZS

# Discussion

Summarize the reproduction outcome and note any deviations from the
original pipeline.
