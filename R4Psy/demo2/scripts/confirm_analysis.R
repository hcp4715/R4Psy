# Confirmatory study analysis (match JASP outputs)
# - Extract JASP analysis specs and results
# - Reproduce analyses in R and compare to JASP tables

get_script_dir <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- cmd_args[grepl("--file=", cmd_args)]
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("--file=", "", file_arg))))
  }
  if (!is.null(sys.frames()[[1]]$ofile)) {
    return(dirname(normalizePath(sys.frames()[[1]]$ofile)))
  }
  return(getwd())
}

script_dir <- get_script_dir()
demo_root <- normalizePath(file.path(script_dir, ".."), winslash = "/")
input_dir <- file.path(demo_root, "data", "processed")
external_dir <- file.path(demo_root, "external", "moralSelf_ddm", "2_confirm_study", "Results", "2_trad_analysis")
output_dir <- file.path(demo_root, "data", "processed", "jasp_reproduction")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

required_pkgs <- c("dplyr", "tidyr", "jsonlite", "afex", "BayesFactor", "effectsize")
repos <- getOption("repos")
if (is.null(repos) || is.na(repos["CRAN"]) || repos["CRAN"] == "@CRAN@") {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
}
for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
}

library(dplyr)
library(tidyr)
library(jsonlite)
library(afex)
library(BayesFactor)
library(effectsize)

files_needed <- c(
  "MS_match_behav_wide.csv",
  "MS_categ_behav_wide.csv",
  "MS_categ_behav_noTask_wide.csv",
  "MS_cross_taskeffect_wide.csv"
)

missing <- files_needed[!file.exists(file.path(input_dir, files_needed))]
if (length(missing) > 0) {
  stop("Missing processed files: ", paste(missing, collapse = ", "))
}

match_wide <- read.csv(file.path(input_dir, "MS_match_behav_wide.csv"), stringsAsFactors = FALSE)
categ_wide <- read.csv(file.path(input_dir, "MS_categ_behav_wide.csv"), stringsAsFactors = FALSE)
categ_notask <- read.csv(file.path(input_dir, "MS_categ_behav_noTask_wide.csv"), stringsAsFactors = FALSE)
cross_task <- read.csv(file.path(input_dir, "MS_cross_taskeffect_wide.csv"), stringsAsFactors = FALSE)

jasp_map <- list(
  MS_rep_match_behav = list(
    jasp = file.path(external_dir, "MS_rep_match_behav.jasp"),
    data = match_wide
  ),
  MS_rep_categ_behav = list(
    jasp = file.path(external_dir, "MS_rep_categ_behav.jasp"),
    data = categ_wide
  ),
  MS_rep_categ_behav_noTask_wide = list(
    jasp = file.path(external_dir, "MS_rep_categ_behav_noTask_wide.jasp"),
    data = categ_notask
  ),
  MS_cross_taskeffect_wide = list(
    jasp = file.path(external_dir, "MS_cross_taskeffect_wide.jasp"),
    data = cross_task
  )
)

table_from_jasp <- function(entry) {
  cols <- entry$colNames$rows
  data <- do.call(rbind, lapply(entry$data, function(x) unlist(x, use.names = FALSE)))
  df <- as.data.frame(data, stringsAsFactors = FALSE)
  if (length(cols) == ncol(df)) {
    names(df) <- cols
  }
  df
}

extract_jasp_results <- function(jasp_path) {
  tmp_dir <- tempfile("jasp_")
  dir.create(tmp_dir)
  utils::unzip(jasp_path, exdir = tmp_dir)
  analyses <- jsonlite::fromJSON(file.path(tmp_dir, "analyses.json"))
  res_files <- list.files(tmp_dir, pattern = "jaspResults.json$", recursive = TRUE, full.names = TRUE)
  results <- lapply(res_files, function(path) {
    jsonlite::fromJSON(path)
  })
  list(analyses = analyses, results = results)
}

map_level <- function(cell, levels) {
  cell_lower <- tolower(cell)
  for (level in levels) {
    level_lower <- tolower(level)
    if (level_lower %in% c("nonmatch", "nm") && grepl("mismatch", cell_lower)) {
      return(level)
    }
    if (level_lower == "match" && grepl("mismatch", cell_lower)) {
      next
    }
    if (grepl(paste0("_", level_lower, "$"), cell_lower) || grepl(paste0("_", level_lower, "_"), cell_lower)) {
      return(level)
    }
  }
  for (level in levels) {
    level_lower <- tolower(level)
    if (grepl(level_lower, cell_lower)) {
      return(level)
    }
  }
  synonyms <- c(nonmatch = "mismatch", nm = "mismatch", m = "match")
  for (level in levels) {
    level_lower <- tolower(level)
    if (level_lower %in% names(synonyms)) {
      syn <- synonyms[[level_lower]]
      if (grepl(syn, cell_lower)) {
        return(level)
      }
    }
  }
  NA_character_
}

build_long_from_cells <- function(df, cells, factors) {
  long_df <- df %>%
    select(Subject, all_of(cells)) %>%
    pivot_longer(cols = all_of(cells), names_to = "cell", values_to = "value")

  long_df$Subject <- factor(long_df$Subject)

  for (idx in seq_len(nrow(factors))) {
    fac_name <- factors$name[idx]
    level_names <- factors$levels[[idx]]
    long_df[[fac_name]] <- vapply(long_df$cell, map_level, character(1), levels = level_names)
    if (any(is.na(long_df[[fac_name]]))) {
      stop("Failed to map factor ", fac_name, " for some cells")
    }
    long_df[[fac_name]] <- factor(long_df[[fac_name]], levels = level_names)
  }
  long_df
}

anova_frequentist <- function(long_df, dv, within) {
  aov <- afex::aov_ez(
    id = "Subject",
    dv = dv,
    within = within,
    data = long_df,
    type = 3
  )
  anova_tbl <- as.data.frame(afex::nice(aov, es = "none"))
  omega_tbl <- effectsize::omega_squared(aov)
  list(anova = anova_tbl, omega = omega_tbl)
}

anova_bayes <- function(long_df, dv, within, rscale_fixed = 0.5, rscale_random = 1.0) {
  formula <- as.formula(paste(dv, "~", paste(within, collapse = "*"), "+ Subject"))
  bf <- BayesFactor::anovaBF(
    formula = formula,
    data = long_df,
    whichRandom = "Subject",
    rscaleFixed = rscale_fixed,
    rscaleRandom = rscale_random
  )
  bf
}

paired_ttests <- function(df, pairs, hypothesis, rscale = 0.707) {
  if (is.data.frame(pairs) || is.matrix(pairs)) {
    pair_list <- lapply(seq_len(nrow(pairs)), function(i) pairs[i, ])
  } else {
    pair_list <- pairs
  }
  results <- lapply(pair_list, function(pair) {
    pair <- as.character(pair)
    if (!pair[1] %in% names(df) || !pair[2] %in% names(df)) {
      return(NULL)
    }
    x <- df[[pair[1]]]
    y <- df[[pair[2]]]
    keep <- complete.cases(x, y)
    x <- x[keep]
    y <- y[keep]

    alt <- "two.sided"
    bf_null <- NULL
    if (hypothesis == "groupOneGreater") {
      alt <- "greater"
      bf_null <- c(0, Inf)
    } else if (hypothesis == "groupTwoGreater") {
      alt <- "less"
      bf_null <- c(-Inf, 0)
    }

    t_res <- t.test(x, y, paired = TRUE, alternative = alt)
    bf_res <- BayesFactor::ttestBF(x = x, y = y, paired = TRUE, rscale = rscale, nullInterval = bf_null)
    data.frame(
      var1 = pair[1],
      var2 = pair[2],
      t = unname(t_res$statistic),
      df = unname(t_res$parameter),
      p = unname(t_res$p.value),
      bf10 = unname(exp(bf_res@bayesFactor$bf))
    )
  })
  results <- results[!vapply(results, is.null, logical(1))]
  if (length(results) == 0) {
    return(data.frame())
  }
  bind_rows(results)
}

correlation_tests <- function(df, variables, hypothesis) {
  combos <- combn(variables, 2, simplify = FALSE)
  alt <- "two.sided"
  if (hypothesis == "correlatedPositively") {
    alt <- "greater"
  } else if (hypothesis == "correlatedNegatively") {
    alt <- "less"
  }

  results <- lapply(combos, function(pair) {
    if (!pair[[1]] %in% names(df) || !pair[[2]] %in% names(df)) {
      return(NULL)
    }
    x <- df[[pair[[1]]]]
    y <- df[[pair[[2]]]]
    keep <- complete.cases(x, y)
    x <- x[keep]
    y <- y[keep]
    ct <- cor.test(x, y, method = "pearson", alternative = alt)
    data.frame(
      var1 = pair[[1]],
      var2 = pair[[2]],
      r = unname(ct$estimate),
      t = unname(ct$statistic),
      df = unname(ct$parameter),
      p = unname(ct$p.value)
    )
  })
  results <- results[!vapply(results, is.null, logical(1))]
  if (length(results) == 0) {
    return(data.frame())
  }
  bind_rows(results)
}

bayes_correlation <- function(df, pairs, hypothesis, prior_width = 1.0) {
  if (is.data.frame(pairs) || is.matrix(pairs)) {
    pair_list <- lapply(seq_len(nrow(pairs)), function(i) pairs[i, ])
  } else {
    pair_list <- pairs
  }
  results <- lapply(pair_list, function(pair) {
    pair <- as.character(pair)
    if (!pair[1] %in% names(df) || !pair[2] %in% names(df)) {
      return(NULL)
    }
    x <- df[[pair[1]]]
    y <- df[[pair[2]]]
    keep <- complete.cases(x, y)
    x <- x[keep]
    y <- y[keep]

    null_interval <- NULL
    if (hypothesis == "correlatedPositively") {
      null_interval <- c(0, 1)
    } else if (hypothesis == "correlatedNegatively") {
      null_interval <- c(-1, 0)
    }

    bf <- BayesFactor::correlationBF(x, y, rscale = prior_width, nullInterval = null_interval)
    data.frame(
      var1 = pair[1],
      var2 = pair[2],
      bf10 = unname(exp(bf@bayesFactor$bf))
    )
  })
  results <- results[!vapply(results, is.null, logical(1))]
  if (length(results) == 0) {
    return(data.frame())
  }
  bind_rows(results)
}

jasp_outputs <- list()
r_outputs <- list()
jasp_tables <- list()

for (name in names(jasp_map)) {
  jasp_path <- jasp_map[[name]]$jasp
  data_df <- jasp_map[[name]]$data
  parsed <- extract_jasp_results(jasp_path)

  jasp_tables <- list()
  for (res in parsed$results) {
    if (!is.null(res$data)) {
      for (key in names(res$data)) {
        entry <- res$data[[key]]
        if (!is.null(entry$type) && entry$type == "table") {
          jasp_tables[[paste(res$title, key, sep = ":")]] <- table_from_jasp(entry)
        }
      }
    }
  }
  jasp_outputs[[name]] <- list(analyses = parsed$analyses, tables = jasp_tables)

  analysis_tables <- list()
  analyses_df <- parsed$analyses$analyses
  for (i in seq_len(nrow(analyses_df))) {
    res <- analyses_df$results[i, , drop = FALSE]
    if (!is.data.frame(res)) next
    if (!is.null(res$withinSubjectsEffects) && is.list(res$withinSubjectsEffects)) {
      ws_data <- res$withinSubjectsEffects$data[[1]]
      if (is.data.frame(ws_data)) {
        analysis_tables[[paste0(name, ":", analyses_df$id[i], ":anova")]] <- ws_data
      }
    }
    if (!is.null(res$ttest) && is.list(res$ttest)) {
      tt_data <- res$ttest$data[[1]]
      if (is.data.frame(tt_data)) {
        analysis_tables[[paste0(name, ":", analyses_df$id[i], ":ttest")]] <- tt_data
      }
    }
    if (!is.null(res$correlations) && is.list(res$correlations)) {
      cor_data <- res$correlations$data[[1]]
      if (is.data.frame(cor_data)) {
        analysis_tables[[paste0(name, ":", analyses_df$id[i], ":cor")]] <- cor_data
      }
    }
  }
  jasp_tables[[name]] <- analysis_tables

  r_results <- list()
  analyses_df <- parsed$analyses$analyses
  for (i in seq_len(nrow(analyses_df))) {
    a_module <- analyses_df$module[i]
    a_name <- analyses_df$name[i]
    a_id <- analyses_df$id[i]
    options_row <- analyses_df$options[i, , drop = FALSE]
    options_list <- lapply(options_row, function(x) if (is.list(x)) x[[1]] else x)

    if (a_module == "ANOVA") {
      factors <- options_list$repeatedMeasuresFactors
      cells <- options_list$repeatedMeasuresCells
      if (is.null(factors) || length(factors) == 0) next
      if (!is.data.frame(factors)) {
        factors <- as.data.frame(factors)
      }
      if (length(cells) == 0) next
      if (!all(cells %in% names(data_df))) {
        message("Skipping analysis ", name, " id ", a_id, ": cells not in data")
        next
      }
      long_df <- tryCatch(
        build_long_from_cells(data_df, cells, factors),
        error = function(e) {
          message("Skipping analysis ", name, " id ", a_id, ": ", e$message)
          NULL
        }
      )
      if (is.null(long_df)) next
      within_names <- unlist(factors$name, use.names = FALSE)
      within_names <- within_names[sapply(within_names, function(f) nlevels(long_df[[f]]) > 1)]
      if (length(within_names) == 0) next
      if (a_name == "AnovaRepeatedMeasures") {
        r_results[[paste0(name, ":", a_id, ":frequentist")]] <- anova_frequentist(long_df, "value", within_names)
      } else if (a_name == "AnovaRepeatedMeasuresBayesian") {
        r_results[[paste0(name, ":", a_id, ":bayes")]] <- anova_bayes(
          long_df,
          "value",
          within_names,
          rscale_fixed = options_list$priorFixedEffects,
          rscale_random = options_list$priorRandomEffects
        )
      }
    }
    if (a_module == "T-Tests") {
      pairs <- options_list$pairs
      hypothesis <- options_list$hypothesis
      if (a_name == "TTestPairedSamples") {
        r_results[[paste0(name, ":", a_id, ":ttest")]] <- paired_ttests(data_df, pairs, hypothesis)
      } else if (a_name == "TTestBayesianPairedSamples") {
        r_results[[paste0(name, ":", a_id, ":ttest_bayes")]] <- paired_ttests(data_df, pairs, hypothesis)
      }
    }
    if (a_module == "Common" && grepl("Correlation", a_name)) {
      if (a_name == "Correlation") {
        r_results[[paste0(name, ":", a_id, ":cor")]] <- correlation_tests(data_df, options_list$variables, options_list$hypothesis)
      } else if (a_name == "CorrelationBayesianPairs") {
        r_results[[paste0(name, ":", a_id, ":cor_bayes")]] <- bayes_correlation(
          data_df,
          options_list$pairs,
          options_list$hypothesis,
          prior_width = options_list$priorWidth
        )
      }
    }
  }

  r_outputs[[name]] <- r_results
}

compare_ttests <- function(jasp_tbl, r_tbl) {
  if (nrow(jasp_tbl) == 0 || nrow(r_tbl) == 0) return(data.frame())
  jasp_tbl <- jasp_tbl %>%
    rename(var1 = v1, var2 = v2) %>%
    mutate(t = as.numeric(t), p = as.numeric(p))
  merged <- dplyr::inner_join(jasp_tbl, r_tbl, by = c("var1", "var2"), suffix = c("_jasp", "_r"))
  if (nrow(merged) == 0) return(data.frame())
  merged %>%
    transmute(
      var1,
      var2,
      t_jasp = t_jasp,
      t_r = t_r,
      t_diff = t_r - t_jasp,
      p_jasp = p_jasp,
      p_r = p_r,
      p_diff = p_r - p_jasp
    )
}

compare_anova <- function(jasp_tbl, r_tbl) {
  if (nrow(jasp_tbl) == 0 || nrow(r_tbl$anova) == 0) return(data.frame())
  jasp_tbl <- jasp_tbl %>%
    rename(effect = case, F_jasp = F, p_jasp = p) %>%
    mutate(F_jasp = as.numeric(F_jasp), p_jasp = as.numeric(p_jasp)) %>%
    filter(!is.na(F_jasp))
  r_tbl_df <- r_tbl$anova %>%
    rename(effect = Effect, F_r = F, p_r = p.value)
  merged <- dplyr::inner_join(jasp_tbl, r_tbl_df, by = "effect")
  if (nrow(merged) == 0) return(data.frame())
  merged %>%
    transmute(
      effect,
      F_jasp,
      F_r,
      F_diff = F_r - F_jasp,
      p_jasp,
      p_r,
      p_diff = p_r - p_jasp
    )
}

compare_cor <- function(jasp_tbl, r_tbl) {
  if (nrow(jasp_tbl) == 0 || nrow(r_tbl) == 0) return(data.frame())
  jasp_tbl <- jasp_tbl %>%
    rename(var1 = .variable1, var2 = .variable2, r_jasp = pearson, p_jasp = `pearson-p`) %>%
    mutate(r_jasp = as.numeric(r_jasp), p_jasp = as.numeric(p_jasp))
  merged <- dplyr::inner_join(jasp_tbl, r_tbl, by = c("var1", "var2"))
  if (nrow(merged) == 0) return(data.frame())
  merged %>%
    transmute(
      var1,
      var2,
      r_jasp,
      r_r = r,
      r_diff = r_r - r_jasp,
      p_jasp,
      p_r = p,
      p_diff = p_r - p_jasp
    )
}

compare_results <- list(ttest = list(), anova = list(), cor = list())
for (name in names(r_outputs)) {
  for (key in names(r_outputs[[name]])) {
    if (grepl(":ttest$", key) && !is.null(jasp_tables[[name]][[key]])) {
      compare_results$ttest[[key]] <- compare_ttests(jasp_tables[[name]][[key]], r_outputs[[name]][[key]])
    }
    if (grepl(":frequentist$", key)) {
      jasp_key <- sub(":frequentist$", ":anova", key)
      if (!is.null(jasp_tables[[name]][[jasp_key]])) {
        compare_results$anova[[key]] <- compare_anova(jasp_tables[[name]][[jasp_key]], r_outputs[[name]][[key]])
      }
    }
    if (grepl(":cor$", key) && !is.null(jasp_tables[[name]][[key]])) {
      compare_results$cor[[key]] <- compare_cor(jasp_tables[[name]][[key]], r_outputs[[name]][[key]])
    }
  }
}

saveRDS(
  list(jasp = jasp_outputs, r = r_outputs, jasp_tables = jasp_tables, compare = compare_results),
  file = file.path(output_dir, "jasp_reproduction_results.rds")
)

write.csv(
  bind_rows(compare_results$ttest, .id = "analysis"),
  file = file.path(output_dir, "compare_ttests.csv"),
  row.names = FALSE
)
write.csv(
  bind_rows(compare_results$anova, .id = "analysis"),
  file = file.path(output_dir, "compare_anova.csv"),
  row.names = FALSE
)
write.csv(
  bind_rows(compare_results$cor, .id = "analysis"),
  file = file.path(output_dir, "compare_cor.csv"),
  row.names = FALSE
)

message("JASP reproduction outputs saved to: ", output_dir)
