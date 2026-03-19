# Confirmatory study preprocessing wrapper
# Runs the upstream preprocessing script and copies key outputs into demo2/data/processed

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
upstream_dir <- file.path(demo_root, "external", "moralSelf_ddm", "2_confirm_study", "Results", "1_preproc")
output_dir <- file.path(demo_root, "data", "processed")

if (!dir.exists(upstream_dir)) {
  stop("Upstream confirm study folder not found: ", upstream_dir)
}

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

old_wd <- getwd()
setwd(upstream_dir)
source_ok <- TRUE
tryCatch(
  source("MS_rep_preproc.r"),
  error = function(e) {
    source_ok <<- FALSE
    message("Upstream preprocessing script failed in this environment: ", e$message)
    message("Falling back to copying existing upstream outputs.")
  }
)
setwd(old_wd)

expected_outputs <- c(
  "MS_match_behav_wide.csv",
  "MS_categ_behav_wide.csv",
  "MS_categ_behav_noTask_wide.csv",
  "MS_categ__rt_acc_long.csv",
  "MS_categ__rt_acc_noTask_long.csv",
  "MS_cross_taskeffect_wide.csv",
  "MS_match__rt_acc_long.csv",
  "MS_match__dprime_long.csv"
)

trad_dir <- file.path(demo_root, "external", "moralSelf_ddm", "2_confirm_study", "Results", "2_trad_analysis")
for (fname in expected_outputs) {
  src <- file.path(trad_dir, fname)
  if (file.exists(src)) {
    file.copy(src, file.path(output_dir, fname), overwrite = TRUE)
  }
}

if (source_ok) {
  message("Preprocessing complete. Outputs copied to: ", output_dir)
} else {
  message("Fallback copy complete. Outputs copied to: ", output_dir)
}
