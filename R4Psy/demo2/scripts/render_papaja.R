# Render papaja report for confirmatory study demo

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
report_path <- file.path(demo_root, "papaja", "hu2020_confirm_demo.Rmd")
log_dir <- file.path(demo_root, "logs")

dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)

if (!file.exists(report_path)) {
  stop("Report not found: ", report_path)
}

log_file <- file.path(log_dir, paste0("render-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".log"))

sink(log_file, split = TRUE)
print(sessionInfo())
cat("\nRendering report...\n")
rmarkdown::render(report_path, output_dir = file.path(demo_root, "papaja"))
cat("\nDone. Output in demo2/papaja/\n")
sink()

message("Render log: ", log_file)
