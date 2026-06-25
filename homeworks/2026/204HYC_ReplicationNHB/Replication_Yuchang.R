# ==============================================================================
# Step 1: Environment Setup
# ==============================================================================

# [Step 1] 1. Load required packages
# ------------------------------------------------------------------------------

options(repos = c(CRAN = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/"))

if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

library(pacman)

pacman::p_load(
  here, tidyverse, data.table, broom, fixest, estimatr, multcomp, twang,
  nnet, modelsummary, flextable, officer, writexl, showtext, sysfonts, grid
)

here::i_am("Replication_Yuchang.R")

# [Step 1] 2. Clean environment
# ------------------------------------------------------------------------------

rm(list = setdiff(ls(), c("pacman")))
gc()

# [Step 1] 3. Global options
# ------------------------------------------------------------------------------

options(scipen = 999)
global_seed <- 123
set.seed(global_seed)

select <- dplyr::select
summarize <- dplyr::summarize

# [Step 1] 4. Output folders
# ------------------------------------------------------------------------------

output_root_dir <- here::here("02 Output")

script_output_steps <- c(
  "Step 01 - setup",
  "Step 02 - analysis data",
  "Step 03 - main ITT effects",
  "Step 04 - ATT effects",
  "Step 05 - main text figures",
  "Step 06 - core heterogeneity",
  "Step 07 - mechanisms"
)

reset_script_outputs <- function(output_root, step_names) {
  dir.create(output_root, recursive = TRUE, showWarnings = FALSE)
  purrr::walk(file.path(output_root, step_names), function(path) {
    if (dir.exists(path)) unlink(path, recursive = TRUE, force = TRUE)
  })
  unlink(
    file.path(output_root, c("Replication_Yuchang_completion_log.csv", "Replication_Yuchang_completion_log.xlsx")),
    force = TRUE
  )
}

create_step_output <- function(step_name, subdirs = character()) {
  output_step_dir <- file.path(output_root_dir, step_name)
  dir.create(output_step_dir, recursive = TRUE, showWarnings = FALSE)
  purrr::walk(subdirs, function(subdir) {
    dir.create(file.path(output_step_dir, subdir), recursive = TRUE, showWarnings = FALSE)
  })
  output_step_dir
}

reset_script_outputs(output_root_dir, script_output_steps)

step_dirs <- list(
  itt = create_step_output("Step 03 - main ITT effects", c("output data", "output tables")),
  att = create_step_output("Step 04 - ATT effects", c("output data", "output tables")),
  figures = create_step_output("Step 05 - main text figures", c("output data", "output figures"))
)

# [Step 1] 5. Plot style
# ------------------------------------------------------------------------------

setup_plot_fonts <- function() {
  path_arial_reg <- here::here(".Fonts", "arial.ttf")
  path_arial_bold <- here::here(".Fonts", "arialbd.ttf")

  if (!file.exists(path_arial_reg)) {
    path_arial_reg <- here::here(".Fonts", "Arial.ttf")
  }

  if (file.exists(path_arial_reg)) {
    if (file.exists(path_arial_bold)) {
      sysfonts::font_add("Arial", regular = path_arial_reg, bold = path_arial_bold)
    } else {
      sysfonts::font_add("Arial", regular = path_arial_reg)
    }
    showtext::showtext_auto()
  } else {
    warning("Arial not found under .Fonts/. Plot will fall back to default sans.")
  }
}

theme_journal_compact <- function(base_size = 6) {
  ggplot2::theme_classic(base_family = "Arial", base_size = base_size) +
    ggplot2::theme(
      text = ggplot2::element_text(family = "Arial"),
      plot.title = ggplot2::element_text(size = 7, face = "bold", hjust = 0),
      plot.title.position = "plot",
      axis.title = ggplot2::element_text(size = 6, face = "bold"),
      axis.text = ggplot2::element_text(size = 6, face = "bold", color = "black"),
      axis.line = ggplot2::element_line(color = "black", linewidth = 0.3),
      axis.ticks = ggplot2::element_line(color = "black", linewidth = 0.3),
      axis.ticks.length = grid::unit(0.8, "mm"),
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 6, face = "bold"),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(size = 6, face = "bold", hjust = 0),
      panel.grid = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.margin = ggplot2::margin(1, 1, 1, 1, unit = "mm")
    )
}

plot_palette <- c(
  purple_light = "#B096B6",
  purple_dark = "#612A6E",
  green_dark = "#2A6B3E",
  green_light = "#96B5A0",
  gray_light = "#9E9FA3",
  gray_dark = "#3A3C45"
)

setup_plot_fonts()

get_group_palette <- function(group_values) {
  group_values <- unique(as.character(stats::na.omit(group_values)))
  default_palette <- unname(plot_palette[c(
    "green_dark", "purple_dark", "gray_dark",
    "green_light", "purple_light", "gray_light"
  )])
  palette <- stats::setNames(rep(default_palette, length.out = length(group_values)), group_values)

  semantic_palette <- c(
    "Low-SES" = plot_palette[["purple_light"]],
    "High-SES" = plot_palette[["purple_dark"]],
    "Low knowledge" = plot_palette[["purple_light"]],
    "High knowledge" = plot_palette[["purple_dark"]],
    "Low" = plot_palette[["purple_light"]],
    "High" = plot_palette[["purple_dark"]],
    "No" = plot_palette[["green_light"]],
    "Yes" = plot_palette[["green_dark"]],
    "Minority" = plot_palette[["green_light"]],
    "Majority" = plot_palette[["green_dark"]],
    "Never used" = plot_palette[["gray_light"]],
    "Already used" = plot_palette[["gray_dark"]],
    "Inactive" = plot_palette[["gray_light"]],
    "Active" = plot_palette[["gray_dark"]]
  )

  matched_names <- intersect(names(palette), names(semantic_palette))
  palette[matched_names] <- semantic_palette[matched_names]
  palette
}

get_group_shapes <- function(group_values) {
  group_values <- unique(as.character(stats::na.omit(group_values)))
  stats::setNames(rep(c(21, 24, 23, 22, 25, 21), length.out = length(group_values)), group_values)
}

make_attention_action_plot <- function(data) {
  control_data <- data %>%
    filter(Assignment == "Control") %>%
    mutate(
      ECSPlanToBaseline = ifelse(
        as.character(ECSPlanToBaseline) %in% c("TRUE", "True", "Yes", "1"),
        1,
        0
      )
    )

  ses_gap <- fixest::feols(
    c(ECSPlanToBaseline, ECSApp, ECSUseYes) ~ Educ2,
    control_data,
    se = "hetero"
  )

  migration_gap <- fixest::feols(
    c(ECSPlanToBaseline, ECSApp, ECSUseYes) ~ i(MigrationBackground, ref = "No"),
    control_data,
    se = "hetero"
  )

  plot_data <- bind_rows(
    modelsummary::modelplot(ses_gap, draw = FALSE) %>%
      mutate(
        Heterogeneity = "Baseline education",
        Outcome = str_remove(model, "lhs:"),
        term = ifelse(str_detect(term, "Low-SES"), "Gap by SES", "Mean High SES")
      ),
    modelsummary::modelplot(migration_gap, draw = FALSE) %>%
      mutate(
        Heterogeneity = "Migration background",
        Outcome = str_remove(model, "lhs:"),
        term = ifelse(str_detect(term, "Yes"), "Gap by migration background", "Mean French")
      )
  ) %>%
    mutate(
      OutcomeLabel = factor(
        case_when(
          str_detect(Outcome, "ECSPlanToBaseline") ~ "Intend to use",
          str_detect(Outcome, "ECSApp") ~ "Apply",
          str_detect(Outcome, "ECSUseYes") ~ "Access"
        ),
        levels = c("Intend to use", "Apply", "Access")
      ),
      termPlot = factor(term, levels = c("Gap by SES", "Gap by migration background"))
    )

  outcome_palette <- c(
    "Intend to use" = plot_palette[["gray_dark"]],
    "Apply" = plot_palette[["purple_dark"]],
    "Access" = plot_palette[["green_dark"]]
  )

  plot_object <- ggplot(plot_data %>% filter(str_detect(term, "Gap"))) +
    geom_bar(
      aes(x = OutcomeLabel, y = estimate, fill = OutcomeLabel),
      stat = "identity",
      alpha = 0.4
    ) +
    geom_errorbar(
      aes(x = OutcomeLabel, ymin = conf.low, ymax = conf.high, color = OutcomeLabel),
      width = 0.3
    ) +
    facet_wrap(~termPlot) +
    scale_x_discrete(name = "Outcomes") +
    scale_fill_manual(values = outcome_palette) +
    scale_color_manual(values = outcome_palette) +
    labs(y = "Estimate") +
    theme_journal_compact() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(size = 6, face = "bold", color = "black")
    )

  list(data = plot_data, plot = plot_object)
}

save_plot_dual <- function(plot, base_path, width_mm = 170, height_mm = 110, dpi = 600) {
  ggplot2::ggsave(
    paste0(base_path, ".pdf"),
    plot = plot,
    width = width_mm,
    height = height_mm,
    units = "mm",
    device = grDevices::cairo_pdf,
    bg = "white"
  )
  ggplot2::ggsave(
    paste0(base_path, ".png"),
    plot = plot,
    width = width_mm,
    height = height_mm,
    units = "mm",
    dpi = dpi,
    bg = "white"
  )
}

save_flextable_docx <- function(table_object, path, title = "Table") {
  flextable::save_as_docx(values = stats::setNames(list(table_object), title), path = path)
}

clean_sheet_name <- function(sheet_name) {
  sheet_name %>%
    str_replace_all("[\\[\\]\\*\\?/\\\\:]", "_") %>%
    str_sub(1, 31)
}

repair_xlsx_colnames <- function(col_names) {
  col_names <- ifelse(is.na(col_names) | col_names == "", "column", col_names)
  lower_names <- tolower(col_names)
  duplicate_index <- ave(seq_along(col_names), lower_names, FUN = seq_along)
  col_names <- ifelse(duplicate_index == 1, col_names, paste0(col_names, "_", duplicate_index))
  make.unique(col_names, sep = "_")
}

prepare_xlsx_data <- function(data) {
  data <- as.data.frame(data)
  names(data) <- repair_xlsx_colnames(names(data))

  data %>%
    mutate(across(
      everything(),
      ~if (is.list(.x)) purrr::map_chr(.x, ~paste(.x, collapse = "; ")) else .x
    )) %>%
    mutate(across(where(is.factor), as.character))
}

write_xlsx_workbook <- function(sheets, path) {
  sheets <- purrr::compact(sheets)
  sheets <- sheets[purrr::map_int(sheets, nrow) > 0]

  if (length(sheets) == 0) {
    warning("No non-empty sheets to write: ", path)
    return(invisible(NULL))
  }

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  sheets <- purrr::map(sheets, prepare_xlsx_data)
  names(sheets) <- make.unique(clean_sheet_name(names(sheets)), sep = "_")

  writexl::write_xlsx(sheets, path = path)
  invisible(path)
}

# ==============================================================================
# Step 2: Load and Prepare Analysis Data
# ==============================================================================

# [Step 2] 1. Read input data
# ------------------------------------------------------------------------------

BaselineFull <- readr::read_csv(
  here::here("01 Input", "data", "BaselineWithRefusals.csv"),
  show_col_types = FALSE
)

MainDB <- readr::read_csv(
  here::here("01 Input", "data", "MainDatabase.csv"),
  show_col_types = FALSE
) %>%
  mutate(
    NormsBaseline = as.factor(NormsBaseline),
    Dep = as.factor(Dep),
    FmilyEarnLessThan2500 = as.factor(FmilyEarnLessThan2500),
    NumberOfChildren3 = as.factor(NumberOfChildren3),
    NormsOpposedYes = as.factor(NormsOpposedYes),
    DescriptiveNorms = as.factor(ifelse(DescriptiveNorms == "Yes", "Majority", "Minority")),
    UsedECEC = as.factor(ifelse(UsedECEC == "Yes", "Already used", "Never used")),
    InfoBaseline = as.factor(ifelse(
      str_detect(str_to_lower(as.character(LevelInfoSubExPost)), "aucun|bas"),
      "Low knowledge",
      "High knowledge"
    )),
    TrustCreche = ifelse(TrustCreche1or0 == "Yes", "High", "Low"),
    BelieveBenefits = as.factor(ifelse(LikertReturnHK1or0 == "Yes", "Believe in benefits", "Do not believe")),
    MigrationBackground = ifelse(FrenchYNBaseline == "Abroad", "Yes", "No"),
    MigrationBackgroundParent2 = ifelse(
      str_detect(str_to_lower(as.character(BirthPlace2)), "france|fran"),
      "No",
      "Yes"
    ),
    MigrationBackgroundOneOfTheTwo = case_when(
      MigrationBackground == "Yes" | MigrationBackgroundParent2 == "Yes" ~ "Yes",
      TRUE ~ "No"
    ),
    MigrationBackground = as.factor(MigrationBackground),
    MigrationBackgroundBoth = case_when(
      MigrationBackground == "Yes" & is.na(MigrationBackgroundParent2) ~ "Yes",
      MigrationBackground == "Yes" & MigrationBackgroundParent2 == "Yes" ~ "Yes",
      TRUE ~ "No"
    ),
    GenderChild = as.factor(ifelse(BabyFemale == TRUE, "Girl", "Boy"))
  ) %>%
  mutate_if(is.character, as.factor)

# [Step 2] 2. Build response and assignment weights
# ------------------------------------------------------------------------------

PredVars <- c("Assignment", "StrataWave")

char_vars <- lapply(MainDB, class) %in% c("character", "logical")
MainDB[, char_vars] <- lapply(MainDB[, char_vars], as.factor)

DBResponse <- MainDB %>%
  select(Responded, one_of(PredVars)) %>%
  as.data.frame()

ps_fit <- twang::ps(
  Responded ~ .,
  data = DBResponse,
  estimand = "ATE",
  verbose = FALSE
)

DBResponse$weight <- twang::get.weights(ps_fit, stop.method = "es.mean")
DBResponse$PsRX <- ps_fit$ps$es.mean.ATE
MainDB$WeightBalance <- DBResponse$weight

fit_basic <- nnet::multinom(Assignment ~ StrataWave, data = MainDB, trace = FALSE)

MainDB$predT1 <- predict(fit_basic, MainDB, "probs")[, "T1"]
MainDB$predT2 <- predict(fit_basic, MainDB, "probs")[, "T2"]

MainDB <- MainDB %>%
  mutate(
    Z1 = ifelse(Assignment == "T1", 1, 0),
    Z2 = ifelse(Assignment == "T2", 1, 0),
    Z1.c = Z1 - predT1,
    Z2.c = Z2 - predT2
  )

# [Step 2] 3. Stack pairwise comparison samples
# ------------------------------------------------------------------------------

SampleT1C <- MainDB %>%
  filter(Assignment %in% c("Control", "T1")) %>%
  mutate(
    SubSample = "T1-C",
    Z = ifelse(Assignment == "T1", 1, 0),
    Z.c = Z - predT1,
    WeightPS = Z / predT1 + (1 - Z) / (1 - predT1)
  )

SampleT2C <- MainDB %>%
  filter(Assignment %in% c("Control", "T2")) %>%
  mutate(
    SubSample = "T2-C",
    Z = ifelse(Assignment == "T2", 1, 0),
    Z.c = Z - predT2,
    WeightPS = Z / predT2 + (1 - Z) / (1 - predT2)
  )

SampleT2T1 <- MainDB %>%
  filter(Assignment %in% c("T2", "T1")) %>%
  mutate(
    SubSample = "T2-T1",
    Z = ifelse(Assignment == "T2", 1, 0),
    Z.c = Z - predT2,
    WeightPS = Z / predT2 + (1 - Z) / (1 - predT2)
  )

StackedDB <- bind_rows(SampleT1C, SampleT2C, SampleT2T1) %>%
  mutate(
    Treat = paste("Z=", Z, ":", SubSample, sep = ""),
    SubSampleStrata = paste(SubSample, StrataWave, sep = ":")
  )

fit_basicStack <- fixest::feglm(
  Z ~ 1 | SubSampleStrata,
  data = StackedDB,
  family = "probit",
  cluster = ~StrataWave
)

StackedDB$psscore <- predict(fit_basicStack, StackedDB)

StackedDB <- StackedDB %>%
  mutate(
    Z.c = Z - psscore,
    WeightPS = Z / psscore + (1 - Z) / (1 - psscore),
    ZT2C.c = Z.c * as.numeric(SubSample == "T2-C"),
    ZT1C.c = Z.c * as.numeric(SubSample == "T1-C"),
    ZT2T1.c = Z.c * as.numeric(SubSample == "T2-T1")
  )

# [Step 2] 4. Create analysis datasets
# ------------------------------------------------------------------------------

PostDB <- StackedDB %>%
  filter(Responded == 1) %>%
  ungroup()

PostDBT2 <- PostDB %>%
  filter(str_detect(SubSample, "T2")) %>%
  mutate(D = Suivi_administratif1_0) %>%
  ungroup()

PostDBT1C <- PostDB %>%
  filter(SubSample == "T1-C") %>%
  ungroup()

PostDBT2C <- PostDBT2 %>%
  filter(SubSample == "T2-C") %>%
  ungroup()

reg_MainDB <- MainDB %>%
  dplyr::select(
    ResponseId, ECSApp, ECSUseYes, AppCreche, UseCreche, Assignment,
    StrataWave, Educ2, FrenchYNBaseline, HighLowECECBaseline, InfoBaseline,
    Discount501or0, HigherThanMeadianISEIMother, HigherThanMeadianSESIndex,
    HighLowCoverageGlobal, UsedECEC, Responded, ResponseStatus, T1, T2,
    High_SES, NoMigrationBackground, HighCoverageBaseline, High_knowledge,
    HighCoverage_total, PresentOrientated
  ) %>%
  filter(Responded == 1) %>%
  mutate(StrataWave = as.factor(StrataWave))

SourcesStacked <- "stacked database of pairwise comparisons."
SourcesMain <- "endline database."

rm(
  DBResponse, PredVars, fit_basic, fit_basicStack, ps_fit,
  SampleT1C, SampleT2C, SampleT2T1
)
gc()

# ==============================================================================
# Step 3: Analysis Functions
# ==============================================================================

# [Step 3] 1. Main ITT estimates
# ------------------------------------------------------------------------------

ITTSimultaneous <- function(Y = "UseCreche",
                            treat = "Z",
                            DB = PostDB,
                            Correction = "Westfall",
                            weights = "WeightPS") {
  DBInside <- DB
  DBInside$Y <- DBInside[[Y]]
  DBInside$Z <- DBInside[[treat]]
  DBInside$w <- if (weights == "") 1 else DBInside[[weights]]

  model <- fixest::feols(
    Y ~ i(Z, SubSample, ref = 0) | SubSampleStrata,
    DBInside,
    cluster = ~StrataWave,
    weights = ~w
  )

  glht.model <- multcomp::glht(model)
  tidyglht <- left_join(
    broom::tidy(glht.model, test = multcomp::adjusted(type = Correction)),
    broom::tidy(stats::confint(glht.model, adjusted(type = Correction)))
  )

  tidy.final <- broom::tidy(model) %>%
    bind_cols(stats::confint(model)[1], stats::confint(model)[2]) %>%
    rename("point.conf.low" = "2.5 %", "point.conf.high" = "97.5 %") %>%
    left_join(tidyglht, by = c("term" = "contrast", "estimate", "std.error", "statistic")) %>%
    separate(term, c("Z", "term", "Var"), sep = "::") %>%
    mutate(
      term = Var,
      Var = str_remove_all(Var, "MaternityWardBaseline|Educ|IntendUse|Which_wave|HighLowECECBaseline")
    )

  ControlMean <- fixest::feols(
    ZO * Y ~ i(ZO, SubSample, ref = 0) | SubSampleStrata,
    DBInside %>% mutate(ZO = 1 - Z),
    cluster = ~StrataWave
  )

  Glht.ControlMean <- multcomp::glht(ControlMean)
  tidy.Glht.ControlMean <- left_join(
    broom::tidy(Glht.ControlMean),
    broom::tidy(stats::confint(Glht.ControlMean))
  )

  tidy.ControlMean <- broom::tidy(ControlMean) %>%
    bind_cols(stats::confint(ControlMean)[1], stats::confint(ControlMean)[2]) %>%
    rename("point.conf.low" = "2.5 %", "point.conf.high" = "97.5 %") %>%
    left_join(tidy.Glht.ControlMean, by = c("term" = "contrast", "estimate", "std.error")) %>%
    mutate(term = str_remove_all(term, "ZO::1:|\\(|\\)|SubSample::"))

  ChisQTest <- multcomp::glht(model) %>%
    summary(test = multcomp::Chisqtest())

  fullModel <- list(
    tidy = tidy.final %>%
      bind_rows(
        tidy.ControlMean %>%
          filter(str_detect(term, "T2-C")) %>%
          mutate(term = "Control mean") %>%
          mutate_at(vars(adj.p.value, p.value), ~NA)
      ),
    glance = modelsummary::get_gof(model) %>%
      bind_cols("Fixed effects" = "X") %>%
      bind_cols(t(c("Chi 2" = ChisQTest$test$SSH, "P-value" = ChisQTest$test$pvalue)))
  )
  class(fullModel) <- "modelsummary_list"

  list("Estimation" = model, "Tidy" = tidy.final, "Correction" = Correction, "ModelSummary" = fullModel)
}

# [Step 3] 2. ATT estimates
# ------------------------------------------------------------------------------

LATESimultaneous <- function(Y = "UseCreche",
                             DB = PostDBT2,
                             Correction = "Westfall",
                             weights = "WeightPS") {
  DBInside <- DB
  DBInside$Y <- DBInside[[Y]]
  DBInside$w <- if (weights == "") 1 else DBInside[[weights]]

  model <- fixest::feols(
    Y ~ 1 | SubSampleStrata | D:SubSample ~ Z.c:SubSample,
    DBInside,
    cluster = ~StrataWave,
    weights = ~w
  )

  glht.model <- multcomp::glht(model)
  tidyglht <- left_join(
    broom::tidy(glht.model, test = multcomp::adjusted(type = Correction)),
    broom::tidy(stats::confint(glht.model, adjusted(type = Correction)))
  )

  tidy.final <- broom::tidy(model) %>%
    bind_cols(stats::confint(model)[1], stats::confint(model)[2]) %>%
    rename("point.conf.low" = "2.5 %", "point.conf.high" = "97.5 %") %>%
    left_join(tidyglht, by = c("term" = "contrast", "estimate", "std.error", "statistic")) %>%
    separate(term, c("D", "term"), sep = ":") %>%
    mutate(term = str_remove_all(term, "SubSample"))

  Y0Compliers <- fixest::feols(
    D0 * Y ~ 1 | SubSampleStrata | D0:SubSample ~ Z.c:SubSample,
    DBInside %>% mutate(ZO = 1 - Z, D0 = 1 - D),
    cluster = ~StrataWave
  )

  Glht.ControlMean <- multcomp::glht(Y0Compliers)
  tidy.Glht.ControlMean <- left_join(
    broom::tidy(Glht.ControlMean),
    broom::tidy(stats::confint(Glht.ControlMean))
  )

  tidy.ControlMean <- broom::tidy(Y0Compliers) %>%
    bind_cols(stats::confint(Y0Compliers)[1], stats::confint(Y0Compliers)[2]) %>%
    rename("point.conf.low" = "2.5 %", "point.conf.high" = "97.5 %") %>%
    left_join(tidy.Glht.ControlMean, by = c("term" = "contrast", "estimate", "std.error")) %>%
    mutate(term = str_remove_all(term, "ZO::1:|\\(|\\)|SubSample"))

  ChisQTest <- multcomp::glht(model) %>%
    summary(test = multcomp::Chisqtest())

  fullModel <- list(
    tidy = tidy.final %>%
      bind_rows(
        tidy.ControlMean %>%
          filter(str_detect(term, "T2-T1")) %>%
          mutate(term = "Avg. cfct.") %>%
          mutate_at(vars(adj.p.value, p.value), ~NA)
      ),
    glance = modelsummary::get_gof(model) %>%
      bind_cols("Fixed effects" = "X") %>%
      bind_cols(t(c("Chi 2" = ChisQTest$test$SSH, "P-value" = ChisQTest$test$pvalue)))
  )
  class(fullModel) <- "modelsummary_list"

  list("Estimation" = model, "Tidy" = tidy.final, "Correction" = Correction, "ModelSummary" = fullModel)
}

ITTSimNoControl <- function(Y = "UseCreche",
                            treat = "Z",
                            DB = PostDB,
                            Correction = "Westfall",
                            weights = "WeightPS") {
  DBInside <- DB
  DBInside$Y <- DBInside[[Y]]
  DBInside$Z <- DBInside[[treat]]
  DBInside$w <- if (weights == "") 1 else DBInside[[weights]]

  model <- fixest::feols(
    Y ~ i(Z, SubSample, ref = 0) | SubSampleStrata,
    DBInside,
    cluster = ~StrataWave,
    weights = ~w
  )

  glht.model <- multcomp::glht(model)
  tidyglht <- left_join(
    broom::tidy(glht.model, test = multcomp::adjusted(type = Correction)),
    broom::tidy(stats::confint(glht.model, adjusted(type = Correction)))
  )

  tidy.final <- broom::tidy(model) %>%
    bind_cols(stats::confint(model)[1], stats::confint(model)[2]) %>%
    rename("point.conf.low" = "2.5 %", "point.conf.high" = "97.5 %") %>%
    left_join(tidyglht, by = c("term" = "contrast", "estimate", "std.error", "statistic")) %>%
    separate(term, c("Z", "term", "Var"), sep = "::") %>%
    mutate(
      term = Var,
      Var = str_remove_all(Var, "MaternityWardBaseline|Educ|IntendUse|Which_wave|HighLowECECBaseline")
    )

  ChisQTest <- multcomp::glht(model) %>%
    summary(test = multcomp::Chisqtest())

  fullModel <- list(
    tidy = tidy.final,
    glance = modelsummary::get_gof(model) %>%
      bind_cols("Fixed effects" = "X") %>%
      bind_cols(t(c("Chi 2" = ChisQTest$test$SSH, "P-value" = ChisQTest$test$pvalue)))
  )
  class(fullModel) <- "modelsummary_list"

  list("Estimation" = model, "Tidy" = tidy.final, "Correction" = Correction, "ModelSummary" = fullModel)
}

# [Step 3] 3. Heterogeneous ITT and ATT estimates
# ------------------------------------------------------------------------------

GroupHeterogeneityFnCTRL <- function(DB = PostDBT2,
                                     Outcome = "UseCreche",
                                     Heterogeneity = "Educ2",
                                     ITT = TRUE,
                                     Weights = "WeightPS",
                                     clusters = "StrataWave",
                                     Correction = "Westfall") {
  DBInside <- DB
  DBInside$Y <- DBInside[[Outcome]]
  DBInside$w <- DBInside[[Weights]]
  DBInside$Het <- droplevels(as.factor(DBInside[[Heterogeneity]]))
  DBInside$Clust <- DBInside[[clusters]]
  DBInside$SubSample <- droplevels(as.factor(DBInside$SubSample))
  DBInside$StrataWave <- droplevels(as.factor(DBInside$StrataWave))

  DBInside <- DBInside %>%
    filter(
      !is.na(Y),
      !is.na(w),
      !is.na(Het),
      !is.na(Clust),
      !is.na(SubSample),
      !is.na(StrataWave),
      !is.na(Z),
      !is.na(Z.c),
      !is.na(psscore)
    )

  if (!ITT) {
    DBInside <- DBInside %>% filter(!is.na(D))
  }

  if (n_distinct(DBInside$Het) < 2) {
    stop(
      paste0(
        "Heterogeneity variable `", Heterogeneity,
        "` has fewer than two observed levels after filtering."
      )
    )
  }

  InteractFE <- Heterogeneity %in% c("HighLowECEC", "Educ2", "IntendUse", "Which_wave")

  if (InteractFE && ITT) {
    model <- fixest::feols(Y ~ Z.c:SubSample:Het | StrataWave^SubSample, DBInside, cluster = ~Clust, weights = ~w)
    model0 <- fixest::feols(
      Y ~ Z.c:SubSample:Het | StrataWave^SubSample,
      DBInside %>% mutate(Y = (1 - Z) * Y, Z.c = psscore - Z),
      cluster = ~Clust,
      weights = ~w
    )
  }

  if (InteractFE && !ITT) {
    model <- fixest::feols(
      Y ~ 1 | StrataWave^SubSample | D:SubSample:Het ~ Z.c:SubSample:Het,
      DBInside,
      cluster = ~Clust,
      weights = ~w
    )
    model0 <- fixest::feols(
      Y ~ 1 | StrataWave^SubSample | D:SubSample:Het ~ Z.c:SubSample:Het,
      DBInside %>% mutate(Y = (1 - D) * Y, D = 1 - D, Z.c = psscore - Z),
      cluster = ~Clust,
      weights = ~w
    )
  }

  if (!InteractFE && ITT) {
    model <- fixest::feols(Y ~ Z.c:SubSample:Het | Het^StrataWave^SubSample, DBInside, cluster = ~Clust, weights = ~w)
    model0 <- fixest::feols(
      Y ~ Z.c:SubSample:Het | Het^StrataWave^SubSample,
      DBInside %>% mutate(Y = (1 - Z) * Y, Z.c = psscore - Z),
      cluster = ~Clust,
      weights = ~w
    )
  }

  if (!InteractFE && !ITT) {
    model <- fixest::feols(
      Y ~ 1 | Het^StrataWave^SubSample | D:SubSample:Het ~ Z.c:SubSample:Het,
      DBInside,
      cluster = ~Clust,
      weights = ~w
    )
    model0 <- fixest::feols(
      Y ~ 1 | Het^StrataWave^SubSample | D:SubSample:Het ~ Z.c:SubSample:Het,
      DBInside %>% mutate(Y = (1 - D) * Y, D = 1 - D, Z.c = psscore - Z),
      cluster = ~Clust,
      weights = ~w
    )
  }

  Fstats <- NULL
  if (!ITT) {
    Fstats <- fixest::fitstat(model, "ivf1") %>%
      unlist()
    Fstats <- Fstats[str_detect(names(Fstats), "stat")]
    names(Fstats) <- names(Fstats) %>%
      str_replace_all("ivf1::PhasesReg", "F-stat ") %>%
      str_remove_all(":D1.stat")
    Fstats <- mean(Fstats)
    names(Fstats) <- "Mean F-stat 1st stage"
  }

  M0 <- modelsummary::modelplot(model, draw = FALSE) %>%
    separate(term, into = c("Treat", "term", "Group"), sep = ":") %>%
    mutate(
      Model = ifelse(str_detect(Treat, "fit"), "TSLS", "OLS"),
      term = str_remove(term, "SubSample"),
      Group = str_remove(Group, "Het")
    ) %>%
    rename("point.conf.low" = "conf.low", "point.conf.high" = "conf.high")

  HetVal <- unique(DBInside$Het) %>%
    as.character()

  GLH <- c()
  for (value in HetVal) {
    temp <- multcomp::glht(
      model,
      paste(paste("`", names(model$coefficients)[str_detect(names(model$coefficients), value)], "`= 0", sep = ""))
    )
    GLH <- bind_rows(
      GLH,
      left_join(
        broom::tidy(summary(temp, adjusted(type = Correction))),
        broom::tidy(stats::confint(temp))
      ) %>%
        mutate(Het = value)
    )
  }

  GLH <- GLH %>%
    separate(contrast, into = c("Treat", "term", "Group"), sep = ":") %>%
    mutate(
      Model = ifelse(str_detect(Treat, "fit"), "TSLS", "OLS"),
      term = str_remove(term, "SubSample"),
      Group = str_remove(Group, "Het")
    ) %>%
    left_join(M0 %>% select(-c(estimate, std.error)))

  M0.ctrl <- modelsummary::modelplot(model0, draw = FALSE) %>%
    separate(term, into = c("Treat", "term", "Group"), sep = ":") %>%
    mutate(
      Model = ifelse(str_detect(Treat, "fit"), "TSLS", "OLS"),
      term = str_remove(term, "SubSample"),
      Group = str_remove(Group, "Het")
    ) %>%
    rename("point.conf.low" = "conf.low", "point.conf.high" = "conf.high")

  GLH0 <- c()
  for (value in HetVal) {
    temp <- multcomp::glht(
      model0,
      paste(paste("`", names(model0$coefficients)[str_detect(names(model0$coefficients), value)], "`= 0", sep = ""))
    )
    GLH0 <- bind_rows(
      GLH0,
      left_join(
        broom::tidy(summary(temp, adjusted(type = Correction))),
        broom::tidy(stats::confint(temp))
      ) %>%
        mutate(Het = value)
    )
  }

  GLH0 <- GLH0 %>%
    separate(contrast, into = c("Treat", "term", "Group"), sep = ":") %>%
    mutate(
      Model = ifelse(str_detect(Treat, "fit"), "TSLS", "OLS"),
      term = str_remove(term, "SubSample"),
      Group = str_remove(Group, "Het")
    ) %>%
    left_join(M0.ctrl %>% select(-c(estimate, std.error)))

  Gof <- modelsummary::get_gof(model) %>%
    bind_cols("Fixed effects" = "X")

  Gof0 <- modelsummary::get_gof(model0) %>%
    bind_cols("Fixed effects" = "X")

  if (!ITT) {
    Gof <- Gof %>% bind_cols(as.data.frame(t(Fstats)))
    Gof0 <- Gof0 %>% bind_cols(as.data.frame(t(Fstats)))
  }

  ForModelSummary <- list(tidy = GLH, glance = Gof)
  ForModelSummary0 <- list(tidy = GLH0, glance = Gof0)
  class(ForModelSummary) <- "modelsummary_list"
  class(ForModelSummary0) <- "modelsummary_list"

  list(
    "Estimation" = model,
    "Tidy" = GLH,
    "ModelSummary" = ForModelSummary,
    "Model 0" = model0,
    "ModelSummary0" = ForModelSummary0
  )
}

# [Step 3] 4. Output helpers
# ------------------------------------------------------------------------------

collect_modelsummary_tables <- function(results_list) {
  estimates <- purrr::imap_dfr(
    results_list,
    ~.x$ModelSummary$tidy %>%
      mutate(result = .y, .before = 1)
  )

  model_fit <- purrr::imap_dfr(
    results_list,
    ~.x$ModelSummary$glance %>%
      as_tibble() %>%
      mutate(result = .y, .before = 1)
  )

  list(estimates = estimates, model_fit = model_fit)
}

save_modelsummary_xlsx <- function(results_list, path) {
  output_tables <- collect_modelsummary_tables(results_list)
  write_xlsx_workbook(output_tables, path)
  output_tables$estimates
}

build_heterogeneity_data <- function(outcomes,
                                     heterogeneity_specs,
                                     term_level = "T2-C") {
  plot_data <- list()

  for (outcome_item in outcomes) {
    for (het_item in heterogeneity_specs) {
      db_itt <- het_item$data_fun(PostDB)
      db_att <- het_item$data_fun(PostDBT2)

      result_key <- paste(outcome_item$key, het_item$key, sep = "__")

      db_itt <- db_itt %>%
        filter(!is.na(.data[[outcome_item$var]]), !is.na(.data[[het_item$var]]))
      db_att <- db_att %>%
        filter(!is.na(.data[[outcome_item$var]]), !is.na(.data[[het_item$var]]))

      db_itt[[het_item$var]] <- droplevels(as.factor(db_itt[[het_item$var]]))
      db_att[[het_item$var]] <- droplevels(as.factor(db_att[[het_item$var]]))

      if (n_distinct(db_itt[[het_item$var]]) < 2 || n_distinct(db_att[[het_item$var]]) < 2) {
        warning(
          paste0(
            "Skipped `", result_key,
            "` because the heterogeneity variable has fewer than two observed levels."
          )
        )
        next
      }

      result_itt <- GroupHeterogeneityFnCTRL(
        DB = db_itt,
        Outcome = outcome_item$var,
        Heterogeneity = het_item$var,
        ITT = TRUE,
        Weights = "WeightPS",
        clusters = "StrataWave"
      )

      result_att <- GroupHeterogeneityFnCTRL(
        DB = db_att,
        Outcome = outcome_item$var,
        Heterogeneity = het_item$var,
        ITT = FALSE,
        Weights = "WeightPS",
        clusters = "StrataWave"
      )

      plot_data[[paste(result_key, "control", sep = "__")]] <- result_itt$ModelSummary0$tidy %>%
        mutate(
          Y = outcome_item$label,
          panel = "Control group",
          Heterogeneity = het_item$label,
          Type = "Control"
        ) %>%
        filter(term %in% term_level)

      plot_data[[paste(result_key, "itt", sep = "__")]] <- result_itt$Tidy %>%
        mutate(
          Y = outcome_item$label,
          panel = "ITT",
          Heterogeneity = het_item$label,
          Type = "ITT"
        ) %>%
        filter(term %in% term_level)

      plot_data[[paste(result_key, "att", sep = "__")]] <- result_att$Tidy %>%
        mutate(
          Y = outcome_item$label,
          panel = "ATT",
          Heterogeneity = het_item$label,
          Type = "ATT"
        ) %>%
        filter(term %in% term_level)
    }
  }

  bind_rows(plot_data) %>%
    mutate(
      term = factor(term, levels = term_level),
      Heterogeneity = factor(Heterogeneity, levels = purrr::map_chr(heterogeneity_specs, "label")),
      panel = factor(panel, levels = c("Control group", "ITT", "ATT"))
    )
}

plot_heterogeneity_estimates <- function(plot_data, title) {
  if (nrow(plot_data) == 0) {
    stop("No rows available for plotting. Check the heterogeneity result table.")
  }

  plot_data <- plot_data %>%
    mutate(
      Het = factor(Het, levels = unique(Het)),
      Group = factor(Group, levels = unique(Group))
    )

  group_palette <- get_group_palette(plot_data$Group)
  group_shapes <- get_group_shapes(plot_data$Group)

  ggplot(plot_data) +
    geom_pointrange(
      aes(
        x = interaction(Het, Heterogeneity, sep = "!"),
        y = estimate,
        ymin = point.conf.low,
        ymax = point.conf.high,
        shape = Group,
        color = Group
      ),
      position = position_dodge(0.4),
      linewidth = 0.28
    ) +
    geom_crossbar(
      aes(
        x = interaction(Het, Heterogeneity, sep = "!"),
        y = estimate,
        ymin = conf.low,
        ymax = conf.high,
        fill = Group,
        color = Group
      ),
      position = position_dodge(0.6),
      alpha = 0.2,
      width = 0.4,
      linewidth = 0.25,
      middle.linewidth = 0.5
    ) +
    facet_grid(rows = vars(fct_rev(Y)), cols = vars(panel), scales = "free_x") +
    coord_flip() +
    geom_hline(
      data = plot_data %>% filter(panel != "Control group"),
      aes(yintercept = 0),
      linetype = 2,
      linewidth = 0.25,
      color = "grey55"
    ) +
    xlab("") +
    scale_x_discrete(labels = function(x) stringr::str_replace_all(x, "!", "\n")) +
    scale_fill_manual("Heterogeneity", values = group_palette) +
    scale_color_manual("Heterogeneity", values = group_palette) +
    scale_shape_manual("Heterogeneity", values = group_shapes) +
    labs(y = "Estimate") +
    theme_journal_compact() +
    theme(
      axis.text.x = element_text(size = 6, face = "bold", color = "black"),
      axis.text.y = element_text(size = 6, color = "black"),
      legend.key.width = grid::unit(3, "mm"),
      legend.key.height = grid::unit(3, "mm")
    )
}

# ==============================================================================
# Step 4: Main ITT Effects
# ==============================================================================

# [Step 4] 1. Estimate ITT effects
# ------------------------------------------------------------------------------

set.seed(999)

itt_results <- list(
  early_childcare_application = ITTSimultaneous(Y = "ECSApp", DB = PostDB),
  daycare_application = ITTSimultaneous(Y = "AppCreche", DB = PostDB),
  early_childcare_access = ITTSimultaneous(Y = "ECSUseYes", DB = PostDB),
  daycare_access = ITTSimultaneous(Y = "UseCreche", DB = PostDB)
)

itt_tidy <- save_modelsummary_xlsx(
  itt_results,
  file.path(step_dirs$itt, "output data", "main_itt_effects.xlsx")
)

# [Step 4] 2. Export ITT table
# ------------------------------------------------------------------------------

cm_itt <- c(
  "T1-C" = "Information-only vs Control",
  "T2-C" = "Information + Support vs Control",
  "T2-T1" = "Information + Support vs Information-only",
  "Control mean" = "Mean control group"
)

main_itt_table <- modelsummary::modelsummary(
  list(
    "Application_Early childcare" = itt_results$early_childcare_application$ModelSummary,
    "Application_Daycare" = itt_results$daycare_application$ModelSummary,
    "Access_Early childcare" = itt_results$early_childcare_access$ModelSummary,
    "Access_Daycare" = itt_results$daycare_access$ModelSummary
  ),
  coef_map = cm_itt,
  fmt = modelsummary::fmt_statistic(
    estimate = 2,
    adj.p.value = 3,
    std.error = 2,
    conf.int = 2,
    "Chi 2" = 2,
    "P-value" = 3
  ),
  estimate = "{estimate}{stars} ({std.error})",
  statistic = c("conf.int", "adj.p.val. = {adj.p.value}"),
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  gof_map = c("Covariates", "Fixed effects", "Chi 2", "P-value", "nobs", "r.squared", "adj.r.squared"),
  title = "Intention-to-treat effects on the main outcomes",
  output = "flextable"
) %>%
  flextable::separate_header(split = "_", opts = c("center-hspan")) %>%
  flextable::fontsize(size = 9, part = "footer") %>%
  flextable::fontsize(size = 10, part = "body") %>%
  flextable::align(part = "header", align = "center") %>%
  flextable::align(part = "body", align = "center") %>%
  flextable::autofit()

save_flextable_docx(
  main_itt_table,
  file.path(step_dirs$itt, "output tables", "main_itt_table.docx"),
  "Main ITT effects"
)

# ==============================================================================
# Step 5: ATT Effects
# ==============================================================================

# [Step 5] 1. Estimate ATT effects
# ------------------------------------------------------------------------------

att_results <- list(
  first_stage = ITTSimNoControl(Y = "D", DB = PostDBT2),
  early_childcare_application = LATESimultaneous(Y = "ECSApp", DB = PostDBT2),
  daycare_application = LATESimultaneous(Y = "AppCreche", DB = PostDBT2),
  early_childcare_access = LATESimultaneous(Y = "ECSUseYes", DB = PostDBT2),
  daycare_access = LATESimultaneous(Y = "UseCreche", DB = PostDBT2)
)

att_tidy <- save_modelsummary_xlsx(
  att_results,
  file.path(step_dirs$att, "output data", "att_effects.xlsx")
)

# [Step 5] 2. Export ATT table
# ------------------------------------------------------------------------------

cm_att <- c(
  "T2-C" = "Information + Support vs Control",
  "T2-T1" = "Information + Support vs Information-only",
  "Avg. cfct." = "Average counterfactual"
)

att_table <- modelsummary::modelsummary(
  list(
    "_First Stage" = att_results$first_stage$ModelSummary,
    "Application_Early childcare" = att_results$early_childcare_application$ModelSummary,
    "Application_Daycare" = att_results$daycare_application$ModelSummary,
    "Access_Early childcare" = att_results$early_childcare_access$ModelSummary,
    "Access_Daycare" = att_results$daycare_access$ModelSummary
  ),
  coef_map = cm_att,
  fmt = modelsummary::fmt_statistic(
    estimate = 2,
    adj.p.value = 3,
    std.error = 2,
    conf.int = 2,
    "Chi 2" = 2,
    "P-value" = 3
  ),
  estimate = "{estimate}{stars} ({std.error})",
  statistic = c("conf.int", "adj.p.val. = {adj.p.value}"),
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  gof_map = c("Covariates", "Fixed effects", "Chi 2", "P-value", "nobs", "r.squared", "adj.r.squared"),
  title = "Average treatment effect on the treated on the main outcomes",
  output = "flextable"
) %>%
  flextable::separate_header(split = "_", opts = c("center-hspan")) %>%
  flextable::fontsize(size = 9, part = "footer") %>%
  flextable::fontsize(size = 10, part = "body") %>%
  flextable::align(part = "header", align = "center") %>%
  flextable::align(part = "body", align = "center") %>%
  flextable::autofit()

save_flextable_docx(
  att_table,
  file.path(step_dirs$att, "output tables", "att_table.docx"),
  "ATT effects"
)

# ==============================================================================
# Step 5: Main-Text Attention-to-Action Figure
# ==============================================================================

# [Step 5] 1. Estimate and plot control-group intention gaps
# ------------------------------------------------------------------------------

main_text_figure_tables <- list()

attention_action_outputs <- make_attention_action_plot(MainDB)

main_text_figure_tables$figure_2_attention_action <- attention_action_outputs$data

save_plot_dual(
  attention_action_outputs$plot,
  file.path(step_dirs$figures, "output figures", "figure_2_attention_action"),
  width_mm = 160,
  height_mm = 80
)

# ==============================================================================
# Step 6: Core Heterogeneity
# ==============================================================================

# [Step 6] 1. Early childcare heterogeneity by SES and migration
# ------------------------------------------------------------------------------

core_outcomes <- list(
  list(key = "apply_early_childcare", var = "ECSApp", label = "Apply for early childcare"),
  list(key = "access_early_childcare", var = "ECSUseYes", label = "Access early childcare")
)

ses_migration_specs <- list(
  list(key = "ses", var = "Educ2", label = "SES", data_fun = identity),
  list(key = "migration", var = "MigrationBackground", label = "Migration background", data_fun = identity)
)

early_childcare_heterogeneity <- build_heterogeneity_data(
  outcomes = core_outcomes,
  heterogeneity_specs = ses_migration_specs
)

main_text_figure_tables$figure_3_early_childcare_heterogeneity <- early_childcare_heterogeneity

early_childcare_heterogeneity_plot <- plot_heterogeneity_estimates(
  early_childcare_heterogeneity,
  "Heterogeneous effects on early childcare application and access"
)

save_plot_dual(
  early_childcare_heterogeneity_plot,
  file.path(step_dirs$figures, "output figures", "figure_3_early_childcare_heterogeneity"),
  width_mm = 180,
  height_mm = 105
)

# [Step 6] 2. Daycare heterogeneity by SES and migration
# ------------------------------------------------------------------------------

daycare_outcomes <- list(
  list(key = "apply_daycare", var = "AppCreche", label = "Apply for daycare"),
  list(key = "access_daycare", var = "UseCreche", label = "Access daycare")
)

daycare_heterogeneity <- build_heterogeneity_data(
  outcomes = daycare_outcomes,
  heterogeneity_specs = ses_migration_specs
)

main_text_figure_tables$figure_4_daycare_heterogeneity <- daycare_heterogeneity

daycare_heterogeneity_plot <- plot_heterogeneity_estimates(
  daycare_heterogeneity,
  "Heterogeneous effects on daycare application and access"
)

save_plot_dual(
  daycare_heterogeneity_plot,
  file.path(step_dirs$figures, "output figures", "figure_4_daycare_heterogeneity"),
  width_mm = 180,
  height_mm = 105
)

# ==============================================================================
# Step 7: Mechanism Figures
# ==============================================================================

# [Step 7] 1. Information-friction mechanisms
# ------------------------------------------------------------------------------

information_specs <- list(
  list(key = "knowledge", var = "InfoBaseline", label = "Knowledge", data_fun = identity),
  list(key = "prior_use", var = "UsedECEC", label = "Prior early childcare use", data_fun = identity),
  list(key = "descriptive_norms", var = "DescriptiveNorms", label = "Descriptive norms", data_fun = identity)
)

information_mechanisms <- build_heterogeneity_data(
  outcomes = core_outcomes,
  heterogeneity_specs = information_specs
)

main_text_figure_tables$figure_5_information_mechanisms <- information_mechanisms

information_mechanisms_plot <- plot_heterogeneity_estimates(
  information_mechanisms,
  "Information-friction mechanisms"
)

save_plot_dual(
  information_mechanisms_plot,
  file.path(step_dirs$figures, "output figures", "figure_5_information_mechanisms"),
  width_mm = 190,
  height_mm = 120
)

# [Step 7] 2. Psychological-cost mechanisms
# ------------------------------------------------------------------------------

present_orientation_data <- function(data) {
  data %>%
    mutate(PresentOrientated = ifelse(
      as.character(PresentOrientated) %in% c("TRUE", "True", "Yes", "1"),
      "Yes",
      "No"
    ))
}

active_data <- function(data) {
  data %>%
    mutate(ActiveBaseline = ifelse(
      as.character(ActiveBaseline) %in% c("TRUE", "True", "Yes", "1", "Active"),
      "Active",
      "Inactive"
    ))
}

psychological_specs <- list(
  list(key = "present_orientation", var = "PresentOrientated", label = "Present orientation", data_fun = present_orientation_data),
  list(key = "trust", var = "TrustCreche", label = "Trust", data_fun = identity),
  list(key = "activity", var = "ActiveBaseline", label = "Activity", data_fun = active_data)
)

psychological_mechanisms <- build_heterogeneity_data(
  outcomes = core_outcomes,
  heterogeneity_specs = psychological_specs
)

main_text_figure_tables$figure_6_psychological_mechanisms <- psychological_mechanisms

psychological_mechanisms_plot <- plot_heterogeneity_estimates(
  psychological_mechanisms,
  "Psychological-cost mechanisms"
)

save_plot_dual(
  psychological_mechanisms_plot,
  file.path(step_dirs$figures, "output figures", "figure_6_psychological_mechanisms"),
  width_mm = 190,
  height_mm = 120
)

write_xlsx_workbook(
  main_text_figure_tables,
  file.path(step_dirs$figures, "output data", "main_text_figure_estimates.xlsx")
)

# ==============================================================================
# Step 8: Completion Log
# ==============================================================================

completion_log <- tibble::tibble(
  completed_at = as.character(Sys.time()),
  input_data_dir = here::here("01 Input", "data"),
  output_dir = output_root_dir,
  spatial_analysis_skipped = TRUE,
  supplementary_analysis_skipped = TRUE
)

write_xlsx_workbook(
  list(completion_log = completion_log),
  file.path(output_root_dir, "Replication_Yuchang_completion_log.xlsx")
)
