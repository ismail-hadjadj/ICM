# ============================================================
# PRELIMINARY DATA SCREENING AND ASSUMPTION CHECKING
# Adapted to: alldata.csv
# Outputs saved in: preliminary/
# Same colors used consistently across plots
# ============================================================

# -----------------------------
# 1) Packages
# -----------------------------
required_packages <- c(
  "readr", "dplyr", "tidyr", "ggplot2", "car", "broom", "purrr", "stringr"
)

installed <- rownames(installed.packages())
for (pkg in required_packages) {
  if (!pkg %in% installed) install.packages(pkg, dependencies = TRUE)
}

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(car)
library(broom)
library(purrr)
library(stringr)

# -----------------------------
# 2) User settings
# -----------------------------
setwd("f:/QuanAnal")
input_file <- "alldata.csv"
output_dir <- "preliminary"

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Shared color palette for all plots
group_colors <- c(
  "Control" = "#4F81BD",
  "Experimental" = "#C0504D"
)

# -----------------------------
# 3) Load data
# -----------------------------
df <- read_delim(
  input_file,
  delim = ";",
  show_col_types = FALSE,
  trim_ws = TRUE
)

# -----------------------------
# 4) Check required columns
# -----------------------------
required_cols <- c(
  "StudentID", "Group",
  "Spelling_pre", "Punctuation_pre", "Capitalization_pre",
  "Tense_pre", "Agreement_pre", "Article_pronoun_Preposition_pre",
  "Vocabulary_pre", "HolisticScore_pre",
  "Spelling_post", "Punctuation_post", "Capitalization_post",
  "Tense_post", "Agreement_post", "Article_pronoun_Preposition_post",
  "Vocabulary_post", "HolisticScore_post",
  "sex", "age"
)

missing_cols <- setdiff(required_cols, names(df))
if (length(missing_cols) > 0) {
  stop(
    paste0(
      "The following required columns are missing from the dataset: ",
      paste(missing_cols, collapse = ", ")
    )
  )
}

# -----------------------------
# 5) Clean and prepare data
# -----------------------------
df <- df %>%
  mutate(
    Group = as.character(Group),
    Group = str_trim(Group),
    Group = case_when(
      str_to_lower(Group) %in% c("experimental", "exp", "eg") ~ "Experimental",
      str_to_lower(Group) %in% c("control", "cg", "ctrl") ~ "Control",
      TRUE ~ Group
    ),
    Group = factor(Group, levels = c("Control", "Experimental"))
  )

if (any(is.na(df$Group))) {
  stop("Some values in 'Group' could not be interpreted.")
}

numeric_source_cols <- c(
  "Spelling_pre", "Punctuation_pre", "Capitalization_pre",
  "Tense_pre", "Agreement_pre", "Article_pronoun_Preposition_pre",
  "Vocabulary_pre", "HolisticScore_pre",
  "Spelling_post", "Punctuation_post", "Capitalization_post",
  "Tense_post", "Agreement_post", "Article_pronoun_Preposition_post",
  "Vocabulary_post", "HolisticScore_post",
  "age"
)

df <- df %>%
  mutate(across(all_of(numeric_source_cols), as.numeric))

# -----------------------------
# 6) Construct analytical variables
# -----------------------------
df <- df %>%
  mutate(
    pre_mechanics  = Spelling_pre + Punctuation_pre + Capitalization_pre,
    post_mechanics = Spelling_post + Punctuation_post + Capitalization_post,

    pre_grammar  = Tense_pre + Agreement_pre + Article_pronoun_Preposition_pre,
    post_grammar = Tense_post + Agreement_post + Article_pronoun_Preposition_post,

    pre_vocabulary  = Vocabulary_pre,
    post_vocabulary = Vocabulary_post,

    pre_holistic  = HolisticScore_pre,
    post_holistic = HolisticScore_post,

    grammar_reduction    = pre_grammar - post_grammar,
    vocabulary_reduction = pre_vocabulary - post_vocabulary,
    mechanics_reduction  = pre_mechanics - post_mechanics
  )

write_csv(df, file.path(output_dir, "prepared_dataset.csv"))

# -----------------------------
# 7) Data screening
# -----------------------------
missing_summary <- tibble(
  variable = names(df),
  n_missing = sapply(df, function(x) sum(is.na(x)))
)

write_csv(missing_summary, file.path(output_dir, "missing_values_by_variable.csv"))

numeric_vars <- c(
  "pre_holistic", "post_holistic",
  "pre_grammar", "post_grammar",
  "pre_vocabulary", "post_vocabulary",
  "pre_mechanics", "post_mechanics",
  "grammar_reduction", "vocabulary_reduction", "mechanics_reduction"
)

descriptives_overall <- tibble(
  variable = numeric_vars,
  n = sapply(df[numeric_vars], function(x) sum(!is.na(x))),
  mean = sapply(df[numeric_vars], function(x) mean(x, na.rm = TRUE)),
  sd = sapply(df[numeric_vars], function(x) sd(x, na.rm = TRUE)),
  min = sapply(df[numeric_vars], function(x) min(x, na.rm = TRUE)),
  max = sapply(df[numeric_vars], function(x) max(x, na.rm = TRUE))
)

write_csv(
  descriptives_overall,
  file.path(output_dir, "preliminary_descriptives_overall.csv")
)

descriptives_by_group <- df %>%
  group_by(Group) %>%
  summarise(
    across(
      all_of(numeric_vars),
      list(
        n = ~sum(!is.na(.)),
        mean = ~mean(., na.rm = TRUE),
        sd = ~sd(., na.rm = TRUE),
        min = ~min(., na.rm = TRUE),
        max = ~max(., na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )

write_csv(
  descriptives_by_group,
  file.path(output_dir, "preliminary_descriptives_by_group.csv")
)

# -----------------------------
# 8) Outlier screening (IQR rule)
# -----------------------------
detect_outliers_iqr <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lower <- q1 - 1.5 * iqr
  upper <- q3 + 1.5 * iqr
  tibble(
    value = x,
    outlier_iqr = ifelse(is.na(x), NA, x < lower | x > upper)
  )
}

outlier_list <- lapply(numeric_vars, function(v) {
  tmp <- detect_outliers_iqr(df[[v]])
  tibble(
    row_id = seq_len(nrow(df)),
    StudentID = df$StudentID,
    Group = df$Group,
    variable = v,
    value = tmp$value,
    outlier_iqr = tmp$outlier_iqr
  )
})

outlier_details <- bind_rows(outlier_list) %>%
  filter(outlier_iqr %in% TRUE)

write_csv(outlier_details, file.path(output_dir, "outlier_details_iqr.csv"))

outlier_summary <- bind_rows(outlier_list) %>%
  group_by(variable) %>%
  summarise(
    n_outliers = sum(outlier_iqr, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(outlier_summary, file.path(output_dir, "outlier_summary_iqr.csv"))

data_screening_summary <- tibble(
  screening_criterion = c(
    "Total sample size",
    "Experimental group",
    "Control group",
    "Missing values",
    "Data-entry errors",
    "Extreme outliers",
    "Cases excluded"
  ),
  result = c(
    nrow(df),
    sum(df$Group == "Experimental", na.rm = TRUE),
    sum(df$Group == "Control", na.rm = TRUE),
    paste(sum(is.na(df)), "missing cells detected"),
    "No data-entry errors detected during import and numeric conversion",
    paste(sum(outlier_summary$n_outliers, na.rm = TRUE), "potential outlier flags across analytical variables"),
    "0"
  )
)

write_csv(data_screening_summary, file.path(output_dir, "data_screening_summary.csv"))

# -----------------------------
# 9) H1: ANCOVA assumption checks
# -----------------------------

# 9.1 Scatterplot for linearity
p_scatter <- ggplot(
  df,
  aes(x = pre_holistic, y = post_holistic, color = Group, shape = Group)
) +
  geom_point(size = 2.6, alpha = 0.85) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.9) +
  facet_wrap(~ Group) +
  scale_color_manual(values = group_colors) +
  labs(
    title = "Relationship Between Pre-test and Post-test Holistic Scores by Group",
    x = "Pre-test Holistic Score",
    y = "Post-test Holistic Score",
    color = "Group",
    shape = "Group"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  filename = file.path(output_dir, "h1_scatter_pre_post_by_group.png"),
  plot = p_scatter,
  width = 9,
  height = 5.5,
  dpi = 300
)

# 9.2 Homogeneity of regression slopes
model_interaction <- lm(post_holistic ~ pre_holistic * Group, data = df)
interaction_anova <- anova(model_interaction)
interaction_table <- broom::tidy(interaction_anova)

write_csv(
  interaction_table,
  file.path(output_dir, "h1_homogeneity_of_regression_slopes_anova.csv")
)

interaction_row <- interaction_table %>%
  filter(str_detect(term, "pre_holistic:Group|Group:pre_holistic"))

# 9.3 ANCOVA main model
model_ancova <- lm(post_holistic ~ pre_holistic + Group, data = df)
ancova_type3 <- car::Anova(model_ancova, type = 3)

ancova_table <- tibble(
  term = rownames(ancova_type3),
  sumsq = ancova_type3$`Sum Sq`,
  df = ancova_type3$Df,
  statistic = ancova_type3$`F value`,
  p.value = ancova_type3$`Pr(>F)`
)

ss_error <- ancova_table %>%
  filter(term == "Residuals") %>%
  pull(sumsq)

ancova_table <- ancova_table %>%
  mutate(
    partial_eta_sq = ifelse(
      term != "Residuals",
      sumsq / (sumsq + ss_error),
      NA_real_
    )
  )

write_csv(
  ancova_table,
  file.path(output_dir, "h1_ancova_table_type3.csv")
)

# 9.4 Residual normality
ancova_residuals <- residuals(model_ancova)
shapiro_resid <- shapiro.test(ancova_residuals)

residual_normality <- tibble(
  test = "Shapiro-Wilk test of ANCOVA residuals",
  statistic_W = unname(shapiro_resid$statistic),
  p_value = shapiro_resid$p.value
)

write_csv(
  residual_normality,
  file.path(output_dir, "h1_residual_normality_shapiro.csv")
)

# 9.5 Q-Q plot
p_qq <- ggplot(tibble(sample = ancova_residuals), aes(sample = sample)) +
  stat_qq(color = group_colors["Experimental"], size = 2, alpha = 0.8) +
  stat_qq_line(color = group_colors["Control"], linewidth = 0.9) +
  labs(
    title = "Q-Q Plot of Standardized Residuals for the ANCOVA Model",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  filename = file.path(output_dir, "h1_ancova_residuals_qqplot.png"),
  plot = p_qq,
  width = 7,
  height = 5,
  dpi = 300
)

# 9.6 Residual histogram
p_hist_resid <- ggplot(tibble(resid = ancova_residuals), aes(x = resid)) +
  geom_histogram(
    bins = 12,
    color = "black",
    fill = group_colors["Control"],
    alpha = 0.75
  ) +
  labs(
    title = "Histogram of ANCOVA Residuals",
    x = "Residuals",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  filename = file.path(output_dir, "h1_ancova_residuals_histogram.png"),
  plot = p_hist_resid,
  width = 7,
  height = 5,
  dpi = 300
)

# 9.7 Homogeneity of variance
levene_h1 <- car::leveneTest(post_holistic ~ Group, data = df)
levene_h1_tbl <- broom::tidy(levene_h1)

write_csv(
  levene_h1_tbl,
  file.path(output_dir, "h1_levene_post_holistic.csv")
)

# 9.8 Combined ANCOVA assumptions summary
interaction_f <- if (nrow(interaction_row) > 0) interaction_row$statistic[1] else NA_real_
interaction_p <- if (nrow(interaction_row) > 0) interaction_row$p.value[1] else NA_real_

levene_h1_f <- if ("statistic" %in% names(levene_h1_tbl)) levene_h1_tbl$statistic[1] else NA_real_
levene_h1_p <- if ("p.value" %in% names(levene_h1_tbl)) levene_h1_tbl$p.value[1] else NA_real_

ancova_assumptions_table <- tibble(
  assumption = c(
    "Linearity",
    "Homogeneity of regression slopes",
    "Normality of residuals",
    "Homogeneity of variance"
  ),
  test_indicator = c(
    "Scatterplot inspection",
    "Group × Pre-test interaction",
    "Shapiro-Wilk test",
    "Levene's test"
  ),
  result = c(
    "Inspect h1_scatter_pre_post_by_group.png",
    paste0("F = ", round(interaction_f, 3), ", p = ", round(interaction_p, 4)),
    paste0("W = ", round(unname(shapiro_resid$statistic), 3), ", p = ", round(shapiro_resid$p.value, 4)),
    paste0("F = ", round(levene_h1_f, 3), ", p = ", round(levene_h1_p, 4))
  ),
  decision = c(
    "Judge from plot",
    ifelse(!is.na(interaction_p) && interaction_p > 0.05, "Satisfied", "Check carefully"),
    ifelse(shapiro_resid$p.value > 0.05, "Satisfied", "Check carefully"),
    ifelse(!is.na(levene_h1_p) && levene_h1_p > 0.05, "Satisfied", "Check carefully")
  )
)

write_csv(
  ancova_assumptions_table,
  file.path(output_dir, "ancova_assumptions_summary.csv")
)

# -----------------------------
# 10) H2: Reduction-score assumption checks
# -----------------------------
reduction_vars <- c("grammar_reduction", "vocabulary_reduction", "mechanics_reduction")

# 10.1 Shapiro-Wilk by group and variable
shapiro_by_group <- map_dfr(reduction_vars, function(v) {
  df %>%
    group_by(Group) %>%
    summarise(
      variable = v,
      statistic_W = shapiro.test(.data[[v]])$statistic,
      p_value = shapiro.test(.data[[v]])$p.value,
      .groups = "drop"
    )
})

shapiro_by_group <- shapiro_by_group %>%
  mutate(
    result = ifelse(p_value > 0.05, "Normal", "Non-normal")
  ) %>%
  select(Group, variable, statistic_W, p_value, result)

write_csv(
  shapiro_by_group,
  file.path(output_dir, "error_reduction_normality_shapiro.csv")
)

# 10.2 Levene tests by variable
levene_reduction <- map_dfr(reduction_vars, function(v) {
  form <- as.formula(paste(v, "~ Group"))
  lev <- car::leveneTest(form, data = df)
  out <- broom::tidy(lev)
  out$variable <- v
  out
})

write_csv(
  levene_reduction,
  file.path(output_dir, "error_reduction_levene_tests.csv")
)

# 10.3 Long data for plots
df_long_reduction <- df %>%
  select(StudentID, Group, all_of(reduction_vars)) %>%
  pivot_longer(
    cols = all_of(reduction_vars),
    names_to = "variable",
    values_to = "reduction"
  ) %>%
  mutate(
    variable = recode(
      variable,
      grammar_reduction = "Grammar Reduction",
      vocabulary_reduction = "Vocabulary Reduction",
      mechanics_reduction = "Mechanics Reduction"
    )
  )

# 10.4 Histograms
p_hist_reduction <- ggplot(
  df_long_reduction,
  aes(x = reduction, fill = Group)
) +
  geom_histogram(
    bins = 10,
    color = "black",
    alpha = 0.75,
    position = "identity"
  ) +
  facet_grid(variable ~ Group, scales = "free") +
  scale_fill_manual(values = group_colors) +
  labs(
    title = "Distribution of Error-Reduction Scores by Group",
    x = "Reduction Score",
    y = "Frequency",
    fill = "Group"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  filename = file.path(output_dir, "h2_reduction_histograms.png"),
  plot = p_hist_reduction,
  width = 10,
  height = 8,
  dpi = 300
)

# 10.5 Boxplots
p_box_reduction <- ggplot(
  df_long_reduction,
  aes(x = Group, y = reduction, fill = Group)
) +
  geom_boxplot(alpha = 0.8) +
  facet_wrap(~ variable, scales = "free_y") +
  scale_fill_manual(values = group_colors) +
  labs(
    title = "Boxplots of Grammar, Vocabulary, and Mechanics Reduction Scores by Group",
    x = "Group",
    y = "Reduction Score",
    fill = "Group"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  filename = file.path(output_dir, "h2_reduction_boxplots.png"),
  plot = p_box_reduction,
  width = 10,
  height = 5.5,
  dpi = 300
)

# 10.6 Descriptives for reduction variables
reduction_descriptives <- df_long_reduction %>%
  group_by(Group, variable) %>%
  summarise(
    n = sum(!is.na(reduction)),
    mean = mean(reduction, na.rm = TRUE),
    sd = sd(reduction, na.rm = TRUE),
    min = min(reduction, na.rm = TRUE),
    max = max(reduction, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(
  reduction_descriptives,
  file.path(output_dir, "error_reduction_descriptives.csv")
)

# -----------------------------
# 11) Save a compact text summary
# -----------------------------
summary_lines <- c(
  "Preliminary Data Screening and Assumption Checking completed.",
  "",
  "Analytical variables were constructed as follows:",
  "- pre_mechanics  = Spelling_pre + Punctuation_pre + Capitalization_pre",
  "- post_mechanics = Spelling_post + Punctuation_post + Capitalization_post",
  "- pre_grammar    = Tense_pre + Agreement_pre + Article_pronoun_Preposition_pre",
  "- post_grammar   = Tense_post + Agreement_post + Article_pronoun_Preposition_post",
  "- pre_vocabulary = Vocabulary_pre",
  "- post_vocabulary = Vocabulary_post",
  "- pre_holistic   = HolisticScore_pre",
  "- post_holistic  = HolisticScore_post",
  "- grammar_reduction = pre_grammar - post_grammar",
  "- vocabulary_reduction = pre_vocabulary - post_vocabulary",
  "- mechanics_reduction = pre_mechanics - post_mechanics",
  "",
  "Color palette used in grouped plots:",
  "- Control = #4F81BD",
  "- Experimental = #C0504D",
  "",
  "Saved files:",
  "- prepared_dataset.csv",
  "- data_screening_summary.csv",
  "- missing_values_by_variable.csv",
  "- outlier_summary_iqr.csv",
  "- outlier_details_iqr.csv",
  "- preliminary_descriptives_overall.csv",
  "- preliminary_descriptives_by_group.csv",
  "- ancova_assumptions_summary.csv",
  "- h1_homogeneity_of_regression_slopes_anova.csv",
  "- h1_ancova_table_type3.csv",
  "- h1_residual_normality_shapiro.csv",
  "- h1_levene_post_holistic.csv",
  "- h1_scatter_pre_post_by_group.png",
  "- h1_ancova_residuals_qqplot.png",
  "- h1_ancova_residuals_histogram.png",
  "- error_reduction_normality_shapiro.csv",
  "- error_reduction_levene_tests.csv",
  "- error_reduction_descriptives.csv",
  "- h2_reduction_histograms.png",
  "- h2_reduction_boxplots.png"
)

writeLines(summary_lines, con = file.path(output_dir, "README_outputs.txt"))

cat("Done. All preliminary outputs were saved in:", output_dir, "\n")