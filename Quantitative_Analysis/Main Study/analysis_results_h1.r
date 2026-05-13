# ============================================================
# ANALYSIS OF HYPOTHESIS 1: ESSAY-WRITING PERFORMANCE
# Adapted to: alldata.csv
# Outputs saved in: results_h1/
# ============================================================

# -----------------------------
# 1) Packages
# -----------------------------
required_packages <- c(
  "readr", "dplyr", "tidyr", "ggplot2", "stringr",
  "car", "broom", "emmeans"
)

installed <- rownames(installed.packages())
for (pkg in required_packages) {
  if (!pkg %in% installed) install.packages(pkg, dependencies = TRUE)
}

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(car)
library(broom)
library(emmeans)

# -----------------------------
# 2) User settings
# -----------------------------
setwd("F:/QuanAnal")
input_file <- "alldata.csv"
output_dir <- "results_h1"

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Same palette across grouped plots
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
  "HolisticScore_pre", "HolisticScore_post"
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
    Group = factor(Group, levels = c("Control", "Experimental")),
    pre_holistic = as.numeric(HolisticScore_pre),
    post_holistic = as.numeric(HolisticScore_post),
    holistic_gain = post_holistic - pre_holistic
  )

if (any(is.na(df$Group))) {
  stop("Some values in 'Group' could not be interpreted.")
}

# Keep only needed columns for H1
h1_df <- df %>%
  select(StudentID, Group, pre_holistic, post_holistic, holistic_gain)

write_csv(h1_df, file.path(output_dir, "h1_prepared_dataset.csv"))

# ============================================================
# 6) DESCRIPTIVE STATISTICS
# ============================================================
h1_long <- h1_df %>%
  pivot_longer(
    cols = c(pre_holistic, post_holistic),
    names_to = "Time",
    values_to = "HolisticScore"
  ) %>%
  mutate(
    Time = recode(
      Time,
      pre_holistic = "Pre-test",
      post_holistic = "Post-test"
    ),
    Time = factor(Time, levels = c("Pre-test", "Post-test"))
  )

h1_descriptives <- h1_long %>%
  group_by(Group, Time) %>%
  summarise(
    n = sum(!is.na(HolisticScore)),
    mean = mean(HolisticScore, na.rm = TRUE),
    sd = sd(HolisticScore, na.rm = TRUE),
    min = min(HolisticScore, na.rm = TRUE),
    max = max(HolisticScore, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(
  h1_descriptives,
  file.path(output_dir, "h1_holistic_descriptives.csv")
)

h1_descriptives_wide <- h1_descriptives %>%
  pivot_wider(
    names_from = Time,
    values_from = c(n, mean, sd, min, max)
  )

write_csv(
  h1_descriptives_wide,
  file.path(output_dir, "h1_holistic_descriptives_wide.csv")
)

# Gain descriptives (supplementary only)
h1_gain_descriptives <- h1_df %>%
  group_by(Group) %>%
  summarise(
    n = sum(!is.na(holistic_gain)),
    mean_gain = mean(holistic_gain, na.rm = TRUE),
    sd_gain = sd(holistic_gain, na.rm = TRUE),
    min_gain = min(holistic_gain, na.rm = TRUE),
    max_gain = max(holistic_gain, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(
  h1_gain_descriptives,
  file.path(output_dir, "h1_gain_descriptives.csv")
)

# ============================================================
# 7) SUPPLEMENTARY WITHIN-GROUP PRE/POST ANALYSES
# ============================================================

# 7.1 Shapiro-Wilk tests on difference scores within each group
difference_normality <- h1_df %>%
  group_by(Group) %>%
  summarise(
    W = shapiro.test(holistic_gain)$statistic,
    p_value = shapiro.test(holistic_gain)$p.value,
    normality = ifelse(p_value > 0.05, "Normal", "Non-normal"),
    .groups = "drop"
  )

write_csv(
  difference_normality,
  file.path(output_dir, "h1_difference_score_normality.csv")
)

# 7.2 Within-group paired tests
run_within_group_test <- function(data_subset, group_name) {
  shapiro_p <- shapiro.test(data_subset$holistic_gain)$p.value
  
  if (shapiro_p > 0.05) {
    test_obj <- t.test(
      data_subset$post_holistic,
      data_subset$pre_holistic,
      paired = TRUE
    )
    
    tibble(
      Group = group_name,
      test_used = "Paired-samples t-test",
      statistic = unname(test_obj$statistic),
      df = unname(test_obj$parameter),
      p_value = test_obj$p.value,
      mean_pre = mean(data_subset$pre_holistic, na.rm = TRUE),
      mean_post = mean(data_subset$post_holistic, na.rm = TRUE),
      mean_difference = mean(data_subset$holistic_gain, na.rm = TRUE)
    )
  } else {
    test_obj <- wilcox.test(
      data_subset$post_holistic,
      data_subset$pre_holistic,
      paired = TRUE,
      exact = FALSE
    )
    
    tibble(
      Group = group_name,
      test_used = "Wilcoxon signed-rank test",
      statistic = unname(test_obj$statistic),
      df = NA_real_,
      p_value = test_obj$p.value,
      mean_pre = mean(data_subset$pre_holistic, na.rm = TRUE),
      mean_post = mean(data_subset$post_holistic, na.rm = TRUE),
      mean_difference = mean(data_subset$holistic_gain, na.rm = TRUE)
    )
  }
}

within_group_results <- bind_rows(
  run_within_group_test(filter(h1_df, Group == "Control"), "Control"),
  run_within_group_test(filter(h1_df, Group == "Experimental"), "Experimental")
)

write_csv(
  within_group_results,
  file.path(output_dir, "h1_within_group_pre_post_tests.csv")
)

# ============================================================
# 8) ANCOVA ASSUMPTION CHECKING
# ============================================================

# 8.1 Linearity plot
p_scatter <- ggplot(
  h1_df,
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

# 8.2 Homogeneity of regression slopes
model_interaction <- lm(post_holistic ~ pre_holistic * Group, data = h1_df)
interaction_anova <- anova(model_interaction)
interaction_table <- broom::tidy(interaction_anova)

write_csv(
  interaction_table,
  file.path(output_dir, "h1_homogeneity_of_regression_slopes.csv")
)

interaction_row <- interaction_table %>%
  filter(str_detect(term, "pre_holistic:Group|Group:pre_holistic"))

# 8.3 Main ANCOVA model
model_ancova <- lm(post_holistic ~ pre_holistic + Group, data = h1_df)
ancova_type3 <- car::Anova(model_ancova, type = 3)

ancova_table <- tibble(
  term = rownames(ancova_type3),
  sumsq = ancova_type3$`Sum Sq`,
  df = ancova_type3$Df,
  statistic = ancova_type3$`F value`,
  p.value = ancova_type3$`Pr(>F)`
)

# Partial eta squared
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

# 8.4 Adjusted means
emm <- emmeans(model_ancova, ~ Group)
adjusted_means <- as.data.frame(emm)

write_csv(
  adjusted_means,
  file.path(output_dir, "h1_adjusted_means.csv")
)

adjusted_means_contrast <- as.data.frame(pairs(emm))

write_csv(
  adjusted_means_contrast,
  file.path(output_dir, "h1_adjusted_means_comparison.csv")
)

# 8.5 Residual normality
ancova_residuals <- residuals(model_ancova)
shapiro_resid <- shapiro.test(ancova_residuals)

residual_normality <- tibble(
  test = "Shapiro-Wilk test of ANCOVA residuals",
  W = unname(shapiro_resid$statistic),
  p_value = shapiro_resid$p.value
)

write_csv(
  residual_normality,
  file.path(output_dir, "h1_residual_normality.csv")
)

# 8.6 Levene's test
levene_h1 <- car::leveneTest(post_holistic ~ Group, data = h1_df)
levene_h1_tbl <- broom::tidy(levene_h1)

write_csv(
  levene_h1_tbl,
  file.path(output_dir, "h1_levene_post_holistic.csv")
)

# 8.7 ANCOVA assumptions summary table
interaction_f <- if (nrow(interaction_row) > 0) interaction_row$statistic[1] else NA_real_
interaction_p <- if (nrow(interaction_row) > 0) interaction_row$p.value[1] else NA_real_

levene_h1_f <- if ("statistic" %in% names(levene_h1_tbl)) levene_h1_tbl$statistic[1] else NA_real_
levene_h1_p <- if ("p.value" %in% names(levene_h1_tbl)) levene_h1_tbl$p.value[1] else NA_real_

ancova_assumptions_summary <- tibble(
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
  ancova_assumptions_summary,
  file.path(output_dir, "h1_ancova_assumptions_summary.csv")
)

# ============================================================
# 9) PLOTS
# ============================================================

# 9.1 Mean pre/post holistic scores by group
p_holistic_means <- ggplot(
  h1_descriptives,
  aes(x = Time, y = mean, color = Group, group = Group)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_color_manual(values = group_colors) +
  labs(
    title = "Mean Holistic Scores at Pre-test and Post-test by Group",
    x = "Testing Time",
    y = "Mean Holistic Score",
    color = "Group"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  filename = file.path(output_dir, "h1_mean_holistic_scores_pre_post.png"),
  plot = p_holistic_means,
  width = 8,
  height = 5,
  dpi = 300
)

# 9.2 Boxplots for raw holistic scores
p_holistic_box <- ggplot(
  h1_long,
  aes(x = Time, y = HolisticScore, fill = Group)
) +
  geom_boxplot(alpha = 0.8, position = position_dodge(width = 0.75)) +
  scale_fill_manual(values = group_colors) +
  labs(
    title = "Distribution of Holistic Scores by Group and Testing Time",
    x = "Testing Time",
    y = "Holistic Score",
    fill = "Group"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  filename = file.path(output_dir, "h1_boxplots_holistic_scores_pre_post.png"),
  plot = p_holistic_box,
  width = 8,
  height = 5,
  dpi = 300
)

# 9.3 Q-Q plot of ANCOVA residuals
p_qq <- ggplot(tibble(sample = ancova_residuals), aes(sample = sample)) +
  stat_qq(color = group_colors["Experimental"], size = 2, alpha = 0.8) +
  stat_qq_line(color = group_colors["Control"], linewidth = 0.9) +
  labs(
    title = "Q-Q Plot of Residuals for the ANCOVA Model",
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

# 9.4 Histogram of ANCOVA residuals
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

# 9.5 Adjusted means plot
adjusted_means_plot_data <- adjusted_means %>%
  mutate(
    Group = factor(Group, levels = c("Control", "Experimental"))
  )

p_adjusted_means <- ggplot(
  adjusted_means_plot_data,
  aes(x = Group, y = emmean, fill = Group)
) +
  geom_col(alpha = 0.85, width = 0.6) +
  geom_errorbar(
    aes(ymin = lower.CL, ymax = upper.CL),
    width = 0.15,
    linewidth = 0.8
  ) +
  scale_fill_manual(values = group_colors) +
  labs(
    title = "Adjusted Post-test Holistic Means by Group",
    x = "Group",
    y = "Adjusted Mean Post-test Holistic Score",
    fill = "Group"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  filename = file.path(output_dir, "h1_adjusted_posttest_means.png"),
  plot = p_adjusted_means,
  width = 7,
  height = 5,
  dpi = 300
)

# ============================================================
# 10) README OF OUTPUTS
# ============================================================
summary_lines <- c(
  "Analysis of Hypothesis 1 completed.",
  "",
  "Hypothesis 1 model:",
  "- Dependent variable: post_holistic",
  "- Fixed factor: Group",
  "- Covariate: pre_holistic",
  "- Main test: ANCOVA",
  "",
  "Supplementary within-group analyses:",
  "- Paired-samples t-test when difference scores are normal",
  "- Wilcoxon signed-rank test when difference scores are non-normal",
  "",
  "Color palette used in grouped plots:",
  "- Control = #4F81BD",
  "- Experimental = #C0504D",
  "",
  "Saved tables:",
  "- h1_prepared_dataset.csv",
  "- h1_holistic_descriptives.csv",
  "- h1_holistic_descriptives_wide.csv",
  "- h1_gain_descriptives.csv",
  "- h1_difference_score_normality.csv",
  "- h1_within_group_pre_post_tests.csv",
  "- h1_homogeneity_of_regression_slopes.csv",
  "- h1_ancova_table_type3.csv",
  "- h1_adjusted_means.csv",
  "- h1_adjusted_means_comparison.csv",
  "- h1_residual_normality.csv",
  "- h1_levene_post_holistic.csv",
  "- h1_ancova_assumptions_summary.csv",
  "",
  "Saved plots:",
  "- h1_mean_holistic_scores_pre_post.png",
  "- h1_boxplots_holistic_scores_pre_post.png",
  "- h1_scatter_pre_post_by_group.png",
  "- h1_ancova_residuals_qqplot.png",
  "- h1_ancova_residuals_histogram.png",
  "- h1_adjusted_posttest_means.png"
)

writeLines(summary_lines, con = file.path(output_dir, "README_outputs.txt"))

cat("Done. All Hypothesis 1 outputs were saved in:", output_dir, "\n")