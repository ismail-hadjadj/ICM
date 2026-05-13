# ============================================================
# ANALYSIS OF HYPOTHESIS 2: ERROR REDUCTION
# Adapted to: alldata.csv
# Outputs saved in: results_h2/
# Includes:
#   1) Main categories: Grammar, Vocabulary, Mechanics
#   2) Full sub-category analysis:
#      Spelling, Punctuation, Capitalization,
#      Tense, Agreement, Article/Pronoun/Preposition, Vocabulary
# ============================================================

# -----------------------------
# 1) Packages
# -----------------------------
required_packages <- c(
  "readr", "dplyr", "tidyr", "ggplot2", "stringr",
  "car", "broom", "purrr"
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
library(purrr)

# -----------------------------
# 2) User settings
# -----------------------------
setwd("f:/QuanAnal")
input_file <- "alldata.csv"
output_dir <- "results_h2"

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

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
  "Vocabulary_pre",
  "Spelling_post", "Punctuation_post", "Capitalization_post",
  "Tense_post", "Agreement_post", "Article_pronoun_Preposition_post",
  "Vocabulary_post"
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
  "Vocabulary_pre",
  "Spelling_post", "Punctuation_post", "Capitalization_post",
  "Tense_post", "Agreement_post", "Article_pronoun_Preposition_post",
  "Vocabulary_post"
)

df <- df %>%
  mutate(across(all_of(numeric_source_cols), as.numeric))

# -----------------------------
# 6) Construct main-category variables
# -----------------------------
df <- df %>%
  mutate(
    # Main categories
    pre_mechanics  = Spelling_pre + Punctuation_pre + Capitalization_pre,
    post_mechanics = Spelling_post + Punctuation_post + Capitalization_post,

    pre_grammar  = Tense_pre + Agreement_pre + Article_pronoun_Preposition_pre,
    post_grammar = Tense_post + Agreement_post + Article_pronoun_Preposition_post,

    pre_vocabulary  = Vocabulary_pre,
    post_vocabulary = Vocabulary_post,

    grammar_reduction    = pre_grammar - post_grammar,
    vocabulary_reduction = pre_vocabulary - post_vocabulary,
    mechanics_reduction  = pre_mechanics - post_mechanics,

    # Sub-category reductions
    spelling_reduction = Spelling_pre - Spelling_post,
    punctuation_reduction = Punctuation_pre - Punctuation_post,
    capitalization_reduction = Capitalization_pre - Capitalization_post,
    tense_reduction = Tense_pre - Tense_post,
    agreement_reduction = Agreement_pre - Agreement_post,
    article_pronoun_preposition_reduction =
      Article_pronoun_Preposition_pre - Article_pronoun_Preposition_post
  )

write_csv(df, file.path(output_dir, "h2_prepared_dataset_full.csv"))

# ============================================================
# 7) HELPER FUNCTIONS
# ============================================================

label_subcategory <- function(x) {
  case_when(
    x == "Spelling" ~ "Spelling",
    x == "Punctuation" ~ "Punctuation",
    x == "Capitalization" ~ "Capitalization",
    x == "Tense" ~ "Tense",
    x == "Agreement" ~ "Agreement",
    x == "Article_pronoun_Preposition" ~ "Article/Pronoun/Preposition",
    x == "Vocabulary" ~ "Vocabulary",
    TRUE ~ x
  )
}

compute_cohens_d <- function(x_control, x_experimental) {
  x_control <- x_control[!is.na(x_control)]
  x_experimental <- x_experimental[!is.na(x_experimental)]

  n1 <- length(x_control)
  n2 <- length(x_experimental)

  if (n1 < 2 || n2 < 2) return(NA_real_)

  m1 <- mean(x_control)
  m2 <- mean(x_experimental)

  sd1 <- sd(x_control)
  sd2 <- sd(x_experimental)

  pooled_sd <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))

  if (is.na(pooled_sd) || pooled_sd == 0) return(NA_real_)

  (m2 - m1) / pooled_sd
}

run_between_group_test <- function(data_long, variable_name, value_col = "Reduction") {
  tmp <- data_long %>% filter(.data[[names(data_long)[3]]] == variable_name)

  control_vals <- tmp %>%
    filter(Group == "Control") %>%
    pull(.data[[value_col]])

  experimental_vals <- tmp %>%
    filter(Group == "Experimental") %>%
    pull(.data[[value_col]])

  shapiro_control <- shapiro.test(control_vals)
  shapiro_experimental <- shapiro.test(experimental_vals)

  lev_tbl <- broom::tidy(car::leveneTest(as.formula(paste(value_col, "~ Group")), data = tmp))

  both_normal <- shapiro_control$p.value > 0.05 && shapiro_experimental$p.value > 0.05
  equal_var <- lev_tbl$p.value[1] > 0.05

  d_value <- compute_cohens_d(control_vals, experimental_vals)

  if (both_normal && equal_var) {
    test_obj <- t.test(as.formula(paste(value_col, "~ Group")), data = tmp, var.equal = TRUE)

    tibble(
      variable = variable_name,
      test_used = "Independent-samples t-test",
      statistic_name = "t",
      statistic = unname(test_obj$statistic),
      df = unname(test_obj$parameter),
      p_value = test_obj$p.value,
      mean_control = mean(control_vals, na.rm = TRUE),
      mean_experimental = mean(experimental_vals, na.rm = TRUE),
      mean_difference = mean(experimental_vals, na.rm = TRUE) - mean(control_vals, na.rm = TRUE),
      effect_size_measure = "Cohen's d",
      effect_size = d_value
    )

  } else if (both_normal && !equal_var) {
    test_obj <- t.test(as.formula(paste(value_col, "~ Group")), data = tmp, var.equal = FALSE)

    tibble(
      variable = variable_name,
      test_used = "Welch t-test",
      statistic_name = "t",
      statistic = unname(test_obj$statistic),
      df = unname(test_obj$parameter),
      p_value = test_obj$p.value,
      mean_control = mean(control_vals, na.rm = TRUE),
      mean_experimental = mean(experimental_vals, na.rm = TRUE),
      mean_difference = mean(experimental_vals, na.rm = TRUE) - mean(control_vals, na.rm = TRUE),
      effect_size_measure = "Cohen's d",
      effect_size = d_value
    )

  } else {
    test_obj <- wilcox.test(
      as.formula(paste(value_col, "~ Group")),
      data = tmp,
      exact = FALSE
    )

    n1 <- sum(tmp$Group == "Control")
    W_value <- unname(test_obj$statistic)
    U_value <- W_value - (n1 * (n1 + 1)) / 2

    tibble(
      variable = variable_name,
      test_used = "Mann-Whitney U test",
      statistic_name = "U",
      statistic = U_value,
      df = NA_real_,
      p_value = test_obj$p.value,
      mean_control = mean(control_vals, na.rm = TRUE),
      mean_experimental = mean(experimental_vals, na.rm = TRUE),
      mean_difference = mean(experimental_vals, na.rm = TRUE) - mean(control_vals, na.rm = TRUE),
      effect_size_measure = "Cohen's d",
      effect_size = d_value
    )
  }
}

# ============================================================
# 8) MAIN CATEGORY ANALYSIS
# ============================================================

# 8.1 Pre/post long data for main categories
category_prepost_long <- df %>%
  select(
    StudentID, Group,
    pre_grammar, post_grammar,
    pre_vocabulary, post_vocabulary,
    pre_mechanics, post_mechanics
  ) %>%
  pivot_longer(
    cols = -c(StudentID, Group),
    names_to = c("TimeRaw", "Category"),
    names_pattern = "^(pre|post)_(.*)$",
    values_to = "Score"
  ) %>%
  mutate(
    Time = case_when(
      TimeRaw == "pre" ~ "Pre-test",
      TimeRaw == "post" ~ "Post-test",
      TRUE ~ TimeRaw
    ),
    Category = case_when(
      Category == "grammar" ~ "Grammar",
      Category == "vocabulary" ~ "Vocabulary",
      Category == "mechanics" ~ "Mechanics",
      TRUE ~ Category
    ),
    Time = factor(Time, levels = c("Pre-test", "Post-test")),
    Category = factor(Category, levels = c("Grammar", "Vocabulary", "Mechanics"))
  ) %>%
  select(StudentID, Group, Category, Time, Score)

category_prepost_descriptives <- category_prepost_long %>%
  group_by(Group, Category, Time) %>%
  summarise(
    n = sum(!is.na(Score)),
    mean = mean(Score, na.rm = TRUE),
    sd = sd(Score, na.rm = TRUE),
    min = min(Score, na.rm = TRUE),
    max = max(Score, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(
  category_prepost_descriptives,
  file.path(output_dir, "h2_category_prepost_descriptives.csv")
)

# 8.2 Reduction long data for main categories
category_reduction_long <- df %>%
  select(
    StudentID, Group,
    grammar_reduction, vocabulary_reduction, mechanics_reduction
  ) %>%
  pivot_longer(
    cols = c(grammar_reduction, vocabulary_reduction, mechanics_reduction),
    names_to = "CategoryRaw",
    values_to = "Reduction"
  ) %>%
  mutate(
    Category = case_when(
      CategoryRaw == "grammar_reduction" ~ "Grammar",
      CategoryRaw == "vocabulary_reduction" ~ "Vocabulary",
      CategoryRaw == "mechanics_reduction" ~ "Mechanics",
      TRUE ~ CategoryRaw
    ),
    Category = factor(Category, levels = c("Grammar", "Vocabulary", "Mechanics"))
  ) %>%
  select(StudentID, Group, Category, Reduction)

category_reduction_descriptives <- category_reduction_long %>%
  group_by(Group, Category) %>%
  summarise(
    n = sum(!is.na(Reduction)),
    mean = mean(Reduction, na.rm = TRUE),
    sd = sd(Reduction, na.rm = TRUE),
    min = min(Reduction, na.rm = TRUE),
    max = max(Reduction, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(
  category_reduction_descriptives,
  file.path(output_dir, "h2_category_reduction_descriptives.csv")
)

# 8.3 Assumption checks for main categories
category_shapiro <- category_reduction_long %>%
  group_by(Category, Group) %>%
  summarise(
    W = shapiro.test(Reduction)$statistic,
    p_value = shapiro.test(Reduction)$p.value,
    result = ifelse(p_value > 0.05, "Normal", "Non-normal"),
    .groups = "drop"
  )

write_csv(
  category_shapiro,
  file.path(output_dir, "h2_category_shapiro_by_group.csv")
)

category_levene <- map_dfr(levels(category_reduction_long$Category), function(cat_name) {
  tmp <- category_reduction_long %>% filter(Category == cat_name)
  lev_tbl <- broom::tidy(car::leveneTest(Reduction ~ Group, data = tmp))

  tibble(
    Category = cat_name,
    statistic = lev_tbl$statistic[1],
    p_value = lev_tbl$p.value[1]
  )
})

write_csv(
  category_levene,
  file.path(output_dir, "h2_category_levene_tests.csv")
)

category_assumptions <- category_shapiro %>%
  group_by(Category) %>%
  summarise(
    control_normality_p = p_value[Group == "Control"],
    experimental_normality_p = p_value[Group == "Experimental"],
    both_groups_normal = all(p_value > 0.05),
    .groups = "drop"
  ) %>%
  left_join(category_levene, by = "Category") %>%
  mutate(equal_variances = p_value > 0.05) %>%
  rename(levene_p = p_value)

write_csv(
  category_assumptions,
  file.path(output_dir, "h2_category_assumption_summary.csv")
)

# 8.4 Inferential tests for main categories
grammar_result <- run_between_group_test(category_reduction_long, "Grammar", value_col = "Reduction") %>%
  rename(Category = variable)
vocabulary_result <- run_between_group_test(category_reduction_long, "Vocabulary", value_col = "Reduction") %>%
  rename(Category = variable)
mechanics_result <- run_between_group_test(category_reduction_long, "Mechanics", value_col = "Reduction") %>%
  rename(Category = variable)

write_csv(grammar_result, file.path(output_dir, "h2_grammar_between_group_test.csv"))
write_csv(vocabulary_result, file.path(output_dir, "h2_vocabulary_between_group_test.csv"))
write_csv(mechanics_result, file.path(output_dir, "h2_mechanics_between_group_test.csv"))

category_inferential <- bind_rows(
  grammar_result,
  vocabulary_result,
  mechanics_result
)

write_csv(
  category_inferential,
  file.path(output_dir, "h2_category_inferential_results.csv")
)

category_effect_sizes <- category_inferential %>%
  select(Category, effect_size_measure, effect_size)

write_csv(
  category_effect_sizes,
  file.path(output_dir, "h2_category_effect_sizes.csv")
)

# ============================================================
# 9) FULL SUB-CATEGORY ANALYSIS
# ============================================================

# 9.1 Pre/post long data for sub-categories
subcategory_prepost_long <- df %>%
  select(
    StudentID, Group,
    Spelling_pre, Spelling_post,
    Punctuation_pre, Punctuation_post,
    Capitalization_pre, Capitalization_post,
    Tense_pre, Tense_post,
    Agreement_pre, Agreement_post,
    Article_pronoun_Preposition_pre, Article_pronoun_Preposition_post,
    Vocabulary_pre, Vocabulary_post
  ) %>%
  pivot_longer(
    cols = -c(StudentID, Group),
    names_to = c("SubRaw", "TimeRaw"),
    names_pattern = "^(.*)_(pre|post)$",
    values_to = "Score"
  ) %>%
  mutate(
    Time = case_when(
      TimeRaw == "pre" ~ "Pre-test",
      TimeRaw == "post" ~ "Post-test",
      TRUE ~ TimeRaw
    ),
    Subcategory = label_subcategory(SubRaw),
    Time = factor(Time, levels = c("Pre-test", "Post-test")),
    Subcategory = factor(
      Subcategory,
      levels = c(
        "Spelling", "Punctuation", "Capitalization",
        "Tense", "Agreement", "Article/Pronoun/Preposition", "Vocabulary"
      )
    )
  ) %>%
  select(StudentID, Group, Subcategory, Time, Score)

subcategory_prepost_descriptives <- subcategory_prepost_long %>%
  group_by(Group, Subcategory, Time) %>%
  summarise(
    n = sum(!is.na(Score)),
    mean = mean(Score, na.rm = TRUE),
    sd = sd(Score, na.rm = TRUE),
    min = min(Score, na.rm = TRUE),
    max = max(Score, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(
  subcategory_prepost_descriptives,
  file.path(output_dir, "h2_subcategory_prepost_descriptives.csv")
)

# 9.2 Reduction long data for sub-categories
subcategory_reduction_long <- df %>%
  select(
    StudentID, Group,
    spelling_reduction, punctuation_reduction, capitalization_reduction,
    tense_reduction, agreement_reduction,
    article_pronoun_preposition_reduction, vocabulary_reduction
  ) %>%
  pivot_longer(
    cols = -c(StudentID, Group),
    names_to = "SubcategoryRaw",
    values_to = "Reduction"
  ) %>%
  mutate(
    Subcategory = case_when(
      SubcategoryRaw == "spelling_reduction" ~ "Spelling",
      SubcategoryRaw == "punctuation_reduction" ~ "Punctuation",
      SubcategoryRaw == "capitalization_reduction" ~ "Capitalization",
      SubcategoryRaw == "tense_reduction" ~ "Tense",
      SubcategoryRaw == "agreement_reduction" ~ "Agreement",
      SubcategoryRaw == "article_pronoun_preposition_reduction" ~ "Article/Pronoun/Preposition",
      SubcategoryRaw == "vocabulary_reduction" ~ "Vocabulary",
      TRUE ~ SubcategoryRaw
    ),
    Subcategory = factor(
      Subcategory,
      levels = c(
        "Spelling", "Punctuation", "Capitalization",
        "Tense", "Agreement", "Article/Pronoun/Preposition", "Vocabulary"
      )
    )
  ) %>%
  select(StudentID, Group, Subcategory, Reduction)

subcategory_reduction_descriptives <- subcategory_reduction_long %>%
  group_by(Group, Subcategory) %>%
  summarise(
    n = sum(!is.na(Reduction)),
    mean = mean(Reduction, na.rm = TRUE),
    sd = sd(Reduction, na.rm = TRUE),
    min = min(Reduction, na.rm = TRUE),
    max = max(Reduction, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(
  subcategory_reduction_descriptives,
  file.path(output_dir, "h2_subcategory_reduction_descriptives.csv")
)

# 9.3 Assumption checks for sub-categories
subcategory_shapiro <- subcategory_reduction_long %>%
  group_by(Subcategory, Group) %>%
  summarise(
    W = shapiro.test(Reduction)$statistic,
    p_value = shapiro.test(Reduction)$p.value,
    result = ifelse(p_value > 0.05, "Normal", "Non-normal"),
    .groups = "drop"
  )

write_csv(
  subcategory_shapiro,
  file.path(output_dir, "h2_subcategory_shapiro_by_group.csv")
)

subcategory_levene <- map_dfr(levels(subcategory_reduction_long$Subcategory), function(sub_name) {
  tmp <- subcategory_reduction_long %>% filter(Subcategory == sub_name)
  lev_tbl <- broom::tidy(car::leveneTest(Reduction ~ Group, data = tmp))

  tibble(
    Subcategory = sub_name,
    statistic = lev_tbl$statistic[1],
    p_value = lev_tbl$p.value[1]
  )
})

write_csv(
  subcategory_levene,
  file.path(output_dir, "h2_subcategory_levene_tests.csv")
)

subcategory_assumptions <- subcategory_shapiro %>%
  group_by(Subcategory) %>%
  summarise(
    control_normality_p = p_value[Group == "Control"],
    experimental_normality_p = p_value[Group == "Experimental"],
    both_groups_normal = all(p_value > 0.05),
    .groups = "drop"
  ) %>%
  left_join(subcategory_levene, by = "Subcategory") %>%
  mutate(equal_variances = p_value > 0.05) %>%
  rename(levene_p = p_value)

write_csv(
  subcategory_assumptions,
  file.path(output_dir, "h2_subcategory_assumption_summary.csv")
)

# 9.4 Inferential tests for sub-categories
subcategory_names <- c(
  "Spelling", "Punctuation", "Capitalization",
  "Tense", "Agreement", "Article/Pronoun/Preposition", "Vocabulary"
)

subcategory_inferential <- map_dfr(subcategory_names, function(sub_name) {
  run_between_group_test(subcategory_reduction_long, sub_name, value_col = "Reduction") %>%
    rename(Subcategory = variable)
})

write_csv(
  subcategory_inferential,
  file.path(output_dir, "h2_subcategory_inferential_results.csv")
)

subcategory_effect_sizes <- subcategory_inferential %>%
  select(Subcategory, effect_size_measure, effect_size)

write_csv(
  subcategory_effect_sizes,
  file.path(output_dir, "h2_subcategory_effect_sizes.csv")
)

# ============================================================
# 10) PLOTS
# ============================================================

# 10.1 Main category pre/post means
p_category_prepost_means <- ggplot(
  category_prepost_descriptives,
  aes(x = Time, y = mean, color = Group, group = Group)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.8) +
  facet_wrap(~ Category, scales = "free_y") +
  scale_color_manual(values = group_colors) +
  labs(
    title = "Mean Error Frequencies at Pre-test and Post-test by Group",
    x = "Testing Time",
    y = "Mean Error Frequency",
    color = "Group"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  filename = file.path(output_dir, "h2_category_error_means_pre_post.png"),
  plot = p_category_prepost_means,
  width = 10,
  height = 5.5,
  dpi = 300
)

# 10.2 Main category reduction means
p_category_reduction_means <- ggplot(
  category_reduction_descriptives,
  aes(x = Category, y = mean, fill = Group)
) +
  geom_col(position = position_dodge(width = 0.75), alpha = 0.85) +
  scale_fill_manual(values = group_colors) +
  labs(
    title = "Mean Error Reduction Scores by Group",
    x = "Error Category",
    y = "Mean Reduction Score",
    fill = "Group"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  filename = file.path(output_dir, "h2_category_reduction_means_by_group.png"),
  plot = p_category_reduction_means,
  width = 8.5,
  height = 5,
  dpi = 300
)

# 10.3 Main category reduction boxplots
p_category_reduction_box <- ggplot(
  category_reduction_long,
  aes(x = Category, y = Reduction, fill = Group)
) +
  geom_boxplot(alpha = 0.8, position = position_dodge(width = 0.75)) +
  scale_fill_manual(values = group_colors) +
  labs(
    title = "Distribution of Main-Category Reduction Scores by Group",
    x = "Error Category",
    y = "Reduction Score",
    fill = "Group"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  filename = file.path(output_dir, "h2_category_reduction_boxplots.png"),
  plot = p_category_reduction_box,
  width = 8.5,
  height = 5,
  dpi = 300
)

# 10.4 Sub-category pre/post means
p_subcategory_prepost_means <- ggplot(
  subcategory_prepost_descriptives,
  aes(x = Time, y = mean, color = Group, group = Group)
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.4) +
  facet_wrap(~ Subcategory, scales = "free_y") +
  scale_color_manual(values = group_colors) +
  labs(
    title = "Mean Error Frequencies at Pre-test and Post-test by Sub-category",
    x = "Testing Time",
    y = "Mean Error Frequency",
    color = "Group"
  ) +
  theme_minimal(base_size = 11)

ggsave(
  filename = file.path(output_dir, "h2_subcategory_error_means_pre_post.png"),
  plot = p_subcategory_prepost_means,
  width = 12,
  height = 8,
  dpi = 300
)

# 10.5 Sub-category reduction means
p_subcategory_reduction_means <- ggplot(
  subcategory_reduction_descriptives,
  aes(x = Subcategory, y = mean, fill = Group)
) +
  geom_col(position = position_dodge(width = 0.75), alpha = 0.85) +
  scale_fill_manual(values = group_colors) +
  labs(
    title = "Mean Reduction Scores by Sub-category and Group",
    x = "Sub-category",
    y = "Mean Reduction Score",
    fill = "Group"
  ) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave(
  filename = file.path(output_dir, "h2_subcategory_reduction_means_by_group.png"),
  plot = p_subcategory_reduction_means,
  width = 12,
  height = 6,
  dpi = 300
)

# 10.6 Sub-category reduction boxplots
p_subcategory_reduction_box <- ggplot(
  subcategory_reduction_long,
  aes(x = Subcategory, y = Reduction, fill = Group)
) +
  geom_boxplot(alpha = 0.8, position = position_dodge(width = 0.75)) +
  scale_fill_manual(values = group_colors) +
  labs(
    title = "Distribution of Sub-category Reduction Scores by Group",
    x = "Sub-category",
    y = "Reduction Score",
    fill = "Group"
  ) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave(
  filename = file.path(output_dir, "h2_subcategory_reduction_boxplots.png"),
  plot = p_subcategory_reduction_box,
  width = 12,
  height = 6,
  dpi = 300
)

# ============================================================
# 11) README
# ============================================================

summary_lines <- c(
  "Analysis of Hypothesis 2 completed.",
  "",
  "Main-category inferential outputs include:",
  "- test used for each category",
  "- test statistic for each category",
  "- p-value for each category",
  "- effect size for each category",
  "",
  "Sub-category inferential outputs include the same fields for:",
  "- Spelling",
  "- Punctuation",
  "- Capitalization",
  "- Tense",
  "- Agreement",
  "- Article/Pronoun/Preposition",
  "- Vocabulary",
  "",
  "Saved main-category files:",
  "- h2_category_prepost_descriptives.csv",
  "- h2_category_reduction_descriptives.csv",
  "- h2_category_shapiro_by_group.csv",
  "- h2_category_levene_tests.csv",
  "- h2_category_assumption_summary.csv",
  "- h2_grammar_between_group_test.csv",
  "- h2_vocabulary_between_group_test.csv",
  "- h2_mechanics_between_group_test.csv",
  "- h2_category_inferential_results.csv",
  "- h2_category_effect_sizes.csv",
  "",
  "Saved sub-category files:",
  "- h2_subcategory_prepost_descriptives.csv",
  "- h2_subcategory_reduction_descriptives.csv",
  "- h2_subcategory_shapiro_by_group.csv",
  "- h2_subcategory_levene_tests.csv",
  "- h2_subcategory_assumption_summary.csv",
  "- h2_subcategory_inferential_results.csv",
  "- h2_subcategory_effect_sizes.csv",
  "",
  "Saved plots:",
  "- h2_category_error_means_pre_post.png",
  "- h2_category_reduction_means_by_group.png",
  "- h2_category_reduction_boxplots.png",
  "- h2_subcategory_error_means_pre_post.png",
  "- h2_subcategory_reduction_means_by_group.png",
  "- h2_subcategory_reduction_boxplots.png"
)

writeLines(summary_lines, con = file.path(output_dir, "README_outputs.txt"))

cat("Done. All Hypothesis 2 outputs were saved in:", output_dir, "\n")