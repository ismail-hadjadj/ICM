# ============================================================
# DESCRIPTIVE STATISTICS
# Adapted to: alldata.csv
# Outputs saved in: descriptive_statistics/
# ============================================================

# -----------------------------
# 1) Packages
# -----------------------------
required_packages <- c(
  "readr", "dplyr", "tidyr", "ggplot2", "stringr"
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

# -----------------------------
# 2) User settings
# -----------------------------
setwd("f:/QuanAnal")
input_file <- "alldata.csv"
output_dir <- "descriptive_statistics"

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Same palette across all grouped plots
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
    Group = factor(Group, levels = c("Control", "Experimental")),
    sex = as.character(sex),
    age = as.numeric(age)
  )

numeric_source_cols <- c(
  "Spelling_pre", "Punctuation_pre", "Capitalization_pre",
  "Tense_pre", "Agreement_pre", "Article_pronoun_Preposition_pre",
  "Vocabulary_pre", "HolisticScore_pre",
  "Spelling_post", "Punctuation_post", "Capitalization_post",
  "Tense_post", "Agreement_post", "Article_pronoun_Preposition_post",
  "Vocabulary_post", "HolisticScore_post"
)

df <- df %>%
  mutate(across(all_of(numeric_source_cols), as.numeric))

# -----------------------------
# 6) Construct dissertation variables
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

write_csv(df, file.path(output_dir, "prepared_dataset_descriptive.csv"))

# ============================================================
# 7) SAMPLE DESCRIPTIVE STATISTICS
# ============================================================

# 7.1 Group size
group_size <- df %>%
  count(Group, name = "n")

write_csv(group_size, file.path(output_dir, "table_group_size.csv"))

# 7.2 Sex distribution
sex_distribution <- df %>%
  count(Group, sex, name = "n") %>%
  group_by(Group) %>%
  mutate(percent = round(100 * n / sum(n), 2)) %>%
  ungroup()

write_csv(sex_distribution, file.path(output_dir, "table_sex_distribution.csv"))

# 7.3 Age descriptives
age_descriptives <- df %>%
  group_by(Group) %>%
  summarise(
    n = sum(!is.na(age)),
    mean_age = mean(age, na.rm = TRUE),
    sd_age = sd(age, na.rm = TRUE),
    min_age = min(age, na.rm = TRUE),
    max_age = max(age, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(age_descriptives, file.path(output_dir, "table_age_descriptives.csv"))

# 7.4 Compact sample profile
sample_profile <- df %>%
  group_by(Group) %>%
  summarise(
    n = n(),
    mean_age = mean(age, na.rm = TRUE),
    sd_age = sd(age, na.rm = TRUE),
    females = sum(sex == "F", na.rm = TRUE),
    males = sum(sex == "M", na.rm = TRUE),
    .groups = "drop"
  )

write_csv(sample_profile, file.path(output_dir, "table_sample_profile.csv"))

# ============================================================
# 8) DESCRIPTIVE STATISTICS FOR HOLISTIC SCORES
# ============================================================

holistic_long <- df %>%
  select(StudentID, Group, pre_holistic, post_holistic) %>%
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

holistic_descriptives <- holistic_long %>%
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
  holistic_descriptives,
  file.path(output_dir, "table_holistic_descriptives.csv")
)

# Wide version for easier thesis table insertion
holistic_descriptives_wide <- holistic_descriptives %>%
  pivot_wider(
    names_from = Time,
    values_from = c(n, mean, sd, min, max)
  )

write_csv(
  holistic_descriptives_wide,
  file.path(output_dir, "table_holistic_descriptives_wide.csv")
)

# ============================================================
# 9) DESCRIPTIVE STATISTICS FOR ERROR CATEGORIES
# ============================================================

errors_long <- df %>%
  select(
    StudentID, Group,
    pre_grammar, post_grammar,
    pre_vocabulary, post_vocabulary,
    pre_mechanics, post_mechanics
  ) %>%
  pivot_longer(
    cols = -c(StudentID, Group),
    names_to = c("TimeRaw", "Category"),
    names_sep = "_",
    values_to = "Score"
  ) %>%
  mutate(
    Time = recode(
      TimeRaw,
      pre = "Pre-test",
      post = "Post-test"
    ),
    Category = recode(
      Category,
      grammar = "Grammar",
      vocabulary = "Vocabulary",
      mechanics = "Mechanics"
    ),
    Time = factor(Time, levels = c("Pre-test", "Post-test")),
    Category = factor(Category, levels = c("Grammar", "Vocabulary", "Mechanics"))
  ) %>%
  select(StudentID, Group, Category, Time, Score)

error_descriptives <- errors_long %>%
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
  error_descriptives,
  file.path(output_dir, "table_error_descriptives.csv")
)

# Wide version for easier thesis table insertion
error_descriptives_wide <- error_descriptives %>%
  pivot_wider(
    names_from = c(Category, Time),
    values_from = c(n, mean, sd, min, max)
  )

write_csv(
  error_descriptives_wide,
  file.path(output_dir, "table_error_descriptives_wide.csv")
)

# ============================================================
# 10) REDUCTION SCORE DESCRIPTIVES
# ============================================================

reduction_long <- df %>%
  select(StudentID, Group, grammar_reduction, vocabulary_reduction, mechanics_reduction) %>%
  pivot_longer(
    cols = c(grammar_reduction, vocabulary_reduction, mechanics_reduction),
    names_to = "Category",
    values_to = "Reduction"
  ) %>%
  mutate(
    Category = recode(
      Category,
      grammar_reduction = "Grammar",
      vocabulary_reduction = "Vocabulary",
      mechanics_reduction = "Mechanics"
    ),
    Category = factor(Category, levels = c("Grammar", "Vocabulary", "Mechanics"))
  )

reduction_descriptives <- reduction_long %>%
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
  reduction_descriptives,
  file.path(output_dir, "table_reduction_descriptives.csv")
)

# ============================================================
# 11) PLOTS
# ============================================================

# 11.1 Holistic means by group and time
p_holistic_means <- ggplot(
  holistic_descriptives,
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
  filename = file.path(output_dir, "plot_holistic_means_pre_post.png"),
  plot = p_holistic_means,
  width = 8,
  height = 5,
  dpi = 300
)

# 11.2 Holistic boxplots
p_holistic_box <- ggplot(
  holistic_long,
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
  filename = file.path(output_dir, "plot_holistic_boxplots_pre_post.png"),
  plot = p_holistic_box,
  width = 8,
  height = 5,
  dpi = 300
)

# 11.3 Error means by group, time, and category
p_error_means <- ggplot(
  error_descriptives,
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
  filename = file.path(output_dir, "plot_error_means_pre_post.png"),
  plot = p_error_means,
  width = 10,
  height = 5.5,
  dpi = 300
)

# 11.4 Error boxplots
p_error_box <- ggplot(
  errors_long,
  aes(x = Time, y = Score, fill = Group)
) +
  geom_boxplot(alpha = 0.8, position = position_dodge(width = 0.75)) +
  facet_wrap(~ Category, scales = "free_y") +
  scale_fill_manual(values = group_colors) +
  labs(
    title = "Distribution of Error Frequencies by Group and Testing Time",
    x = "Testing Time",
    y = "Error Frequency",
    fill = "Group"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  filename = file.path(output_dir, "plot_error_boxplots_pre_post.png"),
  plot = p_error_box,
  width = 10,
  height = 5.5,
  dpi = 300
)

# 11.5 Reduction score means
p_reduction_means <- ggplot(
  reduction_descriptives,
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
  filename = file.path(output_dir, "plot_reduction_means_by_group.png"),
  plot = p_reduction_means,
  width = 8.5,
  height = 5,
  dpi = 300
)

# 11.6 Reduction score boxplots
p_reduction_box <- ggplot(
  reduction_long,
  aes(x = Category, y = Reduction, fill = Group)
) +
  geom_boxplot(alpha = 0.8, position = position_dodge(width = 0.75)) +
  scale_fill_manual(values = group_colors) +
  labs(
    title = "Distribution of Error Reduction Scores by Group",
    x = "Error Category",
    y = "Reduction Score",
    fill = "Group"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  filename = file.path(output_dir, "plot_reduction_boxplots_by_group.png"),
  plot = p_reduction_box,
  width = 8.5,
  height = 5,
  dpi = 300
)

# ============================================================
# 12) README OF OUTPUTS
# ============================================================

summary_lines <- c(
  "Descriptive Statistics completed.",
  "",
  "Analytical variables were constructed as follows:",
  "- Mechanics = Spelling + Punctuation + Capitalization",
  "- Grammar   = Tense + Agreement + Article_pronoun_Preposition",
  "- Vocabulary = Vocabulary",
  "- Holistic score = HolisticScore",
  "",
  "Reduction scores:",
  "- Grammar reduction = pre_grammar - post_grammar",
  "- Vocabulary reduction = pre_vocabulary - post_vocabulary",
  "- Mechanics reduction = pre_mechanics - post_mechanics",
  "",
  "Color palette used in all grouped plots:",
  "- Control = #4F81BD",
  "- Experimental = #C0504D",
  "",
  "Saved tables:",
  "- prepared_dataset_descriptive.csv",
  "- table_group_size.csv",
  "- table_sex_distribution.csv",
  "- table_age_descriptives.csv",
  "- table_sample_profile.csv",
  "- table_holistic_descriptives.csv",
  "- table_holistic_descriptives_wide.csv",
  "- table_error_descriptives.csv",
  "- table_error_descriptives_wide.csv",
  "- table_reduction_descriptives.csv",
  "",
  "Saved plots:",
  "- plot_holistic_means_pre_post.png",
  "- plot_holistic_boxplots_pre_post.png",
  "- plot_error_means_pre_post.png",
  "- plot_error_boxplots_pre_post.png",
  "- plot_reduction_means_by_group.png",
  "- plot_reduction_boxplots_by_group.png"
)

writeLines(summary_lines, con = file.path(output_dir, "README_outputs.txt"))

cat("Done. All descriptive statistics outputs were saved in:", output_dir, "\n")