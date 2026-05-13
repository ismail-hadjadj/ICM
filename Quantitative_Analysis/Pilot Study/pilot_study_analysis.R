# Pilot Study Analysis for Chapter 2
# Source data were transcribed from the pilot-study table in Chapter 2.
# This script reproduces:
#   1) the participant-level pilot dataset
#   2) the descriptive means reported in the chapter
#   3) the exploratory paired Wilcoxon signed-rank results
#
# Notes:
# - The chapter reports Wilcoxon W = 0.00 for all four measures.
# - In base R, wilcox.test() returns the Wilcoxon V statistic for paired data.
#   When all paired differences go in the same direction, V will equal the
#   maximum possible rank sum (21 for n = 6). To reproduce the chapter's W,
#   this script also computes W_chapter = min(V, total_rank_sum - V), which is 0.
# - The chapter table does not provide participant-level sex/age mapping, so the
#   pilot CSV includes only score/error variables that can be extracted directly.

options(stringsAsFactors = FALSE, scipen = 999)

script_dir <- "C:/Users/ismai/OneDrive/Desktop/ICM/Quantitative_Analysis/Pilot Study"  # set manually if needed
input_file <- file.path(script_dir, "pilot_study_data.csv")

if (!file.exists(input_file)) {
  stop("Could not find pilot_study_data.csv in: ", script_dir, call. = FALSE)
}

pilot <- read.csv(input_file)

# ----------------------------
# Derived variables
# ----------------------------
pilot$Holistic_gain      <- pilot$HolisticScore_post - pilot$HolisticScore_pre
pilot$Vocabulary_change  <- pilot$Vocabulary_post - pilot$Vocabulary_pre
pilot$Grammar_change     <- pilot$Grammar_post - pilot$Grammar_pre
pilot$Mechanics_change   <- pilot$Mechanics_post - pilot$Mechanics_pre

# Negative change in errors = fewer errors at post-test.
# Positive gain in holistic score = better performance at post-test.

# ----------------------------
# Descriptive summaries
# ----------------------------
descriptive_summary <- data.frame(
  Measure = c("Overall essay score", "Vocabulary errors", "Grammar errors", "Mechanics errors"),
  Pre_test_M = c(
    mean(pilot$HolisticScore_pre),
    mean(pilot$Vocabulary_pre),
    mean(pilot$Grammar_pre),
    mean(pilot$Mechanics_pre)
  ),
  Post_test_M = c(
    mean(pilot$HolisticScore_post),
    mean(pilot$Vocabulary_post),
    mean(pilot$Grammar_post),
    mean(pilot$Mechanics_post)
  ),
  Mean_change = c(
    mean(pilot$Holistic_gain),
    mean(pilot$Vocabulary_change),
    mean(pilot$Grammar_change),
    mean(pilot$Mechanics_change)
  )
)

# Format mean change to match the dissertation table style
format_change <- function(x) {
  ifelse(x > 0, sprintf("+%.2f", x), sprintf("%.2f", x))
}
descriptive_summary$Mean_change_formatted <- format_change(descriptive_summary$Mean_change)

cat("\n=== Participant-level pilot data ===\n")
print(pilot)

cat("\n=== Descriptive summary (raw numeric) ===\n")
print(descriptive_summary)

cat("\n=== Descriptive summary (chapter-style display) ===\n")
chapter_descriptives <- data.frame(
  Measure = descriptive_summary$Measure,
  Pre_test_M = sprintf("%.2f", descriptive_summary$Pre_test_M),
  Post_test_M = sprintf("%.2f", descriptive_summary$Post_test_M),
  Mean_change = descriptive_summary$Mean_change_formatted
)
print(chapter_descriptives, row.names = FALSE)

# ----------------------------
# Wilcoxon helper
# ----------------------------
paired_wilcox_with_chapter_W <- function(pre, post, measure_name) {
  test <- wilcox.test(pre, post, paired = TRUE, exact = TRUE, correct = FALSE)

  # Base R paired Wilcoxon returns V.
  V <- unname(test$statistic)

  # Total rank sum for n non-zero paired differences
  diffs <- pre - post
  n_nonzero <- sum(diffs != 0)
  total_rank_sum <- n_nonzero * (n_nonzero + 1) / 2

  # Chapter-style W as the smaller of positive vs negative rank sums
  W_chapter <- min(V, total_rank_sum - V)

  data.frame(
    Measure = measure_name,
    Pre_test_M = round(mean(pre), 2),
    Post_test_M = round(mean(post), 2),
    Mean_change = round(mean(post - pre), 2),
    V_R = as.numeric(V),
    W_chapter = as.numeric(W_chapter),
    p_exact = as.numeric(test$p.value),
    stringsAsFactors = FALSE
  )
}

wilcox_results <- rbind(
  paired_wilcox_with_chapter_W(pilot$HolisticScore_pre, pilot$HolisticScore_post, "Overall essay score"),
  paired_wilcox_with_chapter_W(pilot$Vocabulary_pre, pilot$Vocabulary_post, "Vocabulary errors"),
  paired_wilcox_with_chapter_W(pilot$Grammar_pre, pilot$Grammar_post, "Grammar errors"),
  paired_wilcox_with_chapter_W(pilot$Mechanics_pre, pilot$Mechanics_post, "Mechanics errors")
)

cat("\n=== Wilcoxon results (raw) ===\n")
print(wilcox_results)

cat("\n=== Wilcoxon results formatted to match the chapter table ===\n")
chapter_wilcox <- data.frame(
  Measure = wilcox_results$Measure,
  Pre_test_M = sprintf("%.2f", wilcox_results$Pre_test_M),
  Post_test_M = sprintf("%.2f", wilcox_results$Post_test_M),
  Mean_change = ifelse(wilcox_results$Mean_change > 0,
                       sprintf("+%.2f", wilcox_results$Mean_change),
                       sprintf("%.2f", wilcox_results$Mean_change)),
  Wilcoxon_W = sprintf("%.2f", wilcox_results$W_chapter),
  p = sub("^0", "", sprintf("%.3f", wilcox_results$p_exact))
)
print(chapter_wilcox, row.names = FALSE)

# ----------------------------
# Optional CSV exports
# ----------------------------
write.csv(descriptive_summary,
          file = file.path(script_dir, "pilot_descriptive_summary.csv"),
          row.names = FALSE)

write.csv(wilcox_results,
          file = file.path(script_dir, "pilot_wilcox_results.csv"),
          row.names = FALSE)

cat("\nFiles written:\n")
cat(" - ", file.path(script_dir, "pilot_descriptive_summary.csv"), "\n", sep = "")
cat(" - ", file.path(script_dir, "pilot_wilcox_results.csv"), "\n", sep = "")
