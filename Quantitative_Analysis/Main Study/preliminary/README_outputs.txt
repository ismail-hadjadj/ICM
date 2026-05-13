Preliminary Data Screening and Assumption Checking completed.

Analytical variables were constructed as follows:
- pre_mechanics  = Spelling_pre + Punctuation_pre + Capitalization_pre
- post_mechanics = Spelling_post + Punctuation_post + Capitalization_post
- pre_grammar    = Tense_pre + Agreement_pre + Article_pronoun_Preposition_pre
- post_grammar   = Tense_post + Agreement_post + Article_pronoun_Preposition_post
- pre_vocabulary = Vocabulary_pre
- post_vocabulary = Vocabulary_post
- pre_holistic   = HolisticScore_pre
- post_holistic  = HolisticScore_post
- grammar_reduction = pre_grammar - post_grammar
- vocabulary_reduction = pre_vocabulary - post_vocabulary
- mechanics_reduction = pre_mechanics - post_mechanics

Color palette used in grouped plots:
- Control = #4F81BD
- Experimental = #C0504D

Saved files:
- prepared_dataset.csv
- data_screening_summary.csv
- missing_values_by_variable.csv
- outlier_summary_iqr.csv
- outlier_details_iqr.csv
- preliminary_descriptives_overall.csv
- preliminary_descriptives_by_group.csv
- ancova_assumptions_summary.csv
- h1_homogeneity_of_regression_slopes_anova.csv
- h1_ancova_table_type3.csv
- h1_residual_normality_shapiro.csv
- h1_levene_post_holistic.csv
- h1_scatter_pre_post_by_group.png
- h1_ancova_residuals_qqplot.png
- h1_ancova_residuals_histogram.png
- error_reduction_normality_shapiro.csv
- error_reduction_levene_tests.csv
- error_reduction_descriptives.csv
- h2_reduction_histograms.png
- h2_reduction_boxplots.png
