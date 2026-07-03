#PIVOT
Oxford_Knee_pivoted <- Oxford_Knee %>%
  filter(Procedure.type == "Primary") %>%
  # Keep only the most recent entry when a Stage is duplicated
  arrange(FORM_RESPONSE_GROUP_ID, Stage, desc(`Event date`)) %>%
  distinct(FORM_RESPONSE_GROUP_ID, Stage, .keep_all = TRUE) %>%
  # Now pivot
  select(FORM_RESPONSE_GROUP_ID, `MRN Number`, `Date of Birth`,
         Height, BMI, Admitting.consultant, Procedure.type, Stage, Score) %>%
  pivot_wider(
    id_cols = c(FORM_RESPONSE_GROUP_ID, `MRN Number`, `Date of Birth`,
                Height, BMI, Admitting.consultant, Procedure.type),
    names_from = Stage,
    values_from = Score,
    names_prefix = "Score_"
  )

# ---------------------------------------------------------
# 1. Descriptive stats for each score time-point
#    Median (IQR) and Mean (SD) by Admitting Consultant
# ---------------------------------------------------------

score_cols <- c("Score_Pre-op presentation",
                "Score_6 month presentation",
                "Score_2 year presentation",
                "Score_5 year presentation",
                "Score_Extraordinary presentation")

timepoint_stats <- Oxford_Knee_pivoted %>%
  pivot_longer(cols = all_of(score_cols),
               names_to = "Timepoint",
               values_to = "Score") %>%
  filter(!is.na(Score)) %>%
  group_by(Admitting.consultant, Timepoint) %>%
  summarise(
    n        = n(),
    Mean     = mean(Score),
    SD       = sd(Score),
    Median   = median(Score),
    Q1       = quantile(Score, 0.25),
    Q3       = quantile(Score, 0.75),
    IQR      = IQR(Score),
    Mean_SD  = paste0(round(Mean, 1), " (", round(SD, 1), ")"),
    Median_IQR = paste0(round(Median, 1), " (", round(Q1, 1), "–", round(Q3, 1), ")"),
    .groups  = "drop"
  )

timepoint_stats_wide <- timepoint_stats %>%
  mutate(Timepoint = str_remove(Timepoint, "^Score_") %>%
           str_remove(" presentation")) %>%
  select(Admitting.consultant, Timepoint, Mean_SD, Median_IQR) %>%
  pivot_wider(
    names_from  = Timepoint,
    values_from = c(Mean_SD, Median_IQR),
    names_glue  = "{Timepoint}_{.value}"
  ) %>%
  select(
    Admitting.consultant,
    `Pre-op_Mean_SD`, `Pre-op_Median_IQR`,
    `6 month_Mean_SD`, `6 month_Median_IQR`,
    `2 year_Mean_SD`, `2 year_Median_IQR`,
    `5 year_Mean_SD`, `5 year_Median_IQR`,
    `Extraordinary_Mean_SD`, `Extraordinary_Median_IQR`
  )

print(timepoint_stats_wide, n = Inf)

# ---------------------------------------------------------
# 2. Score change (interval) between time-points
#    Each difference is calculated relative to Pre-op
# ---------------------------------------------------------

Oxford_Knee_pivoted <- Oxford_Knee_pivoted %>%
  mutate(
    Change_PreOp_to_6mo = `Score_6 month presentation` - `Score_Pre-op presentation`,
    Change_PreOp_to_2yr = `Score_2 year presentation`  - `Score_Pre-op presentation`,
    Change_PreOp_to_5yr = `Score_5 year presentation`  - `Score_Pre-op presentation`
  )

change_cols <- c("Change_PreOp_to_6mo",
                 "Change_PreOp_to_2yr",
                 "Change_PreOp_to_5yr")

change_stats <- Oxford_Knee_pivoted %>%
  pivot_longer(cols = all_of(change_cols),
               names_to = "Interval",
               values_to = "Change") %>%
  filter(!is.na(Change)) %>%
  group_by(Admitting.consultant, Interval) %>%
  summarise(
    n        = n(),
    Mean     = mean(Change),
    SD       = sd(Change),
    Median   = median(Change),
    Q1       = quantile(Change, 0.25),
    Q3       = quantile(Change, 0.75),
    IQR      = IQR(Change),
    Mean_SD  = paste0(round(Mean, 1), " (", round(SD, 1), ")"),
    Median_IQR = paste0(round(Median, 1), " (", round(Q1, 1), "–", round(Q3, 1), ")"),
    .groups  = "drop"
  )

change_stats_wide <- change_stats %>%
  select(Admitting.consultant, Interval, Mean_SD, Median_IQR) %>%
  pivot_wider(
    names_from  = Interval,
    values_from = c(Mean_SD, Median_IQR),
    names_glue  = "{Interval}_{.value}"
  ) %>%
  rename(
    Change_PreOp_to_6mo_Mean_SD     = Change_PreOp_to_6mo_Mean_SD,
    Change_PreOp_to_6mo_Median_IQR  = Change_PreOp_to_6mo_Median_IQR,
    Change_PreOp_to_2yr_Mean_SD     = Change_PreOp_to_2yr_Mean_SD,
    Change_PreOp_to_2yr_Median_IQR  = Change_PreOp_to_2yr_Median_IQR,
    Change_PreOp_to_5yr_Mean_SD     = Change_PreOp_to_5yr_Mean_SD,
    Change_PreOp_to_5yr_Median_IQR  = Change_PreOp_to_5yr_Median_IQR
  ) %>%
  select(
    Admitting.consultant,
    Change_PreOp_to_6mo_Mean_SD, Change_PreOp_to_6mo_Median_IQR,
    Change_PreOp_to_2yr_Mean_SD, Change_PreOp_to_2yr_Median_IQR,
    Change_PreOp_to_5yr_Mean_SD, Change_PreOp_to_5yr_Median_IQR
  )

print(change_stats_wide, n = Inf)


#PASS
# ---------------------------------------------------------
# Percentage of patients with Score_6 month >= 30
# by Admitting Consultant
# ---------------------------------------------------------

pct_6mo_ge30 <- Oxford_Knee_pivoted %>%
  filter(!is.na(`Score_6 month presentation`)) %>%
  group_by(Admitting.consultant) %>%
  summarise(
    n_total   = n(),
    n_ge30    = sum(`Score_6 month presentation` >= 30),
    pct_ge30  = round((n_ge30 / n_total) * 100, 1),
    .groups   = "drop"
  ) %>%
  arrange(desc(pct_ge30))

print(pct_6mo_ge30, n = Inf)

# ---------------------------------------------------------
# Percentage of patients whose post-op score is greater
# than their pre-op score, by Admitting Consultant
# ---------------------------------------------------------

# Calculate whether each post-op score > pre-op score
Oxford_Knee_pivoted <- Oxford_Knee_pivoted %>%
  mutate(
    improved_6mo = `Score_6 month presentation` > `Score_Pre-op presentation`,
    improved_2yr = `Score_2 year presentation`  > `Score_Pre-op presentation`,
    improved_5yr = `Score_5 year presentation`  > `Score_Pre-op presentation`
  )

# Pivot to long format for easy grouped summaries
improvement_stats <- Oxford_Knee_pivoted %>%
  pivot_longer(
    cols      = c(improved_6mo, improved_2yr, improved_5yr),
    names_to  = "Timepoint",
    values_to = "Improved"
  ) %>%
  filter(!is.na(Improved)) %>%
  group_by(Admitting.consultant, Timepoint) %>%
  summarise(
    n_total     = n(),
    n_improved  = sum(Improved),
    pct_improved = round((n_improved / n_total) * 100, 1),
    .groups     = "drop"
  ) %>%
  mutate(
    Timepoint = case_match(
      Timepoint,
      "improved_6mo" ~ "6 months",
      "improved_2yr" ~ "2 years",
      "improved_5yr" ~ "5 years"
    ),
    Timepoint = factor(Timepoint, levels = c("6 months", "2 years", "5 years"))
  ) %>%
  arrange(Admitting.consultant, Timepoint)

print(improvement_stats, n = Inf)

# ---------------------------------------------------------
# Optional: wider format for a cleaner comparison table
# ---------------------------------------------------------

improvement_wide <- improvement_stats %>%
  pivot_wider(
    id_cols     = Admitting.consultant,
    names_from  = Timepoint,
    values_from = c(n_total, n_improved, pct_improved),
    names_glue  = "{Timepoint}_{.value}"
  )

print(improvement_wide, n = Inf)
