# Load both preop assessment CSVs
preop_v1 <- read.csv("RawData/NAP_INOR_PREOP_ASSESSMENT_V1_2025-09-26_14-40-34.csv", stringsAsFactors = FALSE)
preop_v2 <- read.csv("RawData/NAP_INOR_PREOP_ASSESSMENT_V2_2025-09-26_14-44-34.csv", stringsAsFactors = FALSE)

# Select only the columns needed for the merge
cols_to_add <- c("FORM_RESPONSE_GROUP_ID", "Height", "Weight", "BMI",
                 "Admitting.consultant", "Procedure.type")

preop_v1_subset <- preop_v1[, cols_to_add, drop = FALSE]
preop_v2_subset <- preop_v2[, cols_to_add, drop = FALSE]

# Combine both versions into one dataframe
preop_combined <- rbind(preop_v1_subset, preop_v2_subset)

# Remove duplicate FORM_RESPONSE_GROUP_IDs (keep first occurrence)
preop_combined <- preop_combined[!duplicated(preop_combined$FORM_RESPONSE_GROUP_ID), ]

# Merge into the Oxford Knee Score dataframe
Oxford_Hip <- merge(
  Oxford_Hip,
  preop_combined,
  by = "FORM_RESPONSE_GROUP_ID",
  all.x = TRUE  # left join: keep all rows from the knee score df
)

columns_to_keep <- c(
  "FORM_RESPONSE_GROUP_ID",
  "Event date",
  "Laterality",
  "Stage",
  "1: How would you describe the pain you usually have from your hip?",
  "2: Have you had any trouble with washing and drying yourself (all over) because of your hip?",
  "3: Have you had any trouble getting in and out of a car or using public transport because of your hip? (whichever you tend to use)",
  "4: Have you been able to put on a pair of socks, stockings or tights?",
  "5: Could you do the household shopping on your own?",
  "6: For how long have you been able to walk before pain from your hip becomes severe? (with or without a stick)",
  "7: Have you been able to climb a flight of stairs?",
  "8: After a meal (sat at a table), how painful has it been for you to stand up from a chair because of your hip?",
  "9: Have you been limping when walking, because of your hip?",
  "10: Have you had any sudden, severe pain - 'shooting', 'stabbing' or 'spasms' - from the affected hip?",
  "11: How much has pain from your hip interfered with your usual work (including housework)?",
  "12: Have you been troubled by pain from your hip in bed at night?",
  "Score",
  "MRN Number",
  "Date of Birth",
  "Height",
  "Weight",
  "BMI",
  "Admitting.consultant",
  "Procedure.type"
)

Oxford_Hip <- Oxford_Hip %>%
  select(all_of(columns_to_keep))

#write_csv(Oxford_Hip_pivoted, "Output/Oxford_Hip.csv")

