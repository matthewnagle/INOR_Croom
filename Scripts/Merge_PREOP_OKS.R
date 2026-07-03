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
Oxford_Knee <- merge(
  Oxford_Knee,
  preop_combined,
  by = "FORM_RESPONSE_GROUP_ID",
  all.x = TRUE  # left join: keep all rows from the knee score df
)

columns_to_keep <- c(
  "FORM_RESPONSE_GROUP_ID",
  "Event date",
  "Laterality",
  "Stage",
  "1: How would you describe the pain you usually have from your knee?",
  "2: Have you had any trouble with washing and drying yourself (all over) because of your knee?",
  "3: Have you had any trouble getting in and out of a car or using public transport because of your knee? (whichever you would tend to use)",
  "4: For how long have you been able to walk before pain from your knee becomes severe? (with or without a stick)",
  "5: After a meal (sat at a table), how painful has it been for you to stand up from a chair because of your knee?",
  "6: Have you been limping when walking, because of your knee?",
  "7: Could you kneel down and get up again afterwards?",
  "8: Have you been troubled by pain from your knee in bed at night?",
  "9: How much has pain from your knee interfered with your usual work (including housework)?",
  "10: Have you felt that your knee might suddenly 'give way' or let you down?",
  "11: Could you do the household shopping on your own?",
  "12: Could you walk down one flight of stairs?",
  "Score",
  "MRN Number",
  "Date of Birth",
  "Height",
  "Weight",
  "BMI",
  "Admitting.consultant",
  "Procedure.type"
)

Oxford_Knee <- Oxford_Knee %>%
  select(all_of(columns_to_keep))

write_csv(Oxford_Knee, "Output/Oxford_Knee.csv")
