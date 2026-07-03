# =============================================================================
# R Script: Format NAP_INOR_OXFORD_KNEE_SCORE Data
# =============================================================================
# Reads the CSV export and converts each column to its appropriate R data type
# (character, integer, factor, Date, POSIXct, logical, ordered factor).
# =============================================================================

library(readr)
library(dplyr)
library(lubridate)

# ---- 1. Read raw data (everything as character first to avoid silent coercion)
df <- read_csv(
  "RawData/NAP_INOR_OXFORD_KNEE_SCORE_2026-03-26_11-18-08.csv",
  col_types = cols(.default = col_character())
)

# ---- 2. Helper: convert Excel serial date numbers to R Date ----
excel_serial_to_date <- function(x) {
  nums <- suppressWarnings(as.numeric(x))
  as.Date(nums, origin = "1899-12-30")
}

# ---- 3. Integer ID columns ----
id_cols <- c(
  "Patient Id",
  "FORM_RESPONSE_GROUP_ID",
  "FORM_RESPONSE_ID",
  "CREATED_BY_ID",
  "LAST_MODIFIED_BY_ID",
  "SUBMITTED_BY_ID",
  "ACCESS_POINT_ID",
  "HOSPITAL_ID"
)

df <- df %>%
  mutate(across(all_of(id_cols), as.integer))

# ---- 4. Datetime columns (format: "dd/mm/yyyyThh:mm") ----
datetime_cols <- c(
  "CREATED",
  "LAST_MODIFIED_DATE",
  "SUBMIT_DATE"
)

df <- df %>%
  mutate(across(all_of(datetime_cols), ~ dmy_hm(.x)))

# ---- 5. Date columns stored as Excel serial numbers ----
date_columns <- c(
  "KEY_DATE",
  "Event date",
  "Date of Birth",
  "Date of Death",
  "Participate Consent Date",
  "Anonymised Consent Date",
  "Personal Consent Date"
)

df <- df %>%
  mutate(
    across(
      all_of(date_columns),
      ~ dmy(.x)
    )
  )


# ---- 6. Numeric / integer columns ----
df <- df %>%
  mutate(
    Score                      = as.integer(Score),
    `Patient Age`              = as.integer(`Patient Age`),
    `Is Patient From Overseas`  = as.integer(`Is Patient From Overseas`)
  )

# ---- 7. Logical / boolean column ----
df <- df %>%
  mutate(
    SUBMITTED = case_when(
      SUBMITTED == "Y" ~ TRUE,
      SUBMITTED == "N" ~ FALSE,
      TRUE             ~ NA
    )
  )

# ---- 8. Factor columns ----
df$FORM_NAME  <- as.factor(df$FORM_NAME)
df$FORM_STATE <- factor(df$FORM_STATE,
                        levels = c("Draft", "Submitted", "Approved", "Rejected"))
df$Sex <- factor(df$Sex,
                 levels = c("Male", "Female", "Other", "Unknown"))
df$Laterality <- factor(df$Laterality,
                        levels = c("Left", "Right", "Bilateral"))
df$Stage <- factor(df$Stage)

# Consent Yes/No columns
consent_cols <- c(
  "Consent to participate in the INOR register",
  "Consent for anonymised data to be used for research",
  "Consent for pseudonymised data to be used for research"
)
df <- df %>%
  mutate(across(all_of(consent_cols), ~ factor(.x, levels = c("Yes", "No"))))

# Method of consent columns
consent_method_cols <- c(
  "Participate Method of Consent",
  "Anonymised Method of Consent",
  "Personal Method of Consent"
)
df <- df %>%
  mutate(across(all_of(consent_method_cols),
                ~ factor(.x, levels = c("Written", "Verbal", "Electronic"))))

# ---- 9. Oxford Knee Score Q1-Q12: ordered factors (worst -> best) ----

df$`1: How would you describe the pain you usually have from your knee?` <- factor(
  df$`1: How would you describe the pain you usually have from your knee?`,
  levels = c("Severe", "Moderate", "Mild", "Very mild", "None"),
  ordered = TRUE)

df$`2: Have you had any trouble with washing and drying yourself (all over) because of your knee?` <- factor(
  df$`2: Have you had any trouble with washing and drying yourself (all over) because of your knee?`,
  levels = c("Impossible to do", "Extreme difficulty", "Moderate trouble", "Very little trouble",
             "No trouble at all"),
  ordered = TRUE)

df$`3: Have you had any trouble getting in and out of a car or using public transport because of your knee? (whichever you would tend to use)` <- factor(
  df$`3: Have you had any trouble getting in and out of a car or using public transport because of your knee? (whichever you would tend to use)`,
  levels = c("Impossible to do", "Extreme difficulty", "Moderate trouble","Very little trouble",
             "No trouble at all"),
  ordered = TRUE)

df$`4: For how long have you been able to walk before pain from your knee becomes severe? (with or without a stick)` <- factor(
  df$`4: For how long have you been able to walk before pain from your knee becomes severe? (with or without a stick)`,
  levels = c("Not at all/pain severe on walking", "Around the house only",
             "5-15 minutes", "16-30 minutes",
             "No pain/More than 30 minutes"),
  ordered = TRUE)

df$`5: After a meal (sat at a table), how painful has it been for you to stand up from a chair because of your knee?` <- factor(
  df$`5: After a meal (sat at a table), how painful has it been for you to stand up from a chair because of your knee?`,
  levels = c("Unbearable", "Very painful", "Moderately painful",
             "Slightly painful", "Not at all painful"),
  ordered = TRUE)

df$`6: Have you been limping when walking, because of your knee?` <- factor(
  df$`6: Have you been limping when walking, because of your knee?`,
  levels = c("All of the time", "Most of the time",
             "Often, not just at first", "Sometimes, or just at first",
             "Rarely/ never"),
  ordered = TRUE)

df$`7: Could you kneel down and get up again afterwards?` <- factor(
  df$`7: Could you kneel down and get up again afterwards?`,
  levels = c("No, impossible", "With extreme difficulty",
             "With moderate difficulty", "With little difficulty",
             "Yes, easily"),
  ordered = TRUE)

df$`8: Have you been troubled by pain from your knee in bed at night?` <- factor(
  df$`8: Have you been troubled by pain from your knee in bed at night?`,
  levels = c("Every night", "Most nights", "Some nights",
             "Only 1 or 2 nights", "No nights"),
  ordered = TRUE)

df$`9: How much has pain from your knee interfered with your usual work (including housework)?` <- factor(
  df$`9: How much has pain from your knee interfered with your usual work (including housework)?`,
  levels = c("Totally", "Greatly", "Moderately", "A little bit", "Not at all"),
  ordered = TRUE)

df$`10: Have you felt that your knee might suddenly 'give way' or let you down?` <- factor(
  df$`10: Have you felt that your knee might suddenly 'give way' or let you down?`,
  levels = c("All of the time", "Most of the time",
             "Often, not just at first", "Sometimes, or just at first",
             "Rarely/ never"),
  ordered = TRUE)

df$`11: Could you do the household shopping on your own?` <- factor(
  df$`11: Could you do the household shopping on your own?`,
  levels = c("No, impossible", "With extreme difficulty",
             "With moderate difficulty", "With little difficulty",
             "Yes, easily"),
  ordered = TRUE)

df$`12: Could you walk down one flight of stairs?` <- factor(
  df$`12: Could you walk down one flight of stairs?`,
  levels = c("No, impossible", "With extreme difficulty",
             "With moderate difficulty", "With little difficulty",
             "Yes, easily"),
  ordered = TRUE)

# ---- 10. Remaining columns stay as character ----
#   Names, addresses, phone numbers, MRN, ODS codes, free-text fields

# ---- 11. Verify structure ----
cat("=== Data Dimensions ===\n")
cat(sprintf("Rows: %d | Columns: %d\n\n", nrow(df), ncol(df)))

cat("=== Column Types Summary ===\n")
type_summary <- sapply(df, function(col) {
  cl <- class(col)
  if (length(cl) > 1) paste(cl, collapse = "/") else cl
})
print(table(type_summary))

cat("\n=== Full Column Type Listing ===\n")
for (i in seq_along(df)) {
  col_class <- paste(class(df[[i]]), collapse = "/")
  cat(sprintf("  %-70s : %s\n", names(df)[i], col_class))
}


Oxford_Knee <- df
