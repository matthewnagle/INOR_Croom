################################################################################
# combine_and_summarise_INOR.R
#
# Purpose : Read INOR Peri-Op Assessment V1 and V2 CSVs, harmonise columns,
#           combine into one dataset, and produce summary tables.
#
# Inputs  : Two CSV exports from NAP – one V1, one V2.
# Outputs : Combined CSV + printed summary tables to console (and optional HTML).
#
# Usage   : Update the two file paths below, then:
#             Rscript combine_and_summarise_INOR.R
#           or source() in RStudio.
################################################################################

# ── 0. Packages ──────────────────────────────────────────────────────────────
if (!require("dplyr"))    install.packages("dplyr",    repos = "https://cloud.r-project.org")
if (!require("readr"))    install.packages("readr",    repos = "https://cloud.r-project.org")
if (!require("tidyr"))    install.packages("tidyr",    repos = "https://cloud.r-project.org")
if (!require("stringr"))  install.packages("stringr",  repos = "https://cloud.r-project.org")
if (!require("janitor"))  install.packages("janitor",  repos = "https://cloud.r-project.org")

library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(janitor)

# ── 1. File paths  (EDIT THESE) ─────────────────────────────────────────────
file_v1 <- "RawData/NAP_INOR_PERI_OP_ASSESSMENT_V1_2026-03-26_11-05-31.csv"
file_v2 <- "RawData/NAP_INOR_PERI_OP_ASSESSMENT_V2_2026-03-26_11-07-17.csv"

# ── 2. Read raw data ────────────────────────────────────────────────────────
raw_v1 <- read_csv(file_v1, show_col_types = FALSE, guess_max = 5000)
raw_v2 <- read_csv(file_v2, show_col_types = FALSE, guess_max = 5000)

cat("V1 rows:", nrow(raw_v1), " | columns:", ncol(raw_v1), "\n")
cat("V2 rows:", nrow(raw_v2), " | columns:", ncol(raw_v2), "\n")

# ── 3. Tag source version ───────────────────────────────────────────────────
raw_v1 <- raw_v1 %>% mutate(Form_Version = "V1")
raw_v2 <- raw_v2 %>% mutate(Form_Version = "V2")

# ── 4. Harmonise column names ───────────────────────────────────────────────
# V1 and V2 share most clinical columns but have some naming differences.
# We rename the V1-specific variants so they match the V2 convention,
# then bind rows (unmatched columns become NA automatically).

rename_v1 <- c(
  # Surgeon naming
  "Lead Operating surgeon name"       = "Operating surgeon",
  "Lead Operating surgeon grade"       = "Operating surgeon grade",
  # Tranexamic acid – V1 has a typo ("Transexamic")
  "Tranexamic acid used (Left)"        = "Transexamic acid used (Left)",
  "Tranexamic acid used (Right)"       = "Transexamic acid used (Right)",
  # Position on table – V1 is generic; V2 splits by joint
  "Hip - Position on Table (Left)"     = "Position on Table (Left)",
  "Hip - Position on Table (Right)"    = "Position on table (Right)"
)

# Only rename columns that actually exist in V1
rename_v1_exists <- rename_v1[rename_v1 %in% names(raw_v1)]
if (length(rename_v1_exists) > 0) {
  raw_v1 <- raw_v1 %>% rename(!!!rename_v1_exists)
}

# V2 also has some columns that V1 lacks (e.g. "Revision of", "First or Second Stage",
# Technology Assisted, Robots, etc.) – these will simply be NA for V1 rows after binding.

# ── 5. Combine ──────────────────────────────────────────────────────────────
combined <- bind_rows(raw_v1, raw_v2)
cat("\nCombined dataset:", nrow(combined), "rows |", ncol(combined), "columns\n\n")

# ── 6. Clean key variables ──────────────────────────────────────────────────

# Helper: coerce Excel-serial dates OR dd/mm/yyyy strings to Date
parse_date_flex <- function(x) {
  # Try numeric (Excel serial) first
  num <- suppressWarnings(as.numeric(x))
  out <- as.Date(rep(NA, length(x)))
  # Excel serial dates (origin 1899-12-30)
  idx_num <- !is.na(num) & num > 20000 & num < 60000
  out[idx_num] <- as.Date(num[idx_num], origin = "1899-12-30")
  # dd/mm/yyyy strings
  idx_str <- is.na(out)
  out[idx_str] <- as.Date(x[idx_str], format = "%d/%m/%Y")
  out
}

combined <- combined %>%
  mutate(
    `Date of surgery`              = parse_date_flex(`Date of surgery`),
    `Patient age at time of surgery` = as.numeric(`Patient age at time of surgery`),
    Joint          = str_to_title(trimws(Joint)),
    `Procedure type` = str_to_title(trimws(`Procedure type`)),
    Laterality     = str_to_title(trimws(Laterality)),
    Sex            = str_to_title(trimws(Sex)),
    Hospital       = coalesce(ACCESS_POINT_NAME, HOSPITAL_NAME),
    ASA            = trimws(`American Society of Anesthesiologists (ASA grade)`),
    Consultant     = trimws(`Admitting consultant`)
  )

# ── 7. Summary tables ───────────────────────────────────────────────────────
#
# Every summary is produced TWICE:
#   • Overall (all consultants pooled)
#   • By Admitting Consultant
#
# A helper wraps this pattern so we don't duplicate code everywhere.

divider <- function(title) {
  cat(strrep("=", 70), "\n")
  cat(" ", title, "\n")
  cat(strrep("=", 70), "\n")
}

sub_divider <- function(title) {
  cat(strrep("-", 60), "\n")
  cat("  ", title, "\n")
  cat(strrep("-", 60), "\n")
}

# ── 7a. Record counts by Form Version ───────────────────────────────────────
divider("7a. Record counts by Form Version")
combined %>% count(Form_Version, name = "n_records") %>% print(n = Inf)

sub_divider("By Consultant")
combined %>%
  count(Consultant, Form_Version, name = "n_records") %>%
  arrange(Consultant) %>%
  print(n = Inf)

# ── 7b. Joint × Procedure type ──────────────────────────────────────────────
divider("7b. Procedures by Joint and Procedure Type")
combined %>%
  count(Joint, `Procedure type`, name = "n") %>%
  arrange(Joint, desc(n)) %>%
  print(n = Inf)

sub_divider("By Consultant")
combined %>%
  count(Consultant, Joint, `Procedure type`, name = "n") %>%
  arrange(Consultant, Joint, desc(n)) %>%
  print(n = Inf)

# ── 7c. Joint × Laterality ──────────────────────────────────────────────────
divider("7c. Procedures by Joint and Laterality")
combined %>%
  count(Joint, Laterality, name = "n") %>%
  print(n = Inf)

sub_divider("By Consultant")
combined %>%
  count(Consultant, Joint, Laterality, name = "n") %>%
  arrange(Consultant) %>%
  print(n = Inf)

# ── 7d. Hospital volume ─────────────────────────────────────────────────────
divider("7d. Procedures by Hospital")
combined %>%
  count(Hospital, name = "n") %>%
  arrange(desc(n)) %>%
  print(n = Inf)

sub_divider("By Consultant × Hospital")
combined %>%
  count(Consultant, Hospital, name = "n") %>%
  arrange(Consultant, desc(n)) %>%
  print(n = Inf)

# ── 7e. ASA grade distribution ──────────────────────────────────────────────
divider("7e. ASA Grade Distribution")
combined %>%
  count(ASA, name = "n") %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  print(n = Inf)

sub_divider("By Consultant")
combined %>%
  group_by(Consultant) %>%
  count(ASA, name = "n") %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  ungroup() %>%
  arrange(Consultant, ASA) %>%
  print(n = Inf)

# ── 7f. Patient demographics ────────────────────────────────────────────────
divider("7f. Patient Demographics")
combined %>%
  summarise(
    n          = n(),
    mean_age   = round(mean(`Patient age at time of surgery`, na.rm = TRUE), 1),
    sd_age     = round(sd(`Patient age at time of surgery`, na.rm = TRUE), 1),
    median_age = median(`Patient age at time of surgery`, na.rm = TRUE),
    min_age    = min(`Patient age at time of surgery`, na.rm = TRUE),
    max_age    = max(`Patient age at time of surgery`, na.rm = TRUE)
  ) %>%
  print()

cat("\nAge by Sex:\n")
combined %>%
  group_by(Sex) %>%
  summarise(
    n        = n(),
    mean_age = round(mean(`Patient age at time of surgery`, na.rm = TRUE), 1),
    sd_age   = round(sd(`Patient age at time of surgery`, na.rm = TRUE), 1),
    .groups  = "drop"
  ) %>%
  print(n = Inf)

sub_divider("By Consultant")
combined %>%
  group_by(Consultant) %>%
  summarise(
    n          = n(),
    mean_age   = round(mean(`Patient age at time of surgery`, na.rm = TRUE), 1),
    sd_age     = round(sd(`Patient age at time of surgery`, na.rm = TRUE), 1),
    median_age = median(`Patient age at time of surgery`, na.rm = TRUE),
    min_age    = min(`Patient age at time of surgery`, na.rm = TRUE),
    max_age    = max(`Patient age at time of surgery`, na.rm = TRUE),
    .groups    = "drop"
  ) %>%
  arrange(Consultant) %>%
  print(n = Inf)

cat("\nAge by Consultant × Sex:\n")
combined %>%
  group_by(Consultant, Sex) %>%
  summarise(
    n        = n(),
    mean_age = round(mean(`Patient age at time of surgery`, na.rm = TRUE), 1),
    .groups  = "drop"
  ) %>%
  arrange(Consultant, Sex) %>%
  print(n = Inf)

# ── 7g. Anaesthesia types ───────────────────────────────────────────────────
divider("7g. Anaesthesia Type Frequency")
anaes_cols <- names(combined) %>% str_subset("^Anaesthesia type:")
if (length(anaes_cols) > 0) {
  combined %>%
    summarise(across(all_of(anaes_cols), ~ sum(. == "Y", na.rm = TRUE))) %>%
    pivot_longer(everything(), names_to = "Anaesthesia_Type", values_to = "n") %>%
    mutate(Anaesthesia_Type = str_remove(Anaesthesia_Type, "^Anaesthesia type:")) %>%
    arrange(desc(n)) %>%
    print(n = Inf)

  sub_divider("By Consultant")
  combined %>%
    group_by(Consultant) %>%
    summarise(across(all_of(anaes_cols), ~ sum(. == "Y", na.rm = TRUE)),
              .groups = "drop") %>%
    pivot_longer(-Consultant, names_to = "Anaesthesia_Type", values_to = "n") %>%
    mutate(Anaesthesia_Type = str_remove(Anaesthesia_Type, "^Anaesthesia type:")) %>%
    filter(n > 0) %>%
    arrange(Consultant, desc(n)) %>%
    print(n = Inf)
}

# ── 7h. Antibiotic usage ────────────────────────────────────────────────────
divider("7h. Antibiotic Usage")
combined %>% count(Antibiotics, name = "n") %>% print(n = Inf)

abx_cols <- names(combined) %>% str_subset("^Antibiotics type:")
if (length(abx_cols) > 0) {
  cat("\nAntibiotic types administered:\n")
  combined %>%
    summarise(across(all_of(abx_cols), ~ sum(. == "Y", na.rm = TRUE))) %>%
    pivot_longer(everything(), names_to = "Antibiotic", values_to = "n") %>%
    mutate(Antibiotic = str_remove(Antibiotic, "^Antibiotics type:")) %>%
    filter(n > 0) %>%
    arrange(desc(n)) %>%
    print(n = Inf)

  sub_divider("By Consultant")
  combined %>%
    group_by(Consultant) %>%
    summarise(across(all_of(abx_cols), ~ sum(. == "Y", na.rm = TRUE)),
              .groups = "drop") %>%
    pivot_longer(-Consultant, names_to = "Antibiotic", values_to = "n") %>%
    mutate(Antibiotic = str_remove(Antibiotic, "^Antibiotics type:")) %>%
    filter(n > 0) %>%
    arrange(Consultant, desc(n)) %>%
    print(n = Inf)
}

# ── 7i. Chemical thromboprophylaxis ─────────────────────────────────────────
divider("7i. Chemical Thromboprophylaxis")
chem_cols <- names(combined) %>% str_subset("^Thromboprophylaxis chemical type")
if (length(chem_cols) > 0) {
  combined %>%
    summarise(across(all_of(chem_cols), ~ sum(. %in% c("Y", "Yes"), na.rm = TRUE))) %>%
    pivot_longer(everything(), names_to = "Agent", values_to = "n") %>%
    mutate(Agent = str_remove(Agent, "^Thromboprophylaxis chemical type \\((Left|Right)\\):")) %>%
    group_by(Agent) %>%
    summarise(n = sum(n), .groups = "drop") %>%
    filter(n > 0) %>%
    arrange(desc(n)) %>%
    print(n = Inf)

  sub_divider("By Consultant")
  combined %>%
    group_by(Consultant) %>%
    summarise(across(all_of(chem_cols), ~ sum(. %in% c("Y", "Yes"), na.rm = TRUE)),
              .groups = "drop") %>%
    pivot_longer(-Consultant, names_to = "Agent", values_to = "n") %>%
    mutate(Agent = str_remove(Agent, "^Thromboprophylaxis chemical type \\((Left|Right)\\):")) %>%
    group_by(Consultant, Agent) %>%
    summarise(n = sum(n), .groups = "drop") %>%
    filter(n > 0) %>%
    arrange(Consultant, desc(n)) %>%
    print(n = Inf)
}

# ── 7j. Mechanical thromboprophylaxis ───────────────────────────────────────
divider("7j. Mechanical Thromboprophylaxis")
mech_cols <- names(combined) %>% str_subset("^Thromboprophylaxis mechanical type")
if (length(mech_cols) > 0) {
  combined %>%
    summarise(across(all_of(mech_cols), ~ sum(. %in% c("Y", "Yes"), na.rm = TRUE))) %>%
    pivot_longer(everything(), names_to = "Device", values_to = "n") %>%
    mutate(Device = str_remove(Device, "^Thromboprophylaxis mechanical type \\((Left|Right)\\):")) %>%
    group_by(Device) %>%
    summarise(n = sum(n), .groups = "drop") %>%
    filter(n > 0) %>%
    arrange(desc(n)) %>%
    print(n = Inf)

  sub_divider("By Consultant")
  combined %>%
    group_by(Consultant) %>%
    summarise(across(all_of(mech_cols), ~ sum(. %in% c("Y", "Yes"), na.rm = TRUE)),
              .groups = "drop") %>%
    pivot_longer(-Consultant, names_to = "Device", values_to = "n") %>%
    mutate(Device = str_remove(Device, "^Thromboprophylaxis mechanical type \\((Left|Right)\\):")) %>%
    group_by(Consultant, Device) %>%
    summarise(n = sum(n), .groups = "drop") %>%
    filter(n > 0) %>%
    arrange(Consultant, desc(n)) %>%
    print(n = Inf)
}

# ── 7k. Complications ───────────────────────────────────────────────────────
divider("7k. Intraoperative Complications")
compl_cols <- names(combined) %>%
  str_subset("^(Knee - Complications |Hip - Complications? |Complications )\\(")
if (length(compl_cols) > 0) {
  combined %>%
    summarise(across(all_of(compl_cols),
                     ~ sum(. %in% c("Y", "Yes"), na.rm = TRUE))) %>%
    pivot_longer(everything(), names_to = "Complication_Flag", values_to = "n") %>%
    filter(n > 0) %>%
    print(n = Inf)

  sub_divider("By Consultant")
  combined %>%
    group_by(Consultant) %>%
    summarise(across(all_of(compl_cols),
                     ~ sum(. %in% c("Y", "Yes"), na.rm = TRUE)),
              .groups = "drop") %>%
    pivot_longer(-Consultant, names_to = "Complication_Flag", values_to = "n") %>%
    filter(n > 0) %>%
    arrange(Consultant) %>%
    print(n = Inf)
}

# Complication sub-types
compl_type_cols <- names(combined) %>% str_subset("Complications? type")
if (length(compl_type_cols) > 0) {
  cat("\nComplication sub-types:\n")
  combined %>%
    summarise(across(all_of(compl_type_cols),
                     ~ sum(. %in% c("Y", "Yes"), na.rm = TRUE))) %>%
    pivot_longer(everything(), names_to = "Type", values_to = "n") %>%
    filter(n > 0) %>%
    arrange(desc(n)) %>%
    print(n = Inf)

  sub_divider("Complication sub-types by Consultant")
  combined %>%
    group_by(Consultant) %>%
    summarise(across(all_of(compl_type_cols),
                     ~ sum(. %in% c("Y", "Yes"), na.rm = TRUE)),
              .groups = "drop") %>%
    pivot_longer(-Consultant, names_to = "Type", values_to = "n") %>%
    filter(n > 0) %>%
    arrange(Consultant, desc(n)) %>%
    print(n = Inf)
}

# ── 7l. Fractures ───────────────────────────────────────────────────────────
divider("7l. Intraoperative Fractures")
frac_parent <- names(combined) %>% str_subset("Fracture \\(")
if (length(frac_parent) > 0) {
  combined %>%
    summarise(across(all_of(frac_parent),
                     ~ sum(. %in% c("Y", "Yes"), na.rm = TRUE))) %>%
    pivot_longer(everything(), names_to = "Fracture_Flag", values_to = "n") %>%
    filter(n > 0) %>%
    print(n = Inf)

  sub_divider("By Consultant")
  combined %>%
    group_by(Consultant) %>%
    summarise(across(all_of(frac_parent),
                     ~ sum(. %in% c("Y", "Yes"), na.rm = TRUE)),
              .groups = "drop") %>%
    pivot_longer(-Consultant, names_to = "Fracture_Flag", values_to = "n") %>%
    filter(n > 0) %>%
    arrange(Consultant) %>%
    print(n = Inf)
}

# ── 7m. Tourniquet use (knee) ───────────────────────────────────────────────
divider("7m. Tourniquet Use")
tourn_cols <- names(combined) %>% str_subset("^Tourniquet ")
if (length(tourn_cols) > 0) {
  combined %>%
    filter(Joint == "Knee") %>%
    summarise(across(all_of(tourn_cols),
                     ~ sum(. %in% c("Y", "Yes"), na.rm = TRUE))) %>%
    pivot_longer(everything(), names_to = "Tourniquet", values_to = "n") %>%
    print(n = Inf)

  sub_divider("By Consultant")
  combined %>%
    filter(Joint == "Knee") %>%
    group_by(Consultant) %>%
    summarise(across(all_of(tourn_cols),
                     ~ sum(. %in% c("Y", "Yes"), na.rm = TRUE)),
              .groups = "drop") %>%
    pivot_longer(-Consultant, names_to = "Tourniquet", values_to = "n") %>%
    arrange(Consultant) %>%
    print(n = Inf)
}

# ── 7n. Surgical approach – Hip ─────────────────────────────────────────────
divider("7n. Surgical Approach – Hip")
hip_appr <- names(combined) %>% str_subset("^Hip - Surgical approach")
if (length(hip_appr) > 0) {
  combined %>%
    filter(Joint == "Hip") %>%
    summarise(across(all_of(hip_appr),
                     ~ sum(. %in% c("Y", "Yes"), na.rm = TRUE))) %>%
    pivot_longer(everything(), names_to = "Approach", values_to = "n") %>%
    mutate(Approach = str_remove(Approach, "^Hip - Surgical approach \\((Left|Right)\\):")) %>%
    group_by(Approach) %>%
    summarise(n = sum(n), .groups = "drop") %>%
    filter(n > 0) %>%
    arrange(desc(n)) %>%
    print(n = Inf)

  sub_divider("By Consultant")
  combined %>%
    filter(Joint == "Hip") %>%
    group_by(Consultant) %>%
    summarise(across(all_of(hip_appr),
                     ~ sum(. %in% c("Y", "Yes"), na.rm = TRUE)),
              .groups = "drop") %>%
    pivot_longer(-Consultant, names_to = "Approach", values_to = "n") %>%
    mutate(Approach = str_remove(Approach, "^Hip - Surgical approach \\((Left|Right)\\):")) %>%
    group_by(Consultant, Approach) %>%
    summarise(n = sum(n), .groups = "drop") %>%
    filter(n > 0) %>%
    arrange(Consultant, desc(n)) %>%
    print(n = Inf)
}

# ── 7o. Surgical approach – Knee ────────────────────────────────────────────
divider("7o. Surgical Approach – Knee")
knee_appr <- names(combined) %>% str_subset("^Knee - Surgical approach")
if (length(knee_appr) > 0) {
  combined %>%
    filter(Joint == "Knee") %>%
    summarise(across(all_of(knee_appr),
                     ~ sum(. %in% c("Y", "Yes"), na.rm = TRUE))) %>%
    pivot_longer(everything(), names_to = "Approach", values_to = "n") %>%
    mutate(Approach = str_remove(Approach, "^Knee - Surgical approach \\((Left|Right)\\):")) %>%
    group_by(Approach) %>%
    summarise(n = sum(n), .groups = "drop") %>%
    filter(n > 0) %>%
    arrange(desc(n)) %>%
    print(n = Inf)

  sub_divider("By Consultant")
  combined %>%
    filter(Joint == "Knee") %>%
    group_by(Consultant) %>%
    summarise(across(all_of(knee_appr),
                     ~ sum(. %in% c("Y", "Yes"), na.rm = TRUE)),
              .groups = "drop") %>%
    pivot_longer(-Consultant, names_to = "Approach", values_to = "n") %>%
    mutate(Approach = str_remove(Approach, "^Knee - Surgical approach \\((Left|Right)\\):")) %>%
    group_by(Consultant, Approach) %>%
    summarise(n = sum(n), .groups = "drop") %>%
    filter(n > 0) %>%
    arrange(Consultant, desc(n)) %>%
    print(n = Inf)
}

# ── 7p. Indication for surgery – Hip (primary) ─────────────────────────────
divider("7p. Indication for Surgery – Hip (Primary)")
hip_ind <- names(combined) %>% str_subset("^Hip - Indication for surgery \\(")
if (length(hip_ind) > 0) {
  combined %>%
    filter(Joint == "Hip", `Procedure type` == "Primary") %>%
    summarise(across(all_of(hip_ind),
                     ~ sum(. %in% c("Y", "Yes"), na.rm = TRUE))) %>%
    pivot_longer(everything(), names_to = "Indication", values_to = "n") %>%
    mutate(Indication = str_remove(Indication,
                                   "^Hip - Indication for surgery \\((Left|Right)\\):")) %>%
    group_by(Indication) %>%
    summarise(n = sum(n), .groups = "drop") %>%
    filter(n > 0) %>%
    arrange(desc(n)) %>%
    print(n = Inf)

  sub_divider("By Consultant")
  combined %>%
    filter(Joint == "Hip", `Procedure type` == "Primary") %>%
    group_by(Consultant) %>%
    summarise(across(all_of(hip_ind),
                     ~ sum(. %in% c("Y", "Yes"), na.rm = TRUE)),
              .groups = "drop") %>%
    pivot_longer(-Consultant, names_to = "Indication", values_to = "n") %>%
    mutate(Indication = str_remove(Indication,
                                   "^Hip - Indication for surgery \\((Left|Right)\\):")) %>%
    group_by(Consultant, Indication) %>%
    summarise(n = sum(n), .groups = "drop") %>%
    filter(n > 0) %>%
    arrange(Consultant, desc(n)) %>%
    print(n = Inf)
}

# ── 7q. Indication for surgery – Knee (primary) ────────────────────────────
divider("7q. Indication for Surgery – Knee (Primary)")
knee_ind <- names(combined) %>%
  str_subset("^Knee - Indication for surgery \\((Left|Right)\\):(Avascular|Osteo|Rheum|Post)")
if (length(knee_ind) > 0) {
  combined %>%
    filter(Joint == "Knee", `Procedure type` == "Primary") %>%
    summarise(across(all_of(knee_ind),
                     ~ sum(. %in% c("Y", "Yes"), na.rm = TRUE))) %>%
    pivot_longer(everything(), names_to = "Indication", values_to = "n") %>%
    mutate(Indication = str_remove(Indication,
                                   "^Knee - Indication for surgery \\((Left|Right)\\):")) %>%
    group_by(Indication) %>%
    summarise(n = sum(n), .groups = "drop") %>%
    filter(n > 0) %>%
    arrange(desc(n)) %>%
    print(n = Inf)

  sub_divider("By Consultant")
  combined %>%
    filter(Joint == "Knee", `Procedure type` == "Primary") %>%
    group_by(Consultant) %>%
    summarise(across(all_of(knee_ind),
                     ~ sum(. %in% c("Y", "Yes"), na.rm = TRUE)),
              .groups = "drop") %>%
    pivot_longer(-Consultant, names_to = "Indication", values_to = "n") %>%
    mutate(Indication = str_remove(Indication,
                                   "^Knee - Indication for surgery \\((Left|Right)\\):")) %>%
    group_by(Consultant, Indication) %>%
    summarise(n = sum(n), .groups = "drop") %>%
    filter(n > 0) %>%
    arrange(Consultant, desc(n)) %>%
    print(n = Inf)
}

# ── 7r. Cross-tab: Joint × Procedure type × Hospital ───────────────────────
divider("7r. Cross-tabulation: Joint × Procedure Type × Hospital")
combined %>%
  count(Hospital, Joint, `Procedure type`, name = "n") %>%
  arrange(Hospital, Joint) %>%
  print(n = Inf)

sub_divider("By Consultant")
combined %>%
  count(Consultant, Hospital, Joint, `Procedure type`, name = "n") %>%
  arrange(Consultant, Hospital, Joint) %>%
  print(n = Inf)

# ── 7s. Consultant dashboard (dedicated summary) ───────────────────────────
divider("7s. CONSULTANT DASHBOARD – Full Summary per Consultant")
consultant_summary <- combined %>%
  group_by(Consultant) %>%
  summarise(
    total_procedures  = n(),
    n_hip             = sum(Joint == "Hip", na.rm = TRUE),
    n_knee            = sum(Joint == "Knee", na.rm = TRUE),
    n_primary         = sum(`Procedure type` == "Primary", na.rm = TRUE),
    n_revision        = sum(`Procedure type` == "Revision", na.rm = TRUE),
    n_left            = sum(Laterality == "Left", na.rm = TRUE),
    n_right           = sum(Laterality == "Right", na.rm = TRUE),
    n_bilateral       = sum(Laterality == "Bilateral", na.rm = TRUE),
    mean_age          = round(mean(`Patient age at time of surgery`, na.rm = TRUE), 1),
    sd_age            = round(sd(`Patient age at time of surgery`, na.rm = TRUE), 1),
    n_male            = sum(Sex == "Male", na.rm = TRUE),
    n_female          = sum(Sex == "Female", na.rm = TRUE),
    n_asa1            = sum(ASA == "ASA 1", na.rm = TRUE),
    n_asa2            = sum(ASA == "ASA 2", na.rm = TRUE),
    n_asa3            = sum(ASA == "ASA 3", na.rm = TRUE),
    n_asa4            = sum(ASA == "ASA 4", na.rm = TRUE),
    n_antibiotics_yes = sum(Antibiotics == "Yes", na.rm = TRUE),
    n_hospitals       = n_distinct(Hospital),
    hospitals         = paste(sort(unique(Hospital)), collapse = "; "),
    .groups = "drop"
  ) %>%
  arrange(desc(total_procedures))

print(consultant_summary, n = Inf, width = Inf)

# ── 8. Export combined dataset ──────────────────────────────────────────────
output_file     <- "INOR_Peri_Op_Combined.csv"
consultant_file <- "INOR_Consultant_Summary.csv"

write_csv(combined,            output_file,     na = "")
write_csv(consultant_summary,  consultant_file, na = "")

cat("\n", strrep("=", 70), "\n")
cat(" Combined dataset written to:    ", output_file, "\n")
cat(" Consultant summary written to:  ", consultant_file, "\n")
cat(" Total records:", nrow(combined), "\n")
cat(strrep("=", 70), "\n")
