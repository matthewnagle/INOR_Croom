###############################################################################
# INOR Arthroplasty Data Query Tool
# ----------------------------------
# Linkage model:
#   FORM_RESPONSE_GROUP_ID = unique case identifier (one per joint replacement)
#   A patient with bilateral replacements will have one MRN but TWO
#   FORM_RESPONSE_GROUP_IDs — one per joint. PROMs and post-op assessments
#   for each joint link back to the corresponding FORM_RESPONSE_GROUP_ID.
#
# This script provides functions to query:
#   1. PROMs data (Oxford Knee Score, Oxford Hip Score, EQ-5D)
#   2. Post-operative complications (from V1 and V2 post-op assessments)
#   3. Combined patient/case profiles
#
# USAGE:
#   1. Update the file paths in Section 1 if needed
#   2. Source the script: source("INOR_query_tool.R")
#   3. Call the query functions described in the reference printed at startup
###############################################################################

library(dplyr)
library(tidyr)
library(readr)

inor_query_tool_version <- "v6"

# =============================================================================
# SECTION 1: LOAD DATA
# =============================================================================
# By default the script looks for files in RawData/ under the app folder.
# It also falls back to the current working directory and the parent folder so
# the app can still run when launched from a different location.

candidate_data_dirs <- function() {
  base_dir <- getOption("inor_app_base_dir", Sys.getenv("INOR_APP_BASE_DIR", unset = "."))
  base_dir <- normalizePath(base_dir, winslash = "/", mustWork = FALSE)
  cwd <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)

  unique(c(
    file.path(base_dir, "RawData"),
    base_dir,
    file.path(dirname(base_dir), "RawData"),
    dirname(base_dir),
    file.path(cwd, "RawData"),
    cwd
  ))
}

extract_filename_timestamp_num <- function(path) {
  name <- basename(path)
  match <- regexpr("[0-9]{4}-[0-9]{2}-[0-9]{2}_[0-9]{2}-[0-9]{2}-[0-9]{2}", name)
  if (match[[1]] == -1) {
    return(NA_real_)
  }

  stamp <- regmatches(name, match)
  as.numeric(as.POSIXct(stamp, format = "%Y-%m-%d_%H-%M-%S", tz = "UTC"))
}

match_export_filename <- function(filename, prefix, suffix) {
  filename_upper <- toupper(filename)
  prefix_upper <- toupper(prefix)
  suffix_upper <- toupper(suffix)

  if (!startsWith(filename_upper, prefix_upper)) return(FALSE)
  if (!endsWith(filename_upper, suffix_upper)) return(FALSE)
  if (nchar(filename) < (nchar(prefix) + nchar(suffix))) return(FALSE)

  middle <- substr(filename, nchar(prefix) + 1, nchar(filename) - nchar(suffix))
  if (!nzchar(middle)) return(TRUE)

  grepl("^_[0-9]{4}-[0-9]{2}-[0-9]{2}_[0-9]{2}-[0-9]{2}-[0-9]{2}$", middle)
}

find_matching_files <- function(dir_path, prefix, suffix) {
  files <- list.files(
    dir_path,
    pattern = "\\.csv$",
    full.names = TRUE,
    ignore.case = TRUE
  )

  if (!length(files)) return(character())

  keep <- vapply(
    basename(files),
    match_export_filename,
    logical(1),
    prefix = prefix,
    suffix = suffix
  )

  files[keep]
}

choose_best_match <- function(paths) {
  if (length(paths) == 1) {
    return(paths[[1]])
  }

  stamp <- vapply(paths, extract_filename_timestamp_num, numeric(1))
  mtime <- as.numeric(file.info(paths)$mtime)

  order_index <- order(
    !is.na(stamp),
    stamp,
    mtime,
    decreasing = TRUE,
    na.last = TRUE
  )

  paths[[order_index[[1]]]]
}

resolve_data_file <- function(prefix, suffix = ".csv", label = prefix) {
  searched_dirs <- character()

  for (dir_path in candidate_data_dirs()) {
    if (!dir.exists(dir_path)) next
    searched_dirs <- c(searched_dirs, dir_path)

    matches <- find_matching_files(dir_path, prefix = prefix, suffix = suffix)

    if (length(matches) > 0) {
      selected <- choose_best_match(matches)
      cat(sprintf("Matched %s file: %s\n", label, basename(selected)))
      return(selected)
    }
  }

  stop(
    paste0(
      "Could not locate the ", label, " CSV. Expected a filename like '",
      prefix,
      "_YYYY-MM-DD_HH-MM-SS",
      suffix,
      "' or '",
      prefix,
      suffix,
      "'. Searched in: ",
      paste(unique(searched_dirs), collapse = ", ")
    ),
    call. = FALSE
  )
}


resolve_optional_data_file <- function(prefix, suffix = ".csv", label = prefix) {
  searched_dirs <- character()

  for (dir_path in candidate_data_dirs()) {
    if (!dir.exists(dir_path)) next
    searched_dirs <- c(searched_dirs, dir_path)

    matches <- find_matching_files(dir_path, prefix = prefix, suffix = suffix)

    if (length(matches) > 0) {
      selected <- choose_best_match(matches)
      cat(sprintf("Matched %s file: %s\n", label, basename(selected)))
      return(selected)
    }
  }

  cat(
    sprintf(
      "Optional %s file not found. Searched in: %s\n",
      label,
      paste(unique(searched_dirs), collapse = ", ")
    )
  )
  NULL
}

path_preop        <- resolve_data_file("NAP_INOR_PREOP_ASSESSMENT_V1", ".csv", "pre-op assessment")
path_periop_v1    <- resolve_optional_data_file("NAP_INOR_PERI_OP_ASSESSMENT_V1", ".csv", "peri-op assessment V1")
path_periop_v2    <- resolve_optional_data_file("NAP_INOR_PERI_OP_ASSESSMENT_V2", ".csv", "peri-op assessment V2")
path_postop_v1    <- resolve_data_file("NAP_INOR_POST_OP_ASSESSMENT_V1", ".csv", "post-op assessment V1")
path_postop_v2    <- resolve_data_file("NAP_INOR_POST_OP_ASSESSMENT_V2", ".csv", "post-op assessment V2")
path_oks          <- resolve_data_file("NAP_INOR_OXFORD_KNEE_SCORE", ".csv", "Oxford Knee Score")
path_ohs          <- resolve_data_file("NAP_INOR_OXFORD_HIP_SCORE", ".csv", "Oxford Hip Score")
path_eq5d         <- resolve_data_file("NAP_INOR_EQ5D", ".csv", "EQ-5D")
path_comp_v1_data <- resolve_data_file("NAP_INOR_COMPONENT_LOG_V1", "_Data.csv", "component log V1 surgical times")
path_comp_v1_comp <- resolve_data_file("NAP_INOR_COMPONENT_LOG_V1", "_ComponentData.csv", "component log V1 implant details")
path_comp_v2_data <- resolve_data_file("NAP_INOR_COMPONENT_LOG_V2", "_Data.csv", "component log V2 surgical times")
path_comp_v2_comp <- resolve_data_file("NAP_INOR_COMPONENT_LOG_V2", "_ComponentData.csv", "component log V2 implant details")

cat("Loading datasets...\n")

# Load all columns as character to avoid type-inference warnings.
# Issues include: NAP Id in scientific notation losing precision as double,
# ambiguous date formats (DD/MM/YYYYTHH:MM), and sparse columns.
# We convert the numeric columns we actually need after loading.

all_char <- cols(.default = col_character())

read_optional_csv <- function(path, ...) {
  if (is.null(path) || !file.exists(path)) {
    return(tibble::tibble())
  }
  read_csv(path, show_col_types = FALSE, col_types = all_char, ...)
}

preop     <- read_csv(path_preop, show_col_types = FALSE, col_types = all_char)
periop_v1 <- read_optional_csv(path_periop_v1)
periop_v2 <- read_optional_csv(path_periop_v2)
oks       <- read_csv(path_oks, show_col_types = FALSE, col_types = all_char)
ohs       <- read_csv(path_ohs, show_col_types = FALSE, col_types = all_char)
eq5d      <- read_csv(path_eq5d, show_col_types = FALSE, col_types = all_char)

# Post-op V1 and V2 have duplicate "Revision date (Left/Right)" columns:
#   - First occurrence: date of subsequent revision surgery
#   - Second occurrence: date of revision due to complication
# read_csv auto-renames these with ...N suffixes. We rename them explicitly.

postop_v1 <- read_csv(path_postop_v1, show_col_types = FALSE,
                       col_types = all_char, name_repair = "unique_quiet")
v1_names <- names(postop_v1)
v1_names <- gsub("^Revision date \\(Left\\)\\.\\.\\.30$",
                  "Subsequent revision date (Left)", v1_names)
v1_names <- gsub("^Revision date \\(Right\\)\\.\\.\\.33$",
                  "Subsequent revision date (Right)", v1_names)
v1_names <- gsub("^Revision date \\(Left\\)\\.\\.\\.91$",
                  "Complication revision date (Left)", v1_names)
v1_names <- gsub("^Revision date \\(Right\\)\\.\\.\\.92$",
                  "Complication revision date (Right)", v1_names)
names(postop_v1) <- v1_names

postop_v2 <- read_csv(path_postop_v2, show_col_types = FALSE,
                       col_types = all_char, name_repair = "unique_quiet")
v2_names <- names(postop_v2)
v2_names <- gsub("^Revision date \\(Left\\)\\.\\.\\.81$",
                  "Complication revision date (Left)", v2_names)
v2_names <- gsub("^Revision date \\(Right\\)\\.\\.\\.82$",
                  "Complication revision date (Right)", v2_names)
v2_names <- gsub("^Revision date \\(Left\\)\\.\\.\\.119$",
                  "Subsequent revision date (Left)", v2_names)
v2_names <- gsub("^Revision date \\(Right\\)\\.\\.\\.120$",
                  "Subsequent revision date (Right)", v2_names)
names(postop_v2) <- v2_names

# Clean up column names: trim leading/trailing whitespace and collapse internal
# double spaces (some columns have leading spaces or double spaces in the CSV)
trim_names <- function(df) {
  names(df) <- trimws(names(df))
  names(df) <- gsub("\\s+", " ", names(df))
  df
}

preop     <- trim_names(preop)
periop_v1 <- trim_names(periop_v1)
periop_v2 <- trim_names(periop_v2)
postop_v1 <- trim_names(postop_v1)
postop_v2 <- trim_names(postop_v2)
oks       <- trim_names(oks)
ohs       <- trim_names(ohs)
eq5d      <- trim_names(eq5d)

# Component log files (surgical times + implant details)
comp_v1_data <- read_csv(path_comp_v1_data, show_col_types = FALSE, col_types = all_char)
comp_v1_comp <- read_csv(path_comp_v1_comp, show_col_types = FALSE, col_types = all_char)
comp_v2_data <- read_csv(path_comp_v2_data, show_col_types = FALSE, col_types = all_char)
comp_v2_comp <- read_csv(path_comp_v2_comp, show_col_types = FALSE, col_types = all_char)

comp_v1_data <- trim_names(comp_v1_data)
comp_v1_comp <- trim_names(comp_v1_comp)
comp_v2_data <- trim_names(comp_v2_data)
comp_v2_comp <- trim_names(comp_v2_comp)

# Convert numeric columns we need for analysis
preop <- preop %>%
  mutate(across(c(BMI, Height, Weight), as.numeric))

oks <- oks %>%
  mutate(Score = as.numeric(Score))

ohs <- ohs %>%
  mutate(Score = as.numeric(Score))

eq5d <- eq5d %>%
  mutate(
    Score = as.numeric(Score),
    `Your health today` = as.numeric(`Your health today`)
  )

cat("Datasets loaded successfully.\n")
cat("Note: Duplicate 'Revision date' columns disambiguated as:\n")
cat("  'Subsequent revision date (Left/Right)' — date of subsequent revision surgery\n")
cat("  'Complication revision date (Left/Right)' — date of revision due to complication\n\n")


# =============================================================================
# SECTION 2: CASE REGISTRY
# =============================================================================
# Build a case-level reference from pre-op data.
# Each row = one joint replacement episode, keyed by FORM_RESPONSE_GROUP_ID.

cases <- preop %>%
  select(
    FORM_RESPONSE_GROUP_ID,
    `Patient Id`,
    `MRN Number`,
    `First Name`,
    `Last Name`,
    Sex,
    `Date of Birth`,
    Joint,
    Laterality,
    `Procedure type`,
    `Procedure code`,
    `Procedure date`,
    BMI,
    `Co-morbidities`,
    ACCESS_POINT_NAME
  ) %>%
  rename(
    case_id        = FORM_RESPONSE_GROUP_ID,
    patient_id     = `Patient Id`,
    mrn            = `MRN Number`,
    first_name     = `First Name`,
    last_name      = `Last Name`,
    sex            = Sex,
    dob            = `Date of Birth`,
    joint          = Joint,
    laterality     = Laterality,
    procedure_type = `Procedure type`,
    procedure_code = `Procedure code`,
    procedure_date = `Procedure date`,
    bmi            = BMI,
    comorbidities  = `Co-morbidities`,
    hospital       = ACCESS_POINT_NAME
  )

#' List all cases, optionally filtered
#'
#' @param mrn        Optional: filter by MRN Number
#' @param patient_id Optional: filter by Patient Id
#' @param joint      Optional: "Hip" or "Knee"
#' @param hospital   Optional: hospital name
#' @return A tibble of case records
list_cases <- function(mrn = NULL, patient_id = NULL,
                       joint = NULL, hospital = NULL) {
  result <- cases
  if (!is.null(mrn))        result <- result %>% filter(mrn == !!mrn)
  if (!is.null(patient_id)) result <- result %>% filter(patient_id == !!patient_id)
  if (!is.null(joint))      result <- result %>% filter(joint == !!joint)
  if (!is.null(hospital))   result <- result %>% filter(hospital == !!hospital)
  return(result)
}


# =============================================================================
# SECTION 3: PROMs QUERIES
# =============================================================================

#' Get Oxford Knee Scores
#'
#' @param case_id     Optional: FORM_RESPONSE_GROUP_ID (links to specific joint)
#' @param mrn         Optional: filter by MRN Number (returns all joints for patient)
#' @param patient_id  Optional: filter by Patient Id
#' @param stage       Optional: e.g. "Pre-op presentation", "6 month presentation",
#'                    "1 year presentation", "2 year presentation",
#'                    "5 year presentation", "10 year presentation"
#' @param laterality  Optional: "Left" or "Right"
#' @return A tibble of Oxford Knee Score records
get_oxford_knee_scores <- function(case_id = NULL, mrn = NULL, patient_id = NULL,
                                    stage = NULL, laterality = NULL) {
  result <- oks %>%
    select(
      FORM_RESPONSE_GROUP_ID, `Patient Id`, `MRN Number`,
      `First Name`, `Last Name`,
      `Event date`, Laterality, Stage, Score,
      starts_with("1:"), starts_with("2:"), starts_with("3:"),
      starts_with("4:"), starts_with("5:"), starts_with("6:"),
      starts_with("7:"), starts_with("8:"), starts_with("9:"),
      starts_with("10:"), starts_with("11:"), starts_with("12:"),
      HOSPITAL_NAME
    )

  if (!is.null(case_id))    result <- result %>% filter(FORM_RESPONSE_GROUP_ID == case_id)
  if (!is.null(mrn))        result <- result %>% filter(`MRN Number` == mrn)
  if (!is.null(patient_id)) result <- result %>% filter(`Patient Id` == patient_id)
  if (!is.null(stage))      result <- result %>% filter(Stage == stage)
  if (!is.null(laterality)) result <- result %>% filter(Laterality == laterality)

  return(result)
}


#' Get Oxford Hip Scores
#'
#' @inheritParams get_oxford_knee_scores
#' @return A tibble of Oxford Hip Score records
get_oxford_hip_scores <- function(case_id = NULL, mrn = NULL, patient_id = NULL,
                                   stage = NULL, laterality = NULL) {
  result <- ohs %>%
    select(
      FORM_RESPONSE_GROUP_ID, `Patient Id`, `MRN Number`,
      `First Name`, `Last Name`,
      `Event date`, Laterality, Stage, Score,
      starts_with("1:"), starts_with("2:"),
      starts_with("Have you had any trouble getting"),
      starts_with("4:"), starts_with("5:"), starts_with("6:"),
      starts_with("7:"), starts_with("8:"), starts_with("9:"),
      starts_with("10:"), starts_with("11:"), starts_with("12:"),
      HOSPITAL_NAME
    )

  if (!is.null(case_id))    result <- result %>% filter(FORM_RESPONSE_GROUP_ID == case_id)
  if (!is.null(mrn))        result <- result %>% filter(`MRN Number` == mrn)
  if (!is.null(patient_id)) result <- result %>% filter(`Patient Id` == patient_id)
  if (!is.null(stage))      result <- result %>% filter(Stage == stage)
  if (!is.null(laterality)) result <- result %>% filter(Laterality == laterality)

  return(result)
}


#' Get EQ-5D Scores
#'
#' @param case_id    Optional: FORM_RESPONSE_GROUP_ID
#' @param mrn        Optional: filter by MRN Number
#' @param patient_id Optional: filter by Patient Id
#' @param stage      Optional: filter by Stage
#' @return A tibble of EQ-5D records
get_eq5d_scores <- function(case_id = NULL, mrn = NULL, patient_id = NULL,
                             stage = NULL) {
  result <- eq5d %>%
    select(
      FORM_RESPONSE_GROUP_ID, `Patient Id`, `MRN Number`,
      `First Name`, `Last Name`,
      `Event date`, Stage,
      `1: MOBILITY`, `2: SELF-CARE`,
      starts_with("3:"), `4: PAIN / DISCOMFORT`, `5: ANXIETY / DEPRESSION`,
      `Your health today`, Score,
      HOSPITAL_NAME
    )

  if (!is.null(case_id))    result <- result %>% filter(FORM_RESPONSE_GROUP_ID == case_id)
  if (!is.null(mrn))        result <- result %>% filter(`MRN Number` == mrn)
  if (!is.null(patient_id)) result <- result %>% filter(`Patient Id` == patient_id)
  if (!is.null(stage))      result <- result %>% filter(Stage == stage)

  return(result)
}


#' Get all PROMs for a case or patient
#'
#' @param case_id    Optional: FORM_RESPONSE_GROUP_ID (specific joint)
#' @param mrn        Optional: MRN (all joints for this patient)
#' @param patient_id Optional: Patient Id
#' @return A list with elements: knee, hip, eq5d
get_all_proms <- function(case_id = NULL, mrn = NULL, patient_id = NULL) {
  list(
    knee = get_oxford_knee_scores(case_id = case_id, mrn = mrn, patient_id = patient_id),
    hip  = get_oxford_hip_scores(case_id = case_id, mrn = mrn, patient_id = patient_id),
    eq5d = get_eq5d_scores(case_id = case_id, mrn = mrn, patient_id = patient_id)
  )
}


#' Summarise PROMs scores by stage (mean, median, SD, n)
#'
#' @param prom_type One of "knee", "hip", or "eq5d"
#' @return A summary tibble grouped by Stage
summarise_proms_by_stage <- function(prom_type = c("knee", "hip", "eq5d")) {
  prom_type <- match.arg(prom_type)

  df <- switch(prom_type,
    knee = oks,
    hip  = ohs,
    eq5d = eq5d
  )

  df %>%
    filter(!is.na(Score)) %>%
    group_by(Stage) %>%
    summarise(
      n      = n(),
      mean   = round(mean(Score, na.rm = TRUE), 1),
      median = median(Score, na.rm = TRUE),
      sd     = round(sd(Score, na.rm = TRUE), 1),
      min    = min(Score, na.rm = TRUE),
      max    = max(Score, na.rm = TRUE),
      .groups = "drop"
    )
}


#' Summarise PROMs scores by admitting consultant
#'
#' Joins PROMs data to the pre-op dataset via FORM_RESPONSE_GROUP_ID to
#' bring in the "Admitting consultant" field, then summarises scores.
#'
#' @param prom_type One of "knee", "hip", or "eq5d"
#' @param stage     Optional: filter to a specific stage before summarising
#' @param consultant Optional: filter to a specific consultant name
#' @return A summary tibble grouped by Admitting consultant (and Stage if not filtered)
summarise_proms_by_consultant <- function(prom_type = c("knee", "hip", "eq5d"),
                                           stage = NULL, consultant = NULL) {
  prom_type <- match.arg(prom_type)

  df <- switch(prom_type,
    knee = oks,
    hip  = ohs,
    eq5d = eq5d
  )

  # Join to pre-op to get consultant
  consultant_lookup <- preop %>%
    select(FORM_RESPONSE_GROUP_ID, `Admitting consultant`)

  df <- df %>%
    left_join(consultant_lookup, by = "FORM_RESPONSE_GROUP_ID") %>%
    filter(!is.na(Score))

  if (!is.null(stage))      df <- df %>% filter(Stage == stage)
  if (!is.null(consultant)) df <- df %>% filter(`Admitting consultant` == consultant)

  # Group by consultant, and also by stage if no specific stage was requested
  if (!is.null(stage)) {
    df %>%
      group_by(`Admitting consultant`) %>%
      summarise(
        n      = n(),
        mean   = round(mean(Score, na.rm = TRUE), 1),
        median = median(Score, na.rm = TRUE),
        sd     = round(sd(Score, na.rm = TRUE), 1),
        min    = min(Score, na.rm = TRUE),
        max    = max(Score, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(n))
  } else {
    df %>%
      group_by(`Admitting consultant`, Stage) %>%
      summarise(
        n      = n(),
        mean   = round(mean(Score, na.rm = TRUE), 1),
        median = median(Score, na.rm = TRUE),
        sd     = round(sd(Score, na.rm = TRUE), 1),
        min    = min(Score, na.rm = TRUE),
        max    = max(Score, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(`Admitting consultant`, Stage)
  }
}


#' Track PROMs trajectory for a specific case over time
#'
#' Shows how scores change across stages for one joint replacement.
#'
#' @param case_id FORM_RESPONSE_GROUP_ID for the case
#' @param prom_type One of "knee", "hip", or "eq5d"
#' @return A tibble ordered by stage showing score progression
get_proms_trajectory <- function(case_id, prom_type = c("knee", "hip", "eq5d")) {
  prom_type <- match.arg(prom_type)

  stage_order <- c(
    "Pre-op presentation", "6 month presentation", "1 year presentation",
    "2 year presentation", "5 year presentation", "10 year presentation"
  )

  df <- switch(prom_type,
    knee = get_oxford_knee_scores(case_id = case_id),
    hip  = get_oxford_hip_scores(case_id = case_id),
    eq5d = get_eq5d_scores(case_id = case_id)
  )

  df %>%
    mutate(Stage = factor(Stage, levels = stage_order, ordered = TRUE)) %>%
    arrange(Stage) %>%
    select(FORM_RESPONSE_GROUP_ID, `MRN Number`, `First Name`, `Last Name`,
           any_of("Laterality"), Stage, Score, `Event date`)
}


# =============================================================================
# SECTION 3B: DEMOGRAPHICS QUERIES
# =============================================================================

# Helper: parse a date column that may contain Excel serial numbers (e.g. "26196")
# OR date strings (e.g. "01/01/1971", "1971-01-01", "01/01/1971T10:10").
# Returns a Date vector.
parse_date_flexible <- function(x) {
  x <- as.character(x)
  x[x == "" | is.na(x)] <- NA_character_

  # Try Excel serial first: if value is purely numeric, treat as serial
  is_numeric <- !is.na(x) & grepl("^[0-9]+(\\.[0-9]+)?$", x)

  result <- rep(as.Date(NA), length(x))

  # Excel serials
  if (any(is_numeric)) {
    result[is_numeric] <- as.Date(as.numeric(x[is_numeric]), origin = "1899-12-30")
  }

  # Non-numeric: try common date formats
  if (any(!is_numeric & !is.na(x))) {
    remaining <- which(!is_numeric & !is.na(x))
    # Strip any time component (e.g. "T10:10" or " 10:10")
    date_str <- gsub("[T ].*$", "", x[remaining])
    # Try DD/MM/YYYY
    parsed <- as.Date(date_str, format = "%d/%m/%Y")
    # If that failed, try YYYY-MM-DD
    still_na <- is.na(parsed)
    if (any(still_na)) {
      parsed[still_na] <- as.Date(date_str[still_na], format = "%Y-%m-%d")
    }
    # If that failed, try MM/DD/YYYY
    still_na <- is.na(parsed)
    if (any(still_na)) {
      parsed[still_na] <- as.Date(date_str[still_na], format = "%m/%d/%Y")
    }
    result[remaining] <- parsed
  }

  result
}

# Helper: calculate age in years between two date columns
calc_age <- function(dob, ref_date) {
  floor(as.numeric(difftime(ref_date, dob, units = "days")) / 365.25)
}


blank_to_na <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x[x == "" | is.na(x)] <- NA_character_
  x
}

first_non_missing_chr <- function(x) {
  x <- blank_to_na(x)
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_character_)
  x[[1]]
}

first_non_missing_date <- function(x) {
  x <- as.Date(x)
  x <- x[!is.na(x)]
  if (!length(x)) return(as.Date(NA))
  x[[1]]
}

coalesce_nonempty_character <- function(primary, fallback) {
  dplyr::coalesce(blank_to_na(primary), blank_to_na(fallback))
}

ensure_character_column <- function(df, column_name) {
  if (!column_name %in% names(df)) {
    df[[column_name]] <- rep(NA_character_, nrow(df))
  }
  df
}

normalise_name_key <- function(x) {
  tolower(gsub("[^a-z0-9]+", "", trimws(as.character(x))))
}

resolve_optional_column_name <- function(df, candidates) {
  if (is.null(df) || !length(names(df)) || is.null(candidates) || !length(candidates)) {
    return(NULL)
  }

  nm <- names(df)
  nm_key <- normalise_name_key(nm)
  cand <- as.character(candidates)
  cand_key <- normalise_name_key(cand)

  for (i in seq_along(cand)) {
    exact_idx <- match(cand[[i]], nm)
    if (!is.na(exact_idx)) return(nm[[exact_idx]])

    key_idx <- match(cand_key[[i]], nm_key)
    if (!is.na(key_idx)) return(nm[[key_idx]])
  }

  NULL
}

get_optional_column <- function(df, column_name) {
  if (is.null(df) || !nrow(df) || is.null(column_name) || !length(column_name) || !column_name %in% names(df)) {
    return(rep(NA_character_, nrow(df)))
  }
  as.character(df[[column_name]])
}

build_periop_case_metadata <- function(df,
                                       source_label,
                                       consultant_col = "Admitting consultant",
                                       surgeon_grade_col = NULL,
                                       surgeon_name_col = NULL,
                                       surgery_date_col = "Date of surgery") {
  if (is.null(df) || !nrow(df)) {
    return(tibble::tibble(
      FORM_RESPONSE_GROUP_ID = character(),
      periop_admitting_consultant = character(),
      periop_surgeon_grade = character(),
      periop_operating_surgeon = character(),
      periop_surgery_date = as.Date(character()),
      periop_source = character()
    ))
  }

  id_col <- resolve_optional_column_name(df, "FORM_RESPONSE_GROUP_ID")
  if (is.null(id_col)) {
    return(tibble::tibble(
      FORM_RESPONSE_GROUP_ID = character(),
      periop_admitting_consultant = character(),
      periop_surgeon_grade = character(),
      periop_operating_surgeon = character(),
      periop_surgery_date = as.Date(character()),
      periop_source = character()
    ))
  }

  consultant_col_resolved <- resolve_optional_column_name(df, consultant_col)
  surgeon_grade_col_resolved <- resolve_optional_column_name(df, surgeon_grade_col)
  surgeon_name_col_resolved <- resolve_optional_column_name(df, surgeon_name_col)
  surgery_date_col_resolved <- resolve_optional_column_name(df, surgery_date_col)

  tibble::tibble(
    FORM_RESPONSE_GROUP_ID = as.character(df[[id_col]]),
    periop_admitting_consultant = blank_to_na(get_optional_column(df, consultant_col_resolved)),
    periop_surgeon_grade = blank_to_na(get_optional_column(df, surgeon_grade_col_resolved)),
    periop_operating_surgeon = blank_to_na(get_optional_column(df, surgeon_name_col_resolved)),
    periop_surgery_date = parse_date_flexible(get_optional_column(df, surgery_date_col_resolved)),
    periop_source = source_label
  ) %>%
    filter(!is.na(FORM_RESPONSE_GROUP_ID) & FORM_RESPONSE_GROUP_ID != "")
}

`%||%` <- function(x, y) if (is.null(x)) y else x

periop_case_metadata <- bind_rows(
  build_periop_case_metadata(
    periop_v2,
    source_label = "V2",
    consultant_col = c("Admitting consultant"),
    surgeon_grade_col = c("Lead Operating surgeon grade", "Operating surgeon grade", "Surgeon grade"),
    surgeon_name_col = c("Lead Operating surgeon name", "Operating surgeon")
  ),
  build_periop_case_metadata(
    periop_v1,
    source_label = "V1",
    consultant_col = c("Admitting consultant"),
    surgeon_grade_col = c("Operating surgeon grade", "Lead Operating surgeon grade", "Surgeon grade"),
    surgeon_name_col = c("Operating surgeon", "Lead Operating surgeon name")
  )
) %>%
  group_by(FORM_RESPONSE_GROUP_ID) %>%
  summarise(
    periop_admitting_consultant = first_non_missing_chr(periop_admitting_consultant),
    periop_surgeon_grade = first_non_missing_chr(periop_surgeon_grade),
    periop_operating_surgeon = first_non_missing_chr(periop_operating_surgeon),
    periop_surgery_date = first_non_missing_date(periop_surgery_date),
    periop_source = first_non_missing_chr(periop_source),
    .groups = "drop"
  )

preop <- ensure_character_column(preop, "Admitting consultant")
preop <- ensure_character_column(preop, "Procedure date")
preop <- ensure_character_column(preop, "Surgeon grade")
preop <- ensure_character_column(preop, "Operating surgeon")
preop <- ensure_character_column(preop, "Peri-op source")
preop <- ensure_character_column(preop, "Peri-op surgery date")

if (nrow(periop_case_metadata) > 0) {
  preop <- preop %>%
    left_join(periop_case_metadata, by = "FORM_RESPONSE_GROUP_ID") %>%
    mutate(
      `Admitting consultant` = coalesce_nonempty_character(periop_admitting_consultant, `Admitting consultant`),
      `Procedure date` = coalesce_nonempty_character(as.character(periop_surgery_date), `Procedure date`),
      `Surgeon grade` = coalesce_nonempty_character(periop_surgeon_grade, `Surgeon grade`),
      `Operating surgeon` = coalesce_nonempty_character(periop_operating_surgeon, `Operating surgeon`),
      `Peri-op source` = coalesce_nonempty_character(periop_source, `Peri-op source`),
      `Peri-op surgery date` = coalesce_nonempty_character(as.character(periop_surgery_date), `Peri-op surgery date`)
    ) %>%
    select(-periop_admitting_consultant, -periop_surgeon_grade, -periop_operating_surgeon,
           -periop_surgery_date, -periop_source)

  cat(sprintf("Peri-op case metadata prepared for %d linked case IDs.\n", nrow(periop_case_metadata)))
} else {
  cat("No peri-op case metadata was found, so consultant and surgeon grade will fall back to the pre-op extract where available.\n")
}


#' Summarise patient demographics by admitting consultant
#'
#' Provides age, sex, BMI, and comorbidity breakdown per consultant.
#' Age is calculated from Date of Birth (Excel serial) to today's date,
#' or to Procedure date if available.
#'
#' @param consultant Optional: filter to a specific consultant
#' @param joint      Optional: filter by "Hip" or "Knee"
#' @return A list with: summary (one row per consultant), comorbidities (breakdown)
summarise_demographics_by_consultant <- function(consultant = NULL, joint = NULL) {

  df <- preop %>%
    mutate(
      dob_date  = parse_date_flexible(`Date of Birth`),
      proc_date = parse_date_flexible(`Procedure date`),
      # Age at procedure, or age today if no procedure date
      ref_date  = if_else(!is.na(proc_date), proc_date, Sys.Date()),
      age       = calc_age(dob_date, ref_date)
    )

  if (!is.null(consultant)) df <- df %>% filter(`Admitting consultant` == consultant)
  if (!is.null(joint))      df <- df %>% filter(Joint == joint)

  # --- Main demographics summary ---
  demo_summary <- df %>%
    group_by(`Admitting consultant`) %>%
    summarise(
      n_cases       = n(),
      # Age
      age_mean      = round(mean(age, na.rm = TRUE), 1),
      age_median    = median(age, na.rm = TRUE),
      age_sd        = round(sd(age, na.rm = TRUE), 1),
      age_min       = min(age, na.rm = TRUE),
      age_max       = max(age, na.rm = TRUE),
      # Sex
      n_male        = sum(Sex == "Male", na.rm = TRUE),
      n_female      = sum(Sex == "Female", na.rm = TRUE),
      pct_male      = round(sum(Sex == "Male", na.rm = TRUE) / n() * 100, 1),
      pct_female    = round(sum(Sex == "Female", na.rm = TRUE) / n() * 100, 1),
      # BMI
      bmi_mean      = round(mean(BMI, na.rm = TRUE), 1),
      bmi_median    = round(median(BMI, na.rm = TRUE), 1),
      bmi_sd        = round(sd(BMI, na.rm = TRUE), 1),
      bmi_min       = round(min(BMI, na.rm = TRUE), 1),
      bmi_max       = round(max(BMI, na.rm = TRUE), 1),
      # Comorbidities (any)
      n_comorbid    = sum(`Co-morbidities` == "Yes", na.rm = TRUE),
      pct_comorbid  = round(sum(`Co-morbidities` == "Yes", na.rm = TRUE) / n() * 100, 1),
      # Joint mix
      n_hip         = sum(Joint == "Hip", na.rm = TRUE),
      n_knee        = sum(Joint == "Knee", na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(n_cases))

  # --- Comorbidity type breakdown by consultant ---
  comorb_cols <- names(preop)[grepl("^Co-morbidities region:", names(preop))]

  comorb_detail <- df %>%
    select(`Admitting consultant`, all_of(comorb_cols)) %>%
    pivot_longer(
      cols = all_of(comorb_cols),
      names_to = "comorbidity_type",
      values_to = "present"
    ) %>%
    mutate(
      comorbidity_type = gsub("Co-morbidities region:", "", comorbidity_type)
    ) %>%
    group_by(`Admitting consultant`, comorbidity_type) %>%
    summarise(
      n_yes = sum(present == "Y", na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(
      df %>% count(`Admitting consultant`, name = "total_cases"),
      by = "Admitting consultant"
    ) %>%
    mutate(pct = round(n_yes / total_cases * 100, 1)) %>%
    arrange(`Admitting consultant`, desc(n_yes))

  list(
    summary       = demo_summary,
    comorbidities = comorb_detail
  )
}


#' Overall demographics summary (not grouped by consultant)
#'
#' @param joint Optional: filter by "Hip" or "Knee"
#' @return A tibble with one row of summary statistics
summarise_demographics <- function(joint = NULL) {

  df <- preop %>%
    mutate(
      dob_date  = parse_date_flexible(`Date of Birth`),
      proc_date = parse_date_flexible(`Procedure date`),
      ref_date  = if_else(!is.na(proc_date), proc_date, Sys.Date()),
      age       = calc_age(dob_date, ref_date)
    )

  if (!is.null(joint)) df <- df %>% filter(Joint == joint)

  tibble(
    n_cases       = nrow(df),
    age_mean      = round(mean(df$age, na.rm = TRUE), 1),
    age_median    = median(df$age, na.rm = TRUE),
    age_sd        = round(sd(df$age, na.rm = TRUE), 1),
    age_range     = paste0(min(df$age, na.rm = TRUE), "-", max(df$age, na.rm = TRUE)),
    n_male        = sum(df$Sex == "Male", na.rm = TRUE),
    n_female      = sum(df$Sex == "Female", na.rm = TRUE),
    pct_male      = round(sum(df$Sex == "Male", na.rm = TRUE) / nrow(df) * 100, 1),
    bmi_mean      = round(mean(df$BMI, na.rm = TRUE), 1),
    bmi_sd        = round(sd(df$BMI, na.rm = TRUE), 1),
    bmi_range     = paste0(round(min(df$BMI, na.rm = TRUE), 1), "-",
                           round(max(df$BMI, na.rm = TRUE), 1)),
    pct_comorbid  = round(sum(df$`Co-morbidities` == "Yes", na.rm = TRUE) / nrow(df) * 100, 1),
    n_hip         = sum(df$Joint == "Hip", na.rm = TRUE),
    n_knee        = sum(df$Joint == "Knee", na.rm = TRUE)
  )
}


# =============================================================================
# SECTION 3C: SURGICAL TIMES & IMPLANT QUERIES
# =============================================================================

# Combine V1 and V2 surgical data into one table
surgical_data <- bind_rows(
  comp_v1_data %>%
    select(FORM_RESPONSE_GROUP_ID, `Patient Id`, `MRN Number`,
           `First Name`, `Last Name`, Joint, `Procedure type`, Laterality,
           `Procedure Description`, `Procedure date`,
           `Anaesthetic start time`,
           `Knife to skin time (Left)`, `Wound closure time (Left)`,
           `Knife to skin time (Right)`, `Wound closure time (Right)`,
           ACCESS_POINT_NAME) %>%
    mutate(source = "V1"),
  comp_v2_data %>%
    select(FORM_RESPONSE_GROUP_ID, `Patient Id`, `MRN Number`,
           `First Name`, `Last Name`, Joint, `Procedure type`, Laterality,
           `Procedure Description`, `Procedure date`,
           `Anaesthetic start time`,
           `Knife to skin time (Left)`, `Wound closure time (Left)`,
           `Knife to skin time (Right)`, `Wound closure time (Right)`,
           ACCESS_POINT_NAME) %>%
    mutate(source = "V2")
) %>%
  mutate(
    # Calculate surgical duration in minutes from the relevant side
    knife_time = if_else(
      !is.na(`Knife to skin time (Left)`) & `Knife to skin time (Left)` != "",
      `Knife to skin time (Left)`,
      `Knife to skin time (Right)`
    ),
    closure_time = if_else(
      !is.na(`Wound closure time (Left)`) & `Wound closure time (Left)` != "",
      `Wound closure time (Left)`,
      `Wound closure time (Right)`
    ),
    # Parse HH:MM times and calculate duration
    knife_mins   = as.numeric(substr(knife_time, 1, 2)) * 60 +
                   as.numeric(substr(knife_time, 4, 5)),
    closure_mins = as.numeric(substr(closure_time, 1, 2)) * 60 +
                   as.numeric(substr(closure_time, 4, 5)),
    surgical_duration_mins = closure_mins - knife_mins
  )

# Combine V1 and V2 component/implant data into one table
implant_data <- bind_rows(
  comp_v1_comp %>%
    select(FORM_RESPONSE_GROUP_ID, `Patient Id`, `MRN Number`,
           `First Name`, `Last Name`,
           QUESTION_NAME, MANUFACTURER_NAME, CATALOGUE_NUMBER,
           DESCRIPTION, SERIAL_NUMBER, LOT_NUMBER,
           `Anatomical Side`, `Brand Name`, `Brand Model`,
           `Type of Device (Description)`, `Type of Device (GMDN)`,
           ACCESS_POINT_NAME) %>%
    mutate(source = "V1"),
  comp_v2_comp %>%
    select(FORM_RESPONSE_GROUP_ID, `Patient Id`, `MRN Number`,
           `First Name`, `Last Name`,
           QUESTION_NAME, MANUFACTURER_NAME, CATALOGUE_NUMBER,
           DESCRIPTION, SERIAL_NUMBER, LOT_NUMBER,
           `Anatomical Side`, `Brand Name`, `Brand Model`,
           `Type of Device (Description)`, `Type of Device (GMDN)`,
           ACCESS_POINT_NAME) %>%
    mutate(source = "V2")
)


#' Get surgical times for cases
#'
#' @param case_id    Optional: FORM_RESPONSE_GROUP_ID
#' @param mrn        Optional: MRN Number
#' @param patient_id Optional: Patient Id
#' @param joint      Optional: "Hip" or "Knee"
#' @param hospital   Optional: hospital name
#' @return A tibble with surgical times and calculated duration
get_surgical_times <- function(case_id = NULL, mrn = NULL, patient_id = NULL,
                                joint = NULL, hospital = NULL) {
  result <- surgical_data

  if (!is.null(case_id))    result <- result %>% filter(FORM_RESPONSE_GROUP_ID == case_id)
  if (!is.null(mrn))        result <- result %>% filter(`MRN Number` == mrn)
  if (!is.null(patient_id)) result <- result %>% filter(`Patient Id` == patient_id)
  if (!is.null(joint))      result <- result %>% filter(Joint == joint)
  if (!is.null(hospital))   result <- result %>% filter(ACCESS_POINT_NAME == hospital)

  result %>%
    select(FORM_RESPONSE_GROUP_ID, `MRN Number`, `First Name`, `Last Name`,
           Joint, `Procedure type`, Laterality, `Procedure Description`,
           `Procedure date`, `Anaesthetic start time`,
           knife_time, closure_time, surgical_duration_mins,
           ACCESS_POINT_NAME, source)
}


#' Summarise surgical times by a grouping variable
#'
#' @param by Grouping: "consultant", "joint", "hospital", "procedure_type",
#'   or NULL for overall
#' @param joint Optional: pre-filter by "Hip" or "Knee"
#' @return A summary tibble with duration stats (mean, median, SD, range)
summarise_surgical_times <- function(by = NULL, joint = NULL, consultant = NULL) {
  df <- surgical_data %>%
    filter(!is.na(surgical_duration_mins))

  if (!is.null(joint)) df <- df %>% filter(Joint == joint)

  # Always join to preop for consultant (needed for filtering or grouping)
  consultant_lookup <- preop %>%
    select(FORM_RESPONSE_GROUP_ID, `Admitting consultant`)
  df <- df %>%
    left_join(consultant_lookup, by = "FORM_RESPONSE_GROUP_ID")

  if (!is.null(consultant)) df <- df %>% filter(`Admitting consultant` == consultant)

  # Helper for duration summary stats
  duration_stats <- function(data) {
    data %>%
      summarise(
        n      = n(),
        mean   = round(mean(surgical_duration_mins, na.rm = TRUE), 0),
        median = median(surgical_duration_mins, na.rm = TRUE),
        sd     = round(sd(surgical_duration_mins, na.rm = TRUE), 0),
        min    = min(surgical_duration_mins, na.rm = TRUE),
        max    = max(surgical_duration_mins, na.rm = TRUE),
        .groups = "drop"
      )
  }

  # No grouping: overall summary
  if (is.null(by)) {
    return(duration_stats(df))
  }

  # Grouped summaries
  group_var <- switch(by,
    consultant     = "Admitting consultant",
    joint          = "Joint",
    hospital       = "ACCESS_POINT_NAME",
    procedure_type = "Procedure type",
    stop("Unknown 'by' value: '", by, "'. Use: 'consultant', 'joint', 'hospital', or 'procedure_type'.")
  )

  df %>%
    group_by(across(all_of(group_var))) %>%
    duration_stats() %>%
    arrange(desc(n))
}


#' Get implant/component details for cases
#'
#' @param case_id       Optional: FORM_RESPONSE_GROUP_ID
#' @param mrn           Optional: MRN Number
#' @param patient_id    Optional: Patient Id
#' @param manufacturer  Optional: filter by manufacturer name
#' @param catalogue_number Optional: filter by catalogue number
#' @return A tibble of implant component records
get_implants <- function(case_id = NULL, mrn = NULL, patient_id = NULL,
                          manufacturer = NULL, catalogue_number = NULL) {
  result <- implant_data

  if (!is.null(case_id))          result <- result %>% filter(FORM_RESPONSE_GROUP_ID == case_id)
  if (!is.null(mrn))              result <- result %>% filter(`MRN Number` == mrn)
  if (!is.null(patient_id))       result <- result %>% filter(`Patient Id` == patient_id)
  if (!is.null(manufacturer))     result <- result %>% filter(MANUFACTURER_NAME == manufacturer)
  if (!is.null(catalogue_number)) result <- result %>% filter(CATALOGUE_NUMBER == catalogue_number)

  result %>%
    select(FORM_RESPONSE_GROUP_ID, `MRN Number`, `First Name`, `Last Name`,
           `Anatomical Side`, MANUFACTURER_NAME, CATALOGUE_NUMBER, DESCRIPTION,
           SERIAL_NUMBER, LOT_NUMBER, `Brand Name`, `Brand Model`,
           `Type of Device (Description)`, `Type of Device (GMDN)`,
           ACCESS_POINT_NAME, source)
}


#' Summarise implant usage (count of components by manufacturer, catalogue, etc.)
#'
#' @param by Grouping: "manufacturer", "catalogue", "description", "hospital",
#'   "consultant", or NULL for manufacturer counts
#' @param joint Optional: pre-filter by joining to preop for joint type
#' @return A summary tibble of implant counts
summarise_implants <- function(by = NULL, joint = NULL) {
  df <- implant_data

  # If filtering by joint or grouping by consultant, join to preop
  if (!is.null(joint) || (!is.null(by) && by == "consultant")) {
    preop_lookup <- preop %>%
      select(FORM_RESPONSE_GROUP_ID, `Admitting consultant`, Joint)
    df <- df %>%
      left_join(preop_lookup, by = "FORM_RESPONSE_GROUP_ID")
    if (!is.null(joint)) df <- df %>% filter(Joint == joint)
  }

  if (is.null(by) || by == "manufacturer") {
    df %>%
      filter(!is.na(MANUFACTURER_NAME) & MANUFACTURER_NAME != "") %>%
      count(MANUFACTURER_NAME, sort = TRUE) %>%
      rename(count = n)
  } else if (by == "catalogue") {
    df %>%
      filter(!is.na(CATALOGUE_NUMBER) & CATALOGUE_NUMBER != "") %>%
      count(MANUFACTURER_NAME, CATALOGUE_NUMBER, DESCRIPTION, sort = TRUE) %>%
      rename(count = n)
  } else if (by == "description") {
    df %>%
      filter(!is.na(DESCRIPTION) & DESCRIPTION != "") %>%
      count(DESCRIPTION, MANUFACTURER_NAME, sort = TRUE) %>%
      rename(count = n)
  } else if (by == "hospital") {
    df %>%
      filter(!is.na(MANUFACTURER_NAME) & MANUFACTURER_NAME != "") %>%
      count(ACCESS_POINT_NAME, MANUFACTURER_NAME, sort = TRUE) %>%
      rename(count = n)
  } else if (by == "consultant") {
    df %>%
      filter(!is.na(MANUFACTURER_NAME) & MANUFACTURER_NAME != "") %>%
      count(`Admitting consultant`, MANUFACTURER_NAME, sort = TRUE) %>%
      rename(count = n)
  }
}


#' List distinct implant types used, optionally filtered
#'
#' Returns a deduplicated list of implant components showing manufacturer,
#' catalogue number, and description, with a count of how many times each
#' has been used.
#'
#' @param joint        Optional: "Hip" or "Knee"
#' @param manufacturer Optional: filter to a specific manufacturer
#' @param consultant   Optional: filter to a specific consultant
#' @param hospital     Optional: filter to a specific hospital
#' @return A tibble of distinct implant types with usage counts
list_implant_types <- function(joint = NULL, manufacturer = NULL,
                                consultant = NULL, hospital = NULL) {

  df <- implant_data %>%
    filter(!is.na(DESCRIPTION) & DESCRIPTION != "")

  # Join to preop for joint, consultant filtering
  preop_lookup <- preop %>%
    select(FORM_RESPONSE_GROUP_ID, `Admitting consultant`, Joint)
  df <- df %>%
    left_join(preop_lookup, by = "FORM_RESPONSE_GROUP_ID")

  if (!is.null(joint))        df <- df %>% filter(Joint == joint)
  if (!is.null(manufacturer)) df <- df %>% filter(MANUFACTURER_NAME == manufacturer)
  if (!is.null(consultant))   df <- df %>% filter(`Admitting consultant` == consultant)
  if (!is.null(hospital))     df <- df %>% filter(ACCESS_POINT_NAME == hospital)

  df %>%
    count(MANUFACTURER_NAME, CATALOGUE_NUMBER, DESCRIPTION, sort = TRUE) %>%
    rename(times_used = n)
}


# =============================================================================
# SECTION 3D: IMPLANT CLASSIFICATION
# =============================================================================
# Classifies each case (FORM_RESPONSE_GROUP_ID) by:
#   - Fixation type: Cemented / Uncemented / Hybrid
#   - Knee system (e.g. Triathlon, Attune, PFC Sigma)
#   - Hip femoral component (e.g. Exeter, Corail, Accolade II)
#   - Hip acetabular component (e.g. Pinnacle, Trident, G7)

library(stringr)

# --- Keyword definitions ------------------------------------------------------

cement_keywords <- c(
  "BONE CEMENT", "PALACOS", "SIMPLEX", "CMW 1", "CMW1",
  "SMARTSET", "COPAL", "REFOBACIN", "GENTAMICIN BONE CEMENT",
  "GENTAMICIN 20", "GENTAMICIN 40", "GENATIMICIN"
)

hip_stem_brands <- list(
  list(pattern = "CORAIL",          label = "Corail",          fixation = "uncemented"),
  list(pattern = "ACCOLADE",        label = "Accolade II",     fixation = "uncemented"),
  list(pattern = "TAPERLOC",        label = "Taperloc",        fixation = "uncemented"),
  list(pattern = "ACTIS",           label = "Actis",           fixation = "uncemented"),
  list(pattern = "SYNERGY",         label = "Synergy",         fixation = "uncemented"),
  list(pattern = "ANTHOLOGY",       label = "Anthology",       fixation = "uncemented"),
  list(pattern = "AVENIR",          label = "Avenir",          fixation = "uncemented"),
  list(pattern = "ECHELON",         label = "Echelon",         fixation = "uncemented"),
  list(pattern = "PROFEMUR",        label = "Profemur",        fixation = "uncemented"),
  list(pattern = "TRI-LOCK|TRILOCK",label = "Tri-Lock",        fixation = "uncemented"),
  list(pattern = "S-ROM|SROM",      label = "S-ROM",           fixation = "uncemented"),
  list(pattern = "RECLAIM",         label = "Reclaim",         fixation = "uncemented"),
  list(pattern = "RESTORATION.*HA|RESTORATION.*PLASMA", label = "Restoration", fixation = "uncemented"),
  list(pattern = "SECUR-FIT|SECURFIT",label = "Secur-Fit",     fixation = "uncemented"),
  list(pattern = "SUMMIT",          label = "Summit",          fixation = "uncemented"),
  list(pattern = "WAGNER",          label = "Wagner",          fixation = "uncemented"),
  list(pattern = "FITMORE",         label = "Fitmore",         fixation = "uncemented"),
  list(pattern = "GMRS",            label = "GMRS",            fixation = "uncemented"),
  list(pattern = "STANMORE",        label = "Stanmore",        fixation = "uncemented"),
  list(pattern = "EXETER",          label = "Exeter",          fixation = "cemented"),
  list(pattern = "C-STEM|CSTEM|C STEM", label = "C-Stem AMT",  fixation = "cemented"),
  list(pattern = "CPT STEM",        label = "CPT",             fixation = "cemented"),
  list(pattern = "SPECTRON",        label = "Spectron",        fixation = "cemented"),
  list(pattern = "CHARNLEY",        label = "Charnley",        fixation = "cemented"),
  list(pattern = "POLISHED TAPER",  label = "Polished Taper",  fixation = "cemented"),
  list(pattern = "MS-30",           label = "MS-30",           fixation = "cemented"),
  list(pattern = "LUBINUS",         label = "Lubinus",         fixation = "cemented")
)

hip_cup_brands <- list(
  list(pattern = "PINNACLE",                        label = "Pinnacle",     fixation = "uncemented"),
  list(pattern = "TRIDENT|TRITANIUM",               label = "Trident",      fixation = "uncemented"),
  list(pattern = "G7",                              label = "G7",           fixation = "uncemented"),
  list(pattern = "CONTINUUM",                       label = "Continuum",    fixation = "uncemented"),
  list(pattern = "PRESSFIT",                        label = "Pressfit",     fixation = "uncemented"),
  list(pattern = "TM ACETABULAR",                   label = "TM Shell",     fixation = "uncemented"),
  list(pattern = "DELTA TT",                        label = "Delta TT",     fixation = "uncemented"),
  list(pattern = "R3 ACETABULAR|R3 SHELL",          label = "R3",           fixation = "uncemented"),
  list(pattern = "RESTORATION.*(ADM|MDM|DUAL)",     label = "Restoration ADM/MDM", fixation = "uncemented"),
  list(pattern = "BI-MENTUM|BIMENTUM",              label = "Bi-Mentum",    fixation = "cemented"),
  list(pattern = "EXETER.*CUP|EXETER.*RIMFIT|EXETER.*FLANGED", label = "Exeter Cup", fixation = "cemented"),
  list(pattern = "MARATHON.*CEMENTED|CEMENTED.*CUP.*MARATHON", label = "Marathon Cemented Cup", fixation = "cemented"),
  list(pattern = "ALL.?POLY CONSTRAINED",           label = "All-Poly Constrained", fixation = "cemented")
)

knee_system_brands <- list(
  list(pattern = "ATTUNE",         label = "Attune"),
  list(pattern = "TRIATHLON",      label = "Triathlon"),
  list(pattern = "LCS|MBT",       label = "LCS"),
  list(pattern = "PFC|SIGMA",     label = "PFC Sigma"),
  list(pattern = "NEXGEN",         label = "NexGen"),
  list(pattern = "GEMINI",         label = "Gemini"),
  list(pattern = "COLUMBUS",       label = "Columbus"),
  list(pattern = "NOILES",         label = "Noiles Rotating Hinge"),
  list(pattern = "VANGUARD",       label = "Vanguard"),
  list(pattern = "GENESIS",        label = "Genesis"),
  list(pattern = "LEGION",         label = "Legion"),
  list(pattern = "JOURNEY",        label = "Journey")
)

# --- Build classification table -----------------------------------------------

cat("Classifying implants...\n")

classify_implants <- function(comp_data) {
  df <- comp_data %>%
    mutate(DESC_UPPER = toupper(DESCRIPTION))

  # Flag each row
  df <- df %>%
    mutate(
      is_cement = str_detect(DESC_UPPER, str_c(cement_keywords, collapse = "|")),
      is_cemented_component = str_detect(DESC_UPPER, fixed("CEMENTED")),
      is_porous_tibial = str_detect(DESC_UPPER, "POR") &
        str_detect(DESC_UPPER, "TIBIAL|TIB BASE") &
        !str_detect(DESC_UPPER, "NON[- ]?POR"),
      is_porous_femoral_knee = str_detect(DESC_UPPER, "POR") &
        str_detect(DESC_UPPER, "FEM|FEMUR") &
        !str_detect(DESC_UPPER, "NON[- ]?POR"),
      is_hip_component = str_detect(DESC_UPPER, str_c(c(
        "STEM", "LINER", "SHELL", "CUP", "PINNACLE", "TRIDENT", "TRITANIUM",
        "EXETER", "CORAIL", "C-STEM", "ACCOLADE", "DUAL MOBILITY",
        "ACETABULAR", "BI-MENTUM", "BIMENTUM", "MDM", "CERAMIC", "ARTICUL",
        "V40", "ORTHINOX", "CENTRALISER", "PRESSFIT", "G7", "ACTIS",
        "TAPERLOC", "GMRS", "RESTORATION", "S-ROM", "SROM", "STANMORE"
      ), collapse = "|")),
      is_knee_component = str_detect(DESC_UPPER, str_c(c(
        "TIBIAL", "ATTUNE", "TRIATHLON", "SIGMA", "PFC", "LCS", "MBT",
        "PERSONA", "VANGUARD", "GENESIS", "LEGION", "JOURNEY", "NEXGEN",
        "GEMINI", "COLUMBUS", "NOILES", "PKR"
      ), collapse = "|"))
    )

  # Detect brands per row
  for (brand in hip_stem_brands) {
    col_name <- paste0("stem_", gsub("[^A-Za-z0-9]", "_", brand$label))
    df[[col_name]] <- str_detect(df$DESC_UPPER, brand$pattern)
  }
  for (brand in hip_cup_brands) {
    col_name <- paste0("cup_", gsub("[^A-Za-z0-9]", "_", brand$label))
    df[[col_name]] <- str_detect(df$DESC_UPPER, brand$pattern)
  }
  for (brand in knee_system_brands) {
    col_name <- paste0("knee_", gsub("[^A-Za-z0-9]", "_", brand$label))
    df[[col_name]] <- str_detect(df$DESC_UPPER, brand$pattern)
  }

  # Aggregate per case
  group_results <- df %>%
    group_by(FORM_RESPONSE_GROUP_ID) %>%
    summarise(
      has_cement        = any(is_cement, na.rm = TRUE),
      has_cemented_comp = any(is_cemented_component, na.rm = TRUE),
      has_porous_tibial       = any(is_porous_tibial, na.rm = TRUE),
      has_porous_femoral_knee = any(is_porous_femoral_knee, na.rm = TRUE),
      hip_count  = sum(is_hip_component, na.rm = TRUE),
      knee_count = sum(is_knee_component, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      cement_evidence = has_cement | has_cemented_comp,
      joint_type = case_when(
        hip_count > knee_count  ~ "Hip",
        knee_count > hip_count  ~ "Knee",
        hip_count > 0           ~ "Hip",
        knee_count > 0          ~ "Knee",
        TRUE                    ~ "Unknown"
      )
    )

  # --- Detect brand matches per case using row-level flags ---
  # Build per-row brand labels, then aggregate per case

  # Hip stems: label each row
  df$row_stem_label <- NA_character_
  df$row_stem_fixation <- NA_character_
  for (brand in hip_stem_brands) {
    col_name <- paste0("stem_", gsub("[^A-Za-z0-9]", "_", brand$label))
    match_rows <- which(df[[col_name]] == TRUE)
    df$row_stem_label[match_rows] <- brand$label
    df$row_stem_fixation[match_rows] <- brand$fixation
  }

  # Hip cups: label each row
  df$row_cup_label <- NA_character_
  df$row_cup_fixation <- NA_character_
  for (brand in hip_cup_brands) {
    col_name <- paste0("cup_", gsub("[^A-Za-z0-9]", "_", brand$label))
    match_rows <- which(df[[col_name]] == TRUE)
    df$row_cup_label[match_rows] <- brand$label
    df$row_cup_fixation[match_rows] <- brand$fixation
  }

  # Knee systems: label each row
  df$row_knee_label <- NA_character_
  for (brand in knee_system_brands) {
    col_name <- paste0("knee_", gsub("[^A-Za-z0-9]", "_", brand$label))
    match_rows <- which(df[[col_name]] == TRUE)
    df$row_knee_label[match_rows] <- brand$label
  }

  # Aggregate brand labels per case
  implant_info <- df %>%
    group_by(FORM_RESPONSE_GROUP_ID) %>%
    summarise(
      hip_stem_brands_matched = paste(unique(na.omit(row_stem_label)), collapse = "; "),
      has_cemented_stem   = any(row_stem_fixation == "cemented", na.rm = TRUE),
      has_uncemented_stem = any(row_stem_fixation == "uncemented", na.rm = TRUE),
      hip_cup_brands_matched = paste(unique(na.omit(row_cup_label)), collapse = "; "),
      has_cemented_cup   = any(row_cup_fixation == "cemented", na.rm = TRUE),
      has_uncemented_cup = any(row_cup_fixation == "uncemented", na.rm = TRUE),
      knee_system_matched = paste(unique(na.omit(row_knee_label)), collapse = "; "),
      .groups = "drop"
    )

  group_results <- group_results %>%
    left_join(implant_info, by = "FORM_RESPONSE_GROUP_ID")

  # Classify fixation
  group_results <- group_results %>%
    mutate(
      fixation_type = case_when(
        joint_type == "Hip" & cement_evidence & has_cemented_stem & has_uncemented_cup ~ "Hybrid",
        joint_type == "Hip" & cement_evidence & has_uncemented_stem ~ "Hybrid",
        joint_type == "Hip" & cement_evidence & has_uncemented_cup ~ "Hybrid",
        joint_type == "Hip" & cement_evidence & has_cemented_stem & !has_uncemented_cup ~ "Cemented",
        joint_type == "Hip" & cement_evidence ~ "Cemented",
        joint_type == "Hip" & !cement_evidence & has_cemented_stem ~ "Hybrid",
        joint_type == "Hip" & !cement_evidence ~ "Uncemented",
        joint_type == "Knee" & cement_evidence & (has_porous_tibial | has_porous_femoral_knee) ~ "Hybrid",
        joint_type == "Knee" & cement_evidence ~ "Cemented",
        joint_type == "Knee" & !cement_evidence & has_porous_tibial & has_porous_femoral_knee ~ "Uncemented",
        joint_type == "Knee" & !cement_evidence ~ "Uncemented",
        cement_evidence ~ "Cemented",
        TRUE ~ "Uncemented"
      ),
      knee_system = if_else(joint_type == "Knee" & knee_system_matched != "",
                            knee_system_matched, NA_character_),
      femoral_component = if_else(joint_type == "Hip" & hip_stem_brands_matched != "",
                                   hip_stem_brands_matched, NA_character_),
      acetabular_component = if_else(joint_type == "Hip" & hip_cup_brands_matched != "",
                                      hip_cup_brands_matched, NA_character_)
    )

  group_results %>%
    select(FORM_RESPONSE_GROUP_ID, joint_type, fixation_type,
           femoral_component, acetabular_component, knee_system)
}

# Run classification on combined implant data
implant_classification <- classify_implants(implant_data)

cat(sprintf("  %d cases classified.\n", nrow(implant_classification)))
cat(sprintf("  Joint: %s\n",
            paste(names(table(implant_classification$joint_type)),
                  table(implant_classification$joint_type), sep = "=", collapse = ", ")))
cat(sprintf("  Fixation: %s\n\n",
            paste(names(table(implant_classification$fixation_type)),
                  table(implant_classification$fixation_type), sep = "=", collapse = ", ")))


#' Get implant classification for cases
#'
#' @param case_id    Optional: FORM_RESPONSE_GROUP_ID
#' @param mrn        Optional: MRN Number
#' @param joint      Optional: "Hip" or "Knee"
#' @param fixation   Optional: "Cemented", "Uncemented", or "Hybrid"
#' @return A tibble with classification per case
get_implant_classification <- function(case_id = NULL, mrn = NULL,
                                        joint = NULL, fixation = NULL) {
  result <- implant_classification

  if (!is.null(case_id))  result <- result %>% filter(FORM_RESPONSE_GROUP_ID == case_id)
  if (!is.null(joint))    result <- result %>% filter(joint_type == joint)
  if (!is.null(fixation)) result <- result %>% filter(fixation_type == fixation)

  if (!is.null(mrn)) {
    mrn_cases <- preop %>%
      filter(`MRN Number` == mrn) %>%
      pull(FORM_RESPONSE_GROUP_ID)
    result <- result %>% filter(FORM_RESPONSE_GROUP_ID %in% mrn_cases)
  }

  result
}


#' Summarise PROMs by implant type
#'
#' @param prom_type One of "knee", "hip", or "eq5d"
#' @param by        Implant grouping: "knee_system", "femoral_component",
#'                  "acetabular_component", or "fixation".
#'                  Add " AND consultant" to also group by consultant, e.g.
#'                  "femoral_component AND consultant"
#' @param stage     Optional: filter to a specific PROMs stage
#' @param consultant Optional: filter to a specific consultant
#' @return A summary tibble with score stats per implant group
summarise_proms_by_implant <- function(prom_type = c("knee", "hip", "eq5d"),
                                        by = NULL, stage = NULL,
                                        consultant = NULL) {
  prom_type <- match.arg(prom_type)

  df <- switch(prom_type,
    knee = oks,
    hip  = ohs,
    eq5d = eq5d
  )

  df <- df %>%
    filter(!is.na(Score)) %>%
    left_join(implant_classification, by = "FORM_RESPONSE_GROUP_ID") %>%
    left_join(
      preop %>% select(FORM_RESPONSE_GROUP_ID, `Admitting consultant`),
      by = "FORM_RESPONSE_GROUP_ID"
    )

  if (!is.null(stage))      df <- df %>% filter(Stage == stage)
  if (!is.null(consultant)) df <- df %>% filter(`Admitting consultant` == consultant)

  # Default grouping based on prom type
  if (is.null(by)) {
    by <- switch(prom_type,
      knee = "knee_system",
      hip  = "femoral_component",
      eq5d = "fixation"
    )
  }

  # Check if consultant grouping is requested via "AND consultant"
  include_consultant <- grepl("AND consultant", by, ignore.case = TRUE)
  by_clean <- trimws(gsub("(?i)\\s*AND\\s*consultant", "", by))

  # Map the implant grouping keyword to the actual column name
  implant_col_map <- c(
    knee_system          = "knee_system",
    femoral_component    = "femoral_component",
    acetabular_component = "acetabular_component",
    fixation             = "fixation_type"
  )

  if (!by_clean %in% names(implant_col_map)) {
    stop("Unknown 'by' value: '", by_clean,
         "'. Use: 'knee_system', 'femoral_component', 'acetabular_component', or 'fixation'.",
         "\n  Optionally add ' AND consultant' to also group by consultant.")
  }

  implant_col <- implant_col_map[[by_clean]]

  # Build grouping columns
  group_cols <- c(implant_col)
  if (include_consultant) group_cols <- c(group_cols, "Admitting consultant")
  group_cols <- c(group_cols, "Stage")

  df %>%
    filter(!is.na(!!sym(implant_col)) & !!sym(implant_col) != "") %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(
      n      = n(),
      mean   = round(mean(Score, na.rm = TRUE), 1),
      median = median(Score, na.rm = TRUE),
      sd     = round(sd(Score, na.rm = TRUE), 1),
      min    = min(Score, na.rm = TRUE),
      max    = max(Score, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(across(all_of(group_cols)))
}


#' Summarise surgical times by implant type
#'
#' @param by   Implant grouping: "knee_system", "femoral_component",
#'             "acetabular_component", or "fixation"
#' @param joint Optional: pre-filter by "Hip" or "Knee"
#' @return A summary tibble with duration stats per implant group
summarise_surgical_times_by_implant <- function(by = "fixation", joint = NULL) {
  df <- surgical_data %>%
    filter(!is.na(surgical_duration_mins)) %>%
    left_join(implant_classification, by = "FORM_RESPONSE_GROUP_ID")

  if (!is.null(joint)) df <- df %>% filter(joint_type == joint)

  group_col <- switch(by,
    knee_system          = "knee_system",
    femoral_component    = "femoral_component",
    acetabular_component = "acetabular_component",
    fixation             = "fixation_type",
    stop("Unknown 'by' value: '", by,
         "'. Use: 'knee_system', 'femoral_component', 'acetabular_component', or 'fixation'.")
  )

  df %>%
    filter(!is.na(!!sym(group_col)) & !!sym(group_col) != "") %>%
    group_by(across(all_of(group_col))) %>%
    summarise(
      n      = n(),
      mean   = round(mean(surgical_duration_mins, na.rm = TRUE), 0),
      median = median(surgical_duration_mins, na.rm = TRUE),
      sd     = round(sd(surgical_duration_mins, na.rm = TRUE), 0),
      min    = min(surgical_duration_mins, na.rm = TRUE),
      max    = max(surgical_duration_mins, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(n))
}


#' Summarise complications by implant type
#'
#' @param by              Implant grouping: "knee_system", "femoral_component",
#'                        "acetabular_component", or "fixation"
#' @param complication_type Optional: filter to a specific complication
#' @param joint           Optional: pre-filter by "Hip" or "Knee"
#' @return A summary tibble with complication counts per implant group
summarise_complications_by_implant <- function(by = "fixation",
                                                complication_type = NULL,
                                                joint = NULL) {
  all_comp <- get_all_complications(complication_type = complication_type) %>%
    left_join(implant_classification, by = "FORM_RESPONSE_GROUP_ID")

  if (!is.null(joint)) all_comp <- all_comp %>% filter(joint_type == joint)

  group_col <- switch(by,
    knee_system          = "knee_system",
    femoral_component    = "femoral_component",
    acetabular_component = "acetabular_component",
    fixation             = "fixation_type",
    stop("Unknown 'by' value.")
  )

  # Count complications per implant group
  comp_counts <- all_comp %>%
    filter(!is.na(!!sym(group_col)) & !!sym(group_col) != "") %>%
    count(across(all_of(group_col)), complication, sort = TRUE) %>%
    rename(count = n)

  # Total cases per implant group (for rates)
  case_counts <- implant_classification %>%
    { if (!is.null(joint)) filter(., joint_type == joint) else . } %>%
    filter(!is.na(!!sym(group_col)) & !!sym(group_col) != "") %>%
    count(across(all_of(group_col)), name = "total_cases")

  comp_counts %>%
    left_join(case_counts, by = group_col) %>%
    mutate(rate_pct = round(count / total_cases * 100, 1)) %>%
    arrange(across(all_of(group_col)), desc(count))
}


# =============================================================================
# SECTION 4: COMPLICATIONS QUERIES
# =============================================================================

#' Extract complications from Post-Op V1 data
#'
#' Returns a long-format table: one row per complication per case.
#'
#' @param case_id          Optional: FORM_RESPONSE_GROUP_ID
#' @param mrn              Optional: MRN Number
#' @param patient_id       Optional: Patient Id
#' @param complication_type Optional: filter by type. One of:
#'   "periprosthetic_fracture", "instability", "wound_infection",
#'   "suspected_pji", "haematoma", "revision", "clot", "cardiopulmonary",
#'   "readmission", or NULL for all
#' @return A tibble of complication records
get_complications_v1 <- function(case_id = NULL, mrn = NULL, patient_id = NULL,
                                  complication_type = NULL) {

  df <- postop_v1 %>%
    select(
      FORM_RESPONSE_GROUP_ID, `Patient Id`, `MRN Number`,
      `First Name`, `Last Name`,
      Joint, `Procedure type`, `Laterality of this Post-Op Assessment`,
      `Presentation type`, `Date of Assessment`,
      ACCESS_POINT_NAME,
      `Periprosthetic fracture (Left)`, `Periprosthetic fracture (Right)`,
      `Periprosthetic fracture treatment (Left)`, `Periprosthetic fracture date (Left)`,
      `Periprosthetic fracture treatment (Right)`, `Periprosthetic fracture date (Right)`,
      `Instability (Left)`, `Instability (Right)`,
      `Instability date (Left)`, `Instability date (Right)`,
      `Wound infection (Left)`, `Wound infection (Right)`,
      `Wound infection date (Left)`, `Wound infection date (Right)`,
      `Suspected Periprosthetic joint infection (Left)`,
      `Suspected Periprosthetic joint infection (Right)`,
      `Wound haematoma washout (Left)`, `Wound haematoma washout (Right)`,
      `Revision due to complication (Left)`, `Revision due to complication (Right)`,
      `Clot (Left)`, `Clot (Right)`,
      `Clot location (Left)`, `Clot treatment (Left)`,
      `Clot location (Right)`, `Clot treatment (Right)`,
      `Cardiopulmonary complication (Left)`, `Cardiopulmonary complication (Right)`,
      `Cardiopulmonary complication outcome (Left)`,
      `Cardiopulmonary complication outcome (Right)`,
      `Readmission within 30 days of date of surgery (Left)`,
      `Readmission within 30 days of date of surgery (Right)`
    )

  if (!is.null(case_id))    df <- df %>% filter(FORM_RESPONSE_GROUP_ID == case_id)
  if (!is.null(mrn))        df <- df %>% filter(`MRN Number` == mrn)
  if (!is.null(patient_id)) df <- df %>% filter(`Patient Id` == patient_id)

  # Helper: build complication rows for a given side
  build_comp <- function(data, field, label, side,
                         detail_field = NULL, date_field = NULL) {
    filtered <- data %>% filter(!!sym(field) == "Yes")
    if (nrow(filtered) == 0) return(tibble())
    result <- filtered %>%
      transmute(
        FORM_RESPONSE_GROUP_ID, `Patient Id`, `MRN Number`,
        `First Name`, `Last Name`,
        Joint, `Procedure type`,
        presentation = `Presentation type`,
        assessment_date = `Date of Assessment`,
        hospital = ACCESS_POINT_NAME,
        complication = label,
        side = side
      )
    result$detail <- if (!is.null(detail_field)) filtered[[detail_field]] else NA_character_
    result$date   <- if (!is.null(date_field))   filtered[[date_field]]   else NA_character_
    result
  }

  complications <- bind_rows(
    build_comp(df, "Periprosthetic fracture (Left)", "Periprosthetic fracture", "Left",
               "Periprosthetic fracture treatment (Left)", "Periprosthetic fracture date (Left)"),
    build_comp(df, "Periprosthetic fracture (Right)", "Periprosthetic fracture", "Right",
               "Periprosthetic fracture treatment (Right)", "Periprosthetic fracture date (Right)"),
    build_comp(df, "Instability (Left)", "Instability", "Left",
               date_field = "Instability date (Left)"),
    build_comp(df, "Instability (Right)", "Instability", "Right",
               date_field = "Instability date (Right)"),
    build_comp(df, "Wound infection (Left)", "Wound infection", "Left",
               date_field = "Wound infection date (Left)"),
    build_comp(df, "Wound infection (Right)", "Wound infection", "Right",
               date_field = "Wound infection date (Right)"),
    build_comp(df, "Suspected Periprosthetic joint infection (Left)", "Suspected PJI", "Left"),
    build_comp(df, "Suspected Periprosthetic joint infection (Right)", "Suspected PJI", "Right"),
    build_comp(df, "Wound haematoma washout (Left)", "Haematoma washout", "Left"),
    build_comp(df, "Wound haematoma washout (Right)", "Haematoma washout", "Right"),
    build_comp(df, "Revision due to complication (Left)", "Revision due to complication", "Left"),
    build_comp(df, "Revision due to complication (Right)", "Revision due to complication", "Right"),
    build_comp(df, "Clot (Left)", "Clot/VTE", "Left"),
    build_comp(df, "Clot (Right)", "Clot/VTE", "Right"),
    build_comp(df, "Cardiopulmonary complication (Left)", "Cardiopulmonary", "Left",
               detail_field = "Cardiopulmonary complication outcome (Left)"),
    build_comp(df, "Cardiopulmonary complication (Right)", "Cardiopulmonary", "Right",
               detail_field = "Cardiopulmonary complication outcome (Right)"),
    build_comp(df, "Readmission within 30 days of date of surgery (Left)",
               "Readmission within 30 days", "Left"),
    build_comp(df, "Readmission within 30 days of date of surgery (Right)",
               "Readmission within 30 days", "Right")
  ) %>%
    mutate(source = "V1")

  if (!is.null(complication_type)) {
    type_map <- c(
      periprosthetic_fracture = "Periprosthetic fracture",
      instability             = "Instability",
      wound_infection         = "Wound infection",
      suspected_pji           = "Suspected PJI",
      haematoma               = "Haematoma washout",
      revision                = "Revision due to complication",
      clot                    = "Clot/VTE",
      cardiopulmonary         = "Cardiopulmonary",
      readmission             = "Readmission within 30 days"
    )
    if (complication_type %in% names(type_map)) {
      complications <- complications %>% filter(complication == type_map[[complication_type]])
    }
  }

  return(complications)
}


#' Extract complications from Post-Op V2 data
#'
#' @inheritParams get_complications_v1
#' @return A tibble of complication records (V2 column naming)
get_complications_v2 <- function(case_id = NULL, mrn = NULL, patient_id = NULL,
                                  complication_type = NULL) {

  df <- postop_v2

  if (!is.null(case_id))    df <- df %>% filter(FORM_RESPONSE_GROUP_ID == case_id)
  if (!is.null(mrn))        df <- df %>% filter(`MRN Number` == mrn)
  if (!is.null(patient_id)) df <- df %>% filter(`Patient Id` == patient_id)

  build_comp <- function(data, field, label, side,
                         detail_field = NULL, date_field = NULL) {
    filtered <- data %>% filter(!!sym(field) == "Yes")
    if (nrow(filtered) == 0) return(tibble())
    result <- filtered %>%
      transmute(
        FORM_RESPONSE_GROUP_ID, `Patient Id`, `MRN Number`,
        `First Name`, `Last Name`,
        Joint, `Procedure type`,
        presentation = `Presentation type`,
        assessment_date = `Date of Assessment`,
        hospital = ACCESS_POINT_NAME,
        complication = label,
        side = side
      )
    result$detail <- if (!is.null(detail_field)) filtered[[detail_field]] else NA_character_
    result$date   <- if (!is.null(date_field))   filtered[[date_field]]   else NA_character_
    result
  }

  complications <- bind_rows(
    build_comp(df, "Periprosthetic fracture (Left)", "Periprosthetic fracture", "Left",
               "Periprosthetic fracture treatment (Left)", "Periprosthetic fracture date (Left)"),
    build_comp(df, "Periprosthetic fracture (Right)", "Periprosthetic fracture", "Right",
               "Periprosthetic fracture treatment (Right)", "Periprosthetic fracture date (Right)"),
    build_comp(df, "Instability (Left)", "Instability", "Left",
               date_field = "Instability date (Left)"),
    build_comp(df, "Instability (Right)", "Instability", "Right",
               date_field = "Instability date (Right)"),
    build_comp(df, "Surgical Site infection (Left)", "Surgical site infection", "Left"),
    build_comp(df, "Surgical Site infection (Right)", "Surgical site infection", "Right"),
    build_comp(df, "Wound haematoma washout (Left)", "Haematoma washout", "Left",
               date_field = "Wound haematoma washout date (Left)"),
    build_comp(df, "Wound haematoma washout (Right)", "Haematoma washout", "Right",
               date_field = "Wound haematoma washout date (Right)"),
    build_comp(df, "Revision due to complication (Left)", "Revision due to complication", "Left"),
    build_comp(df, "Revision due to complication (Right)", "Revision due to complication", "Right"),
    build_comp(df, "Venous thromboembolism (VTE) (Left)", "VTE", "Left"),
    build_comp(df, "Venous thromboembolism (VTE) (Right)", "VTE", "Right"),
    build_comp(df, "Cardiopulmonary complication (Left)", "Cardiopulmonary", "Left",
               detail_field = "Cardiopulmonary complication outcome (Left)",
               date_field = "Cardiopulmonary complication date (Left)"),
    build_comp(df, "Cardiopulmonary complication (Right)", "Cardiopulmonary", "Right",
               detail_field = "Cardiopulmonary complication outcome (Right)",
               date_field = "Cardiopulmonary complication date (Right)"),
    build_comp(df, "Readmission within 30 days after date of surgery (Left)",
               "Readmission within 30 days", "Left"),
    build_comp(df, "Readmission within 30 days after date of surgery (Right)",
               "Readmission within 30 days", "Right")
  ) %>%
    mutate(source = "V2")

  if (!is.null(complication_type)) {
    type_map <- c(
      periprosthetic_fracture = "Periprosthetic fracture",
      instability             = "Instability",
      wound_infection         = "Surgical site infection",
      haematoma               = "Haematoma washout",
      revision                = "Revision due to complication",
      vte                     = "VTE",
      cardiopulmonary         = "Cardiopulmonary",
      readmission             = "Readmission within 30 days"
    )
    if (complication_type %in% names(type_map)) {
      complications <- complications %>% filter(complication == type_map[[complication_type]])
    }
  }

  return(complications)
}


#' Get all complications from both V1 and V2 post-op assessments
#'
#' @inheritParams get_complications_v1
#' @return A tibble combining V1 and V2 complications
get_all_complications <- function(case_id = NULL, mrn = NULL, patient_id = NULL,
                                   complication_type = NULL) {
  v1 <- get_complications_v1(case_id, mrn, patient_id, complication_type)
  v2 <- get_complications_v2(case_id, mrn, patient_id, complication_type)
  bind_rows(v1, v2)
}


#' Summarise complication counts
#'
#' @param by Optional grouping: "joint", "hospital", "procedure_type",
#'   "consultant", or NULL for overall counts
#' @return A summary tibble
summarise_complications <- function(by = NULL) {
  all_comp <- get_all_complications()

  # If grouping by consultant, join to pre-op to get Admitting consultant
  if (!is.null(by) && by == "consultant") {
    consultant_lookup <- preop %>%
      select(FORM_RESPONSE_GROUP_ID, `Admitting consultant`)
    all_comp <- all_comp %>%
      left_join(consultant_lookup, by = "FORM_RESPONSE_GROUP_ID")
    return(
      all_comp %>%
        count(`Admitting consultant`, complication, sort = TRUE) %>%
        rename(count = n)
    )
  }

  if (is.null(by)) {
    all_comp %>% count(complication, sort = TRUE) %>% rename(count = n)
  } else if (by == "joint") {
    all_comp %>% count(Joint, complication, sort = TRUE) %>% rename(count = n)
  } else if (by == "hospital") {
    all_comp %>% count(hospital, complication, sort = TRUE) %>% rename(count = n)
  } else if (by == "procedure_type") {
    all_comp %>% count(`Procedure type`, complication, sort = TRUE) %>% rename(count = n)
  }
}


#' Detailed complication summary by admitting consultant
#'
#' Joins complications to pre-op via FORM_RESPONSE_GROUP_ID and provides
#' counts and rates per consultant. Optionally filter by complication type
#' or consultant name.
#'
#' @param consultant      Optional: filter to a specific consultant
#' @param complication_type Optional: filter to a specific complication type
#' @param joint           Optional: filter by "Hip" or "Knee"
#' @return A summary tibble
summarise_complications_by_consultant <- function(consultant = NULL,
                                                   complication_type = NULL,
                                                   joint = NULL) {
  # Get all complications
  all_comp <- get_all_complications(complication_type = complication_type)

  # Join to pre-op for consultant and case counts
  consultant_lookup <- preop %>%
    select(FORM_RESPONSE_GROUP_ID, `Admitting consultant`, Joint)

  all_comp <- all_comp %>%
    left_join(
      consultant_lookup %>% rename(preop_joint = Joint),
      by = "FORM_RESPONSE_GROUP_ID"
    )

  if (!is.null(consultant)) all_comp <- all_comp %>%
    filter(`Admitting consultant` == consultant)
  if (!is.null(joint)) all_comp <- all_comp %>%
    filter(preop_joint == joint)

  # Total cases per consultant (for rate calculation)
  case_counts <- preop %>%
    { if (!is.null(joint)) filter(., Joint == joint) else . } %>%
    count(`Admitting consultant`, name = "total_cases")

  # Complication counts per consultant
  comp_summary <- all_comp %>%
    count(`Admitting consultant`, complication, name = "count") %>%
    arrange(`Admitting consultant`, desc(count))

  # Add total cases and complication rate
  comp_summary %>%
    left_join(case_counts, by = "Admitting consultant") %>%
    mutate(rate_pct = round(count / total_cases * 100, 1)) %>%
    arrange(`Admitting consultant`, desc(count))
}


# =============================================================================
# SECTION 5: COMBINED CASE PROFILE
# =============================================================================

#' Get a complete case profile: demographics, PROMs, and complications
#'
#' When using case_id, returns data for that specific joint replacement.
#' When using mrn, returns data for ALL joint replacements for that patient.
#'
#' @param case_id FORM_RESPONSE_GROUP_ID (specific joint)
#' @param mrn     MRN Number (all joints for patient)
#' @return A list with: demographics, proms (knee/hip/eq5d), complications
get_case_profile <- function(case_id = NULL, mrn = NULL) {

  if (is.null(case_id) & is.null(mrn)) {
    stop("Provide either case_id (FORM_RESPONSE_GROUP_ID) or mrn.")
  }

  demo <- preop
  if (!is.null(case_id)) demo <- demo %>% filter(FORM_RESPONSE_GROUP_ID == case_id)
  if (!is.null(mrn))     demo <- demo %>% filter(`MRN Number` == mrn)

  demo <- demo %>%
    select(FORM_RESPONSE_GROUP_ID, `Patient Id`, `MRN Number`,
           `First Name`, `Last Name`, Sex, `Date of Birth`,
           Joint, Laterality, `Procedure type`, `Procedure code`,
           `Procedure date`, BMI, `Co-morbidities`, ACCESS_POINT_NAME)

  list(
    demographics  = demo,
    proms         = get_all_proms(case_id = case_id, mrn = mrn),
    complications = get_all_complications(case_id = case_id, mrn = mrn)
  )
}


#' Get a patient-level overview showing all their cases
#'
#' Useful for patients with multiple joint replacements (e.g. bilateral hips).
#'
#' @param mrn MRN Number
#' @return A list with: cases (all joint replacements), and per-case PROMs/complications
get_patient_overview <- function(mrn) {
  patient_cases <- list_cases(mrn = mrn)

  if (nrow(patient_cases) == 0) {
    message("No cases found for MRN: ", mrn)
    return(NULL)
  }

  cat(sprintf("Patient: %s %s (MRN: %s)\n",
              patient_cases$first_name[1], patient_cases$last_name[1], mrn))
  cat(sprintf("Number of joint replacement cases: %d\n\n", nrow(patient_cases)))

  for (i in seq_len(nrow(patient_cases))) {
    row <- patient_cases[i, ]
    cat(sprintf("  Case %d: %s %s %s (%s) - FORM_RESPONSE_GROUP_ID: %s\n",
                i, row$procedure_type, row$laterality, row$joint,
                row$hospital, row$case_id))
  }

  list(
    cases         = patient_cases,
    proms         = get_all_proms(mrn = mrn),
    complications = get_all_complications(mrn = mrn)
  )
}


# =============================================================================
# SECTION 6: QUICK REFERENCE
# =============================================================================

cat("=============================================================\n")
cat("  INOR Query Tool - Ready\n")
cat("=============================================================\n\n")
cat("KEY CONCEPT:\n")
cat("  FORM_RESPONSE_GROUP_ID = unique case per joint replacement\n")
cat("  A bilateral patient has ONE MRN but TWO case IDs.\n")
cat("  All functions accept case_id for joint-specific queries,\n")
cat("  or mrn for all joints belonging to a patient.\n\n")
cat("CASE REGISTRY:\n")
cat("  list_cases(mrn, patient_id, joint, hospital)\n\n")
cat("PROMs FUNCTIONS:\n")
cat("  get_oxford_knee_scores(case_id, mrn, patient_id, stage, laterality)\n")
cat("  get_oxford_hip_scores(case_id, mrn, patient_id, stage, laterality)\n")
cat("  get_eq5d_scores(case_id, mrn, patient_id, stage)\n")
cat("  get_all_proms(case_id, mrn, patient_id)\n")
cat("  get_proms_trajectory(case_id, prom_type)     # score over time\n")
cat("  summarise_proms_by_stage('knee' | 'hip' | 'eq5d')\n")
cat("  summarise_proms_by_consultant(prom_type, stage, consultant)\n\n")
cat("DEMOGRAPHICS FUNCTIONS:\n")
cat("  summarise_demographics(joint)                   # overall summary\n")
cat("  summarise_demographics_by_consultant(consultant, joint)\n")
cat("    Returns a list with:\n")
cat("      $summary       - age, sex, BMI, comorbidity rate per consultant\n")
cat("      $comorbidities - breakdown by comorbidity type per consultant\n\n")
cat("SURGICAL TIMES FUNCTIONS:\n")
cat("  get_surgical_times(case_id, mrn, patient_id, joint, hospital)\n")
cat("  summarise_surgical_times(by, joint, consultant)\n")
cat("IMPLANT FUNCTIONS:\n")
cat("  get_implants(case_id, mrn, patient_id, manufacturer, catalogue_number)\n")
cat("  list_implant_types(joint, manufacturer, consultant, hospital)\n")
cat("  summarise_implants(by = 'manufacturer'|'catalogue'|'description'|'hospital'|'consultant')\n")
cat("  get_implant_classification(case_id, mrn, joint, fixation)\n")
cat("  summarise_proms_by_implant(prom_type, by, stage)\n")
cat("  summarise_surgical_times_by_implant(by, joint)\n")
cat("  summarise_complications_by_implant(by, complication_type, joint)\n")
cat("    by: 'knee_system'|'femoral_component'|'acetabular_component'|'fixation'\n\n")
cat("COMPLICATIONS FUNCTIONS:\n")
cat("  get_complications_v1(case_id, mrn, patient_id, complication_type)\n")
cat("  get_complications_v2(case_id, mrn, patient_id, complication_type)\n")
cat("  get_all_complications(case_id, mrn, patient_id, complication_type)\n")
cat("  summarise_complications(by = 'joint'|'hospital'|'procedure_type'|'consultant')\n")
cat("  summarise_complications_by_consultant(consultant, complication_type, joint)\n\n")
cat("COMBINED:\n")
cat("  get_case_profile(case_id, mrn)   # one case or all for a patient\n")
cat("  get_patient_overview(mrn)        # summary of all cases for a patient\n\n")
cat("COMPLICATION TYPES (for filtering):\n")
cat("  V1: periprosthetic_fracture, instability, wound_infection,\n")
cat("      suspected_pji, haematoma, revision, clot, cardiopulmonary,\n")
cat("      readmission\n")
cat("  V2: periprosthetic_fracture, instability, wound_infection,\n")
cat("      haematoma, revision, vte, cardiopulmonary, readmission\n\n")
cat("EXAMPLE USAGE:\n")
cat('  list_cases(joint = "Knee")                               # all knee cases\n')
cat('  get_oxford_knee_scores(stage = "Pre-op presentation")    # all pre-op OKS\n')
cat('  get_proms_trajectory(case_id = 200000100, "knee")        # score over time\n')
cat('  get_all_complications(complication_type = "wound_infection")\n')
cat('  get_case_profile(case_id = 900000050)                    # single joint\n')
cat('  get_patient_overview(mrn = "D688508")                    # all joints\n')
cat('  summarise_proms_by_stage("knee")\n')
cat('  summarise_proms_by_consultant("knee", stage = "1 year presentation")\n')
cat('  summarise_proms_by_consultant("hip", consultant = "Healy, Laura")\n')
cat('  summarise_complications(by = "joint")\n')
cat('  summarise_complications(by = "consultant")\n')
cat('  summarise_complications_by_consultant()                          # all consultants with rates\n')
cat('  summarise_complications_by_consultant(consultant = "Healy, Laura")\n')
cat('  summarise_complications_by_consultant(joint = "Knee")            # knee cases only\n')
cat('  summarise_complications_by_consultant(complication_type = "wound_infection")\n')
cat('  summarise_demographics()                                  # overall\n')
cat('  summarise_demographics(joint = "Knee")                    # knee cases only\n')
cat('  summarise_demographics_by_consultant()                    # by consultant\n')
cat('  summarise_demographics_by_consultant(joint = "Hip")       # hip cases by consultant\n')
cat('  summarise_demographics_by_consultant(consultant = "Healy, Laura")\n')
cat('  get_surgical_times(joint = "Knee")                        # all knee surgical times\n')
cat('  summarise_surgical_times(by = "consultant")               # duration by consultant\n')
cat('  summarise_surgical_times(by = "joint")                    # duration by joint\n')
cat('  summarise_surgical_times(consultant = "Healy, Laura")       # one consultant\n')
cat('  summarise_surgical_times(by = "joint", consultant = "Healy, Laura") # by joint for one consultant\n')
cat('  get_implants(manufacturer = "Stryker")                    # all Stryker implants\n')
cat('  get_implants(case_id = "200000100")                       # implants for one case\n')
cat('  list_implant_types(joint = "Knee")                        # all knee implant types\n')
cat('  list_implant_types(joint = "Hip", manufacturer = "Stryker") # Stryker hip implants\n')
cat('  list_implant_types(consultant = "Healy, Laura")           # implants used by consultant\n')
cat('  summarise_implants()                                      # counts by manufacturer\n')
cat('  summarise_implants(by = "catalogue")                      # by catalogue number\n')
cat('  summarise_implants(by = "consultant", joint = "Hip")      # by consultant, hips only\n')
cat('  get_implant_classification(joint = "Knee")               # all knee classifications\n')
cat('  get_implant_classification(fixation = "Cemented")        # all cemented cases\n')
cat('  summarise_proms_by_implant("knee", by = "knee_system")   # OKS by knee system\n')
cat('  summarise_proms_by_implant("knee", by = "fixation", stage = "1 year presentation")\n')
cat('  summarise_proms_by_implant("hip", by = "femoral_component")  # OHS by stem\n')
cat('  summarise_proms_by_implant("hip", by = "acetabular_component") # OHS by cup\n')
cat('  summarise_surgical_times_by_implant(by = "knee_system")  # op time by knee system\n')
cat('  summarise_surgical_times_by_implant(by = "fixation", joint = "Hip")\n')
cat('  summarise_complications_by_implant(by = "knee_system")   # complications by system\n')
cat('  summarise_complications_by_implant(by = "fixation", joint = "Hip")\n')
cat("=============================================================\n")
