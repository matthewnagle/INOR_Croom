
suppressPackageStartupMessages({
  library(shiny)
  library(DT)
  library(dplyr)
  library(ggplot2)
  library(bslib)
  library(htmltools)
  library(readr)
  library(tidyr)
  library(stringr)
})

options(shiny.maxRequestSize = 100 * 1024^2)

has_local_csvs <- function(path) {
  if (!nzchar(path) || !dir.exists(path)) return(FALSE)
  same_dir_csv <- list.files(path, pattern = "\\.csv$", ignore.case = TRUE, full.names = TRUE)
  rawdata_dir <- file.path(path, "RawData")
  rawdata_csv <- if (dir.exists(rawdata_dir)) {
    list.files(rawdata_dir, pattern = "\\.csv$", ignore.case = TRUE, full.names = TRUE)
  } else {
    character()
  }
  length(c(same_dir_csv, rawdata_csv)) > 0
}

get_app_dir <- function() {
  wd <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)

  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  script_dir <- character()
  if (length(file_arg) > 0) {
    script_dir <- dirname(normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = FALSE))
  }

  frame_files <- vapply(
    sys.frames(),
    function(x) {
      if (!is.null(x$ofile)) {
        normalizePath(x$ofile, winslash = "/", mustWork = FALSE)
      } else {
        NA_character_
      }
    },
    character(1)
  )
  frame_files <- stats::na.omit(frame_files)

  candidates <- unique(c(
    wd,
    if (length(frame_files) > 0) dirname(frame_files[[length(frame_files)]]) else character(),
    script_dir
  ))

  for (cand in candidates) {
    if (!nzchar(cand)) next

    helper_here <- file.path(cand, "INOR_query_tool.R")
    if (file.exists(helper_here) && has_local_csvs(cand)) {
      return(cand)
    }
    if (file.exists(helper_here)) {
      return(cand)
    }

    parent <- dirname(cand)
    helper_parent <- file.path(parent, "INOR_query_tool.R")
    if (file.exists(helper_parent) && has_local_csvs(parent)) {
      return(parent)
    }
    if (file.exists(helper_parent)) {
      return(parent)
    }
  }

  stop(
    paste0(
      "Could not find the app folder. ",
      "Keep app.R and INOR_query_tool.R together. CSV files can sit in RawData/ or beside the app, ",
      "and export timestamps in the filenames are now ignored."
    ),
    call. = FALSE
  )
}

app_dir <- get_app_dir()
options(inor_app_base_dir = app_dir)
source(file.path(app_dir, "INOR_query_tool.R"), local = TRUE, chdir = TRUE)

normalise_name_key_app <- function(x) {
  tolower(gsub("[^a-z0-9]+", "", trimws(as.character(x))))
}

resolve_optional_colname_app <- function(df, candidates) {
  if (is.null(df) || !length(names(df)) || is.null(candidates) || !length(candidates)) {
    return(NULL)
  }

  nm <- names(df)
  nm_key <- normalise_name_key_app(nm)
  cand <- as.character(candidates)
  cand_key <- normalise_name_key_app(cand)

  for (i in seq_along(cand)) {
    exact_idx <- match(cand[[i]], nm)
    if (!is.na(exact_idx)) return(nm[[exact_idx]])

    key_idx <- match(cand_key[[i]], nm_key)
    if (!is.na(key_idx)) return(nm[[key_idx]])
  }

  NULL
}

trim_names_app <- function(df) {
  if (is.null(df) || !is.data.frame(df)) return(df)
  names(df) <- trimws(names(df))
  names(df) <- gsub("\\s+", " ", names(df))
  df
}

blank_to_na_app <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x[x == ""] <- NA_character_
  x
}

coalesce_nonempty_character_app <- function(primary, fallback) {
  dplyr::coalesce(blank_to_na_app(primary), blank_to_na_app(fallback))
}

first_non_missing_chr_app <- function(x) {
  x <- blank_to_na_app(x)
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_character_)
  x[[1]]
}

ensure_app_column <- function(df, column_name, default = NA_character_) {
  if (!column_name %in% names(df)) {
    df[[column_name]] <- rep(default, nrow(df))
  }
  df
}

extract_filename_timestamp_num_app <- function(path) {
  name <- basename(path)
  match <- regexpr("[0-9]{4}-[0-9]{2}-[0-9]{2}_[0-9]{2}-[0-9]{2}-[0-9]{2}", name)
  if (match[[1]] == -1) return(NA_real_)
  stamp <- regmatches(name, match)
  as.numeric(as.POSIXct(stamp, format = "%Y-%m-%d_%H-%M-%S", tz = "UTC"))
}

match_export_filename_app <- function(filename, prefix, suffix) {
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

choose_best_match_app <- function(paths) {
  if (!length(paths)) return(NULL)
  if (length(paths) == 1) return(paths[[1]])

  stamp <- vapply(paths, extract_filename_timestamp_num_app, numeric(1))
  mtime <- as.numeric(file.info(paths)$mtime)
  order_index <- order(!is.na(stamp), stamp, mtime, decreasing = TRUE, na.last = TRUE)
  paths[[order_index[[1]]]]
}

candidate_data_dirs_app <- function() {
  unique(c(
    file.path(app_dir, "RawData"),
    app_dir,
    file.path(dirname(app_dir), "RawData"),
    dirname(app_dir),
    file.path(getwd(), "RawData"),
    getwd()
  ))
}

resolve_optional_export_app <- function(prefix, suffix = ".csv") {
  for (dir_path in candidate_data_dirs_app()) {
    if (!nzchar(dir_path) || !dir.exists(dir_path)) next

    files <- list.files(
      dir_path,
      pattern = "\\.csv$",
      full.names = TRUE,
      ignore.case = TRUE
    )

    if (!length(files)) next

    keep <- vapply(
      basename(files),
      match_export_filename_app,
      logical(1),
      prefix = prefix,
      suffix = suffix
    )

    matches <- files[keep]
    if (length(matches)) return(choose_best_match_app(matches))
  }

  NULL
}

read_optional_export_app <- function(prefix, suffix = ".csv") {
  path <- resolve_optional_export_app(prefix, suffix)
  if (is.null(path) || !file.exists(path)) return(tibble::tibble())
  trim_names_app(
    readr::read_csv(
      path,
      show_col_types = FALSE,
      col_types = readr::cols(.default = readr::col_character())
    )
  )
}

build_periop_case_metadata_app <- function(df, source_label) {
  if (is.null(df) || !is.data.frame(df) || !nrow(df)) {
    return(tibble::tibble(
      FORM_RESPONSE_GROUP_ID = character(),
      periop_admitting_consultant = character(),
      periop_surgeon_grade = character(),
      periop_operating_surgeon = character(),
      periop_surgery_date = as.Date(character()),
      periop_source = character()
    ))
  }

  df <- trim_names_app(df)
  id_col <- resolve_optional_colname_app(df, "FORM_RESPONSE_GROUP_ID")
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

  consultant_col <- resolve_optional_colname_app(df, c("Admitting consultant"))
  surgeon_grade_col <- resolve_optional_colname_app(df, c("Lead Operating surgeon grade", "Operating surgeon grade", "Surgeon grade"))
  surgeon_name_col <- resolve_optional_colname_app(df, c("Lead Operating surgeon name", "Operating surgeon"))
  surgery_date_col <- resolve_optional_colname_app(df, c("Date of surgery", "Procedure date"))

  tibble::tibble(
    FORM_RESPONSE_GROUP_ID = as.character(df[[id_col]]),
    periop_admitting_consultant = blank_to_na_app(if (!is.null(consultant_col)) df[[consultant_col]] else rep(NA_character_, nrow(df))),
    periop_surgeon_grade = blank_to_na_app(if (!is.null(surgeon_grade_col)) df[[surgeon_grade_col]] else rep(NA_character_, nrow(df))),
    periop_operating_surgeon = blank_to_na_app(if (!is.null(surgeon_name_col)) df[[surgeon_name_col]] else rep(NA_character_, nrow(df))),
    periop_surgery_date = parse_date_flexible(if (!is.null(surgery_date_col)) df[[surgery_date_col]] else rep(NA_character_, nrow(df))),
    periop_source = source_label
  ) %>%
    filter(!is.na(FORM_RESPONSE_GROUP_ID) & FORM_RESPONSE_GROUP_ID != "")
}

enrich_preop_from_periop_fallback <- function(preop_df) {
  if (is.null(preop_df) || !is.data.frame(preop_df)) return(preop_df)

  preop_df <- trim_names_app(preop_df)
  for (col in c("Admitting consultant", "Procedure date", "Surgeon grade", "Operating surgeon", "Peri-op source", "Peri-op surgery date")) {
    preop_df <- ensure_app_column(preop_df, col, NA_character_)
  }

  periop_v2_local <- if (exists("periop_v2", inherits = FALSE) && is.data.frame(periop_v2) && nrow(periop_v2) > 0) {
    trim_names_app(periop_v2)
  } else {
    read_optional_export_app("NAP_INOR_PERI_OP_ASSESSMENT_V2")
  }

  periop_v1_local <- if (exists("periop_v1", inherits = FALSE) && is.data.frame(periop_v1) && nrow(periop_v1) > 0) {
    trim_names_app(periop_v1)
  } else {
    read_optional_export_app("NAP_INOR_PERI_OP_ASSESSMENT_V1")
  }

  periop_meta <- bind_rows(
    build_periop_case_metadata_app(periop_v2_local, "V2"),
    build_periop_case_metadata_app(periop_v1_local, "V1")
  ) %>%
    group_by(FORM_RESPONSE_GROUP_ID) %>%
    summarise(
      periop_admitting_consultant = first_non_missing_chr_app(periop_admitting_consultant),
      periop_surgeon_grade = first_non_missing_chr_app(periop_surgeon_grade),
      periop_operating_surgeon = first_non_missing_chr_app(periop_operating_surgeon),
      periop_surgery_date = if (all(is.na(periop_surgery_date))) as.Date(NA) else stats::na.omit(periop_surgery_date)[[1]],
      periop_source = first_non_missing_chr_app(periop_source),
      .groups = "drop"
    )

  if (!nrow(periop_meta) || !"FORM_RESPONSE_GROUP_ID" %in% names(preop_df)) return(preop_df)

  preop_df %>%
    left_join(periop_meta, by = "FORM_RESPONSE_GROUP_ID") %>%
    mutate(
      `Admitting consultant` = coalesce_nonempty_character_app(periop_admitting_consultant, `Admitting consultant`),
      `Procedure date` = coalesce_nonempty_character_app(as.character(periop_surgery_date), `Procedure date`),
      `Surgeon grade` = coalesce_nonempty_character_app(periop_surgeon_grade, `Surgeon grade`),
      `Operating surgeon` = coalesce_nonempty_character_app(periop_operating_surgeon, `Operating surgeon`),
      `Peri-op source` = coalesce_nonempty_character_app(periop_source, `Peri-op source`),
      `Peri-op surgery date` = coalesce_nonempty_character_app(as.character(periop_surgery_date), `Peri-op surgery date`)
    ) %>%
    select(-periop_admitting_consultant, -periop_surgeon_grade, -periop_operating_surgeon,
           -periop_surgery_date, -periop_source)
}

if (exists("preop", inherits = FALSE) && is.data.frame(preop)) {
  helper_version <- get0("inor_query_tool_version", ifnotfound = NA_character_)
  had_surgeon_grade <- "Surgeon grade" %in% names(preop)

  preop <- enrich_preop_from_periop_fallback(preop)

  for (col in c(
    "FORM_RESPONSE_GROUP_ID", "Patient Id", "MRN Number", "First Name", "Last Name",
    "Sex", "Date of Birth", "Joint", "Laterality", "Procedure type", "Procedure code",
    "Procedure date", "Co-morbidities", "Admitting consultant", "Surgeon grade",
    "ACCESS_POINT_NAME"
  )) {
    preop <- ensure_app_column(preop, col, NA_character_)
  }

  if (!"BMI" %in% names(preop)) {
    preop[["BMI"]] <- rep(NA_real_, nrow(preop))
  }
  preop[["BMI"]] <- suppressWarnings(as.numeric(preop[["BMI"]]))

  if (!had_surgeon_grade || is.na(helper_version) || !identical(helper_version, "v6")) {
    message(
      "App startup applied compatibility fallback logic for peri-op fields. ",
      "If you mixed file versions locally, replace both app.R and INOR_query_tool.R together."
    )
  }
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x)) || identical(x, "")) {
    y
  } else {
    x
  }
}

stage_order <- c(
  "Pre-op presentation", "6 month presentation", "1 year presentation",
  "2 year presentation", "5 year presentation", "10 year presentation",
  "Extraordinary presentation"
)

# Stages used for pre-op vs post-op change analysis. "Extraordinary
# presentation" is deliberately excluded: it is an unscheduled review, not a
# fixed follow-up point, so it cannot be pooled with the timed reviews.
prom_preop_stage <- "Pre-op presentation"
prom_followup_stages <- c(
  "6 month presentation", "1 year presentation",
  "2 year presentation", "5 year presentation", "10 year presentation"
)

count_unique_ids <- function(df, key = "FORM_RESPONSE_GROUP_ID") {
  if (!key %in% names(df)) return(NA_integer_)
  length(unique(stats::na.omit(df[[key]])))
}

count_overlap_ids <- function(df1, df2, key = "FORM_RESPONSE_GROUP_ID") {
  if (!key %in% names(df1) || !key %in% names(df2)) return(0L)
  ids1 <- unique(stats::na.omit(df1[[key]]))
  ids2 <- unique(stats::na.omit(df2[[key]]))
  length(intersect(ids1, ids2))
}

metric_box <- function(title, value, subtitle = NULL) {
  tags$div(
    class = "metric-box",
    tags$div(class = "metric-title", title),
    tags$div(class = "metric-value", value),
    if (!is.null(subtitle)) tags$div(class = "metric-subtitle", subtitle)
  )
}

datatable_or_message <- function(df, message = "No matching records.") {
  if (is.null(df) || nrow(df) == 0) {
    return(
      DT::datatable(
        data.frame(Message = message, check.names = FALSE),
        rownames = FALSE,
        options = list(dom = "t", pageLength = 1)
      )
    )
  }

  DT::datatable(
    df,
    rownames = FALSE,
    filter = "top",
    options = list(
      pageLength = 10,
      lengthMenu = c(10, 25, 50, 100),
      scrollX = TRUE,
      autoWidth = TRUE
    )
  )
}

write_export <- function(df, file) {
  readr::write_csv(as.data.frame(df), file, na = "")
}

make_choice_vector <- function(x) {
  x <- as.character(x)
  x <- unique(stats::na.omit(x))
  x <- trimws(x)
  x <- x[nzchar(x)]
  c("All", sort(x))
}

split_delimited_choices <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x)]
  x <- trimws(x)
  x <- x[nzchar(x)]
  if (!length(x)) return("All")
  values <- trimws(unlist(strsplit(x, ";", fixed = TRUE)))
  values <- unique(values[nzchar(values)])
  c("All", sort(values))
}

match_delimited_value <- function(x, value) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  vapply(
    strsplit(x, ";", fixed = TRUE),
    function(parts) value %in% trimws(parts),
    logical(1)
  )
}

apply_single_filter <- function(df, column, value) {
  if (!column %in% names(df)) return(df)
  value <- trimws(as.character(value %||% ""))
  if (!nzchar(value) || identical(value, "All")) return(df)
  df %>% filter(.data[[column]] == value)
}

apply_component_filter <- function(df, column, value) {
  if (!column %in% names(df)) return(df)
  value <- trimws(as.character(value %||% ""))
  if (!nzchar(value) || identical(value, "All")) return(df)
  df[match_delimited_value(df[[column]], value), , drop = FALSE]
}

apply_date_filter <- function(df, column, range_value) {
  if (!column %in% names(df)) return(df)
  if (is.null(range_value) || length(range_value) < 2 || any(is.na(range_value))) return(df)
  start_date <- as.Date(range_value[[1]])
  end_date <- as.Date(range_value[[2]])
  df %>%
    filter(!is.na(.data[[column]]) & .data[[column]] >= start_date & .data[[column]] <= end_date)
}

date_bounds <- function(x) {
  x <- as.Date(x)
  x <- stats::na.omit(x)
  if (!length(x)) return(NULL)
  list(min = min(x), max = max(x))
}

make_date_input <- function(input_id, label, bounds) {
  if (is.null(bounds)) {
    return(tags$p(class = "help-note", "No usable dates were found for this dataset."))
  }

  dateRangeInput(
    inputId = input_id,
    label = label,
    start = bounds$min,
    end = bounds$max,
    min = bounds$min,
    max = bounds$max
  )
}

first_non_missing_chr <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x)]
  x <- trimws(x)
  x <- x[nzchar(x)]
  if (!length(x)) return(NA_character_)
  x[[1]]
}

first_non_missing_date <- function(x) {
  x <- as.Date(x)
  x <- x[!is.na(x)]
  if (!length(x)) return(as.Date(NA))
  x[[1]]
}

expand_group_column <- function(df, group_col) {
  if (identical(group_col, "overall")) {
    return(df)
  }

  if (!group_col %in% names(df)) {
    return(df[0, , drop = FALSE])
  }

  result <- df %>%
    mutate(.group_value = as.character(.data[[group_col]])) %>%
    filter(!is.na(.group_value))

  if (group_col %in% c("knee_system", "femoral_component", "acetabular_component")) {
    result <- result %>%
      separate_rows(.group_value, sep = ";") %>%
      mutate(.group_value = trimws(.group_value))
  }

  result <- result %>%
    filter(!is.na(.group_value) & .group_value != "")

  # Age and BMI bands are ordered factors; keep that order so every summary and
  # chart reads low-to-high rather than alphabetically ("Under 50" last, or
  # "Obese III" before "Overweight").
  if (is.factor(df[[group_col]]) && nrow(result) > 0) {
    present <- levels(df[[group_col]])
    present <- present[present %in% unique(result$.group_value)]
    if (length(present)) {
      result$.group_value <- factor(
        result$.group_value,
        levels = present,
        ordered = is.ordered(df[[group_col]])
      )
    }
  }

  result
}

friendly_group_label <- function(group_col) {
  switch(
    group_col,
    overall = "Overall",
    consultant = "Consultant",
    surgeon_grade = "Surgeon grade",
    fixation_type = "Implant fixation type",
    knee_system = "Knee system",
    femoral_component = "Femoral component",
    acetabular_component = "Acetabular component",
    patient_sex = "Sex",
    age_band = "Age band",
    bmi_band = "BMI band",
    patient_age = "Age",
    bmi = "BMI",
    Stage = "Stage",
    Joint = "Joint",
    ACCESS_POINT_NAME = "Hospital",
    `Procedure type` = "Procedure type",
    source = "Source",
    analysis_joint = "Joint",
    analysis_hospital = "Hospital",
    analysis_proc_type = "Procedure type",
    group_col
  )
}

rename_group_column <- function(df, label) {
  if (".group_value" %in% names(df)) {
    names(df)[names(df) == ".group_value"] <- label
  }
  df
}

summarise_scores <- function(df, group_cols = NULL, score_col = "Score",
                             pass_threshold = NULL, digits = 1) {
  if (is.null(df) || nrow(df) == 0 || !score_col %in% names(df)) {
    return(data.frame())
  }

  df <- df %>% filter(!is.na(.data[[score_col]]))
  if (nrow(df) == 0) return(data.frame())

  if (!is.null(group_cols) && length(group_cols) > 0) {
    df <- df %>% group_by(across(all_of(group_cols)))
  }

  out <- df %>%
    summarise(
      n = n(),
      mean = round(mean(.data[[score_col]], na.rm = TRUE), digits),
      median = median(.data[[score_col]], na.rm = TRUE),
      sd = round(sd(.data[[score_col]], na.rm = TRUE), digits),
      min = min(.data[[score_col]], na.rm = TRUE),
      max = max(.data[[score_col]], na.rm = TRUE),
      `PASS (%)` = pass_rate(.data[[score_col]], pass_threshold),
      .groups = "drop"
    )

  if (is.null(pass_threshold) || length(pass_threshold) != 1 || is.na(pass_threshold)) {
    out[["PASS (%)"]] <- NULL
  }

  out
}

# -----------------------------------------------------------------------------
# Pre-op vs post-op change helpers
# -----------------------------------------------------------------------------
# All three PROMs in this app run "higher is better" (OKS 0-48, OHS 0-48,
# EQ-5D index up to 1.0), so change = follow-up score - pre-op score and a
# positive change is an improvement.

# Minimal clinically important difference defaults — the individual-patient
# (ROC-derived) values, since the app reports the proportion of individual
# patients achieving a meaningful improvement.
#   OKS 7 and OHS 8: Beard et al., J Clin Epidemiol 2015;68(1):73-79.
#   EQ-5D index 0.074: Walters & Brazier, Qual Life Res 2005.
prom_default_mcid <- function(prom_type) {
  switch(prom_type, knee = 7, hip = 8, eq5d = 0.074, 0)
}

# Patient acceptable symptom state (PASS) defaults — the score at or above which
# a patient rates their state as acceptable. Unlike the MCID this is an absolute
# post-op score, so it needs no pre-op baseline.
#   OKS 30 at 12 and 24 months (27 at 3 months):
#     Ingelsrud et al., Acta Orthop 2020;92(1):85-90.
#   OHS 40 at 1 year (34 at 3 months, 39 at 2 years):
#     Galea et al., Acta Orthop 2020;91(4):372-377.
#   EQ-5D index 0.8 at 12 months — reported thresholds run 0.68 to 0.85
#     depending on the value set used:
#     Conner-Spady et al., Qual Life Res 2022 (doi:10.1007/s11136-022-03287-9).
# Published thresholds drift with follow-up length and case mix, so the app
# treats these as editable defaults rather than fixed truths.
prom_default_pass <- function(prom_type) {
  switch(prom_type, knee = 30, hip = 40, eq5d = 0.8, NA_real_)
}

prom_pass_reference_note <- function(prom_type) {
  switch(
    prom_type,
    knee = "Published OKS thresholds: 27 at 3 months, 30 at 12 and 24 months (Ingelsrud, Acta Orthop 2020).",
    hip = "Published OHS thresholds: 34 at 3 months, 40 at 1 year, 39 at 2 years (Galea, Acta Orthop 2020).",
    eq5d = "Published EQ-5D index thresholds run 0.68 to 0.85 depending on the value set (Conner-Spady, Qual Life Res 2022).",
    ""
  )
}

# PASS describes a post-operative state, so headline rates and group comparisons
# are computed over follow-up records only. Including pre-op rows (where almost
# nobody is in an acceptable state) would drag every rate down, and would let a
# group's stage mix masquerade as a difference in outcome.
prom_postop_records <- function(df) {
  if (is.null(df) || nrow(df) == 0 || !"Stage" %in% names(df)) return(df)
  df %>% filter(as.character(Stage) %in% prom_followup_stages)
}

# -----------------------------------------------------------------------------
# Patient demographics
# -----------------------------------------------------------------------------

age_band_levels <- c("Under 50", "50-59", "60-69", "70-79", "80 and over")

band_age <- function(age) {
  age <- suppressWarnings(as.numeric(age))
  factor(
    dplyr::case_when(
      is.na(age) ~ NA_character_,
      age < 50 ~ "Under 50",
      age < 60 ~ "50-59",
      age < 70 ~ "60-69",
      age < 80 ~ "70-79",
      TRUE ~ "80 and over"
    ),
    levels = age_band_levels,
    ordered = TRUE
  )
}

# WHO categories. Obesity classes are kept separate because operative risk and
# PROM gain differ across them, and pooling everything over 30 hides that.
bmi_band_levels <- c(
  "Underweight (<18.5)", "Healthy (18.5-24.9)", "Overweight (25-29.9)",
  "Obese I (30-34.9)", "Obese II (35-39.9)", "Obese III (40+)"
)

band_bmi <- function(bmi) {
  bmi <- suppressWarnings(as.numeric(bmi))
  factor(
    dplyr::case_when(
      is.na(bmi) ~ NA_character_,
      bmi < 18.5 ~ "Underweight (<18.5)",
      bmi < 25 ~ "Healthy (18.5-24.9)",
      bmi < 30 ~ "Overweight (25-29.9)",
      bmi < 35 ~ "Obese I (30-34.9)",
      bmi < 40 ~ "Obese II (35-39.9)",
      TRUE ~ "Obese III (40+)"
    ),
    levels = bmi_band_levels,
    ordered = TRUE
  )
}

tidy_sex_value <- function(x) {
  x <- trimws(as.character(x))
  x[is.na(x) | !nzchar(x)] <- NA_character_
  x
}

# Range filter that keeps or drops unrecorded values explicitly, rather than
# letting a slider silently delete every case with no value on file.
apply_range_filter <- function(df, column, range_value, include_missing = TRUE) {
  if (!column %in% names(df)) return(df)
  values <- suppressWarnings(as.numeric(df[[column]]))

  if (is.null(range_value) || length(range_value) < 2 || any(is.na(range_value))) {
    if (isTRUE(include_missing)) return(df)
    return(df[!is.na(values), , drop = FALSE])
  }

  in_range <- !is.na(values) & values >= range_value[[1]] & values <= range_value[[2]]
  keep <- if (isTRUE(include_missing)) in_range | is.na(values) else in_range
  df[keep, , drop = FALSE]
}

numeric_bounds <- function(x, pad = TRUE) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  if (!length(x)) return(NULL)
  lo <- floor(min(x))
  hi <- ceiling(max(x))
  if (lo == hi) hi <- lo + 1
  list(min = lo, max = hi)
}

# One colour per follow-up stage, dark to light with time since surgery, so the
# same stage reads the same way on every chart.
prom_stage_palette <- function() {
  stats::setNames(
    c("#1e3a8a", "#1d4ed8", "#3b82f6", "#60a5fa", "#93c5fd"),
    prom_followup_stages
  )
}

# Share of records at or above the PASS threshold. Returns NA rather than 0 when
# there is nothing to judge, so an empty cohort never reads as "0% acceptable".
pass_rate <- function(x, threshold) {
  if (length(threshold) != 1 || is.na(threshold)) return(NA_real_)
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_real_)
  round(100 * mean(x >= threshold), 1)
}

prom_score_digits <- function(prom_type) {
  if (identical(prom_type, "eq5d")) 3L else 2L
}

prom_score_label <- function(prom_type) {
  switch(
    prom_type,
    knee = "Oxford Knee Score",
    hip = "Oxford Hip Score",
    eq5d = "EQ-5D index",
    "Score"
  )
}

# Cases are keyed by FORM_RESPONSE_GROUP_ID, but a handful of dummy records
# reuse an ID across both sides, so pair on side as well. EQ-5D carries no
# laterality at all.
prom_pair_side <- function(df) {
  if (!"Laterality" %in% names(df)) return(rep("Not recorded", nrow(df)))
  side <- trimws(as.character(df$Laterality))
  side[is.na(side) | !nzchar(side) | side == "None"] <- "Not recorded"
  side
}

# Collapse repeat submissions so each case / side / stage contributes one score.
# Pre-op keeps the LAST record (closest to surgery); a follow-up stage keeps the
# FIRST record at that stage (closest to the nominal review point).
collapse_prom_records <- function(df) {
  if (is.null(df) || nrow(df) == 0 || !"Score" %in% names(df)) return(NULL)

  df <- df %>% filter(!is.na(Score), !is.na(Stage))
  if (nrow(df) == 0) return(NULL)

  df$pair_side <- prom_pair_side(df)
  df$stage_label <- as.character(df$Stage)
  df$record_date <- if ("event_date_parsed" %in% names(df)) {
    dplyr::coalesce(df$event_date_parsed, df$analysis_date)
  } else {
    df$analysis_date
  }

  df <- df %>% filter(stage_label %in% c(prom_preop_stage, prom_followup_stages))
  if (nrow(df) == 0) return(NULL)

  df %>%
    mutate(sort_date = dplyr::if_else(is.na(record_date), as.Date("1900-01-01"), record_date)) %>%
    group_by(FORM_RESPONSE_GROUP_ID, pair_side, stage_label) %>%
    arrange(sort_date, .by_group = TRUE) %>%
    slice(if (identical(dplyr::first(stage_label), prom_preop_stage)) dplyr::n() else 1L) %>%
    ungroup() %>%
    select(-sort_date)
}

# Pair each follow-up record back to the same case/side pre-op baseline.
# Metadata (consultant, implant, hospital) is carried from the follow-up row so
# the existing comparison groupings work unchanged.
build_prom_change_data <- function(df, followup_stages = prom_followup_stages) {
  collapsed <- collapse_prom_records(df)
  if (is.null(collapsed) || nrow(collapsed) == 0) return(data.frame())

  baseline <- collapsed %>%
    filter(stage_label == prom_preop_stage) %>%
    select(
      FORM_RESPONSE_GROUP_ID,
      pair_side,
      preop_score = Score,
      preop_date = record_date
    )

  followup <- collapsed %>% filter(stage_label %in% followup_stages)

  if (nrow(baseline) == 0 || nrow(followup) == 0) return(data.frame())

  followup %>%
    inner_join(baseline, by = c("FORM_RESPONSE_GROUP_ID", "pair_side")) %>%
    mutate(
      followup_stage = factor(stage_label, levels = prom_followup_stages, ordered = TRUE),
      followup_score = Score,
      followup_date = record_date,
      change = followup_score - preop_score,
      days_from_preop = as.numeric(followup_date - preop_date)
    ) %>%
    arrange(followup_stage, FORM_RESPONSE_GROUP_ID)
}

# Count cases that could not be paired, so an unpaired cohort is never silently
# dropped from the headline numbers.
prom_pairing_counts <- function(df, followup_stage) {
  empty <- list(paired = 0L, preop_only = 0L, followup_only = 0L)
  collapsed <- collapse_prom_records(df)
  if (is.null(collapsed) || nrow(collapsed) == 0 || is.null(followup_stage)) return(empty)

  key <- function(x) paste(x$FORM_RESPONSE_GROUP_ID, x$pair_side, sep = "|")
  pre_keys <- unique(key(collapsed %>% filter(stage_label == prom_preop_stage)))
  post_keys <- unique(key(collapsed %>% filter(stage_label == followup_stage)))

  list(
    paired = length(intersect(pre_keys, post_keys)),
    preop_only = length(setdiff(pre_keys, post_keys)),
    followup_only = length(setdiff(post_keys, pre_keys))
  )
}

mean_ci_bounds <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 2) return(c(NA_real_, NA_real_))
  se <- stats::sd(x) / sqrt(length(x))
  if (!is.finite(se)) return(c(NA_real_, NA_real_))
  if (se == 0) return(c(mean(x), mean(x)))
  tcrit <- stats::qt(0.975, df = length(x) - 1)
  mean(x) + c(-1, 1) * tcrit * se
}

paired_p_value <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 2 || !is.finite(stats::sd(x)) || stats::sd(x) == 0) return(NA_real_)
  stats::t.test(x)$p.value
}

format_p_value <- function(p) {
  if (length(p) != 1 || is.na(p)) return("NA")
  if (p < 0.001) return("<0.001")
  formatC(p, format = "f", digits = 3)
}

format_change_value <- function(x, digits = 2) {
  if (length(x) != 1 || is.na(x)) return("NA")
  paste0(if (x > 0) "+" else "", formatC(x, format = "f", digits = digits))
}

# One summary row per group: paired n, both means, the change with a 95% CI,
# the proportions improved / worse / meeting MCID, and a paired t-test.
summarise_prom_change <- function(df, group_cols = NULL, mcid = 0, digits = 2,
                                  pass_threshold = NULL) {
  if (is.null(df) || nrow(df) == 0 || !"change" %in% names(df)) return(data.frame())

  df <- df %>% filter(!is.na(change), !is.na(preop_score), !is.na(followup_score))
  if (nrow(df) == 0) return(data.frame())

  if (!is.null(group_cols) && length(group_cols) > 0) {
    df <- df %>% group_by(across(all_of(group_cols)))
  }

  out <- df %>%
    summarise(
      `Paired cases` = dplyr::n(),
      `Mean pre-op` = round(mean(preop_score, na.rm = TRUE), digits),
      `Mean follow-up` = round(mean(followup_score, na.rm = TRUE), digits),
      `Mean change` = round(mean(change, na.rm = TRUE), digits),
      `CI low` = round(mean_ci_bounds(change)[[1]], digits),
      `CI high` = round(mean_ci_bounds(change)[[2]], digits),
      `SD change` = round(stats::sd(change, na.rm = TRUE), digits),
      `Median change` = round(stats::median(change, na.rm = TRUE), digits),
      `Improved (%)` = round(100 * mean(change > 0, na.rm = TRUE), 1),
      `Met MCID (%)` = round(100 * mean(change >= mcid, na.rm = TRUE), 1),
      `Worse (%)` = round(100 * mean(change < 0, na.rm = TRUE), 1),
      `PASS (%)` = pass_rate(followup_score, pass_threshold),
      `p value` = format_p_value(paired_p_value(change)),
      .groups = "drop"
    )

  if (is.null(pass_threshold) || length(pass_threshold) != 1 || is.na(pass_threshold)) {
    out[["PASS (%)"]] <- NULL
  }

  out
}

# Cross-tabulate the two responder definitions. They answer different questions —
# MCID asks "did this patient gain enough?", PASS asks "is this patient's state
# acceptable now?" — and a patient starting from a very low baseline can gain a
# great deal and still fall short of an acceptable state.
responder_matrix <- function(df, mcid, pass_threshold) {
  if (is.null(df) || nrow(df) == 0) return(data.frame())
  if (length(pass_threshold) != 1 || is.na(pass_threshold)) return(data.frame())

  df <- df %>% filter(!is.na(change), !is.na(followup_score))
  if (nrow(df) == 0) return(data.frame())

  counts <- df %>%
    mutate(
      met_mcid = change >= mcid,
      met_pass = followup_score >= pass_threshold
    ) %>%
    count(met_mcid, met_pass, name = "Cases")

  total <- sum(counts$Cases)
  label_for <- function(met_mcid, met_pass) {
    dplyr::case_when(
      met_mcid & met_pass ~ "Met MCID and PASS",
      met_mcid & !met_pass ~ "Met MCID only (improved, state still not acceptable)",
      !met_mcid & met_pass ~ "Met PASS only (already close to acceptable)",
      TRUE ~ "Met neither"
    )
  }

  order_levels <- c(
    "Met MCID and PASS",
    "Met MCID only (improved, state still not acceptable)",
    "Met PASS only (already close to acceptable)",
    "Met neither"
  )

  counts %>%
    mutate(Outcome = label_for(met_mcid, met_pass)) %>%
    group_by(Outcome) %>%
    summarise(Cases = sum(Cases), .groups = "drop") %>%
    tidyr::complete(Outcome = order_levels, fill = list(Cases = 0)) %>%
    mutate(
      `% of paired cases` = round(100 * Cases / total, 1),
      Outcome = factor(Outcome, levels = order_levels)
    ) %>%
    arrange(Outcome) %>%
    mutate(Outcome = as.character(Outcome))
}

# -----------------------------------------------------------------------------
# Rolling-window trend helpers
# -----------------------------------------------------------------------------
# Cases are ordered by procedure date and each point summarises the preceding
# window of cases, so the x axis is operative sequence rather than calendar time.
# A right-aligned window means a point is only ever informed by cases at or
# before it — no value is influenced by surgery that had not happened yet.

# Right-aligned rolling statistic. Returns NA until a full window is available,
# so a trend never opens with a point computed from two or three cases.
rolling_stat <- function(x, window, fun) {
  n <- length(x)
  if (n == 0) return(numeric(0))
  window <- max(1L, as.integer(window))
  if (window > n) return(rep(NA_real_, n))

  vapply(
    seq_len(n),
    function(i) {
      if (i < window) return(NA_real_)
      value <- suppressWarnings(fun(x[(i - window + 1):i]))
      if (length(value) != 1 || !is.finite(value)) NA_real_ else as.numeric(value)
    },
    numeric(1)
  )
}

rolling_mean <- function(x, window) {
  rolling_stat(x, window, function(v) mean(v, na.rm = TRUE))
}

# Rolling percentage meeting a condition, ignoring cases where the condition
# cannot be evaluated.
rolling_percent <- function(flag, window) {
  rolling_stat(flag, window, function(v) {
    v <- v[!is.na(v)]
    if (!length(v)) return(NA_real_)
    100 * mean(v)
  })
}

prom_trend_metrics <- function() {
  c(
    "Mean follow-up score" = "followup_score",
    "Mean pre-op score" = "preop_score",
    "Mean change" = "change",
    "Met MCID (%)" = "met_mcid",
    "PASS rate (%)" = "met_pass"
  )
}

prom_trend_metric_label <- function(metric) {
  labels <- prom_trend_metrics()
  hit <- names(labels)[match(metric, labels)]
  ifelse(is.na(hit), metric, hit)
}

# Build the per-case series the trend chart and its table are drawn from.
# `group_col` "overall" keeps one series; anything else produces one series per
# group, each ordered and windowed independently.
build_prom_trend_data <- function(change_df, window, mcid, pass_threshold,
                                  group_col = "overall") {
  if (is.null(change_df) || nrow(change_df) == 0) return(data.frame())

  df <- change_df
  df$case_date <- dplyr::coalesce(df$analysis_date, df$followup_date, df$preop_date)
  df <- df %>% filter(!is.na(case_date))
  if (nrow(df) == 0) return(data.frame())

  df <- df %>%
    mutate(
      met_mcid = ifelse(is.na(change), NA, change >= mcid),
      met_pass = if (length(pass_threshold) == 1 && !is.na(pass_threshold)) {
        ifelse(is.na(followup_score), NA, followup_score >= pass_threshold)
      } else {
        NA
      }
    )

  if (identical(group_col, "overall")) {
    df$.group_value <- "All filtered cases"
  } else {
    df <- expand_group_column(df, group_col)
    if (nrow(df) == 0) return(data.frame())
  }

  df %>%
    group_by(.group_value) %>%
    arrange(case_date, FORM_RESPONSE_GROUP_ID, .by_group = TRUE) %>%
    mutate(
      case_sequence = dplyr::row_number(),
      group_cases = dplyr::n(),
      `Mean follow-up score` = rolling_mean(followup_score, window),
      `Mean pre-op score` = rolling_mean(preop_score, window),
      `Mean change` = rolling_mean(change, window),
      `Met MCID (%)` = rolling_percent(met_mcid, window),
      `PASS rate (%)` = rolling_percent(met_pass, window)
    ) %>%
    ungroup()
}

# -----------------------------------------------------------------------------
# Monitoring: CUSUM and funnel plots
# -----------------------------------------------------------------------------
# These are unadjusted. Neither chart knows anything about case mix, so a signal
# means "this differs from the pooled cohort", NOT "this is poor care" — a
# surgeon taking on worse baseline function or more complex cases will drift
# towards a signal for that reason alone. Both are screening tools that say where
# to look, never conclusions in themselves.

prom_monitor_outcomes <- function() {
  c(
    "Met PASS" = "met_pass",
    "Met MCID" = "met_mcid",
    "Mean change" = "change",
    "Mean follow-up score" = "followup_score"
  )
}

prom_monitor_is_binary <- function(outcome) {
  outcome %in% c("met_pass", "met_mcid")
}

prom_monitor_label <- function(outcome) {
  labels <- prom_monitor_outcomes()
  hit <- names(labels)[match(outcome, labels)]
  if (length(hit) != 1 || is.na(hit)) outcome else hit
}

# A CUSUM is a random walk reflected at zero. Rather than iterate, use the
# identity S_t = C_t - min(0, min_{j<=t} C_j) for the lower-reflected form (and
# its mirror for the upper-reflected one), which is exact and vectorised.
reflected_walk <- function(w, floor_at_zero = TRUE) {
  if (!length(w)) return(numeric(0))
  running <- cumsum(w)
  if (floor_at_zero) {
    running - pmin(0, cummin(running))
  } else {
    running - pmax(0, cummax(running))
  }
}

# Expected number of cases between false signals when the true rate never moves
# off p0. Without this an operator cannot tell whether a control limit is strict
# or permissive, and h = 5 is far more permissive than it looks.
cusum_in_control_arl <- function(p0, odds_ratio, limit, replications = 200,
                                 max_cases = 20000, seed = 1) {
  if (length(p0) != 1 || is.na(p0) || p0 <= 0 || p0 >= 1) return(NA_real_)
  if (length(odds_ratio) != 1 || is.na(odds_ratio) || odds_ratio <= 1) return(NA_real_)
  if (length(limit) != 1 || is.na(limit) || limit <= 0) return(NA_real_)

  p1 <- odds_ratio * p0 / (1 - p0 + odds_ratio * p0)
  w_fail <- log(p1 / p0)
  w_succ <- log((1 - p1) / (1 - p0))

  # Fixed seed so the figure does not jitter between redraws, and the caller's
  # random stream is left exactly as it was found.
  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv)) {
    get(".Random.seed", envir = .GlobalEnv)
  } else {
    NULL
  }
  set.seed(seed)
  on.exit({
    if (is.null(old_seed)) {
      if (exists(".Random.seed", envir = .GlobalEnv)) rm(".Random.seed", envir = .GlobalEnv)
    } else {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    }
  }, add = TRUE)

  # Each replication is one independent run of fixed length; a run that never
  # crosses the limit is censored rather than extended, which would bias the mean.
  run_lengths <- vapply(seq_len(replications), function(i) {
    draws <- stats::rbinom(max_cases, 1, p0) == 1
    walk <- reflected_walk(ifelse(draws, w_fail, w_succ), floor_at_zero = TRUE)
    hit <- which(walk >= limit)
    if (length(hit)) as.numeric(hit[[1]]) else NA_real_
  }, numeric(1))

  censored <- sum(is.na(run_lengths))
  # With many runs never signalling, the mean of those that did is meaningless.
  if (censored > replications * 0.1) return(NA_real_)
  round(mean(run_lengths, na.rm = TRUE))
}

# Bernoulli CUSUM, Steiner et al., Biostatistics 2000.
# `failure` is TRUE when the case did NOT achieve the outcome. The upper chart
# accumulates evidence that the failure odds have risen by `odds_ratio`; the
# lower chart accumulates the mirror-image evidence that they have fallen.
bernoulli_cusum <- function(failure, p0, odds_ratio = 2, limit = 5) {
  failure <- as.logical(failure)
  keep <- !is.na(failure)
  n <- sum(keep)
  if (n == 0) return(data.frame())

  # Weights are log-ratios, so a baseline rate of exactly 0 or 1 has no
  # detectable alternative and the chart is undefined.
  if (length(p0) != 1 || is.na(p0) || p0 <= 0 || p0 >= 1) return(data.frame())
  if (length(odds_ratio) != 1 || is.na(odds_ratio) || odds_ratio <= 1) return(data.frame())

  failure <- failure[keep]

  weights_for <- function(ratio) {
    p1 <- ratio * p0 / (1 - p0 + ratio * p0)
    ifelse(failure, log(p1 / p0), log((1 - p1) / (1 - p0)))
  }

  w_up <- weights_for(odds_ratio)
  w_down <- weights_for(1 / odds_ratio)

  # Reset to zero after a signal, as standard CUSUM practice requires. Without
  # it a single crossing leaves the chart flagged for every subsequent case, so
  # one detection reads as hundreds and later changes are masked.
  accumulate <- function(w, limit, upward) {
    value <- numeric(n)
    signal <- logical(n)
    running <- 0
    for (i in seq_len(n)) {
      running <- if (upward) max(0, running + w[[i]]) else min(0, running + w[[i]])
      crossed <- if (upward) running >= limit else running <= -limit
      value[[i]] <- running
      signal[[i]] <- crossed
      if (crossed) running <- 0
    }
    list(value = value, signal = signal)
  }

  up <- accumulate(w_up, limit, upward = TRUE)
  down <- accumulate(w_down, limit, upward = FALSE)

  data.frame(
    index = seq_len(n),
    failure = failure,
    upper = up$value,
    lower = down$value,
    signal_up = up$signal,
    signal_down = down$signal,
    stringsAsFactors = FALSE
  )
}

# Exact binomial funnel limits. The normal approximation misbehaves badly at the
# low-volume end of a funnel, which is exactly where registry units sit, so use
# the binomial quantiles directly and accept the stepped edge.
funnel_limits_proportion <- function(n_grid, p_bar, alpha) {
  n_grid <- unique(sort(n_grid[n_grid >= 1]))
  if (!length(n_grid) || is.na(p_bar)) return(data.frame())

  data.frame(
    n = n_grid,
    lower = 100 * stats::qbinom(alpha / 2, n_grid, p_bar) / n_grid,
    upper = 100 * stats::qbinom(1 - alpha / 2, n_grid, p_bar) / n_grid,
    alpha = alpha,
    stringsAsFactors = FALSE
  )
}

funnel_limits_mean <- function(n_grid, mean_bar, sd_pooled, alpha) {
  n_grid <- unique(sort(n_grid[n_grid >= 1]))
  if (!length(n_grid) || is.na(mean_bar) || is.na(sd_pooled) || sd_pooled <= 0) {
    return(data.frame())
  }

  z <- stats::qnorm(1 - alpha / 2)
  data.frame(
    n = n_grid,
    lower = mean_bar - z * sd_pooled / sqrt(n_grid),
    upper = mean_bar + z * sd_pooled / sqrt(n_grid),
    alpha = alpha,
    stringsAsFactors = FALSE
  )
}

# Per-group point estimates for the funnel, plus the pooled centre line.
build_funnel_data <- function(df, outcome, group_col) {
  if (is.null(df) || nrow(df) == 0 || identical(group_col, "overall")) return(NULL)

  grouped <- expand_group_column(df, group_col)
  if (nrow(grouped) == 0) return(NULL)

  binary <- prom_monitor_is_binary(outcome)
  grouped <- grouped %>% filter(!is.na(.data[[outcome]]))
  if (nrow(grouped) == 0) return(NULL)

  if (binary) {
    points <- grouped %>%
      group_by(.group_value) %>%
      summarise(
        n = dplyr::n(),
        value = 100 * mean(.data[[outcome]], na.rm = TRUE),
        .groups = "drop"
      )
    centre <- 100 * mean(grouped[[outcome]], na.rm = TRUE)
    spread <- NA_real_
  } else {
    points <- grouped %>%
      group_by(.group_value) %>%
      summarise(
        n = dplyr::n(),
        value = mean(.data[[outcome]], na.rm = TRUE),
        .groups = "drop"
      )
    centre <- mean(grouped[[outcome]], na.rm = TRUE)
    spread <- stats::sd(grouped[[outcome]], na.rm = TRUE)
  }

  list(points = points, centre = centre, spread = spread, binary = binary)
}

# Flag each group against the limits so the table matches what the chart shows.
funnel_flag_points <- function(points, centre, spread, binary, alpha) {
  if (is.null(points) || nrow(points) == 0) return(data.frame())

  limits <- if (binary) {
    funnel_limits_proportion(points$n, centre / 100, alpha)
  } else {
    funnel_limits_mean(points$n, centre, spread, alpha)
  }
  if (nrow(limits) == 0) return(data.frame())

  points %>%
    left_join(limits, by = "n") %>%
    mutate(
      position = dplyr::case_when(
        is.na(lower) | is.na(upper) ~ "Not assessable",
        value > upper ~ "Above upper limit (better than cohort)",
        value < lower ~ "Below lower limit (worse than cohort)",
        TRUE ~ "Within limits"
      )
    )
}

summarise_duration <- function(df, group_cols = NULL) {
  if (is.null(df) || nrow(df) == 0 || !"surgical_duration_mins" %in% names(df)) {
    return(data.frame())
  }

  df <- df %>% filter(!is.na(surgical_duration_mins))
  if (nrow(df) == 0) return(data.frame())

  if (!is.null(group_cols) && length(group_cols) > 0) {
    df <- df %>% group_by(across(all_of(group_cols)))
  }

  df %>%
    summarise(
      n = n(),
      mean = round(mean(surgical_duration_mins, na.rm = TRUE), 1),
      median = median(surgical_duration_mins, na.rm = TRUE),
      sd = round(sd(surgical_duration_mins, na.rm = TRUE), 1),
      min = min(surgical_duration_mins, na.rm = TRUE),
      max = max(surgical_duration_mins, na.rm = TRUE),
      .groups = "drop"
    )
}

filter_exact_field <- function(df, field, value) {
  if (!field %in% names(df) || identical(trimws(value %||% ""), "")) {
    return(df[0, , drop = FALSE])
  }
  df %>% filter(.data[[field]] == value)
}


ensure_case_registry_columns_app <- function(df) {
  if (is.null(df) || !is.data.frame(df)) {
    df <- tibble::tibble()
  }

  for (col in c(
    "FORM_RESPONSE_GROUP_ID", "Patient Id", "MRN Number", "First Name", "Last Name",
    "Sex", "Date of Birth", "Patient age at time of surgery", "Joint", "Laterality",
    "Procedure type", "Procedure code", "Procedure Description", "Procedure date",
    "Co-morbidities", "Admitting consultant", "Surgeon grade", "Operating surgeon",
    "ACCESS_POINT_NAME", "HOSPITAL_NAME", "Case registry source"
  )) {
    df <- ensure_app_column(df, col, NA_character_)
  }

  if (!"BMI" %in% names(df)) {
    df[["BMI"]] <- rep(NA_real_, nrow(df))
  }
  df[["BMI"]] <- suppressWarnings(as.numeric(df[["BMI"]]))

  df[["ACCESS_POINT_NAME"]] <- coalesce_nonempty_character_app(df[["ACCESS_POINT_NAME"]], df[["HOSPITAL_NAME"]])
  df[["HOSPITAL_NAME"]] <- coalesce_nonempty_character_app(df[["HOSPITAL_NAME"]], df[["ACCESS_POINT_NAME"]])
  df[["Case registry source"]] <- coalesce_nonempty_character_app(df[["Case registry source"]], rep("Unknown", nrow(df)))
  df
}

preop_enriched <- preop %>%
  mutate(
    dob_date = parse_date_flexible(`Date of Birth`),
    proc_date = parse_date_flexible(`Procedure date`),
    ref_date = if_else(!is.na(proc_date), proc_date, Sys.Date()),
    age = if_else(!is.na(dob_date), as.numeric(calc_age(dob_date, ref_date)), NA_real_),
    procedure_year = if_else(!is.na(proc_date), format(proc_date, "%Y"), NA_character_)
  )

operative_registry_raw <- get0("operative_case_registry", ifnotfound = preop)
metadata_registry_raw <- get0("case_metadata_registry", ifnotfound = operative_registry_raw)
periop_registry_raw <- get0("periop_case_registry", ifnotfound = tibble::tibble())
periop_complications_raw <- get0("periop_complications_long", ifnotfound = tibble::tibble())

operative_cases_enriched <- ensure_case_registry_columns_app(operative_registry_raw) %>%
  mutate(
    dob_date = parse_date_flexible(`Date of Birth`),
    proc_date = parse_date_flexible(`Procedure date`),
    ref_date = if_else(!is.na(proc_date), proc_date, Sys.Date()),
    age = dplyr::coalesce(
      suppressWarnings(as.numeric(`Patient age at time of surgery`)),
      if_else(!is.na(dob_date), as.numeric(calc_age(dob_date, ref_date)), NA_real_)
    ),
    procedure_year = if_else(!is.na(proc_date), format(proc_date, "%Y"), NA_character_)
  )

metadata_cases_enriched <- ensure_case_registry_columns_app(metadata_registry_raw) %>%
  mutate(
    dob_date = parse_date_flexible(`Date of Birth`),
    proc_date = parse_date_flexible(`Procedure date`),
    ref_date = if_else(!is.na(proc_date), proc_date, Sys.Date()),
    age = dplyr::coalesce(
      suppressWarnings(as.numeric(`Patient age at time of surgery`)),
      if_else(!is.na(dob_date), as.numeric(calc_age(dob_date, ref_date)), NA_real_)
    ),
    procedure_year = if_else(!is.na(proc_date), format(proc_date, "%Y"), NA_character_)
  )

periop_cases_enriched <- ensure_case_registry_columns_app(periop_registry_raw) %>%
  mutate(
    dob_date = parse_date_flexible(`Date of Birth`),
    proc_date = parse_date_flexible(`Procedure date`),
    ref_date = if_else(!is.na(proc_date), proc_date, Sys.Date()),
    age = dplyr::coalesce(
      suppressWarnings(as.numeric(`Patient age at time of surgery`)),
      if_else(!is.na(dob_date), as.numeric(calc_age(dob_date, ref_date)), NA_real_)
    ),
    procedure_year = if_else(!is.na(proc_date), format(proc_date, "%Y"), NA_character_)
  )

case_core <- metadata_cases_enriched %>%
  transmute(
    FORM_RESPONSE_GROUP_ID,
    consultant = `Admitting consultant`,
    surgeon_grade = `Surgeon grade`,
    case_joint = Joint,
    case_laterality = Laterality,
    case_procedure_type = `Procedure type`,
    case_proc_date = proc_date,
    case_hospital = ACCESS_POINT_NAME,
    case_source = `Case registry source`,
    bmi = BMI,
    case_age = age,
    case_sex = Sex
  ) %>%
  distinct()

case_cohort <- case_core %>%
  left_join(implant_classification, by = "FORM_RESPONSE_GROUP_ID")

periop_case_core <- periop_cases_enriched %>%
  transmute(
    FORM_RESPONSE_GROUP_ID,
    consultant = `Admitting consultant`,
    surgeon_grade = `Surgeon grade`,
    case_joint = Joint,
    case_laterality = Laterality,
    case_procedure_type = `Procedure type`,
    case_proc_date = proc_date,
    case_hospital = ACCESS_POINT_NAME,
    case_source = `Case registry source`
  ) %>%
  distinct()

periop_case_cohort <- periop_case_core %>%
  left_join(implant_classification, by = "FORM_RESPONSE_GROUP_ID")

surgical_date_lookup <- surgical_data %>%
  mutate(proc_date_parsed = parse_date_flexible(`Procedure date`)) %>%
  group_by(FORM_RESPONSE_GROUP_ID) %>%
  summarise(
    surgical_proc_date = first_non_missing_date(proc_date_parsed),
    .groups = "drop"
  )

prepare_prom_data <- function(prom_type) {
  df <- switch(
    prom_type,
    knee = oks,
    hip = ohs,
    eq5d = eq5d
  )

  df <- df %>%
    mutate(
      event_date_parsed = if ("Event date" %in% names(df)) parse_date_flexible(`Event date`) else as.Date(NA)
    ) %>%
    left_join(case_core, by = "FORM_RESPONSE_GROUP_ID") %>%
    left_join(implant_classification, by = "FORM_RESPONSE_GROUP_ID") %>%
    mutate(
      analysis_date = dplyr::coalesce(case_proc_date, event_date_parsed)
    )

  # Sex and date of birth sit on the PROM record itself and are fully populated,
  # so they survive a broken case link; BMI only exists on the case record.
  prom_sex <- if ("Sex at Birth" %in% names(df)) tidy_sex_value(df$`Sex at Birth`) else NA_character_
  case_sex <- if ("case_sex" %in% names(df)) tidy_sex_value(df$case_sex) else NA_character_
  df$patient_sex <- dplyr::coalesce(prom_sex, case_sex)

  # Age at procedure where the record links to the case table; otherwise age at
  # the PROM event, which for a follow-up is older than age at surgery.
  dob <- if ("Date of Birth" %in% names(df)) parse_date_flexible(df$`Date of Birth`) else as.Date(NA)
  case_age <- if ("case_age" %in% names(df)) suppressWarnings(as.numeric(df$case_age)) else NA_real_
  df$patient_age <- dplyr::coalesce(
    case_age,
    suppressWarnings(as.numeric(calc_age(dob, df$analysis_date)))
  )

  df$age_band <- band_age(df$patient_age)
  df$bmi_band <- band_bmi(df$bmi)

  if ("Stage" %in% names(df)) {
    df$Stage <- factor(as.character(df$Stage), levels = stage_order, ordered = TRUE)
  }

  df
}

prepare_postop_complication_data <- function(source_choice = "all") {
  df <- switch(
    source_choice,
    V1 = get_complications_v1(),
    V2 = get_complications_v2(),
    get_all_complications()
  )

  df %>%
    mutate(
      assessment_date_parsed = parse_date_flexible(assessment_date),
      complication_date_parsed = parse_date_flexible(date)
    ) %>%
    left_join(case_core, by = "FORM_RESPONSE_GROUP_ID") %>%
    left_join(implant_classification, by = "FORM_RESPONSE_GROUP_ID") %>%
    mutate(
      analysis_joint = dplyr::coalesce(case_joint, Joint),
      analysis_hospital = dplyr::coalesce(case_hospital, hospital),
      analysis_proc_type = dplyr::coalesce(case_procedure_type, `Procedure type`),
      analysis_date = dplyr::coalesce(case_proc_date, complication_date_parsed, assessment_date_parsed),
      phase = "Post-op"
    )
}

prepare_complication_data <- prepare_postop_complication_data
all_postop_complications <- prepare_postop_complication_data("all")
all_complications <- all_postop_complications

prepare_periop_complication_data <- function(source_choice = "all") {
  df <- periop_complications_raw
  if (identical(source_choice, "V1")) {
    df <- df %>% filter(source == "Peri-op V1")
  } else if (identical(source_choice, "V2")) {
    df <- df %>% filter(source == "Peri-op V2")
  }

  df %>%
    mutate(
      assessment_date_parsed = parse_date_flexible(assessment_date),
      complication_date_parsed = parse_date_flexible(date)
    ) %>%
    left_join(case_core, by = "FORM_RESPONSE_GROUP_ID") %>%
    left_join(implant_classification, by = "FORM_RESPONSE_GROUP_ID") %>%
    mutate(
      analysis_joint = dplyr::coalesce(case_joint, Joint),
      analysis_hospital = dplyr::coalesce(case_hospital, hospital),
      analysis_proc_type = dplyr::coalesce(case_procedure_type, `Procedure type`),
      analysis_date = dplyr::coalesce(case_proc_date, complication_date_parsed, assessment_date_parsed),
      phase = "Peri-op"
    )
}

all_periop_complications <- prepare_periop_complication_data("all")

surgical_enriched <- surgical_data %>%
  mutate(
    proc_date_parsed = parse_date_flexible(`Procedure date`)
  ) %>%
  left_join(case_core, by = "FORM_RESPONSE_GROUP_ID") %>%
  left_join(implant_classification, by = "FORM_RESPONSE_GROUP_ID") %>%
  mutate(
    analysis_date = dplyr::coalesce(proc_date_parsed, case_proc_date)
  )

implant_joined <- implant_data %>%
  left_join(implant_classification, by = "FORM_RESPONSE_GROUP_ID") %>%
  left_join(case_core, by = "FORM_RESPONSE_GROUP_ID") %>%
  left_join(surgical_date_lookup, by = "FORM_RESPONSE_GROUP_ID") %>%
  mutate(
    analysis_date = dplyr::coalesce(case_proc_date, surgical_proc_date)
  )

inventory_table <- data.frame(
  Dataset = c(
    "Operative case registry (peri-op preferred)",
    "Case metadata registry (peri-op + pre-op)",
    "Pre-op assessment",
    "Peri-op assessment V1",
    "Peri-op assessment V2",
    "Peri-op complications (long format)",
    "Post-op assessment V1",
    "Post-op assessment V2",
    "Post-op complications (long format)",
    "Oxford Knee Score",
    "Oxford Hip Score",
    "EQ-5D",
    "Component log V1 (surgical times)",
    "Component log V2 (surgical times)",
    "Component log V1 (implant components)",
    "Component log V2 (implant components)",
    "Combined implant classification"
  ),
  Rows = c(
    nrow(operative_cases_enriched),
    nrow(metadata_cases_enriched),
    nrow(preop),
    nrow(periop_v1),
    nrow(periop_v2),
    nrow(all_periop_complications),
    nrow(postop_v1),
    nrow(postop_v2),
    nrow(all_postop_complications),
    nrow(oks),
    nrow(ohs),
    nrow(eq5d),
    nrow(comp_v1_data),
    nrow(comp_v2_data),
    nrow(comp_v1_comp),
    nrow(comp_v2_comp),
    nrow(implant_classification)
  ),
  Unique_FORM_RESPONSE_GROUP_ID = c(
    count_unique_ids(operative_cases_enriched),
    count_unique_ids(metadata_cases_enriched),
    count_unique_ids(preop),
    count_unique_ids(periop_v1),
    count_unique_ids(periop_v2),
    count_unique_ids(all_periop_complications),
    count_unique_ids(postop_v1),
    count_unique_ids(postop_v2),
    count_unique_ids(all_postop_complications),
    count_unique_ids(oks),
    count_unique_ids(ohs),
    count_unique_ids(eq5d),
    count_unique_ids(comp_v1_data),
    count_unique_ids(comp_v2_data),
    count_unique_ids(comp_v1_comp),
    count_unique_ids(comp_v2_comp),
    count_unique_ids(implant_classification)
  ),
  stringsAsFactors = FALSE
)

linkage_table <- data.frame(
  Join_check = c(
    "Operative registry ↔ Peri-op V1",
    "Operative registry ↔ Peri-op V2",
    "Metadata registry ↔ Oxford Knee Score",
    "Metadata registry ↔ Oxford Hip Score",
    "Metadata registry ↔ EQ-5D",
    "Metadata registry ↔ Post-op V1",
    "Metadata registry ↔ Post-op V2",
    "Peri-op registry ↔ Peri-op complications",
    "Peri-op registry ↔ Combined surgical times",
    "Metadata registry ↔ Combined surgical times",
    "Metadata registry ↔ Combined implant components",
    "Combined implant classification ↔ Oxford Knee Score",
    "Combined implant classification ↔ Oxford Hip Score",
    "Combined implant classification ↔ EQ-5D",
    "Combined implant classification ↔ Peri-op complications",
    "Combined implant classification ↔ Post-op complications",
    "Combined implant classification ↔ Combined surgical times"
  ),
  Shared_FORM_RESPONSE_GROUP_ID = c(
    count_overlap_ids(operative_cases_enriched, periop_v1),
    count_overlap_ids(operative_cases_enriched, periop_v2),
    count_overlap_ids(metadata_cases_enriched, oks),
    count_overlap_ids(metadata_cases_enriched, ohs),
    count_overlap_ids(metadata_cases_enriched, eq5d),
    count_overlap_ids(metadata_cases_enriched, postop_v1),
    count_overlap_ids(metadata_cases_enriched, postop_v2),
    count_overlap_ids(periop_cases_enriched, all_periop_complications),
    count_overlap_ids(periop_cases_enriched, surgical_data),
    count_overlap_ids(metadata_cases_enriched, surgical_data),
    count_overlap_ids(metadata_cases_enriched, implant_data),
    count_overlap_ids(implant_classification, oks),
    count_overlap_ids(implant_classification, ohs),
    count_overlap_ids(implant_classification, eq5d),
    count_overlap_ids(implant_classification, all_periop_complications),
    count_overlap_ids(implant_classification, all_postop_complications),
    count_overlap_ids(implant_classification, surgical_data)
  ),
  stringsAsFactors = FALSE
)

prom_stage_counts <- bind_rows(
  oks %>% count(Stage, name = "Records") %>% mutate(Dataset = "Oxford Knee Score"),
  ohs %>% count(Stage, name = "Records") %>% mutate(Dataset = "Oxford Hip Score"),
  eq5d %>% count(Stage, name = "Records") %>% mutate(Dataset = "EQ-5D")
) %>%
  mutate(Stage = factor(as.character(Stage), levels = stage_order, ordered = TRUE))

has_preop_link_for_prom <- function(prom_type) {
  switch(
    prom_type,
    knee = count_overlap_ids(metadata_cases_enriched, oks) > 0,
    hip = count_overlap_ids(metadata_cases_enriched, ohs) > 0,
    eq5d = count_overlap_ids(metadata_cases_enriched, eq5d) > 0
  )
}

has_implant_link_for_prom <- function(prom_type) {
  switch(
    prom_type,
    knee = count_overlap_ids(implant_classification, oks) > 0,
    hip = count_overlap_ids(implant_classification, ohs) > 0,
    eq5d = count_overlap_ids(implant_classification, eq5d) > 0
  )
}

complication_has_preop_link <- function(source_choice) {
  if (identical(source_choice, "V1")) return(count_overlap_ids(metadata_cases_enriched, postop_v1) > 0)
  if (identical(source_choice, "V2")) return(count_overlap_ids(metadata_cases_enriched, postop_v2) > 0)
  count_overlap_ids(metadata_cases_enriched, postop_v1) > 0 || count_overlap_ids(metadata_cases_enriched, postop_v2) > 0
}

complication_has_implant_link <- function(source_choice) {
  if (identical(source_choice, "V1")) return(count_overlap_ids(implant_classification, get_complications_v1()) > 0)
  if (identical(source_choice, "V2")) return(count_overlap_ids(implant_classification, get_complications_v2()) > 0)
  count_overlap_ids(implant_classification, all_postop_complications) > 0
}

periop_complication_has_case_link <- function(source_choice) {
  if (identical(source_choice, "V1")) return(count_overlap_ids(periop_cases_enriched, prepare_periop_complication_data("V1")) > 0)
  if (identical(source_choice, "V2")) return(count_overlap_ids(periop_cases_enriched, prepare_periop_complication_data("V2")) > 0)
  count_overlap_ids(periop_cases_enriched, all_periop_complications) > 0
}

periop_complication_has_implant_link <- function(source_choice) {
  if (identical(source_choice, "V1")) return(count_overlap_ids(implant_classification, prepare_periop_complication_data("V1")) > 0)
  if (identical(source_choice, "V2")) return(count_overlap_ids(implant_classification, prepare_periop_complication_data("V2")) > 0)
  count_overlap_ids(implant_classification, all_periop_complications) > 0
}

surgical_has_preop_link <- count_overlap_ids(metadata_cases_enriched, surgical_data) > 0
surgical_has_implant_link <- count_overlap_ids(implant_classification, surgical_data) > 0

key_link_counts <- c(
  count_overlap_ids(metadata_cases_enriched, oks),
  count_overlap_ids(metadata_cases_enriched, ohs),
  count_overlap_ids(metadata_cases_enriched, eq5d),
  count_overlap_ids(metadata_cases_enriched, postop_v1),
  count_overlap_ids(metadata_cases_enriched, postop_v2),
  count_overlap_ids(periop_cases_enriched, all_periop_complications),
  count_overlap_ids(metadata_cases_enriched, surgical_data),
  count_overlap_ids(metadata_cases_enriched, implant_data)
)

no_shared_links <- all(key_link_counts == 0)

consultant_choices <- make_choice_vector(metadata_cases_enriched$`Admitting consultant`)
surgeon_grade_choices <- make_choice_vector(metadata_cases_enriched$`Surgeon grade`)
fixation_choices <- make_choice_vector(implant_classification$fixation_type)
knee_system_choices <- split_delimited_choices(implant_classification$knee_system)
femoral_component_choices <- split_delimited_choices(implant_classification$femoral_component)
acetabular_component_choices <- split_delimited_choices(implant_classification$acetabular_component)

cases_date_bounds <- date_bounds(operative_cases_enriched$proc_date)
surgical_date_bounds <- date_bounds(surgical_enriched$analysis_date)
implant_date_bounds <- date_bounds(implant_joined$analysis_date)

lookup_field_map <- c(
  case_id = "FORM_RESPONSE_GROUP_ID",
  mrn = "MRN Number",
  patient_id = "Patient Id"
)

theme <- bs_theme(version = 5, bootswatch = "flatly")

ui <- navbarPage(
  title = "INOR Arthroplasty Explorer",
  theme = theme,
  header = tags$head(
    tags$style(HTML(
      ".metric-box{background:#ffffff;border:1px solid #dbe3ea;border-radius:12px;padding:16px;margin-bottom:16px;box-shadow:0 1px 2px rgba(16,24,40,.04);}
       .metric-title{font-size:12px;font-weight:700;color:#5b6472;text-transform:uppercase;letter-spacing:.05em;}
       .metric-value{font-size:30px;font-weight:700;line-height:1.1;margin-top:6px;color:#0f172a;}
       .metric-subtitle{font-size:12px;color:#64748b;margin-top:6px;}
       .notice-box{background:#fff7ed;border-left:4px solid #f59e0b;padding:12px 14px;border-radius:8px;margin-bottom:16px;}
       .success-box{background:#ecfdf5;border-left:4px solid #10b981;padding:12px 14px;border-radius:8px;margin-bottom:16px;}
       .help-note{color:#64748b;font-size:13px;}
       .control-heading{font-size:12px;font-weight:700;color:#5b6472;text-transform:uppercase;letter-spacing:.05em;margin-bottom:10px;}
       .tab-content{padding-top:12px;}"
    ))
  ),

  tabPanel(
    "Overview",
    br(),
    uiOutput("overview_notice"),
    uiOutput("overview_metrics"),
    fluidRow(
      column(6, plotOutput("overview_joint_plot", height = 300)),
      column(6, plotOutput("overview_stage_plot", height = 300))
    ),
    hr(),
    h4("Dataset inventory"),
    DTOutput("inventory_table"),
    hr(),
    h4("Linkage diagnostics"),
    p(class = "help-note", "This checks whether datasets share FORM_RESPONSE_GROUP_ID values for case-level joins."),
    DTOutput("linkage_table")
  ),

  tabPanel(
    "Cases & demographics",
    sidebarLayout(
      sidebarPanel(
        make_date_input("cases_date_range", "Procedure date range", cases_date_bounds),
        selectInput("cases_joint", "Joint", choices = make_choice_vector(operative_cases_enriched$Joint)),
        selectInput("cases_consultant", "Consultant", choices = consultant_choices),
        selectInput("cases_hospital", "Hospital", choices = make_choice_vector(operative_cases_enriched$ACCESS_POINT_NAME)),
        selectInput("cases_laterality", "Laterality", choices = make_choice_vector(operative_cases_enriched$Laterality)),
        selectInput("cases_proc", "Procedure type", choices = make_choice_vector(operative_cases_enriched$`Procedure type`)),
        textInput("cases_search", "Search name / MRN / case ID"),
        downloadButton("download_cases", "Download filtered cases")
      ),
      mainPanel(
        uiOutput("cases_note"),
        uiOutput("cases_metrics"),
        fluidRow(
          column(6, plotOutput("cases_age_plot", height = 280)),
          column(6, plotOutput("cases_bmi_plot", height = 280))
        ),
        fluidRow(
          column(12, plotOutput("cases_consultant_plot", height = 300))
        ),
        h4("Filtered case registry"),
        DTOutput("cases_table")
      )
    )
  ),

  tabPanel(
    "PROMs",
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "prom_type",
          "PROM dataset",
          choices = c(
            "Oxford Knee Score" = "knee",
            "Oxford Hip Score" = "hip",
            "EQ-5D" = "eq5d"
          )
        ),
        uiOutput("prom_date_ui"),
        uiOutput("prom_stage_ui"),
        uiOutput("prom_laterality_ui"),
        selectInput("prom_consultant", "Consultant", choices = consultant_choices),
        selectInput("prom_surgeon_grade", "Surgeon grade", choices = surgeon_grade_choices),
        selectInput("prom_fixation", "Implant fixation type", choices = fixation_choices),
        selectInput("prom_knee_system", "Knee system", choices = knee_system_choices),
        selectInput("prom_femoral_component", "Femoral component", choices = femoral_component_choices),
        selectInput("prom_acetabular_component", "Acetabular component", choices = acetabular_component_choices),
        tags$hr(),
        tags$div(class = "control-heading", "Patient demographics"),
        uiOutput("prom_sex_ui"),
        uiOutput("prom_age_ui"),
        uiOutput("prom_bmi_ui"),
        selectInput(
          "prom_compare",
          "Compare scores by",
          choices = c(
            "Overall" = "overall",
            "Consultant" = "consultant",
            "Surgeon grade" = "surgeon_grade",
            "Implant fixation type" = "fixation_type",
            "Knee system" = "knee_system",
            "Femoral component" = "femoral_component",
            "Acetabular component" = "acetabular_component",
            "Sex" = "patient_sex",
            "Age band" = "age_band",
            "BMI band" = "bmi_band"
          )
        ),
        selectizeInput(
          "prom_case_id",
          "Case trajectory lookup (FORM_RESPONSE_GROUP_ID)",
          choices = NULL,
          options = list(placeholder = "Select a case ID")
        ),
        tags$hr(),
        tags$div(class = "control-heading", "Score thresholds"),
        uiOutput("prom_pass_ui"),
        conditionalPanel(
          condition = "input.prom_view == 'change' || input.prom_view == 'trends' || input.prom_view == 'monitoring'",
          uiOutput("prom_change_mcid_ui")
        ),
        conditionalPanel(
          condition = "input.prom_view == 'change' || input.prom_view == 'trends' || input.prom_view == 'monitoring'",
          tags$hr(),
          tags$div(class = "control-heading", "Paired analysis"),
          uiOutput("prom_change_stage_ui"),
          tags$p(
            class = "help-note",
            "Change = follow-up score minus pre-op score for the same case and side, so a positive change is an improvement. The Stage filter above does not apply here — the paired analysis always needs both stages."
          )
        ),
        conditionalPanel(
          condition = "input.prom_view == 'monitoring'",
          tags$hr(),
          tags$div(class = "control-heading", "Monitoring"),
          selectInput(
            "prom_monitor_outcome",
            "Outcome",
            choices = prom_monitor_outcomes(),
            selected = "met_pass"
          ),
          sliderInput(
            "prom_monitor_alpha",
            "Funnel limits",
            min = 1, max = 3, value = 2, step = 1,
            ticks = FALSE
          ),
          tags$p(class = "help-note", "1 = 95% only, 2 = 95% and 99.8%, 3 = 99.8% only."),
          numericInput("prom_cusum_or", "CUSUM: odds ratio to detect", value = 2, min = 1.1, step = 0.5),
          numericInput("prom_cusum_limit", "CUSUM: control limit (h)", value = 5, min = 1, step = 0.5),
          uiOutput("prom_cusum_baseline_ui"),
          tags$p(
            class = "help-note",
            "Neither chart adjusts for case mix. A signal says a group or a run differs from the pooled cohort, not that care was worse."
          )
        ),
        conditionalPanel(
          condition = "input.prom_view == 'trends'",
          tags$hr(),
          tags$div(class = "control-heading", "Rolling trend"),
          uiOutput("prom_trend_window_ui"),
          checkboxGroupInput(
            "prom_trend_metrics",
            "Metrics to plot",
            choices = prom_trend_metrics(),
            selected = c("followup_score", "change", "met_mcid", "met_pass")
          ),
          radioButtons(
            "prom_trend_xaxis",
            "Plot against",
            choices = c("Case sequence" = "sequence", "Procedure date" = "date"),
            selected = "sequence"
          ),
          tags$p(
            class = "help-note",
            "Cases are ordered by procedure date. Each point averages that case and the preceding cases in the window, so a trend only starts once a full window of cases exists. Series are split by the Compare scores by selection above."
          )
        ),
        tags$p(
          class = "help-note",
          "Date filtering uses procedure date when the PROM record links back to the case table; otherwise it falls back to the PROM event date."
        ),
        downloadButton("download_proms", "Download filtered PROMs")
      ),
      mainPanel(
        uiOutput("prom_link_note"),
        uiOutput("prom_demographics_note"),
        tabsetPanel(
          id = "prom_view",
          tabPanel(
            "Scores by stage",
            value = "levels",
            uiOutput("prom_metrics"),
            fluidRow(
              column(6, plotOutput("prom_stage_plot", height = 300)),
              column(6, plotOutput("prom_compare_plot", height = 300))
            ),
            h4("Patient acceptable symptom state"),
            uiOutput("prom_pass_note"),
            fluidRow(
              column(6, plotOutput("prom_pass_stage_plot", height = 320)),
              column(6, plotOutput("prom_pass_group_plot", height = 320))
            ),
            DTOutput("prom_pass_stage_table"),
            h4("Stage summary"),
            DTOutput("prom_stage_summary_table"),
            h4("Comparison summary"),
            DTOutput("prom_compare_summary_table"),
            h4("Filtered PROM records"),
            DTOutput("prom_raw_table"),
            h4("Trajectory for selected case"),
            DTOutput("prom_trajectory_table")
          ),
          tabPanel(
            "Pre-op vs post-op change",
            value = "change",
            uiOutput("prom_change_note"),
            uiOutput("prom_change_metrics"),
            fluidRow(
              column(6, plotOutput("prom_change_hist", height = 320)),
              column(6, plotOutput("prom_change_scatter", height = 320))
            ),
            h4("Mean change by follow-up stage"),
            plotOutput("prom_change_stage_plot", height = 320),
            DTOutput("prom_change_stage_table"),
            h4("Improvement vs acceptable symptom state"),
            uiOutput("prom_responder_note"),
            DTOutput("prom_responder_table"),
            h4("Change summary by comparison group"),
            plotOutput("prom_change_group_plot", height = 340),
            DTOutput("prom_change_group_table"),
            h4("Paired cases"),
            downloadButton("download_prom_change", "Download paired change data"),
            tags$br(), tags$br(),
            DTOutput("prom_change_case_table")
          ),
          tabPanel(
            "Trends",
            value = "trends",
            uiOutput("prom_trend_note"),
            uiOutput("prom_trend_metrics_boxes"),
            plotOutput("prom_trend_plot", height = 520),
            h4("First vs most recent window"),
            uiOutput("prom_trend_shift_note"),
            DTOutput("prom_trend_shift_table"),
            h4("Rolling series"),
            downloadButton("download_prom_trend", "Download rolling series"),
            tags$br(), tags$br(),
            DTOutput("prom_trend_table")
          ),
          tabPanel(
            "Monitoring",
            value = "monitoring",
            uiOutput("prom_monitor_caveat"),
            h4("Funnel plot"),
            uiOutput("prom_funnel_note"),
            plotOutput("prom_funnel_plot", height = 460),
            DTOutput("prom_funnel_table"),
            h4("CUSUM"),
            uiOutput("prom_cusum_note"),
            plotOutput("prom_cusum_plot", height = 460),
            DTOutput("prom_cusum_table")
          )
        )
      )
    )
  ),

  tabPanel(
    "Peri-op complications",
    sidebarLayout(
      sidebarPanel(
        selectInput("peri_comp_source", "Source", choices = c("All" = "all", "Peri-op V1" = "V1", "Peri-op V2" = "V2")),
        uiOutput("peri_comp_date_ui"),
        selectInput("peri_comp_joint", "Joint", choices = make_choice_vector(all_periop_complications$analysis_joint)),
        selectInput("peri_comp_type", "Complication type", choices = make_choice_vector(all_periop_complications$complication)),
        selectInput("peri_comp_group", "Complication group", choices = make_choice_vector(all_periop_complications$complication_group)),
        selectInput("peri_comp_side", "Side", choices = make_choice_vector(all_periop_complications$side)),
        selectInput("peri_comp_consultant", "Consultant", choices = consultant_choices),
        selectInput("peri_comp_surgeon_grade", "Surgeon grade", choices = surgeon_grade_choices),
        selectInput("peri_comp_fixation", "Implant fixation type", choices = fixation_choices),
        selectInput("peri_comp_knee_system", "Knee system", choices = knee_system_choices),
        selectInput("peri_comp_femoral_component", "Femoral component", choices = femoral_component_choices),
        selectInput("peri_comp_acetabular_component", "Acetabular component", choices = acetabular_component_choices),
        selectInput(
          "peri_comp_compare",
          "Compare complications by",
          choices = c(
            "Overall" = "overall",
            "Consultant" = "consultant",
            "Surgeon grade" = "surgeon_grade",
            "Implant fixation type" = "fixation_type",
            "Knee system" = "knee_system",
            "Femoral component" = "femoral_component",
            "Acetabular component" = "acetabular_component"
          )
        ),
        radioButtons(
          "peri_comp_measure",
          "Display complications as",
          choices = c(
            "Absolute numbers" = "count",
            "Rates (% of peri-op cases)" = "rate"
          )
        ),
        tags$p(
          class = "help-note",
          "Rate calculations use the filtered peri-op operative case cohort as the denominator."
        ),
        downloadButton("download_peri_complications", "Download peri-op complications")
      ),
      mainPanel(
        uiOutput("peri_comp_link_note"),
        uiOutput("peri_comp_metrics"),
        fluidRow(
          column(6, plotOutput("peri_comp_type_plot", height = 300)),
          column(6, plotOutput("peri_comp_compare_plot", height = 300))
        ),
        h4("Peri-op complication summary"),
        DTOutput("peri_comp_summary_table"),
        h4("Filtered peri-op complication records"),
        DTOutput("peri_comp_raw_table")
      )
    )
  ),

  tabPanel(
    "Post-op complications",
    sidebarLayout(
      sidebarPanel(
        selectInput("comp_source", "Source", choices = c("All" = "all", "Post-op V1" = "V1", "Post-op V2" = "V2")),
        uiOutput("comp_date_ui"),
        selectInput("comp_joint", "Joint", choices = make_choice_vector(all_postop_complications$analysis_joint)),
        selectInput("comp_type", "Complication type", choices = make_choice_vector(all_postop_complications$complication)),
        selectInput("comp_presentation", "Presentation", choices = make_choice_vector(all_postop_complications$presentation)),
        selectInput("comp_side", "Side", choices = make_choice_vector(all_postop_complications$side)),
        selectInput("comp_consultant", "Consultant", choices = consultant_choices),
        selectInput("comp_surgeon_grade", "Surgeon grade", choices = surgeon_grade_choices),
        selectInput("comp_fixation", "Implant fixation type", choices = fixation_choices),
        selectInput("comp_knee_system", "Knee system", choices = knee_system_choices),
        selectInput("comp_femoral_component", "Femoral component", choices = femoral_component_choices),
        selectInput("comp_acetabular_component", "Acetabular component", choices = acetabular_component_choices),
        selectInput(
          "comp_compare",
          "Compare complications by",
          choices = c(
            "Overall" = "overall",
            "Consultant" = "consultant",
            "Surgeon grade" = "surgeon_grade",
            "Implant fixation type" = "fixation_type",
            "Knee system" = "knee_system",
            "Femoral component" = "femoral_component",
            "Acetabular component" = "acetabular_component"
          )
        ),
        radioButtons(
          "comp_measure",
          "Display complications as",
          choices = c(
            "Absolute numbers" = "count",
            "Rates (% of linked cases)" = "rate"
          )
        ),
        tags$p(
          class = "help-note",
          "Rate calculations require complications to link back to the case-level registry."
        ),
        downloadButton("download_complications", "Download post-op complications")
      ),
      mainPanel(
        uiOutput("comp_link_note"),
        uiOutput("comp_metrics"),
        fluidRow(
          column(6, plotOutput("comp_type_plot", height = 300)),
          column(6, plotOutput("comp_compare_plot", height = 300))
        ),
        h4("Post-op complication summary"),
        DTOutput("comp_summary_table"),
        h4("Filtered post-op complication records"),
        DTOutput("comp_raw_table")
      )
    )
  ),

  tabPanel(
    "Surgical times",
    sidebarLayout(
      sidebarPanel(
        make_date_input("surg_date_range", "Procedure date range", surgical_date_bounds),
        selectInput("surg_source", "Source", choices = make_choice_vector(surgical_enriched$source)),
        selectInput("surg_joint", "Joint", choices = make_choice_vector(surgical_enriched$Joint)),
        selectInput("surg_hospital", "Hospital", choices = make_choice_vector(surgical_enriched$ACCESS_POINT_NAME)),
        selectInput("surg_proc", "Procedure type", choices = make_choice_vector(surgical_enriched$`Procedure type`)),
        selectInput("surg_consultant", "Consultant", choices = consultant_choices),
        selectInput("surg_surgeon_grade", "Surgeon grade", choices = surgeon_grade_choices),
        selectInput("surg_fixation", "Implant fixation type", choices = fixation_choices),
        selectInput("surg_knee_system", "Knee system", choices = knee_system_choices),
        selectInput("surg_femoral_component", "Femoral component", choices = femoral_component_choices),
        selectInput("surg_acetabular_component", "Acetabular component", choices = acetabular_component_choices),
        selectInput(
          "surg_compare",
          "Compare duration by",
          choices = c(
            "Overall" = "overall",
            "Consultant" = "consultant",
            "Surgeon grade" = "surgeon_grade",
            "Implant fixation type" = "fixation_type",
            "Knee system" = "knee_system",
            "Femoral component" = "femoral_component",
            "Acetabular component" = "acetabular_component",
            "Joint" = "Joint",
            "Hospital" = "ACCESS_POINT_NAME",
            "Procedure type" = "Procedure type",
            "Source" = "source"
          )
        ),
        conditionalPanel(
          condition = "input.surg_view == 'trend'",
          tags$hr(),
          tags$div(class = "control-heading", "Rolling trend"),
          uiOutput("surg_trend_window_ui"),
          checkboxGroupInput(
            "surg_trend_metrics",
            "Metrics to plot",
            choices = c(
              "Mean duration" = "mean",
              "Median duration" = "median",
              "Variability (SD)" = "sd",
              "Long cases (%)" = "long"
            ),
            selected = c("mean", "median", "long")
          ),
          numericInput("surg_long_threshold", "Long case threshold (minutes)", value = 120, min = 1, step = 10),
          radioButtons(
            "surg_trend_xaxis",
            "Plot against",
            choices = c("Case sequence" = "sequence", "Procedure date" = "date"),
            selected = "sequence"
          ),
          tags$p(
            class = "help-note",
            "Cases are ordered by procedure date. Each point covers that case and the preceding cases in the window. Series are split by the Compare duration by selection above."
          )
        ),
        downloadButton("download_surgical", "Download filtered surgical times")
      ),
      mainPanel(
        uiOutput("surg_link_note"),
        tabsetPanel(
          id = "surg_view",
          tabPanel(
            "Distribution and comparison",
            value = "distribution",
            uiOutput("surg_metrics"),
            fluidRow(
              column(6, plotOutput("surg_hist_plot", height = 300)),
              column(6, plotOutput("surg_compare_plot", height = 300))
            ),
            h4("Duration summary"),
            DTOutput("surg_summary_table"),
            h4("Filtered surgical timing records"),
            DTOutput("surg_raw_table")
          ),
          tabPanel(
            "Rolling trend",
            value = "trend",
            uiOutput("surg_trend_note"),
            uiOutput("surg_trend_boxes"),
            plotOutput("surg_trend_plot", height = 520),
            h4("First vs most recent window"),
            DTOutput("surg_trend_shift_table"),
            h4("Rolling series"),
            downloadButton("download_surg_trend", "Download rolling series"),
            tags$br(), tags$br(),
            DTOutput("surg_trend_table")
          )
        )
      )
    )
  ),

  tabPanel(
    "Implants",
    sidebarLayout(
      sidebarPanel(
        make_date_input("impl_date_range", "Procedure date range", implant_date_bounds),
        selectInput("impl_source", "Source", choices = make_choice_vector(implant_joined$source)),
        selectInput("impl_joint", "Classified joint", choices = make_choice_vector(implant_joined$joint_type)),
        selectInput("impl_consultant", "Consultant", choices = consultant_choices),
        selectInput("impl_fixation", "Implant fixation type", choices = fixation_choices),
        selectInput("impl_knee_system", "Knee system", choices = knee_system_choices),
        selectInput("impl_femoral_component", "Femoral component", choices = femoral_component_choices),
        selectInput("impl_acetabular_component", "Acetabular component", choices = acetabular_component_choices),
        selectInput("impl_manufacturer", "Manufacturer", choices = make_choice_vector(implant_joined$MANUFACTURER_NAME)),
        textInput("impl_search", "Search description / brand / catalogue"),
        downloadButton("download_implants", "Download filtered implants")
      ),
      mainPanel(
        uiOutput("impl_link_note"),
        uiOutput("impl_metrics"),
        fluidRow(
          column(6, plotOutput("impl_manufacturer_plot", height = 300)),
          column(6, plotOutput("impl_fixation_plot", height = 300))
        ),
        h4("Case-level implant classification"),
        DTOutput("impl_classification_table"),
        h4("Implant type summary"),
        DTOutput("impl_types_table"),
        h4("Filtered implant component records"),
        DTOutput("impl_raw_table")
      )
    )
  ),

  tabPanel(
    "Lookup",
    sidebarLayout(
      sidebarPanel(
        radioButtons(
          "lookup_mode",
          "Lookup by",
          choices = c(
            "FORM_RESPONSE_GROUP_ID" = "case_id",
            "MRN Number" = "mrn",
            "Patient Id" = "patient_id"
          )
        ),
        textInput("lookup_value", "Search value"),
        tags$p(
          class = "help-note",
          "In the dummy data, the same lookup value will often only match one dataset at a time because the case IDs are not linked across files."
        )
      ),
      mainPanel(
        uiOutput("lookup_note"),
        tabsetPanel(
          tabPanel("Cases", DTOutput("lookup_cases")),
          tabPanel("Knee PROMs", DTOutput("lookup_knee")),
          tabPanel("Hip PROMs", DTOutput("lookup_hip")),
          tabPanel("EQ-5D", DTOutput("lookup_eq5d")),
          tabPanel("Peri/Post-op complications", DTOutput("lookup_complications")),
          tabPanel("Surgical times", DTOutput("lookup_surgical")),
          tabPanel("Implants", DTOutput("lookup_implants")),
          tabPanel("Classification", DTOutput("lookup_classification"))
        )
      )
    )
  ),

  tabPanel(
    "Documentation",
    br(),
    tags$div(
      class = "notice-box",
      HTML(
        "<strong>Linkage assumption:</strong> <code>FORM_RESPONSE_GROUP_ID</code> is the case-level key for one joint replacement episode. 
        In the real registry export, pre-op, PROMs, post-op, surgical timing, and implant component data should share that value for the same case."
      )
    ),
    tags$h4("What this version adds"),
    tags$ul(
      tags$li("Operative case counts now come from the peri-op assessment sheets when they are available, with pre-op used as a metadata supplement."),
      tags$li("Procedure-date filtering across the operative registry, peri-op complications, post-op complications, surgical times, and implant tables."),
      tags$li("PROMs and complication date filters that use procedure date when linked, otherwise the native event / assessment date."),
      tags$li("Consultant is sourced from the peri-op assessment where linked to the case, and surgeon grade is available across the main outcome tabs."),
      tags$li("Separate peri-op and post-op complication tabs, each with count or rate displays and comparison by consultant, surgeon grade, fixation, knee system, femoral component, and acetabular component."),
      tags$li("A BMI histogram in the demographics tab, with BMI populated from pre-op where the case IDs link.")
    ),
    tags$h4("Using the dummy files"),
    tags$ul(
      tags$li("The dummy extracts still demonstrate the UI, plots, and summaries."),
      tags$li("In the current dummy set, the peri-op and pre-op case IDs do not overlap, so operative totals now come from peri-op while BMI/comorbidity completion may stay sparse."),
      tags$li("Some consultant-linked or implant-linked outcome analyses still stay empty because the dummy files do not consistently share FORM_RESPONSE_GROUP_ID values across all datasets."),
      tags$li("Once your real extracts are linked, those comparison sections should populate automatically.")
    ),
    tags$h4("Dataset inventory"),
    DTOutput("documentation_inventory")
  )
)

server <- function(input, output, session) {

  output$overview_notice <- renderUI({
    registry_note <- if (nrow(periop_cases_enriched) > 0) {
      "Operative case counts in this version come from the peri-op assessment sheets when they are available."
    } else {
      "Peri-op sheets were not found, so operative case counts are falling back to the pre-op extract."
    }

    if (no_shared_links) {
      tags$div(
        class = "notice-box",
        HTML(
          paste0(
            registry_note,
            " These dummy datasets do not share <code>FORM_RESPONSE_GROUP_ID</code> values consistently across the main files, so the app focuses on within-dataset exploration and suppresses linked analyses when required."
          )
        )
      )
    } else {
      tags$div(
        class = "success-box",
        HTML(paste0(registry_note, " Shared <code>FORM_RESPONSE_GROUP_ID</code> values were detected across key datasets, so linked analyses can run."))
      )
    }
  })

  output$overview_metrics <- renderUI({
    joint_counts <- operative_cases_enriched %>% count(Joint)
    hip_cases <- joint_counts %>% filter(Joint == "Hip") %>% pull(n)
    knee_cases <- joint_counts %>% filter(Joint == "Knee") %>% pull(n)
    hip_cases <- ifelse(length(hip_cases) == 0, 0, hip_cases)
    knee_cases <- ifelse(length(knee_cases) == 0, 0, knee_cases)

    fluidRow(
      column(3, metric_box("Operative cases", format(nrow(operative_cases_enriched), big.mark = ","), paste("Hip:", hip_cases, "| Knee:", knee_cases))),
      column(3, metric_box("PROM records", format(nrow(oks) + nrow(ohs) + nrow(eq5d), big.mark = ","), "OKS + OHS + EQ-5D")),
      column(3, metric_box("Complication rows", format(nrow(all_periop_complications) + nrow(all_postop_complications), big.mark = ","), paste("Peri-op:", nrow(all_periop_complications), "| Post-op:", nrow(all_postop_complications)))),
      column(3, metric_box("Implant components", format(nrow(implant_data), big.mark = ","), paste("Classified cases:", nrow(implant_classification))))
    )
  })

  output$overview_joint_plot <- renderPlot({
    validate(need(nrow(operative_cases_enriched) > 0, "No operative case records available."))
    plot_df <- operative_cases_enriched %>% count(Joint)
    ggplot(plot_df, aes(x = Joint, y = n)) +
      geom_col() +
      labs(title = "Operative case mix", x = NULL, y = "Cases") +
      theme_minimal(base_size = 12)
  })

  output$overview_stage_plot <- renderPlot({
    validate(need(nrow(prom_stage_counts) > 0, "No PROM stage data available."))
    ggplot(prom_stage_counts, aes(x = Stage, y = Records, group = Dataset)) +
      geom_line(aes(linetype = Dataset), linewidth = 0.8) +
      geom_point(aes(shape = Dataset), size = 2.5) +
      labs(title = "PROM stage distribution", x = NULL, y = "Records") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
  })

  output$inventory_table <- renderDT({
    datatable_or_message(inventory_table)
  })

  output$linkage_table <- renderDT({
    datatable_or_message(linkage_table)
  })

  filtered_cases <- reactive({
    df <- operative_cases_enriched
    df <- apply_date_filter(df, "proc_date", input$cases_date_range)
    df <- apply_single_filter(df, "Joint", input$cases_joint)
    df <- apply_single_filter(df, "Admitting consultant", input$cases_consultant)
    df <- apply_single_filter(df, "ACCESS_POINT_NAME", input$cases_hospital)
    df <- apply_single_filter(df, "Laterality", input$cases_laterality)
    df <- apply_single_filter(df, "Procedure type", input$cases_proc)

    search_value <- trimws(input$cases_search %||% "")
    if (!identical(search_value, "")) {
      search_blob <- paste(
        df$FORM_RESPONSE_GROUP_ID,
        df$`MRN Number`,
        df$`First Name`,
        df$`Last Name`,
        sep = " | "
      )
      df <- df[grepl(search_value, search_blob, ignore.case = TRUE), , drop = FALSE]
    }

    df
  })

  output$cases_note <- renderUI({
    source_text <- if (nrow(periop_cases_enriched) > 0) {
      "Cases & demographics uses the peri-op operative registry when it is available. BMI and co-morbidity fields are supplemented from the pre-op extract when the case IDs link."
    } else {
      "Peri-op sheets were not found, so the cases tab is using the pre-op registry as a fallback."
    }

    if (nrow(operative_cases_enriched) > 0 && sum(!is.na(operative_cases_enriched$BMI)) == 0) {
      source_text <- paste0(source_text, " In the current dummy files, the operative registry does not link to pre-op BMI rows, so the BMI chart may be empty.")
    }

    tags$div(class = "notice-box", source_text)
  })

  output$cases_metrics <- renderUI({
    df <- filtered_cases()
    female_pct <- if (nrow(df) > 0) round(mean(df$Sex == "Female", na.rm = TRUE) * 100, 1) else NA_real_
    comorb_pct <- if (nrow(df) > 0) round(mean(df$`Co-morbidities` == "Yes", na.rm = TRUE) * 100, 1) else NA_real_

    fluidRow(
      column(3, metric_box("Filtered cases", nrow(df), "Operative registry rows")),
      column(3, metric_box("Mean age", ifelse(all(is.na(df$age)), "NA", round(mean(df$age, na.rm = TRUE), 1)), "Age at procedure / reference date")),
      column(3, metric_box("Mean BMI", ifelse(all(is.na(df$BMI)), "NA", round(mean(df$BMI, na.rm = TRUE), 1)), "Body mass index")),
      column(3, metric_box("Female / comorbidity", ifelse(is.na(female_pct), "NA", paste0(female_pct, "%")), ifelse(is.na(comorb_pct), "No cases", paste0("Comorbidity: ", comorb_pct, "%"))))
    )
  })

  output$cases_age_plot <- renderPlot({
    df <- filtered_cases()
    validate(need(nrow(df) > 0, "No matching cases."))
    validate(need(any(!is.na(df$age)), "Age could not be calculated from the selected cases."))

    ggplot(df, aes(x = age)) +
      geom_histogram(bins = 12) +
      labs(title = "Age distribution", x = "Age (years)", y = "Cases") +
      theme_minimal(base_size = 12)
  })

  output$cases_bmi_plot <- renderPlot({
    df <- filtered_cases()
    validate(need(nrow(df) > 0, "No matching cases."))
    validate(need(any(!is.na(df$BMI)), "BMI is missing for the selected cases."))

    ggplot(df, aes(x = BMI)) +
      geom_histogram(bins = 12) +
      labs(title = "BMI distribution", x = "BMI", y = "Cases") +
      theme_minimal(base_size = 12)
  })

  output$cases_consultant_plot <- renderPlot({
    df <- filtered_cases()
    validate(need(nrow(df) > 0, "No matching cases."))

    plot_df <- df %>%
      count(`Admitting consultant`, sort = TRUE) %>%
      mutate(`Admitting consultant` = ifelse(is.na(`Admitting consultant`), "Missing", `Admitting consultant`)) %>%
      slice_head(n = 12)

    validate(need(nrow(plot_df) > 0, "No consultant values available."))

    ggplot(plot_df, aes(x = reorder(`Admitting consultant`, n), y = n)) +
      geom_col() +
      coord_flip() +
      labs(title = "Top consultants in filtered cases", x = NULL, y = "Cases") +
      theme_minimal(base_size = 12)
  })

  output$cases_table <- renderDT({
    df <- filtered_cases() %>%
      transmute(
        FORM_RESPONSE_GROUP_ID,
        `Patient Id`,
        `MRN Number`,
        `First Name`,
        `Last Name`,
        Sex,
        age,
        Joint,
        Laterality,
        `Procedure type`,
        `Procedure code`,
        `Procedure date` = as.character(proc_date),
        `Procedure description` = `Procedure Description`,
        BMI,
        `Co-morbidities`,
        `Admitting consultant`,
        `Surgeon grade`,
        `Case registry source` = `Case registry source`,
        Hospital = ACCESS_POINT_NAME
      )

    datatable_or_message(df)
  })

  output$download_cases <- downloadHandler(
    filename = function() paste0("inor_filtered_cases_", Sys.Date(), ".csv"),
    content = function(file) write_export(filtered_cases(), file)
  )

  output$prom_date_ui <- renderUI({
    req(input$prom_type)
    bounds <- date_bounds(prepare_prom_data(input$prom_type)$analysis_date)
    make_date_input("prom_date_range", "Date range", bounds)
  })

  output$prom_stage_ui <- renderUI({
    req(input$prom_type)
    df <- prepare_prom_data(input$prom_type)
    choices <- sort(unique(as.character(df$Stage)))
    selectizeInput("prom_stage", "Stage", choices = choices, selected = choices, multiple = TRUE)
  })

  output$prom_laterality_ui <- renderUI({
    req(input$prom_type)
    if (identical(input$prom_type, "eq5d")) return(NULL)
    df <- prepare_prom_data(input$prom_type)
    choices <- sort(unique(stats::na.omit(df$Laterality)))
    checkboxGroupInput("prom_laterality", "Laterality", choices = choices, selected = choices, inline = TRUE)
  })

  observe({
    req(input$prom_type)
    df <- prepare_prom_data(input$prom_type)
    choices <- sort(unique(stats::na.omit(as.character(df$FORM_RESPONSE_GROUP_ID))))
    updateSelectizeInput(session, "prom_case_id", choices = choices, selected = head(choices, 1), server = TRUE)
  })

  # ---------------------------------------------------------------------------
  # Patient demographic filters
  # ---------------------------------------------------------------------------

  output$prom_sex_ui <- renderUI({
    req(input$prom_type)
    values <- tidy_sex_value(prepare_prom_data(input$prom_type)$patient_sex)
    selectInput("prom_sex", "Sex", choices = make_choice_vector(values))
  })

  output$prom_age_ui <- renderUI({
    req(input$prom_type)
    df <- prepare_prom_data(input$prom_type)
    bounds <- numeric_bounds(df$patient_age)
    if (is.null(bounds)) {
      return(tags$p(class = "help-note", "No usable age values were found for this dataset."))
    }
    missing <- sum(is.na(df$patient_age))
    tagList(
      sliderInput("prom_age_range", "Age at procedure", min = bounds$min, max = bounds$max,
                  value = c(bounds$min, bounds$max), step = 1),
      if (missing > 0) {
        checkboxInput("prom_age_include_missing",
                      sprintf("Include records with no age (%s in this dataset)", format(missing, big.mark = ",")),
                      value = TRUE)
      }
    )
  })

  output$prom_bmi_ui <- renderUI({
    req(input$prom_type)
    df <- prepare_prom_data(input$prom_type)
    bounds <- numeric_bounds(df$bmi)
    if (is.null(bounds)) {
      return(tags$p(class = "help-note", "No usable BMI values were found for this dataset."))
    }
    missing <- sum(is.na(df$bmi))
    tagList(
      sliderInput("prom_bmi_range", "BMI", min = bounds$min, max = bounds$max,
                  value = c(bounds$min, bounds$max), step = 1),
      if (missing > 0) {
        checkboxInput("prom_bmi_include_missing",
                      sprintf("Include records with no BMI (%s in this dataset)", format(missing, big.mark = ",")),
                      value = TRUE)
      }
    )
  })

  output$prom_demographics_note <- renderUI({
    req(input$prom_type)
    df <- filtered_prom_all_stages()
    if (nrow(df) == 0) return(NULL)

    total <- nrow(df)
    age <- df$patient_age[!is.na(df$patient_age)]
    bmi <- df$bmi[!is.na(df$bmi)]
    sex <- tidy_sex_value(df$patient_sex)
    female <- sum(!is.na(sex) & tolower(sex) == "female")
    sex_known <- sum(!is.na(sex))

    parts <- c(
      sprintf("%s records", format(total, big.mark = ",")),
      if (length(age)) {
        sprintf("mean age %.1f (range %s-%s)%s", mean(age), min(age), max(age),
                if (length(age) < total) sprintf(", %s with no age", format(total - length(age), big.mark = ",")) else "")
      },
      if (sex_known > 0) sprintf("%.1f%% female", 100 * female / sex_known),
      if (length(bmi)) {
        sprintf("mean BMI %.1f%s", mean(bmi),
                if (length(bmi) < total) sprintf(", %s with no BMI", format(total - length(bmi), big.mark = ",")) else "")
      }
    )

    tags$p(class = "help-note", paste(parts[!vapply(parts, is.null, logical(1))], collapse = " | "))
  })

  # Every PROM filter except Stage. The paired change analysis needs the pre-op
  # and follow-up rows for a case to survive together, so it starts from here.
  filtered_prom_all_stages <- reactive({
    req(input$prom_type)
    df <- prepare_prom_data(input$prom_type)

    df <- apply_date_filter(df, "analysis_date", input$prom_date_range)

    if (!identical(input$prom_type, "eq5d")) {
      lats <- input$prom_laterality %||% character(0)
      if (length(lats) > 0) {
        df <- df %>% filter(Laterality %in% lats)
      }
    }

    df <- apply_single_filter(df, "consultant", input$prom_consultant)
    df <- apply_single_filter(df, "surgeon_grade", input$prom_surgeon_grade)
    df <- apply_single_filter(df, "fixation_type", input$prom_fixation)
    df <- apply_component_filter(df, "knee_system", input$prom_knee_system)
    df <- apply_component_filter(df, "femoral_component", input$prom_femoral_component)
    df <- apply_component_filter(df, "acetabular_component", input$prom_acetabular_component)

    df <- apply_single_filter(df, "patient_sex", input$prom_sex)
    df <- apply_range_filter(df, "patient_age", input$prom_age_range,
                             include_missing = input$prom_age_include_missing %||% TRUE)
    df <- apply_range_filter(df, "bmi", input$prom_bmi_range,
                             include_missing = input$prom_bmi_include_missing %||% TRUE)

    df %>% arrange(Stage, analysis_date, event_date_parsed)
  })

  filtered_prom <- reactive({
    df <- filtered_prom_all_stages()

    stages <- input$prom_stage
    if (!is.null(stages) && length(stages) > 0) {
      df <- df %>% filter(as.character(Stage) %in% stages)
    }

    df
  })

  output$prom_link_note <- renderUI({
    req(input$prom_type)

    messages <- character()
    if (!has_preop_link_for_prom(input$prom_type)) {
      messages <- c(
        messages,
        "This PROM extract does not link back to the case registry in the dummy data, so consultant filters and procedure-date based cohorting may stay empty."
      )
    }
    if (!has_implant_link_for_prom(input$prom_type)) {
      messages <- c(
        messages,
        "This PROM extract does not link back to the implant classification table in the dummy data, so implant-based filters and comparisons may stay empty."
      )
    }

    if (!length(messages)) {
      tags$div(class = "success-box", "PROM records link to both case-level and implant-level metadata, so consultant and implant comparisons are enabled.")
    } else {
      tags$div(
        class = "notice-box",
        tags$ul(lapply(messages, tags$li))
      )
    }
  })

  output$prom_metrics <- renderUI({
    req(input$prom_type)
    df <- filtered_prom()
    threshold <- prom_pass_threshold()
    digits <- prom_score_digits(input$prom_type)

    postop_df <- prom_postop_records(df)
    pass_value <- pass_rate(postop_df$Score, threshold)
    pass_display <- if (is.na(pass_value)) "NA" else paste0(pass_value, "%")
    n_stages <- length(unique(stats::na.omit(as.character(postop_df$Stage))))
    pass_subtitle <- if (is.na(threshold)) {
      "Set a PASS threshold in the sidebar"
    } else {
      sprintf(
        "%s+ across %s, all records (n=%s)",
        threshold,
        if (n_stages == 1) "1 follow-up stage" else paste(n_stages, "follow-up stages pooled"),
        format(nrow(postop_df), big.mark = ",")
      )
    }

    fluidRow(
      column(3, metric_box("Filtered records", nrow(df), "PROM rows after filters")),
      column(3, metric_box("Mean score", ifelse(nrow(df) == 0 || all(is.na(df$Score)), "NA", round(mean(df$Score, na.rm = TRUE), digits)), "Average score")),
      column(3, metric_box("Median score", ifelse(nrow(df) == 0 || all(is.na(df$Score)), "NA", median(df$Score, na.rm = TRUE)), "Middle score")),
      column(3, metric_box("Unique cases", count_unique_ids(df), "Distinct FORM_RESPONSE_GROUP_ID")),
      column(3, metric_box("PASS rate", pass_display, pass_subtitle))
    )
  })

  output$prom_stage_plot <- renderPlot({
    df <- summarise_scores(filtered_prom(), group_cols = c("Stage"), digits = prom_score_digits(input$prom_type))
    validate(need(nrow(df) > 0, "No PROM scores available for the current filters."))

    ggplot(df, aes(x = Stage, y = mean, group = 1)) +
      geom_line(linewidth = 0.8) +
      geom_point(size = 2.5) +
      labs(title = "Mean score by stage", x = NULL, y = "Mean score") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
  })

  prom_compare_summary <- reactive({
    df <- filtered_prom()
    df <- df %>% filter(!is.na(Score))

    compare_col <- input$prom_compare %||% "overall"
    digits <- prom_score_digits(input$prom_type)
    threshold <- prom_pass_threshold()

    if (identical(compare_col, "overall")) {
      return(summarise_scores(df, pass_threshold = threshold, digits = digits))
    }

    grouped_df <- expand_group_column(df, compare_col)
    if (nrow(grouped_df) == 0) return(data.frame())

    group_cols <- ".group_value"
    if (length(unique(stats::na.omit(as.character(grouped_df$Stage)))) > 1) {
      group_cols <- c(group_cols, "Stage")
    }

    summary_df <- summarise_scores(grouped_df, group_cols = group_cols, pass_threshold = threshold, digits = digits)
    rename_group_column(summary_df, friendly_group_label(compare_col))
  })

  output$prom_compare_plot <- renderPlot({
    compare_col <- input$prom_compare %||% "overall"
    if (identical(compare_col, "overall")) {
      df <- filtered_prom()
      validate(need(nrow(df) > 0, "No PROM scores available for the current filters."))
      validate(need(any(!is.na(df$Score)), "The selected PROM records do not contain usable scores."))

      if (length(unique(stats::na.omit(as.character(df$Stage)))) > 1) {
        ggplot(df, aes(x = Stage, y = Score)) +
          geom_boxplot() +
          labs(title = "Score distribution by stage", x = NULL, y = "Score") +
          theme_minimal(base_size = 12) +
          theme(axis.text.x = element_text(angle = 30, hjust = 1))
      } else {
        ggplot(df, aes(x = Score)) +
          geom_histogram(bins = 12) +
          labs(title = "Score distribution", x = "Score", y = "Records") +
          theme_minimal(base_size = 12)
      }
    } else {
      summary_df <- prom_compare_summary()
      label <- friendly_group_label(compare_col)
      validate(need(nrow(summary_df) > 0, "No linked PROM rows are available for the selected comparison."))

      if ("Stage" %in% names(summary_df)) {
        top_groups_df <- summary_df %>%
          group_by(.data[[label]]) %>%
          summarise(total_n = sum(n, na.rm = TRUE), .groups = "drop") %>%
          arrange(desc(total_n)) %>%
          slice_head(n = 10)

        top_groups <- top_groups_df[[label]]

        plot_df <- summary_df %>%
          filter(.data[[label]] %in% top_groups)

        ggplot(plot_df, aes(x = .data[[label]], y = mean, fill = Stage)) +
          geom_col(position = "dodge") +
          coord_flip() +
          labs(title = paste("Mean score by", label), x = NULL, y = "Mean score") +
          theme_minimal(base_size = 12)
      } else {
        plot_df <- summary_df %>%
          arrange(desc(mean)) %>%
          slice_head(n = 12)

        ggplot(plot_df, aes(x = reorder(.data[[label]], mean), y = mean)) +
          geom_col() +
          coord_flip() +
          labs(title = paste("Mean score by", label), x = NULL, y = "Mean score") +
          theme_minimal(base_size = 12)
      }
    }
  })

  output$prom_stage_summary_table <- renderDT({
    datatable_or_message(
      summarise_scores(
        filtered_prom(),
        group_cols = c("Stage"),
        pass_threshold = prom_pass_threshold(),
        digits = prom_score_digits(input$prom_type)
      )
    )
  })

  output$prom_compare_summary_table <- renderDT({
    compare_col <- input$prom_compare %||% "overall"
    message <- if (identical(compare_col, "overall")) {
      "No PROM rows are available for the current filters."
    } else {
      "No linked PROM rows are available for the selected comparison."
    }
    datatable_or_message(prom_compare_summary(), message)
  })

  output$prom_raw_table <- renderDT({
    df <- filtered_prom()

    display_df <- data.frame(
      FORM_RESPONSE_GROUP_ID = df$FORM_RESPONSE_GROUP_ID,
      `Patient Id` = df$`Patient Id`,
      `MRN Number` = df$`MRN Number`,
      `First Name` = df$`First Name`,
      `Last Name` = df$`Last Name`,
      Laterality = if ("Laterality" %in% names(df)) df$Laterality else NA_character_,
      Sex = df$patient_sex,
      Age = df$patient_age,
      BMI = df$bmi,
      Stage = if ("Stage" %in% names(df)) as.character(df$Stage) else NA_character_,
      Score = df$Score,
      `Filter date` = as.character(df$analysis_date),
      Consultant = df$consultant,
      `Surgeon grade` = df$surgeon_grade,
      `Implant fixation type` = df$fixation_type,
      `Knee system` = df$knee_system,
      `Femoral component` = df$femoral_component,
      `Acetabular component` = df$acetabular_component,
      Hospital = df$HOSPITAL_NAME,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )

    datatable_or_message(display_df)
  })

  output$prom_trajectory_table <- renderDT({
    case_id <- input$prom_case_id %||% ""
    if (identical(trimws(case_id), "")) {
      return(datatable_or_message(data.frame(), "Select a case ID to view its PROM trajectory."))
    }

    prom_type <- input$prom_type
    traj <- switch(
      prom_type,
      knee = get_proms_trajectory(case_id, "knee"),
      hip = get_proms_trajectory(case_id, "hip"),
      eq5d = get_proms_trajectory(case_id, "eq5d")
    )

    datatable_or_message(traj, "No trajectory rows found for that case ID.")
  })

  output$download_proms <- downloadHandler(
    filename = function() paste0("inor_proms_", input$prom_type, "_", Sys.Date(), ".csv"),
    content = function(file) write_export(filtered_prom(), file)
  )

  # ---------------------------------------------------------------------------
  # Patient acceptable symptom state (PASS)
  # ---------------------------------------------------------------------------

  output$prom_pass_ui <- renderUI({
    req(input$prom_type)
    tagList(
      numericInput(
        "prom_pass_threshold",
        "PASS threshold (score at or above)",
        value = prom_default_pass(input$prom_type),
        min = 0,
        step = if (identical(input$prom_type, "eq5d")) 0.01 else 1
      ),
      tags$p(class = "help-note", prom_pass_reference_note(input$prom_type))
    )
  })

  prom_pass_threshold <- reactive({
    req(input$prom_type)
    value <- suppressWarnings(as.numeric(input$prom_pass_threshold))
    if (length(value) != 1 || is.na(value)) prom_default_pass(input$prom_type) else value
  })

  output$prom_pass_note <- renderUI({
    req(input$prom_type)
    threshold <- prom_pass_threshold()
    if (length(threshold) != 1 || is.na(threshold)) {
      return(tags$div(class = "notice-box", "Set a PASS threshold in the sidebar to see acceptable-state rates."))
    }

    tags$p(
      class = "help-note",
      sprintf(
        "PASS rate = the share of records scoring %s or above on the %s. Unlike the MCID this is an absolute score, so it needs no pre-op baseline. Everything here counts every follow-up record, pre-op excluded, and is broken down by stage — PASS climbs with time since surgery, so a group's mix of follow-up stages would otherwise masquerade as a difference in outcome. The Pre-op vs post-op change tab reports PASS for one stage and for paired cases only, so its figures are lower; that is the cohort differing, not the calculation.",
        threshold, prom_score_label(input$prom_type)
      )
    )
  })

  prom_pass_stage_summary <- reactive({
    req(input$prom_type)
    df <- filtered_prom()
    threshold <- prom_pass_threshold()
    if (nrow(df) == 0 || is.na(threshold)) return(data.frame())

    df %>%
      filter(!is.na(Score), !is.na(Stage)) %>%
      group_by(Stage) %>%
      summarise(
        Records = dplyr::n(),
        `Meeting PASS` = sum(Score >= threshold, na.rm = TRUE),
        `PASS (%)` = pass_rate(Score, threshold),
        .groups = "drop"
      ) %>%
      arrange(Stage)
  })

  output$prom_pass_stage_plot <- renderPlot({
    req(input$prom_type)
    summary_df <- prom_pass_stage_summary()
    validate(need(nrow(summary_df) > 0, "No scored PROM records for the current filters."))

    ggplot(summary_df, aes(x = Stage, y = `PASS (%)`)) +
      geom_col(fill = "#1d4ed8") +
      geom_text(aes(label = paste0(`PASS (%)`, "%\nn=", Records)), vjust = -0.3, size = 3.2, colour = "#64748b") +
      scale_y_continuous(limits = c(0, 100), expand = ggplot2::expansion(mult = c(0, 0.18))) +
      labs(
        title = paste0("PASS rate by stage (threshold ", prom_pass_threshold(), ")"),
        x = NULL, y = "% of records at or above threshold"
      ) +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
  })

  prom_pass_group_summary <- reactive({
    req(input$prom_type)
    threshold <- prom_pass_threshold()
    compare_col <- input$prom_compare %||% "overall"
    df <- prom_postop_records(filtered_prom()) %>% filter(!is.na(Score))
    if (nrow(df) == 0 || is.na(threshold)) return(data.frame())

    if (identical(compare_col, "overall")) {
      return(
        data.frame(
          Overall = "All filtered records",
          Records = nrow(df),
          `PASS (%)` = pass_rate(df$Score, threshold),
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
      )
    }

    grouped_df <- expand_group_column(df, compare_col)
    if (nrow(grouped_df) == 0) return(data.frame())

    summary_df <- grouped_df %>%
      mutate(Stage = factor(as.character(Stage), levels = prom_followup_stages, ordered = TRUE)) %>%
      group_by(.group_value, Stage) %>%
      summarise(
        Records = dplyr::n(),
        `PASS (%)` = pass_rate(Score, threshold),
        .groups = "drop"
      ) %>%
      arrange(.group_value, Stage)

    rename_group_column(summary_df, friendly_group_label(compare_col))
  })

  output$prom_pass_group_plot <- renderPlot({
    req(input$prom_type)
    compare_col <- input$prom_compare %||% "overall"
    threshold <- prom_pass_threshold()

    # A single bar says nothing, so with no grouping selected show where the
    # cohort actually sits relative to the threshold instead.
    if (identical(compare_col, "overall")) {
      df <- prom_postop_records(filtered_prom()) %>% filter(!is.na(Score))
      validate(need(nrow(df) > 0, "No scored follow-up records for the current filters."))
      validate(need(!is.na(threshold), "Set a PASS threshold in the sidebar."))

      rate <- pass_rate(df$Score, threshold)
      return(
        ggplot(df, aes(x = Score, fill = Score >= threshold)) +
          geom_histogram(bins = 24, colour = "white") +
          geom_vline(xintercept = threshold, linetype = "dashed", colour = "#0f172a", linewidth = 0.8) +
          scale_fill_manual(
            values = c(`TRUE` = "#0ea5e9", `FALSE` = "#cbd5e1"),
            labels = c(`TRUE` = "At or above PASS", `FALSE` = "Below PASS"),
            breaks = c("TRUE", "FALSE")
          ) +
          labs(
            title = "Score distribution against the PASS threshold",
            subtitle = sprintf("Follow-up records only: %s%% of %s are at or above %s", rate, format(nrow(df), big.mark = ","), threshold),
            x = prom_score_label(input$prom_type), y = "Records", fill = NULL
          ) +
          theme_minimal(base_size = 12) +
          theme(legend.position = "top")
      )
    }

    summary_df <- prom_pass_group_summary()
    validate(need(nrow(summary_df) > 0, "No linked PROM rows are available for the selected comparison."))

    label <- friendly_group_label(compare_col)

    # Rank groups by total records, but order the bars by the group's overall
    # PASS rate so the chart still reads top to bottom.
    group_totals <- summary_df %>%
      group_by(.data[[label]]) %>%
      summarise(
        total_records = sum(Records, na.rm = TRUE),
        overall_pass = stats::weighted.mean(`PASS (%)`, Records, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(total_records)) %>%
      slice_head(n = 10)

    plot_df <- summary_df %>%
      filter(.data[[label]] %in% group_totals[[label]]) %>%
      left_join(group_totals, by = label) %>%
      mutate(group_label = stats::reorder(.data[[label]], overall_pass))

    ggplot(plot_df, aes(x = `PASS (%)`, y = group_label, fill = Stage)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.75) +
      scale_fill_manual(values = prom_stage_palette(), drop = TRUE) +
      scale_x_continuous(
        limits = c(0, 100),
        breaks = seq(0, 100, 25),
        expand = ggplot2::expansion(mult = c(0, 0.02))
      ) +
      labs(
        title = paste("PASS rate by", tolower(label), "and stage"),
        subtitle = "Follow-up records only; up to 10 groups with the most records",
        x = "% of records at or above threshold", y = NULL, fill = NULL
      ) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "top")
  })

  output$prom_pass_stage_table <- renderDT({
    datatable_or_message(
      prom_pass_stage_summary(),
      "No scored PROM records for the current filters."
    )
  })

  output$prom_responder_note <- renderUI({
    req(input$prom_type)
    threshold <- prom_pass_threshold()
    if (length(threshold) != 1 || is.na(threshold)) {
      return(tags$div(class = "notice-box", "Set a PASS threshold in the sidebar to split the paired cohort by responder status."))
    }

    tags$p(
      class = "help-note",
      tagList(
        sprintf(
          "MCID asks whether a patient gained enough (change of %s or more); PASS asks whether their state is acceptable now (%s or above at follow-up). A patient starting from a very low baseline can clear the MCID and still fall short of PASS.",
          prom_change_mcid(), threshold
        ),
        tags$br(),
        sprintf(
          "Every PASS figure on this tab covers one stage (%s) and only cases that also have a pre-op baseline, so it will not match the Scores by stage tab, which counts every follow-up record across all selected stages.",
          tolower(input$prom_change_stage %||% "the selected follow-up stage")
        )
      )
    )
  })

  output$prom_responder_table <- renderDT({
    req(input$prom_type)
    datatable_or_message(
      responder_matrix(prom_change_data(), prom_change_mcid(), prom_pass_threshold()),
      "No paired cases to classify, or no PASS threshold set."
    )
  })

  # ---------------------------------------------------------------------------
  # Pre-op vs post-op change
  # ---------------------------------------------------------------------------

  prom_available_followups <- reactive({
    req(input$prom_type)
    present <- unique(as.character(prepare_prom_data(input$prom_type)$Stage))
    intersect(prom_followup_stages, present)
  })

  output$prom_change_stage_ui <- renderUI({
    choices <- prom_available_followups()
    if (!length(choices)) {
      return(tags$p(class = "help-note", "This PROM extract has no timed follow-up stages, so no change can be calculated."))
    }
    selectInput("prom_change_stage", "Follow-up stage", choices = choices, selected = choices[[1]])
  })

  output$prom_change_mcid_ui <- renderUI({
    req(input$prom_type)
    default_mcid <- prom_default_mcid(input$prom_type)
    numericInput(
      "prom_change_mcid",
      "Minimal clinically important difference",
      value = default_mcid,
      min = 0,
      step = if (identical(input$prom_type, "eq5d")) 0.001 else 1
    )
  })

  prom_change_mcid <- reactive({
    req(input$prom_type)
    value <- suppressWarnings(as.numeric(input$prom_change_mcid))
    if (length(value) != 1 || is.na(value)) prom_default_mcid(input$prom_type) else value
  })

  prom_change_all_stages <- reactive({
    build_prom_change_data(filtered_prom_all_stages(), prom_followup_stages)
  })

  prom_change_data <- reactive({
    df <- prom_change_all_stages()
    stage <- input$prom_change_stage
    if (nrow(df) == 0 || is.null(stage) || !nzchar(stage)) return(df[0, , drop = FALSE])
    df %>% filter(as.character(followup_stage) == stage)
  })

  output$prom_change_note <- renderUI({
    req(input$prom_type)
    stage <- input$prom_change_stage
    if (is.null(stage) || !nzchar(stage)) {
      return(tags$div(class = "notice-box", "Select a follow-up stage to compare against the pre-op baseline."))
    }

    counts <- prom_pairing_counts(filtered_prom_all_stages(), stage)

    if (counts$paired == 0) {
      return(
        tags$div(
          class = "notice-box",
          sprintf(
            "No case has both a pre-op score and a %s score under the current filters (%s pre-op only, %s follow-up only). Widen the date range or clear a filter.",
            tolower(stage), format(counts$preop_only, big.mark = ","), format(counts$followup_only, big.mark = ",")
          )
        )
      )
    }

    tags$div(
      class = "success-box",
      sprintf(
        "%s case-sides paired pre-op to %s. Unpaired and excluded: %s with a pre-op score but no %s score, %s with a %s score but no pre-op baseline.",
        format(counts$paired, big.mark = ","),
        tolower(stage),
        format(counts$preop_only, big.mark = ","),
        tolower(stage),
        format(counts$followup_only, big.mark = ","),
        tolower(stage)
      )
    )
  })

  output$prom_change_metrics <- renderUI({
    req(input$prom_type)
    df <- prom_change_data()
    digits <- prom_score_digits(input$prom_type)
    mcid <- prom_change_mcid()

    if (nrow(df) == 0) {
      return(
        fluidRow(
          column(3, metric_box("Paired cases", 0, "Pre-op and follow-up both present")),
          column(3, metric_box("Mean pre-op", "NA", prom_score_label(input$prom_type))),
          column(3, metric_box("Mean follow-up", "NA", "Selected follow-up stage")),
          column(3, metric_box("Mean change", "NA", "Follow-up minus pre-op"))
        )
      )
    }

    p_text <- format_p_value(paired_p_value(df$change))
    p_label <- if (identical(p_text, "NA")) {
      "Paired t-test not estimable"
    } else if (startsWith(p_text, "<")) {
      paste0("p ", p_text, " (paired t-test)")
    } else {
      paste0("p = ", p_text, " (paired t-test)")
    }

    ci <- mean_ci_bounds(df$change)
    ci_text <- if (any(is.na(ci))) {
      "95% CI not estimable"
    } else {
      sprintf("95%% CI %s to %s", round(ci[[1]], digits), round(ci[[2]], digits))
    }
    met_mcid <- round(100 * mean(df$change >= mcid, na.rm = TRUE), 1)

    threshold <- prom_pass_threshold()
    pass_value <- pass_rate(df$followup_score, threshold)
    pass_display <- if (is.na(pass_value)) "NA" else paste0(pass_value, "%")
    pass_subtitle <- if (is.na(threshold)) {
      "Set a PASS threshold in the sidebar"
    } else {
      sprintf("%s+ at %s, paired cases only", threshold, tolower(input$prom_change_stage %||% "follow-up"))
    }

    fluidRow(
      column(3, metric_box("Paired cases", format(nrow(df), big.mark = ","), p_label)),
      column(3, metric_box("Mean pre-op", round(mean(df$preop_score, na.rm = TRUE), digits), prom_score_label(input$prom_type))),
      column(3, metric_box("Mean follow-up", round(mean(df$followup_score, na.rm = TRUE), digits), input$prom_change_stage)),
      column(3, metric_box("Mean change", format_change_value(mean(df$change, na.rm = TRUE), digits), ci_text)),
      column(3, metric_box("Improved", paste0(round(100 * mean(df$change > 0, na.rm = TRUE), 1), "%"), "Any gain over pre-op")),
      column(3, metric_box("Met MCID", paste0(met_mcid, "%"), sprintf("Change of %s or more", mcid))),
      column(3, metric_box("Unchanged", paste0(round(100 * mean(df$change == 0, na.rm = TRUE), 1), "%"), "Same score as pre-op")),
      column(3, metric_box("Worse", paste0(round(100 * mean(df$change < 0, na.rm = TRUE), 1), "%"), "Below pre-op score")),
      column(3, metric_box("Met PASS", pass_display, pass_subtitle))
    )
  })

  output$prom_change_hist <- renderPlot({
    req(input$prom_type)
    df <- prom_change_data()
    validate(need(nrow(df) > 0, "No paired pre-op and follow-up scores for the current filters."))

    mcid <- prom_change_mcid()
    mean_change <- mean(df$change, na.rm = TRUE)

    ggplot(df, aes(x = change)) +
      geom_histogram(bins = 20, fill = "#3b82f6", colour = "white") +
      geom_vline(xintercept = 0, linetype = "dashed", colour = "#475569") +
      geom_vline(xintercept = mcid, linetype = "dotted", colour = "#10b981", linewidth = 0.8) +
      geom_vline(xintercept = mean_change, colour = "#b91c1c", linewidth = 0.8) +
      labs(
        title = paste("Change from pre-op to", tolower(input$prom_change_stage %||% "follow-up")),
        subtitle = "Dashed = no change, dotted green = MCID, solid red = mean change",
        x = "Change in score", y = "Case-sides"
      ) +
      theme_minimal(base_size = 12)
  })

  output$prom_change_scatter <- renderPlot({
    req(input$prom_type)
    df <- prom_change_data()
    validate(need(nrow(df) > 0, "No paired pre-op and follow-up scores for the current filters."))

    ggplot(df, aes(x = preop_score, y = followup_score)) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "#475569") +
      geom_point(alpha = 0.35, colour = "#1d4ed8") +
      labs(
        title = "Pre-op score vs follow-up score",
        subtitle = "Points above the line improved; points below deteriorated",
        x = paste("Pre-op", prom_score_label(input$prom_type)),
        y = paste("Follow-up", prom_score_label(input$prom_type))
      ) +
      theme_minimal(base_size = 12)
  })

  prom_change_stage_summary <- reactive({
    req(input$prom_type)
    summarise_prom_change(
      prom_change_all_stages(),
      group_cols = "followup_stage",
      mcid = prom_change_mcid(),
      digits = prom_score_digits(input$prom_type),
      pass_threshold = prom_pass_threshold()
    )
  })

  output$prom_change_stage_plot <- renderPlot({
    req(input$prom_type)
    summary_df <- prom_change_stage_summary()
    validate(need(nrow(summary_df) > 0, "No paired pre-op and follow-up scores for the current filters."))

    ggplot(summary_df, aes(x = followup_stage, y = `Mean change`, group = 1)) +
      geom_hline(yintercept = 0, linetype = "dashed", colour = "#475569") +
      geom_hline(yintercept = prom_change_mcid(), linetype = "dotted", colour = "#10b981") +
      geom_line(colour = "#1d4ed8", linewidth = 0.8) +
      geom_errorbar(aes(ymin = `CI low`, ymax = `CI high`), width = 0.12, colour = "#1d4ed8") +
      geom_point(size = 3, colour = "#1d4ed8") +
      geom_text(aes(label = paste0("n=", `Paired cases`)), vjust = -1.4, size = 3.4, colour = "#64748b") +
      scale_y_continuous(expand = ggplot2::expansion(mult = c(0.08, 0.18))) +
      labs(
        title = "Mean change from pre-op, by follow-up stage",
        subtitle = "Bars show the 95% confidence interval; dotted line is the MCID",
        x = NULL, y = "Mean change in score"
      ) +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 20, hjust = 1))
  })

  output$prom_change_stage_table <- renderDT({
    summary_df <- prom_change_stage_summary()
    if (nrow(summary_df) > 0) {
      names(summary_df)[names(summary_df) == "followup_stage"] <- "Follow-up stage"
    }
    datatable_or_message(
      summary_df,
      "No case has both a pre-op score and a follow-up score under the current filters."
    )
  })

  prom_change_group_summary <- reactive({
    req(input$prom_type)
    df <- prom_change_data()
    compare_col <- input$prom_compare %||% "overall"
    mcid <- prom_change_mcid()
    digits <- prom_score_digits(input$prom_type)

    threshold <- prom_pass_threshold()

    if (identical(compare_col, "overall")) {
      overall_df <- summarise_prom_change(df, mcid = mcid, digits = digits, pass_threshold = threshold)
      if (nrow(overall_df) > 0) {
        overall_df <- dplyr::bind_cols(
          data.frame(Overall = "All filtered cases", check.names = FALSE),
          overall_df
        )
      }
      return(overall_df)
    }

    grouped_df <- expand_group_column(df, compare_col)
    if (nrow(grouped_df) == 0) return(data.frame())

    summary_df <- summarise_prom_change(
      grouped_df,
      group_cols = ".group_value",
      mcid = mcid,
      digits = digits,
      pass_threshold = threshold
    )
    rename_group_column(summary_df, friendly_group_label(compare_col))
  })

  output$prom_change_group_plot <- renderPlot({
    req(input$prom_type)
    compare_col <- input$prom_compare %||% "overall"
    summary_df <- prom_change_group_summary()

    validate(need(
      nrow(summary_df) > 0,
      if (identical(compare_col, "overall")) {
        "No paired pre-op and follow-up scores for the current filters."
      } else {
        "No linked paired rows are available for the selected comparison."
      }
    ))

    label <- friendly_group_label(compare_col)
    if (identical(compare_col, "overall")) {
      summary_df[[label]] <- "All filtered cases"
    }

    plot_df <- summary_df %>%
      arrange(desc(`Paired cases`)) %>%
      slice_head(n = 12) %>%
      mutate(group_label = factor(.data[[label]], levels = .data[[label]][order(`Mean change`)]))

    ggplot(plot_df, aes(y = group_label)) +
      geom_segment(aes(x = `Mean pre-op`, xend = `Mean follow-up`, yend = group_label), colour = "#94a3b8", linewidth = 1.1) +
      geom_point(aes(x = `Mean pre-op`, colour = "Pre-op"), size = 3) +
      geom_point(aes(x = `Mean follow-up`, colour = "Follow-up"), size = 3) +
      scale_colour_manual(values = c("Pre-op" = "#f59e0b", "Follow-up" = "#1d4ed8"), breaks = c("Pre-op", "Follow-up")) +
      labs(
        title = paste("Mean pre-op and follow-up score by", tolower(label)),
        subtitle = "Up to 12 groups with the most paired cases",
        x = "Mean score", y = NULL, colour = NULL
      ) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "top")
  })

  output$prom_change_group_table <- renderDT({
    compare_col <- input$prom_compare %||% "overall"
    message <- if (identical(compare_col, "overall")) {
      "No paired pre-op and follow-up scores for the current filters."
    } else {
      "No linked paired rows are available for the selected comparison."
    }
    datatable_or_message(prom_change_group_summary(), message)
  })

  prom_change_display <- reactive({
    req(input$prom_type)
    df <- prom_change_data()
    if (nrow(df) == 0) return(data.frame())

    mcid <- prom_change_mcid()
    threshold <- prom_pass_threshold()
    met_pass <- if (length(threshold) == 1 && !is.na(threshold)) {
      ifelse(is.na(df$followup_score), NA, ifelse(df$followup_score >= threshold, "Yes", "No"))
    } else {
      NA_character_
    }

    data.frame(
      FORM_RESPONSE_GROUP_ID = df$FORM_RESPONSE_GROUP_ID,
      `Patient Id` = df$`Patient Id`,
      `MRN Number` = df$`MRN Number`,
      `First Name` = df$`First Name`,
      `Last Name` = df$`Last Name`,
      Side = df$pair_side,
      Sex = df$patient_sex,
      Age = df$patient_age,
      BMI = df$bmi,
      `Follow-up stage` = as.character(df$followup_stage),
      `Pre-op score` = df$preop_score,
      `Follow-up score` = df$followup_score,
      Change = df$change,
      `Met MCID` = ifelse(is.na(df$change), NA, ifelse(df$change >= mcid, "Yes", "No")),
      `Met PASS` = met_pass,
      `Pre-op date` = as.character(df$preop_date),
      `Follow-up date` = as.character(df$followup_date),
      `Days between` = df$days_from_preop,
      Consultant = df$consultant,
      `Surgeon grade` = df$surgeon_grade,
      `Implant fixation type` = df$fixation_type,
      `Knee system` = df$knee_system,
      `Femoral component` = df$femoral_component,
      `Acetabular component` = df$acetabular_component,
      Hospital = df$HOSPITAL_NAME,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })

  output$prom_change_case_table <- renderDT({
    datatable_or_message(
      prom_change_display(),
      "No case has both a pre-op score and a follow-up score under the current filters."
    )
  })

  output$download_prom_change <- downloadHandler(
    filename = function() {
      paste0("inor_proms_change_", input$prom_type, "_", Sys.Date(), ".csv")
    },
    content = function(file) write_export(prom_change_display(), file)
  )


  # ---------------------------------------------------------------------------
  # Rolling trends
  # ---------------------------------------------------------------------------

  # The window can never exceed the cases available, or every point would be NA.
  prom_trend_max_window <- reactive({
    n <- nrow(prom_change_data())
    if (n < 10) return(0L)
    as.integer(min(400, floor(n / 2)))
  })

  output$prom_trend_window_ui <- renderUI({
    max_window <- prom_trend_max_window()
    if (max_window < 5) {
      return(tags$p(class = "help-note", "Not enough paired cases at this stage to draw a rolling trend."))
    }

    default <- max(5L, min(50L, max_window))
    sliderInput(
      "prom_trend_window",
      "Rolling window (cases)",
      min = 5,
      max = max_window,
      value = default,
      step = 5
    )
  })

  prom_trend_window <- reactive({
    max_window <- prom_trend_max_window()
    if (max_window < 5) return(NA_integer_)
    value <- suppressWarnings(as.integer(input$prom_trend_window))
    if (length(value) != 1 || is.na(value)) return(max(5L, min(50L, max_window)))
    max(5L, min(value, max_window))
  })

  prom_trend_data <- reactive({
    req(input$prom_type)
    window <- prom_trend_window()
    if (is.na(window)) return(data.frame())

    build_prom_trend_data(
      prom_change_data(),
      window = window,
      mcid = prom_change_mcid(),
      pass_threshold = prom_pass_threshold(),
      group_col = input$prom_compare %||% "overall"
    )
  })

  # Long form, one row per case per selected metric, for faceting.
  prom_trend_long <- reactive({
    df <- prom_trend_data()
    selected <- input$prom_trend_metrics %||% character(0)
    if (nrow(df) == 0 || !length(selected)) return(data.frame())

    metric_labels <- prom_trend_metric_label(selected)
    present <- metric_labels[metric_labels %in% names(df)]
    if (!length(present)) return(data.frame())

    df %>%
      select(
        .group_value, case_sequence, case_date, group_cases,
        FORM_RESPONSE_GROUP_ID, all_of(present)
      ) %>%
      tidyr::pivot_longer(
        cols = all_of(present),
        names_to = "Metric",
        values_to = "Value"
      ) %>%
      filter(!is.na(Value)) %>%
      mutate(Metric = factor(Metric, levels = present))
  })

  output$prom_trend_note <- renderUI({
    req(input$prom_type)
    window <- prom_trend_window()
    stage <- input$prom_change_stage %||% "the selected follow-up stage"

    if (is.na(window)) {
      return(tags$div(
        class = "notice-box",
        sprintf(
          "Fewer than 10 paired cases at %s under the current filters, so there is nothing to trend. Widen the date range, clear a filter, or pick another follow-up stage.",
          tolower(stage)
        )
      ))
    }

    df <- prom_trend_data()
    if (nrow(df) == 0) {
      return(tags$div(class = "notice-box", "No paired cases are available for the selected comparison."))
    }

    short_groups <- df %>%
      distinct(.group_value, group_cases) %>%
      filter(group_cases < window)

    messages <- sprintf(
      "Rolling average of %s cases, ordered by procedure date, over %s paired cases at %s. Each point covers that case and the %s before it.",
      window, format(nrow(df), big.mark = ","), tolower(stage), window - 1
    )

    if (nrow(short_groups) > 0) {
      messages <- c(
        messages,
        sprintf(
          "%s group(s) have fewer than %s cases and so produce no line: %s.",
          nrow(short_groups), window,
          paste(utils::head(short_groups$.group_value, 5), collapse = ", ")
        )
      )
    }

    tags$div(
      class = if (nrow(short_groups) > 0) "notice-box" else "success-box",
      tags$ul(lapply(messages, tags$li))
    )
  })

  output$prom_trend_metrics_boxes <- renderUI({
    req(input$prom_type)
    df <- prom_trend_data()
    window <- prom_trend_window()
    if (nrow(df) == 0 || is.na(window)) return(NULL)

    digits <- prom_score_digits(input$prom_type)

    # Compare the earliest complete window against the most recent one, pooling
    # groups so the headline reads on the whole filtered cohort.
    overall <- df %>% arrange(case_date, FORM_RESPONSE_GROUP_ID)
    first_window <- utils::head(overall, window)
    last_window <- utils::tail(overall, window)

    shift_box <- function(title, first_value, last_value, digits, suffix = "") {
      delta <- last_value - first_value
      metric_box(
        title,
        paste0(round(last_value, digits), suffix),
        sprintf(
          "%s%s%s vs first %s cases (%s%s)",
          if (is.na(delta)) "" else if (delta > 0) "+" else "",
          if (is.na(delta)) "NA" else round(delta, digits),
          suffix, window, round(first_value, digits), suffix
        )
      )
    }

    fluidRow(
      column(3, shift_box("Mean follow-up score", mean(first_window$followup_score, na.rm = TRUE), mean(last_window$followup_score, na.rm = TRUE), digits)),
      column(3, shift_box("Mean change", mean(first_window$change, na.rm = TRUE), mean(last_window$change, na.rm = TRUE), digits)),
      column(3, shift_box("Met MCID", 100 * mean(first_window$met_mcid, na.rm = TRUE), 100 * mean(last_window$met_mcid, na.rm = TRUE), 1, "%")),
      column(3, shift_box("PASS rate", 100 * mean(first_window$met_pass, na.rm = TRUE), 100 * mean(last_window$met_pass, na.rm = TRUE), 1, "%"))
    )
  })

  output$prom_trend_plot <- renderPlot({
    req(input$prom_type)
    plot_df <- prom_trend_long()
    validate(need(nrow(plot_df) > 0, "Select at least one metric, and make sure enough paired cases exist for the chosen window."))

    compare_col <- input$prom_compare %||% "overall"
    single_series <- identical(compare_col, "overall")
    window <- prom_trend_window()

    # Cap the number of series so a consultant-level split stays legible.
    if (!single_series) {
      top_groups <- plot_df %>%
        distinct(.group_value, group_cases) %>%
        arrange(desc(group_cases)) %>%
        slice_head(n = 8)
      plot_df <- plot_df %>% filter(.group_value %in% top_groups$.group_value)
      validate(need(nrow(plot_df) > 0, "No group has enough cases for the chosen window."))
    }

    x_by_date <- identical(input$prom_trend_xaxis %||% "sequence", "date")
    plot_df$x <- if (x_by_date) plot_df$case_date else plot_df$case_sequence

    # Whole-cohort mean per metric, so it is obvious when a window sits above or
    # below the cohort's own average rather than an arbitrary zero.
    cohort <- prom_trend_data()
    reference_df <- data.frame(
      Metric = c("Mean follow-up score", "Mean pre-op score", "Mean change", "Met MCID (%)", "PASS rate (%)"),
      Value = c(
        mean(cohort$followup_score, na.rm = TRUE),
        mean(cohort$preop_score, na.rm = TRUE),
        mean(cohort$change, na.rm = TRUE),
        100 * mean(cohort$met_mcid, na.rm = TRUE),
        100 * mean(cohort$met_pass, na.rm = TRUE)
      ),
      stringsAsFactors = FALSE
    ) %>%
      filter(Metric %in% levels(plot_df$Metric), is.finite(Value)) %>%
      mutate(Metric = factor(Metric, levels = levels(plot_df$Metric)))

    p <- ggplot(plot_df, aes(x = x, y = Value))

    if (nrow(reference_df) > 0) {
      p <- p + geom_hline(
        data = reference_df,
        aes(yintercept = Value),
        linetype = "dashed",
        colour = "#94a3b8",
        linewidth = 0.6
      )
    }

    if (single_series) {
      p <- p + geom_line(linewidth = 0.8, colour = "#1d4ed8")
    } else {
      p <- p +
        geom_line(aes(colour = .group_value), linewidth = 0.7) +
        labs(colour = NULL)
    }

    p +
      facet_wrap(~Metric, ncol = 2, scales = "free_y") +
      labs(
        title = sprintf("Rolling average of %s cases", window),
        subtitle = sprintf(
          "Paired cases at %s, ordered by procedure date",
          tolower(input$prom_change_stage %||% "the selected follow-up stage")
        ),
        x = if (x_by_date) "Procedure date of most recent case in window" else "Case sequence",
        y = NULL,
        caption = paste0(
          "Dashed line = mean across all filtered paired cases.",
          if (!single_series && !x_by_date) " Each series is numbered from its own first case, so series are not aligned in time — switch to procedure date to compare calendar periods." else ""
        )
      ) +
      theme_minimal(base_size = 12) +
      theme(
        legend.position = if (single_series) "none" else "top",
        panel.spacing = grid::unit(1, "lines"),
        plot.caption = element_text(colour = "#64748b", hjust = 0)
      )
  })

  prom_trend_shift <- reactive({
    req(input$prom_type)
    df <- prom_trend_data()
    window <- prom_trend_window()
    if (nrow(df) == 0 || is.na(window)) return(data.frame())

    digits <- prom_score_digits(input$prom_type)

    df %>%
      group_by(.group_value) %>%
      arrange(case_date, FORM_RESPONSE_GROUP_ID, .by_group = TRUE) %>%
      filter(dplyr::n() >= window) %>%
      summarise(
        Cases = dplyr::n(),
        `First window: mean score` = round(mean(utils::head(followup_score, window), na.rm = TRUE), digits),
        `Latest window: mean score` = round(mean(utils::tail(followup_score, window), na.rm = TRUE), digits),
        `First window: mean change` = round(mean(utils::head(change, window), na.rm = TRUE), digits),
        `Latest window: mean change` = round(mean(utils::tail(change, window), na.rm = TRUE), digits),
        `First window: MCID (%)` = round(100 * mean(utils::head(met_mcid, window), na.rm = TRUE), 1),
        `Latest window: MCID (%)` = round(100 * mean(utils::tail(met_mcid, window), na.rm = TRUE), 1),
        `First window: PASS (%)` = round(100 * mean(utils::head(met_pass, window), na.rm = TRUE), 1),
        `Latest window: PASS (%)` = round(100 * mean(utils::tail(met_pass, window), na.rm = TRUE), 1),
        .groups = "drop"
      ) %>%
      rename_group_column(friendly_group_label(input$prom_compare %||% "overall"))
  })

  output$prom_trend_shift_note <- renderUI({
    window <- prom_trend_window()
    if (is.na(window)) return(NULL)
    tags$p(
      class = "help-note",
      sprintf(
        "The first and last %s cases of each series, side by side. This is a crude before-and-after on two windows, not a test for trend — read it alongside the chart rather than instead of it, and treat small groups with caution.",
        window
      )
    )
  })

  output$prom_trend_shift_table <- renderDT({
    datatable_or_message(
      prom_trend_shift(),
      "Not enough paired cases for the chosen window."
    )
  })

  prom_trend_display <- reactive({
    req(input$prom_type)
    df <- prom_trend_data()
    if (nrow(df) == 0) return(data.frame())

    digits <- prom_score_digits(input$prom_type)
    group_label <- friendly_group_label(input$prom_compare %||% "overall")

    out <- data.frame(
      Series = df$.group_value,
      `Case sequence` = df$case_sequence,
      `Procedure date` = as.character(df$case_date),
      FORM_RESPONSE_GROUP_ID = df$FORM_RESPONSE_GROUP_ID,
      Side = df$pair_side,
      `Pre-op score` = df$preop_score,
      `Follow-up score` = df$followup_score,
      Change = df$change,
      `Met MCID` = ifelse(is.na(df$met_mcid), NA, ifelse(df$met_mcid, "Yes", "No")),
      `Met PASS` = ifelse(is.na(df$met_pass), NA, ifelse(df$met_pass, "Yes", "No")),
      `Rolling mean follow-up score` = round(df$`Mean follow-up score`, digits),
      `Rolling mean pre-op score` = round(df$`Mean pre-op score`, digits),
      `Rolling mean change` = round(df$`Mean change`, digits),
      `Rolling MCID (%)` = round(df$`Met MCID (%)`, 1),
      `Rolling PASS (%)` = round(df$`PASS rate (%)`, 1),
      Consultant = df$consultant,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )

    names(out)[names(out) == "Series"] <- group_label
    out
  })

  output$prom_trend_table <- renderDT({
    datatable_or_message(
      prom_trend_display(),
      "No paired cases available for a rolling trend under the current filters."
    )
  })

  output$download_prom_trend <- downloadHandler(
    filename = function() {
      paste0("inor_proms_trend_", input$prom_type, "_", Sys.Date(), ".csv")
    },
    content = function(file) write_export(prom_trend_display(), file)
  )

  # ---------------------------------------------------------------------------
  # Monitoring: funnel plot and CUSUM
  # ---------------------------------------------------------------------------

  prom_monitor_base <- reactive({
    req(input$prom_type)
    df <- prom_change_data()
    if (nrow(df) == 0) return(df)

    mcid <- prom_change_mcid()
    threshold <- prom_pass_threshold()

    df %>%
      mutate(
        met_mcid = ifelse(is.na(change), NA, change >= mcid),
        met_pass = if (length(threshold) == 1 && !is.na(threshold)) {
          ifelse(is.na(followup_score), NA, followup_score >= threshold)
        } else {
          NA
        }
      )
  })

  prom_monitor_outcome <- reactive({
    outcome <- input$prom_monitor_outcome %||% "met_pass"
    if (!outcome %in% prom_monitor_outcomes()) "met_pass" else outcome
  })

  prom_monitor_alphas <- reactive({
    switch(
      as.character(input$prom_monitor_alpha %||% 2),
      "1" = 0.05,
      "3" = 0.002,
      c(0.05, 0.002)
    )
  })

  output$prom_monitor_caveat <- renderUI({
    tags$div(
      class = "notice-box",
      tags$b("Neither chart adjusts for case mix."),
      " A funnel point outside the limits, or a CUSUM crossing, means that group or that run differs from the pooled cohort under the current filters. It is not evidence that care was worse: a surgeon operating on patients with lower baseline function, more complex deformity, or higher comorbidity will drift towards a signal for those reasons alone. Treat both as a prompt to look, never as a conclusion."
    )
  })

  # -- Funnel ----------------------------------------------------------------

  prom_funnel_parts <- reactive({
    req(input$prom_type)
    build_funnel_data(
      prom_monitor_base(),
      prom_monitor_outcome(),
      input$prom_compare %||% "overall"
    )
  })

  output$prom_funnel_note <- renderUI({
    compare_col <- input$prom_compare %||% "overall"
    if (identical(compare_col, "overall")) {
      return(tags$div(
        class = "notice-box",
        "A funnel plot compares units against each other, so choose something other than Overall under Compare scores by — consultant, surgeon grade, fixation type or a component."
      ))
    }

    parts <- prom_funnel_parts()
    if (is.null(parts)) {
      return(tags$div(class = "notice-box", "No linked rows are available for the selected comparison."))
    }

    outcome <- prom_monitor_outcome()
    digits <- if (prom_monitor_is_binary(outcome)) 1 else prom_score_digits(input$prom_type)

    tags$p(
      class = "help-note",
      sprintf(
        "%s by %s, across %s groups and %s paired cases at %s. Centre line = pooled %s (%s%s). Limits are %s, drawn from the exact binomial distribution for a proportion so they stay honest at low volume.",
        prom_monitor_label(outcome),
        tolower(friendly_group_label(compare_col)),
        nrow(parts$points),
        format(sum(parts$points$n), big.mark = ","),
        tolower(input$prom_change_stage %||% "the selected stage"),
        tolower(prom_monitor_label(outcome)),
        round(parts$centre, digits),
        if (parts$binary) "%" else "",
        paste(ifelse(prom_monitor_alphas() == 0.05, "95%", "99.8%"), collapse = " and ")
      )
    )
  })

  output$prom_funnel_plot <- renderPlot({
    req(input$prom_type)
    validate(need(
      !identical(input$prom_compare %||% "overall", "overall"),
      "Choose a grouping under Compare scores by to draw a funnel plot."
    ))

    parts <- prom_funnel_parts()
    validate(need(!is.null(parts) && nrow(parts$points) > 0, "No linked rows are available for the selected comparison."))

    points <- parts$points
    outcome <- prom_monitor_outcome()
    label <- friendly_group_label(input$prom_compare %||% "overall")

    # A smooth grid of volumes so the funnel is drawn as a curve rather than
    # only at the volumes that happen to occur.
    n_grid <- unique(round(seq(1, max(points$n), length.out = 300)))
    bands <- lapply(prom_monitor_alphas(), function(alpha) {
      band <- if (parts$binary) {
        funnel_limits_proportion(n_grid, parts$centre / 100, alpha)
      } else {
        funnel_limits_mean(n_grid, parts$centre, parts$spread, alpha)
      }
      if (nrow(band) == 0) return(NULL)
      band$label <- if (alpha == 0.05) "95%" else "99.8%"
      band
    })
    bands <- bind_rows(bands[!vapply(bands, is.null, logical(1))])
    validate(need(nrow(bands) > 0, "Control limits could not be computed for this outcome."))

    flagged <- funnel_flag_points(points, parts$centre, parts$spread, parts$binary, min(prom_monitor_alphas()))
    points$outside <- flagged$position[match(points$.group_value, flagged$.group_value)] != "Within limits"
    points$outside[is.na(points$outside)] <- FALSE

    ggplot() +
      geom_line(data = bands, aes(x = n, y = lower, linetype = label), colour = "#94a3b8") +
      geom_line(data = bands, aes(x = n, y = upper, linetype = label), colour = "#94a3b8") +
      geom_hline(yintercept = parts$centre, colour = "#0f172a", linewidth = 0.6) +
      geom_point(data = points, aes(x = n, y = value, colour = outside), size = 3) +
      # ggrepel gives much better label placement on a funnel, but it is not one
      # of the app's declared dependencies, so fall back rather than fail.
      (if (requireNamespace("ggrepel", quietly = TRUE)) {
        ggrepel::geom_text_repel(
          data = points,
          aes(x = n, y = value, label = .group_value),
          size = 3.2, colour = "#334155", max.overlaps = 20, seed = 1
        )
      } else {
        geom_text(
          data = points,
          aes(x = n, y = value, label = .group_value),
          size = 3.2, colour = "#334155", hjust = -0.1, vjust = -0.6
        )
      }) +
      scale_colour_manual(values = c(`TRUE` = "#b91c1c", `FALSE` = "#1d4ed8"), guide = "none") +
      labs(
        title = paste(prom_monitor_label(outcome), "by", tolower(label)),
        subtitle = "Solid line = pooled cohort value; dashed lines = control limits",
        x = "Paired cases in group",
        y = paste0(prom_monitor_label(outcome), if (parts$binary) " (%)" else ""),
        linetype = NULL
      ) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "top")
  })

  prom_funnel_table_data <- reactive({
    parts <- prom_funnel_parts()
    if (is.null(parts)) return(data.frame())

    outcome <- prom_monitor_outcome()
    digits <- if (prom_monitor_is_binary(outcome)) 1 else prom_score_digits(input$prom_type)
    label <- friendly_group_label(input$prom_compare %||% "overall")

    rows <- lapply(prom_monitor_alphas(), function(alpha) {
      flagged <- funnel_flag_points(parts$points, parts$centre, parts$spread, parts$binary, alpha)
      if (nrow(flagged) == 0) return(NULL)
      flagged$Limits <- if (alpha == 0.05) "95%" else "99.8%"
      flagged
    })
    rows <- bind_rows(rows[!vapply(rows, is.null, logical(1))])
    if (nrow(rows) == 0) return(data.frame())

    out <- rows %>%
      transmute(
        Group = .group_value,
        `Paired cases` = n,
        Value = round(value, digits),
        Limits,
        `Lower limit` = round(lower, digits),
        `Upper limit` = round(upper, digits),
        Position = position
      ) %>%
      arrange(desc(`Paired cases`), Limits)

    names(out)[names(out) == "Group"] <- label
    out
  })

  output$prom_funnel_table <- renderDT({
    datatable_or_message(
      prom_funnel_table_data(),
      "Choose a grouping under Compare scores by to draw a funnel plot."
    )
  })

  # -- CUSUM -----------------------------------------------------------------

  prom_cusum_observed_rate <- reactive({
    outcome <- prom_monitor_outcome()
    if (!prom_monitor_is_binary(outcome)) return(NA_real_)
    df <- prom_monitor_base()
    values <- df[[outcome]]
    values <- values[!is.na(values)]
    if (!length(values)) return(NA_real_)
    # CUSUM monitors failures, so the baseline is the rate of NOT achieving it.
    1 - mean(values)
  })

  output$prom_cusum_baseline_ui <- renderUI({
    observed <- prom_cusum_observed_rate()
    if (is.na(observed)) {
      return(tags$p(class = "help-note", "The CUSUM monitors a yes/no outcome; pick Met PASS or Met MCID above."))
    }
    numericInput(
      "prom_cusum_baseline",
      "CUSUM: baseline failure rate",
      value = round(observed, 3),
      min = 0.001, max = 0.999, step = 0.01
    )
  })

  prom_cusum_baseline <- reactive({
    value <- suppressWarnings(as.numeric(input$prom_cusum_baseline))
    if (length(value) != 1 || is.na(value) || value <= 0 || value >= 1) {
      return(prom_cusum_observed_rate())
    }
    value
  })

  prom_cusum_params <- reactive({
    odds_ratio <- suppressWarnings(as.numeric(input$prom_cusum_or))
    limit <- suppressWarnings(as.numeric(input$prom_cusum_limit))
    list(
      odds_ratio = if (length(odds_ratio) != 1 || is.na(odds_ratio) || odds_ratio <= 1) 2 else odds_ratio,
      limit = if (length(limit) != 1 || is.na(limit) || limit <= 0) 5 else limit
    )
  })

  prom_cusum_data <- reactive({
    req(input$prom_type)
    outcome <- prom_monitor_outcome()
    if (!prom_monitor_is_binary(outcome)) return(data.frame())

    p0 <- prom_cusum_baseline()
    if (is.na(p0)) return(data.frame())

    params <- prom_cusum_params()
    compare_col <- input$prom_compare %||% "overall"

    df <- prom_monitor_base() %>% filter(!is.na(.data[[outcome]]))
    if (nrow(df) == 0) return(data.frame())

    df$case_date <- dplyr::coalesce(df$analysis_date, df$followup_date, df$preop_date)
    df <- df %>% filter(!is.na(case_date))
    if (nrow(df) == 0) return(data.frame())

    if (identical(compare_col, "overall")) {
      df$.group_value <- "All filtered cases"
    } else {
      df <- expand_group_column(df, compare_col)
      if (nrow(df) == 0) return(data.frame())
    }

    df %>%
      group_by(.group_value) %>%
      arrange(case_date, FORM_RESPONSE_GROUP_ID, .by_group = TRUE) %>%
      group_modify(function(group_df, key) {
        chart <- bernoulli_cusum(
          !group_df[[outcome]],
          p0 = p0,
          odds_ratio = params$odds_ratio,
          limit = params$limit
        )
        if (nrow(chart) == 0) return(group_df[0, , drop = FALSE])
        group_df %>%
          mutate(
            case_sequence = chart$index,
            cusum_upper = chart$upper,
            cusum_lower = chart$lower,
            signal_up = chart$signal_up,
            signal_down = chart$signal_down
          )
      }) %>%
      ungroup()
  })

  output$prom_cusum_note <- renderUI({
    outcome <- prom_monitor_outcome()
    if (!prom_monitor_is_binary(outcome)) {
      return(tags$div(
        class = "notice-box",
        sprintf(
          "The CUSUM here monitors a yes/no outcome. %s is a continuous measure — pick Met PASS or Met MCID to chart it, or read the funnel plot above, which handles both.",
          prom_monitor_label(outcome)
        )
      ))
    }

    p0 <- prom_cusum_baseline()
    params <- prom_cusum_params()
    if (is.na(p0)) {
      return(tags$div(class = "notice-box", "No cases with a recorded outcome under the current filters."))
    }

    arl <- cusum_in_control_arl(p0, params$odds_ratio, params$limit)
    arl_text <- if (is.na(arl)) {
      "more than 20,000 cases"
    } else {
      paste(format(arl, big.mark = ","), "cases")
    }

    tags$p(
      class = "help-note",
      sprintf(
        "Monitoring failure to achieve %s, baseline failure rate %s%%, tuned to detect the failure odds rising (upper) or falling (lower) by a factor of %s, signalling at h = %s. If the true rate never moved, a false signal would still be expected roughly every %s — read that before acting on a crossing.",
        prom_monitor_label(outcome),
        round(100 * p0, 1),
        params$odds_ratio,
        params$limit,
        arl_text
      )
    )
  })

  output$prom_cusum_plot <- renderPlot({
    req(input$prom_type)
    outcome <- prom_monitor_outcome()
    validate(need(
      prom_monitor_is_binary(outcome),
      "The CUSUM monitors a yes/no outcome — pick Met PASS or Met MCID."
    ))

    df <- prom_cusum_data()
    validate(need(nrow(df) > 0, "No cases with a recorded outcome under the current filters."))

    params <- prom_cusum_params()
    compare_col <- input$prom_compare %||% "overall"
    single_series <- identical(compare_col, "overall")

    if (!single_series) {
      top_groups <- df %>%
        count(.group_value, name = "cases") %>%
        arrange(desc(cases)) %>%
        slice_head(n = 8)
      df <- df %>% filter(.group_value %in% top_groups$.group_value)
      validate(need(nrow(df) > 0, "No group has cases for this outcome."))
    }

    plot_df <- df %>%
      select(.group_value, case_sequence, cusum_upper, cusum_lower) %>%
      tidyr::pivot_longer(
        c(cusum_upper, cusum_lower),
        names_to = "Direction",
        values_to = "Value"
      ) %>%
      mutate(
        Direction = factor(
          ifelse(Direction == "cusum_upper", "Upper: worsening", "Lower: improving"),
          levels = c("Upper: worsening", "Lower: improving")
        )
      )

    limit_df <- data.frame(
      Direction = factor(
        c("Upper: worsening", "Lower: improving"),
        levels = c("Upper: worsening", "Lower: improving")
      ),
      limit = c(params$limit, -params$limit)
    )

    p <- ggplot(plot_df, aes(x = case_sequence, y = Value))

    if (single_series) {
      p <- p + geom_line(linewidth = 0.8, colour = "#1d4ed8")
    } else {
      p <- p + geom_line(aes(colour = .group_value), linewidth = 0.7) + labs(colour = NULL)
    }

    signal_df <- df %>%
      select(.group_value, case_sequence, cusum_upper, cusum_lower, signal_up, signal_down) %>%
      tidyr::pivot_longer(
        c(cusum_upper, cusum_lower),
        names_to = "Direction",
        values_to = "Value"
      ) %>%
      mutate(
        is_signal = ifelse(Direction == "cusum_upper", signal_up, signal_down),
        Direction = factor(
          ifelse(Direction == "cusum_upper", "Upper: worsening", "Lower: improving"),
          levels = c("Upper: worsening", "Lower: improving")
        )
      ) %>%
      filter(is_signal)

    p <- p +
      geom_hline(data = limit_df, aes(yintercept = limit), linetype = "dashed", colour = "#b91c1c") +
      geom_hline(yintercept = 0, colour = "#cbd5e1")

    if (nrow(signal_df) > 0) {
      p <- p + geom_point(
        data = signal_df,
        aes(x = case_sequence, y = Value),
        colour = "#b91c1c", size = 2, shape = 4, stroke = 1.1
      )
    }

    p +
      facet_wrap(~Direction, ncol = 2, scales = "free_y") +
      labs(
        title = paste("CUSUM:", prom_monitor_label(outcome)),
        subtitle = sprintf(
          "Cases ordered by procedure date. Crosses mark signals; the chart resets to zero after each one (h = %s)",
          params$limit
        ),
        x = "Case sequence",
        y = "Cumulative log-likelihood ratio"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        legend.position = if (single_series) "none" else "top",
        panel.spacing = grid::unit(1, "lines")
      )
  })

  output$prom_cusum_table <- renderDT({
    outcome <- prom_monitor_outcome()
    if (!prom_monitor_is_binary(outcome)) {
      return(datatable_or_message(data.frame(), "Pick Met PASS or Met MCID to run a CUSUM."))
    }

    df <- prom_cusum_data()
    if (nrow(df) == 0) {
      return(datatable_or_message(data.frame(), "No cases with a recorded outcome under the current filters."))
    }

    label <- friendly_group_label(input$prom_compare %||% "overall")

    out <- df %>%
      group_by(.group_value) %>%
      summarise(
        Cases = dplyr::n(),
        `Outcome achieved (%)` = round(100 * mean(.data[[outcome]], na.rm = TRUE), 1),
        `Peak upper` = round(max(cusum_upper, na.rm = TRUE), 2),
        `Worsening signals` = sum(signal_up, na.rm = TRUE),
        `First worsening signal` = if (any(signal_up)) min(case_sequence[signal_up]) else NA_integer_,
        `Lowest lower` = round(min(cusum_lower, na.rm = TRUE), 2),
        `Improving signals` = sum(signal_down, na.rm = TRUE),
        `First improving signal` = if (any(signal_down)) min(case_sequence[signal_down]) else NA_integer_,
        .groups = "drop"
      ) %>%
      arrange(desc(Cases))

    names(out)[names(out) == ".group_value"] <- label
    datatable_or_message(out)
  })

  output$peri_comp_date_ui <- renderUI({
    bounds <- date_bounds(prepare_periop_complication_data(input$peri_comp_source %||% "all")$analysis_date)
    make_date_input("peri_comp_date_range", "Date range", bounds)
  })

  filtered_peri_complications <- reactive({
    df <- prepare_periop_complication_data(input$peri_comp_source %||% "all")

    df <- apply_date_filter(df, "analysis_date", input$peri_comp_date_range)
    df <- apply_single_filter(df, "analysis_joint", input$peri_comp_joint)
    df <- apply_single_filter(df, "complication", input$peri_comp_type)
    df <- apply_single_filter(df, "complication_group", input$peri_comp_group)
    df <- apply_single_filter(df, "side", input$peri_comp_side)
    df <- apply_single_filter(df, "consultant", input$peri_comp_consultant)
    df <- apply_single_filter(df, "surgeon_grade", input$peri_comp_surgeon_grade)
    df <- apply_single_filter(df, "fixation_type", input$peri_comp_fixation)
    df <- apply_component_filter(df, "knee_system", input$peri_comp_knee_system)
    df <- apply_component_filter(df, "femoral_component", input$peri_comp_femoral_component)
    df <- apply_component_filter(df, "acetabular_component", input$peri_comp_acetabular_component)

    df
  })

  peri_comp_can_rate <- reactive({
    periop_complication_has_case_link(input$peri_comp_source %||% "all")
  })

  peri_comp_denominator_cases <- reactive({
    if (!peri_comp_can_rate()) {
      return(periop_case_cohort[0, , drop = FALSE])
    }

    df <- periop_case_cohort
    df <- apply_date_filter(df, "case_proc_date", input$peri_comp_date_range)
    df <- apply_single_filter(df, "case_joint", input$peri_comp_joint)
    df <- apply_single_filter(df, "consultant", input$peri_comp_consultant)
    df <- apply_single_filter(df, "surgeon_grade", input$peri_comp_surgeon_grade)
    df <- apply_single_filter(df, "fixation_type", input$peri_comp_fixation)
    df <- apply_component_filter(df, "knee_system", input$peri_comp_knee_system)
    df <- apply_component_filter(df, "femoral_component", input$peri_comp_femoral_component)
    df <- apply_component_filter(df, "acetabular_component", input$peri_comp_acetabular_component)

    df
  })

  output$peri_comp_link_note <- renderUI({
    messages <- character()

    if (!periop_complication_has_case_link(input$peri_comp_source %||% "all")) {
      messages <- c(
        messages,
        "The selected peri-op complication extract does not link back to the peri-op operative case registry in the dummy data, so rate calculations are disabled."
      )
    }
    if (!periop_complication_has_implant_link(input$peri_comp_source %||% "all")) {
      messages <- c(
        messages,
        "The selected peri-op complication extract does not link back to implant classification in the dummy data, so implant-based filters and comparisons may stay empty."
      )
    }

    if (!length(messages)) {
      tags$div(class = "success-box", "Peri-op complications link to both the operative case registry and implant classification, so rates and comparisons are enabled.")
    } else {
      tags$div(class = "notice-box", tags$ul(lapply(messages, tags$li)))
    }
  })

  output$peri_comp_metrics <- renderUI({
    df <- filtered_peri_complications()
    denom_n <- nrow(peri_comp_denominator_cases())

    fluidRow(
      column(3, metric_box("Complication events", nrow(df), "Peri-op long-format rows")),
      column(3, metric_box("Affected cases", count_unique_ids(df), "Unique FORM_RESPONSE_GROUP_ID")),
      column(3, metric_box("Complication types", length(unique(stats::na.omit(df$complication))), "Distinct categories")),
      column(3, metric_box("Rate denominator", ifelse(denom_n == 0, "Not linked", denom_n), "Peri-op cases available for rate calculation"))
    )
  })

  output$peri_comp_type_plot <- renderPlot({
    df <- filtered_peri_complications()
    validate(need(nrow(df) > 0, "No peri-op complication records for the current filters."))

    if (identical(input$peri_comp_measure, "rate")) {
      denom_n <- nrow(peri_comp_denominator_cases())
      validate(need(denom_n > 0, "Rates are unavailable because the selected peri-op extract is not linked to the peri-op case cohort."))

      plot_df <- df %>%
        distinct(FORM_RESPONSE_GROUP_ID, complication) %>%
        group_by(complication) %>%
        summarise(affected_cases = n(), .groups = "drop") %>%
        mutate(value = round(affected_cases / denom_n * 100, 1))

      ggplot(plot_df, aes(x = reorder(complication, value), y = value)) +
        geom_col() +
        coord_flip() +
        labs(title = "Peri-op complication rates", x = NULL, y = "Rate (%)") +
        theme_minimal(base_size = 12)
    } else {
      plot_df <- df %>%
        group_by(complication) %>%
        summarise(value = n(), .groups = "drop")

      ggplot(plot_df, aes(x = reorder(complication, value), y = value)) +
        geom_col() +
        coord_flip() +
        labs(title = "Peri-op complication counts", x = NULL, y = "Events") +
        theme_minimal(base_size = 12)
    }
  })

  output$peri_comp_compare_plot <- renderPlot({
    compare_col <- input$peri_comp_compare %||% "overall"

    if (identical(compare_col, "overall")) {
      df <- filtered_peri_complications() %>%
        group_by(complication_group) %>%
        summarise(events = n(), .groups = "drop") %>%
        arrange(desc(events))

      validate(need(nrow(df) > 0, "No complication-group values are available."))

      ggplot(df, aes(x = reorder(complication_group, events), y = events)) +
        geom_col() +
        coord_flip() +
        labs(title = "Peri-op events by complication group", x = NULL, y = "Events") +
        theme_minimal(base_size = 12)
    } else {
      label <- friendly_group_label(compare_col)
      grouped_df <- expand_group_column(filtered_peri_complications(), compare_col)
      validate(need(nrow(grouped_df) > 0, "No linked peri-op complication rows are available for the selected comparison."))

      if (identical(input$peri_comp_measure, "rate")) {
        denom_df <- expand_group_column(peri_comp_denominator_cases(), compare_col) %>%
          group_by(.group_value) %>%
          summarise(total_cases = n_distinct(FORM_RESPONSE_GROUP_ID), .groups = "drop")

        plot_df <- grouped_df %>%
          distinct(FORM_RESPONSE_GROUP_ID, .group_value) %>%
          group_by(.group_value) %>%
          summarise(affected_cases = n(), .groups = "drop") %>%
          left_join(denom_df, by = ".group_value") %>%
          mutate(value = round(affected_cases / total_cases * 100, 1)) %>%
          filter(!is.na(value))

        validate(need(nrow(plot_df) > 0, "Rates are unavailable for the selected peri-op comparison."))

        plot_df <- plot_df %>%
          arrange(desc(value)) %>%
          slice_head(n = 12)

        ggplot(plot_df, aes(x = reorder(.group_value, value), y = value)) +
          geom_col() +
          coord_flip() +
          labs(title = paste("Peri-op complication rate by", label), x = NULL, y = "Rate (%)") +
          theme_minimal(base_size = 12)
      } else {
        plot_df <- grouped_df %>%
          group_by(.group_value) %>%
          summarise(value = n(), .groups = "drop") %>%
          arrange(desc(value)) %>%
          slice_head(n = 12)

        ggplot(plot_df, aes(x = reorder(.group_value, value), y = value)) +
          geom_col() +
          coord_flip() +
          labs(title = paste("Peri-op complication counts by", label), x = NULL, y = "Events") +
          theme_minimal(base_size = 12)
      }
    }
  })

  peri_comp_detail_summary <- reactive({
    df <- filtered_peri_complications()
    compare_col <- input$peri_comp_compare %||% "overall"

    if (nrow(df) == 0) return(data.frame())

    if (identical(compare_col, "overall")) {
      event_counts <- df %>%
        group_by(complication) %>%
        summarise(event_count = n(), .groups = "drop")

      affected_cases <- df %>%
        distinct(FORM_RESPONSE_GROUP_ID, complication) %>%
        group_by(complication) %>%
        summarise(affected_cases = n(), .groups = "drop")

      summary_df <- event_counts %>%
        left_join(affected_cases, by = "complication")

      if (identical(input$peri_comp_measure, "rate")) {
        denom_n <- nrow(peri_comp_denominator_cases())
        if (denom_n == 0) return(data.frame())
        summary_df <- summary_df %>%
          mutate(
            total_cases = denom_n,
            rate_pct = round(affected_cases / total_cases * 100, 1)
          )
      }

      return(summary_df %>% arrange(desc(event_count)))
    }

    grouped_df <- expand_group_column(df, compare_col)
    if (nrow(grouped_df) == 0) return(data.frame())

    event_counts <- grouped_df %>%
      group_by(.group_value, complication) %>%
      summarise(event_count = n(), .groups = "drop")

    affected_cases <- grouped_df %>%
      distinct(FORM_RESPONSE_GROUP_ID, .group_value, complication) %>%
      group_by(.group_value, complication) %>%
      summarise(affected_cases = n(), .groups = "drop")

    summary_df <- event_counts %>%
      left_join(affected_cases, by = c(".group_value", "complication"))

    if (identical(input$peri_comp_measure, "rate")) {
      denom_df <- expand_group_column(peri_comp_denominator_cases(), compare_col) %>%
        group_by(.group_value) %>%
        summarise(total_cases = n_distinct(FORM_RESPONSE_GROUP_ID), .groups = "drop")

      summary_df <- summary_df %>%
        left_join(denom_df, by = ".group_value") %>%
        mutate(rate_pct = round(affected_cases / total_cases * 100, 1))
    }

    summary_df %>%
      rename_group_column(friendly_group_label(compare_col)) %>%
      arrange(desc(event_count))
  })

  output$peri_comp_summary_table <- renderDT({
    message <- if (identical(input$peri_comp_measure, "rate")) {
      "No linked peri-op complication rows are available for rate calculation with the current filters."
    } else {
      "No peri-op complication rows are available for the current filters."
    }
    datatable_or_message(peri_comp_detail_summary(), message)
  })

  output$peri_comp_raw_table <- renderDT({
    df <- filtered_peri_complications() %>%
      transmute(
        FORM_RESPONSE_GROUP_ID,
        `Patient Id`,
        `MRN Number`,
        `First Name`,
        `Last Name`,
        Joint = analysis_joint,
        `Procedure type` = analysis_proc_type,
        complication_group,
        complication,
        side,
        presentation,
        `Assessment / event date` = as.character(analysis_date),
        Consultant = consultant,
        `Surgeon grade` = surgeon_grade,
        `Implant fixation type` = fixation_type,
        `Knee system` = knee_system,
        `Femoral component` = femoral_component,
        `Acetabular component` = acetabular_component,
        source,
        detail
      )

    datatable_or_message(df)
  })

  output$download_peri_complications <- downloadHandler(
    filename = function() paste0("inor_periop_complications_", Sys.Date(), ".csv"),
    content = function(file) write_export(filtered_peri_complications(), file)
  )

  output$comp_date_ui <- renderUI({
    bounds <- date_bounds(prepare_complication_data(input$comp_source %||% "all")$analysis_date)
    make_date_input("comp_date_range", "Date range", bounds)
  })

  filtered_complications <- reactive({
    df <- prepare_complication_data(input$comp_source %||% "all")

    df <- apply_date_filter(df, "analysis_date", input$comp_date_range)
    df <- apply_single_filter(df, "analysis_joint", input$comp_joint)
    df <- apply_single_filter(df, "complication", input$comp_type)
    df <- apply_single_filter(df, "presentation", input$comp_presentation)
    df <- apply_single_filter(df, "side", input$comp_side)
    df <- apply_single_filter(df, "consultant", input$comp_consultant)
    df <- apply_single_filter(df, "surgeon_grade", input$comp_surgeon_grade)
    df <- apply_single_filter(df, "fixation_type", input$comp_fixation)
    df <- apply_component_filter(df, "knee_system", input$comp_knee_system)
    df <- apply_component_filter(df, "femoral_component", input$comp_femoral_component)
    df <- apply_component_filter(df, "acetabular_component", input$comp_acetabular_component)

    df
  })

  comp_can_rate <- reactive({
    complication_has_preop_link(input$comp_source %||% "all")
  })

  comp_denominator_cases <- reactive({
    if (!comp_can_rate()) {
      return(case_cohort[0, , drop = FALSE])
    }

    df <- case_cohort
    df <- apply_date_filter(df, "case_proc_date", input$comp_date_range)
    df <- apply_single_filter(df, "case_joint", input$comp_joint)
    df <- apply_single_filter(df, "consultant", input$comp_consultant)
    df <- apply_single_filter(df, "surgeon_grade", input$comp_surgeon_grade)
    df <- apply_single_filter(df, "fixation_type", input$comp_fixation)
    df <- apply_component_filter(df, "knee_system", input$comp_knee_system)
    df <- apply_component_filter(df, "femoral_component", input$comp_femoral_component)
    df <- apply_component_filter(df, "acetabular_component", input$comp_acetabular_component)

    df
  })

  output$comp_link_note <- renderUI({
    messages <- character()

    if (!complication_has_preop_link(input$comp_source %||% "all")) {
      messages <- c(
        messages,
        "The selected complication extract does not link back to the case table in the dummy data, so rate calculations and consultant-linked cohorting are disabled."
      )
    }
    if (!complication_has_implant_link(input$comp_source %||% "all")) {
      messages <- c(
        messages,
        "The selected complication extract does not link back to implant classification in the dummy data, so implant-based filters and comparisons may stay empty."
      )
    }

    if (!length(messages)) {
      tags$div(class = "success-box", "Complications link to both case-level and implant-level metadata, so rates and implant comparisons are enabled.")
    } else {
      tags$div(class = "notice-box", tags$ul(lapply(messages, tags$li)))
    }
  })

  output$comp_metrics <- renderUI({
    df <- filtered_complications()
    denom_n <- nrow(comp_denominator_cases())

    fluidRow(
      column(3, metric_box("Complication events", nrow(df), "Rows in long format")),
      column(3, metric_box("Affected cases", count_unique_ids(df), "Unique FORM_RESPONSE_GROUP_ID")),
      column(3, metric_box("Complication types", length(unique(stats::na.omit(df$complication))), "Distinct categories")),
      column(3, metric_box("Rate denominator", ifelse(denom_n == 0, "Not linked", denom_n), "Cases available for rate calculation"))
    )
  })

  output$comp_type_plot <- renderPlot({
    df <- filtered_complications()
    validate(need(nrow(df) > 0, "No complication records for the current filters."))

    if (identical(input$comp_measure, "rate")) {
      denom_n <- nrow(comp_denominator_cases())
      validate(need(denom_n > 0, "Rates are unavailable because the selected complication extract is not linked to the case cohort."))

      plot_df <- df %>%
        distinct(FORM_RESPONSE_GROUP_ID, complication) %>%
        group_by(complication) %>%
        summarise(affected_cases = n(), .groups = "drop") %>%
        mutate(value = round(affected_cases / denom_n * 100, 1))

      ggplot(plot_df, aes(x = reorder(complication, value), y = value)) +
        geom_col() +
        coord_flip() +
        labs(title = "Complication rates", x = NULL, y = "Rate (%)") +
        theme_minimal(base_size = 12)
    } else {
      plot_df <- df %>%
        group_by(complication) %>%
        summarise(value = n(), .groups = "drop")

      ggplot(plot_df, aes(x = reorder(complication, value), y = value)) +
        geom_col() +
        coord_flip() +
        labs(title = "Complication counts", x = NULL, y = "Events") +
        theme_minimal(base_size = 12)
    }
  })

  output$comp_compare_plot <- renderPlot({
    compare_col <- input$comp_compare %||% "overall"

    if (identical(compare_col, "overall")) {
      df <- filtered_complications() %>%
        group_by(presentation) %>%
        summarise(events = n(), .groups = "drop") %>%
        arrange(desc(events))

      validate(need(nrow(df) > 0, "No presentation values are available."))

      ggplot(df, aes(x = reorder(presentation, events), y = events)) +
        geom_col() +
        coord_flip() +
        labs(title = "Events by presentation", x = NULL, y = "Events") +
        theme_minimal(base_size = 12)
    } else {
      label <- friendly_group_label(compare_col)
      grouped_df <- expand_group_column(filtered_complications(), compare_col)
      validate(need(nrow(grouped_df) > 0, "No linked complication rows are available for the selected comparison."))

      if (identical(input$comp_measure, "rate")) {
        denom_df <- expand_group_column(comp_denominator_cases(), compare_col) %>%
          group_by(.group_value) %>%
          summarise(total_cases = n_distinct(FORM_RESPONSE_GROUP_ID), .groups = "drop")

        plot_df <- grouped_df %>%
          distinct(FORM_RESPONSE_GROUP_ID, .group_value) %>%
          group_by(.group_value) %>%
          summarise(affected_cases = n(), .groups = "drop") %>%
          left_join(denom_df, by = ".group_value") %>%
          mutate(value = round(affected_cases / total_cases * 100, 1)) %>%
          filter(!is.na(value))

        validate(need(nrow(plot_df) > 0, "Rates are unavailable for the selected comparison."))

        plot_df <- plot_df %>%
          arrange(desc(value)) %>%
          slice_head(n = 12)

        ggplot(plot_df, aes(x = reorder(.group_value, value), y = value)) +
          geom_col() +
          coord_flip() +
          labs(title = paste("Complication rate by", label), x = NULL, y = "Rate (%)") +
          theme_minimal(base_size = 12)
      } else {
        plot_df <- grouped_df %>%
          group_by(.group_value) %>%
          summarise(value = n(), .groups = "drop") %>%
          arrange(desc(value)) %>%
          slice_head(n = 12)

        ggplot(plot_df, aes(x = reorder(.group_value, value), y = value)) +
          geom_col() +
          coord_flip() +
          labs(title = paste("Complication counts by", label), x = NULL, y = "Events") +
          theme_minimal(base_size = 12)
      }
    }
  })

  comp_detail_summary <- reactive({
    df <- filtered_complications()
    compare_col <- input$comp_compare %||% "overall"

    if (nrow(df) == 0) return(data.frame())

    if (identical(compare_col, "overall")) {
      event_counts <- df %>%
        group_by(complication) %>%
        summarise(event_count = n(), .groups = "drop")

      affected_cases <- df %>%
        distinct(FORM_RESPONSE_GROUP_ID, complication) %>%
        group_by(complication) %>%
        summarise(affected_cases = n(), .groups = "drop")

      summary_df <- event_counts %>%
        left_join(affected_cases, by = "complication")

      if (identical(input$comp_measure, "rate")) {
        denom_n <- nrow(comp_denominator_cases())
        if (denom_n == 0) return(data.frame())
        summary_df <- summary_df %>%
          mutate(
            total_cases = denom_n,
            rate_pct = round(affected_cases / total_cases * 100, 1)
          )
      }

      return(summary_df %>% arrange(desc(event_count)))
    }

    grouped_df <- expand_group_column(df, compare_col)
    if (nrow(grouped_df) == 0) return(data.frame())

    event_counts <- grouped_df %>%
      group_by(.group_value, complication) %>%
      summarise(event_count = n(), .groups = "drop")

    affected_cases <- grouped_df %>%
      distinct(FORM_RESPONSE_GROUP_ID, .group_value, complication) %>%
      group_by(.group_value, complication) %>%
      summarise(affected_cases = n(), .groups = "drop")

    summary_df <- event_counts %>%
      left_join(affected_cases, by = c(".group_value", "complication"))

    if (identical(input$comp_measure, "rate")) {
      denom_df <- expand_group_column(comp_denominator_cases(), compare_col) %>%
        group_by(.group_value) %>%
        summarise(total_cases = n_distinct(FORM_RESPONSE_GROUP_ID), .groups = "drop")

      summary_df <- summary_df %>%
        left_join(denom_df, by = ".group_value") %>%
        mutate(rate_pct = round(affected_cases / total_cases * 100, 1))
    }

    summary_df %>%
      rename_group_column(friendly_group_label(compare_col)) %>%
      arrange(desc(event_count))
  })

  output$comp_summary_table <- renderDT({
    message <- if (identical(input$comp_measure, "rate")) {
      "No linked complication rows are available for rate calculation with the current filters."
    } else {
      "No complication rows are available for the current filters."
    }
    datatable_or_message(comp_detail_summary(), message)
  })

  output$comp_raw_table <- renderDT({
    df <- filtered_complications() %>%
      transmute(
        FORM_RESPONSE_GROUP_ID,
        `Patient Id`,
        `MRN Number`,
        `First Name`,
        `Last Name`,
        Joint = analysis_joint,
        `Procedure type` = analysis_proc_type,
        complication,
        side,
        presentation,
        `Assessment / event date` = as.character(analysis_date),
        Consultant = consultant,
        `Surgeon grade` = surgeon_grade,
        `Implant fixation type` = fixation_type,
        `Knee system` = knee_system,
        `Femoral component` = femoral_component,
        `Acetabular component` = acetabular_component,
        source,
        detail
      )

    datatable_or_message(df)
  })

  output$download_complications <- downloadHandler(
    filename = function() paste0("inor_complications_", Sys.Date(), ".csv"),
    content = function(file) write_export(filtered_complications(), file)
  )

  output$surg_link_note <- renderUI({
    messages <- character()

    if (!surgical_has_preop_link) {
      messages <- c(
        messages,
        "The surgical timing extract does not link back to the case registry in the dummy data, so consultant-linked filtering may stay empty."
      )
    }
    if (!surgical_has_implant_link) {
      messages <- c(
        messages,
        "The surgical timing extract does not link back to implant classification in the dummy data, so implant-based filters and comparisons may stay empty."
      )
    }

    if (!length(messages)) {
      tags$div(class = "success-box", "Surgical timing records link to both case-level and implant-level metadata, so consultant and implant comparisons are enabled.")
    } else {
      tags$div(class = "notice-box", tags$ul(lapply(messages, tags$li)))
    }
  })

  filtered_surgical <- reactive({
    df <- surgical_enriched %>%
      filter(!is.na(surgical_duration_mins))

    df <- apply_date_filter(df, "analysis_date", input$surg_date_range)
    df <- apply_single_filter(df, "source", input$surg_source)
    df <- apply_single_filter(df, "Joint", input$surg_joint)
    df <- apply_single_filter(df, "ACCESS_POINT_NAME", input$surg_hospital)
    df <- apply_single_filter(df, "Procedure type", input$surg_proc)
    df <- apply_single_filter(df, "consultant", input$surg_consultant)
    df <- apply_single_filter(df, "surgeon_grade", input$surg_surgeon_grade)
    df <- apply_single_filter(df, "fixation_type", input$surg_fixation)
    df <- apply_component_filter(df, "knee_system", input$surg_knee_system)
    df <- apply_component_filter(df, "femoral_component", input$surg_femoral_component)
    df <- apply_component_filter(df, "acetabular_component", input$surg_acetabular_component)

    df
  })

  # ---------------------------------------------------------------------------
  # Surgical times: rolling trend
  # ---------------------------------------------------------------------------

  surg_trend_max_window <- reactive({
    n <- nrow(filtered_surgical())
    if (n < 10) return(0L)
    as.integer(min(400, floor(n / 2)))
  })

  output$surg_trend_window_ui <- renderUI({
    max_window <- surg_trend_max_window()
    if (max_window < 5) {
      return(tags$p(class = "help-note", "Not enough timed cases to draw a rolling trend."))
    }
    sliderInput(
      "surg_trend_window",
      "Rolling window (cases)",
      min = 5, max = max_window,
      value = max(5L, min(50L, max_window)), step = 5
    )
  })

  surg_trend_window <- reactive({
    max_window <- surg_trend_max_window()
    if (max_window < 5) return(NA_integer_)
    value <- suppressWarnings(as.integer(input$surg_trend_window))
    if (length(value) != 1 || is.na(value)) return(max(5L, min(50L, max_window)))
    max(5L, min(value, max_window))
  })

  surg_long_threshold <- reactive({
    value <- suppressWarnings(as.numeric(input$surg_long_threshold))
    if (length(value) != 1 || is.na(value) || value <= 0) 120 else value
  })

  surg_trend_data <- reactive({
    window <- surg_trend_window()
    if (is.na(window)) return(data.frame())

    df <- filtered_surgical() %>%
      filter(!is.na(surgical_duration_mins), !is.na(analysis_date))
    if (nrow(df) == 0) return(data.frame())

    threshold <- surg_long_threshold()
    df$is_long <- df$surgical_duration_mins > threshold

    compare_col <- input$surg_compare %||% "overall"
    if (identical(compare_col, "overall")) {
      df$.group_value <- "All filtered cases"
    } else {
      df <- expand_group_column(df, compare_col)
      if (nrow(df) == 0) return(data.frame())
    }

    df %>%
      group_by(.group_value) %>%
      arrange(analysis_date, FORM_RESPONSE_GROUP_ID, .by_group = TRUE) %>%
      mutate(
        case_sequence = dplyr::row_number(),
        group_cases = dplyr::n(),
        `Mean duration` = rolling_mean(surgical_duration_mins, window),
        `Median duration` = rolling_stat(surgical_duration_mins, window, function(v) stats::median(v, na.rm = TRUE)),
        `Variability (SD)` = rolling_stat(surgical_duration_mins, window, function(v) stats::sd(v, na.rm = TRUE)),
        `Long cases (%)` = rolling_percent(is_long, window)
      ) %>%
      ungroup()
  })

  surg_trend_metric_labels <- function(selected) {
    labels <- c(
      mean = "Mean duration",
      median = "Median duration",
      sd = "Variability (SD)",
      long = "Long cases (%)"
    )
    unname(labels[selected[selected %in% names(labels)]])
  }

  surg_trend_long <- reactive({
    df <- surg_trend_data()
    present <- surg_trend_metric_labels(input$surg_trend_metrics %||% character(0))
    present <- present[present %in% names(df)]
    if (nrow(df) == 0 || !length(present)) return(data.frame())

    df %>%
      select(.group_value, case_sequence, analysis_date, group_cases, all_of(present)) %>%
      tidyr::pivot_longer(all_of(present), names_to = "Metric", values_to = "Value") %>%
      filter(!is.na(Value)) %>%
      mutate(Metric = factor(Metric, levels = present))
  })

  output$surg_trend_note <- renderUI({
    window <- surg_trend_window()
    if (is.na(window)) {
      return(tags$div(
        class = "notice-box",
        "Fewer than 10 timed cases under the current filters, so there is nothing to trend. Widen the date range or clear a filter."
      ))
    }

    df <- surg_trend_data()
    if (nrow(df) == 0) {
      return(tags$div(class = "notice-box", "No timed cases are available for the selected comparison."))
    }

    short_groups <- df %>%
      distinct(.group_value, group_cases) %>%
      filter(group_cases < window)

    messages <- sprintf(
      "Rolling average of %s cases, ordered by procedure date, over %s timed cases. Long cases are those over %s minutes.",
      window, format(nrow(df), big.mark = ","), surg_long_threshold()
    )

    if (nrow(short_groups) > 0) {
      messages <- c(
        messages,
        sprintf(
          "%s group(s) have fewer than %s cases and so produce no line: %s.",
          nrow(short_groups), window,
          paste(utils::head(as.character(short_groups$.group_value), 5), collapse = ", ")
        )
      )
    }

    tags$div(
      class = if (nrow(short_groups) > 0) "notice-box" else "success-box",
      tags$ul(lapply(messages, tags$li))
    )
  })

  output$surg_trend_boxes <- renderUI({
    df <- surg_trend_data()
    window <- surg_trend_window()
    if (nrow(df) == 0 || is.na(window)) return(NULL)

    overall <- df %>% arrange(analysis_date, FORM_RESPONSE_GROUP_ID)
    first_window <- utils::head(overall, window)
    last_window <- utils::tail(overall, window)

    shift_box <- function(title, first_value, last_value, digits, suffix = "") {
      delta <- last_value - first_value
      metric_box(
        title,
        paste0(round(last_value, digits), suffix),
        sprintf(
          "%s%s%s vs first %s cases (%s%s)",
          if (is.na(delta)) "" else if (delta > 0) "+" else "",
          if (is.na(delta)) "NA" else round(delta, digits),
          suffix, window, round(first_value, digits), suffix
        )
      )
    }

    fluidRow(
      column(3, shift_box("Mean duration", mean(first_window$surgical_duration_mins, na.rm = TRUE), mean(last_window$surgical_duration_mins, na.rm = TRUE), 1, " min")),
      column(3, shift_box("Median duration", stats::median(first_window$surgical_duration_mins, na.rm = TRUE), stats::median(last_window$surgical_duration_mins, na.rm = TRUE), 1, " min")),
      column(3, shift_box("Variability (SD)", stats::sd(first_window$surgical_duration_mins, na.rm = TRUE), stats::sd(last_window$surgical_duration_mins, na.rm = TRUE), 1, " min")),
      column(3, shift_box("Long cases", 100 * mean(first_window$is_long, na.rm = TRUE), 100 * mean(last_window$is_long, na.rm = TRUE), 1, "%"))
    )
  })

  output$surg_trend_plot <- renderPlot({
    plot_df <- surg_trend_long()
    validate(need(nrow(plot_df) > 0, "Select at least one metric, and make sure enough timed cases exist for the chosen window."))

    compare_col <- input$surg_compare %||% "overall"
    single_series <- identical(compare_col, "overall")
    window <- surg_trend_window()

    if (!single_series) {
      top_groups <- plot_df %>%
        distinct(.group_value, group_cases) %>%
        arrange(desc(group_cases)) %>%
        slice_head(n = 8)
      plot_df <- plot_df %>% filter(.group_value %in% top_groups$.group_value)
      validate(need(nrow(plot_df) > 0, "No group has enough cases for the chosen window."))
    }

    x_by_date <- identical(input$surg_trend_xaxis %||% "sequence", "date")
    plot_df$x <- if (x_by_date) plot_df$analysis_date else plot_df$case_sequence

    cohort <- surg_trend_data()
    reference_df <- data.frame(
      Metric = c("Mean duration", "Median duration", "Variability (SD)", "Long cases (%)"),
      Value = c(
        mean(cohort$surgical_duration_mins, na.rm = TRUE),
        stats::median(cohort$surgical_duration_mins, na.rm = TRUE),
        stats::sd(cohort$surgical_duration_mins, na.rm = TRUE),
        100 * mean(cohort$is_long, na.rm = TRUE)
      ),
      stringsAsFactors = FALSE
    ) %>%
      filter(Metric %in% levels(plot_df$Metric), is.finite(Value)) %>%
      mutate(Metric = factor(Metric, levels = levels(plot_df$Metric)))

    p <- ggplot(plot_df, aes(x = x, y = Value))

    if (nrow(reference_df) > 0) {
      p <- p + geom_hline(
        data = reference_df, aes(yintercept = Value),
        linetype = "dashed", colour = "#94a3b8", linewidth = 0.6
      )
    }

    p <- if (single_series) {
      p + geom_line(linewidth = 0.8, colour = "#1d4ed8")
    } else {
      p + geom_line(aes(colour = .group_value), linewidth = 0.7) + labs(colour = NULL)
    }

    p +
      facet_wrap(~Metric, ncol = 2, scales = "free_y") +
      labs(
        title = sprintf("Rolling average of %s cases", window),
        subtitle = "Surgical duration, cases ordered by procedure date",
        x = if (x_by_date) "Procedure date of most recent case in window" else "Case sequence",
        y = NULL,
        caption = paste0(
          "Dashed line = mean across all filtered cases.",
          if (!single_series && !x_by_date) " Each series is numbered from its own first case, so series are not aligned in time — switch to procedure date to compare calendar periods." else ""
        )
      ) +
      theme_minimal(base_size = 12) +
      theme(
        legend.position = if (single_series) "none" else "top",
        panel.spacing = grid::unit(1, "lines"),
        plot.caption = element_text(colour = "#64748b", hjust = 0)
      )
  })

  output$surg_trend_shift_table <- renderDT({
    df <- surg_trend_data()
    window <- surg_trend_window()
    if (nrow(df) == 0 || is.na(window)) {
      return(datatable_or_message(data.frame(), "Not enough timed cases for the chosen window."))
    }

    out <- df %>%
      group_by(.group_value) %>%
      arrange(analysis_date, FORM_RESPONSE_GROUP_ID, .by_group = TRUE) %>%
      filter(dplyr::n() >= window) %>%
      summarise(
        Cases = dplyr::n(),
        `First window: mean` = round(mean(utils::head(surgical_duration_mins, window), na.rm = TRUE), 1),
        `Latest window: mean` = round(mean(utils::tail(surgical_duration_mins, window), na.rm = TRUE), 1),
        `First window: median` = round(stats::median(utils::head(surgical_duration_mins, window), na.rm = TRUE), 1),
        `Latest window: median` = round(stats::median(utils::tail(surgical_duration_mins, window), na.rm = TRUE), 1),
        `First window: long (%)` = round(100 * mean(utils::head(is_long, window), na.rm = TRUE), 1),
        `Latest window: long (%)` = round(100 * mean(utils::tail(is_long, window), na.rm = TRUE), 1),
        .groups = "drop"
      ) %>%
      rename_group_column(friendly_group_label(input$surg_compare %||% "overall"))

    datatable_or_message(out, "Not enough timed cases for the chosen window.")
  })

  surg_trend_display <- reactive({
    df <- surg_trend_data()
    if (nrow(df) == 0) return(data.frame())

    group_label <- friendly_group_label(input$surg_compare %||% "overall")

    out <- data.frame(
      Series = as.character(df$.group_value),
      `Case sequence` = df$case_sequence,
      `Procedure date` = as.character(df$analysis_date),
      FORM_RESPONSE_GROUP_ID = df$FORM_RESPONSE_GROUP_ID,
      Joint = df$Joint,
      Consultant = df$consultant,
      `Duration (mins)` = df$surgical_duration_mins,
      `Long case` = ifelse(is.na(df$is_long), NA, ifelse(df$is_long, "Yes", "No")),
      `Rolling mean` = round(df$`Mean duration`, 1),
      `Rolling median` = round(df$`Median duration`, 1),
      `Rolling SD` = round(df$`Variability (SD)`, 1),
      `Rolling long cases (%)` = round(df$`Long cases (%)`, 1),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )

    names(out)[names(out) == "Series"] <- group_label
    out
  })

  output$surg_trend_table <- renderDT({
    datatable_or_message(
      surg_trend_display(),
      "No timed cases available for a rolling trend under the current filters."
    )
  })

  output$download_surg_trend <- downloadHandler(
    filename = function() paste0("inor_surgical_trend_", Sys.Date(), ".csv"),
    content = function(file) write_export(surg_trend_display(), file)
  )

  output$surg_metrics <- renderUI({
    df <- filtered_surgical()
    fluidRow(
      column(3, metric_box("Timing rows", nrow(df), "Records with duration")),
      column(3, metric_box("Mean duration", ifelse(nrow(df) == 0, "NA", round(mean(df$surgical_duration_mins, na.rm = TRUE), 1)), "Minutes")),
      column(3, metric_box("Median duration", ifelse(nrow(df) == 0, "NA", median(df$surgical_duration_mins, na.rm = TRUE)), "Minutes")),
      column(3, metric_box("Distinct cases", count_unique_ids(df), "FORM_RESPONSE_GROUP_ID"))
    )
  })

  output$surg_hist_plot <- renderPlot({
    df <- filtered_surgical()
    validate(need(nrow(df) > 0, "No surgical timing rows for the current filters."))

    ggplot(df, aes(x = surgical_duration_mins)) +
      geom_histogram(bins = 12) +
      labs(title = "Surgical duration distribution", x = "Minutes", y = "Rows") +
      theme_minimal(base_size = 12)
  })

  surg_compare_summary <- reactive({
    df <- filtered_surgical()
    compare_col <- input$surg_compare %||% "overall"

    if (identical(compare_col, "overall")) {
      return(summarise_duration(df))
    }

    grouped_df <- expand_group_column(df, compare_col)
    if (nrow(grouped_df) == 0) return(data.frame())

    summary_df <- summarise_duration(grouped_df, group_cols = ".group_value")
    rename_group_column(summary_df, friendly_group_label(compare_col))
  })

  output$surg_compare_plot <- renderPlot({
    df <- filtered_surgical()
    validate(need(nrow(df) > 0, "No surgical timing rows for the current filters."))

    compare_col <- input$surg_compare %||% "overall"

    if (identical(compare_col, "overall")) {
      if (length(unique(stats::na.omit(df$Joint))) > 1) {
        ggplot(df, aes(y = Joint, x = surgical_duration_mins)) +
          geom_boxplot() +
          labs(title = "Duration by joint", y = NULL, x = "Minutes") +
          theme_minimal(base_size = 12)
      } else {
        ggplot(df, aes(x = surgical_duration_mins)) +
          geom_histogram(bins = 12) +
          labs(title = "Surgical duration distribution", x = "Minutes", y = "Rows") +
          theme_minimal(base_size = 12)
      }
    } else {
      grouped_df <- expand_group_column(df, compare_col)
      validate(need(nrow(grouped_df) > 0, "No linked timing rows are available for the selected comparison."))

      top_groups <- grouped_df %>%
        group_by(.group_value) %>%
        summarise(total_n = n(), .groups = "drop") %>%
        arrange(desc(total_n)) %>%
        slice_head(n = 12) %>%
        pull(.group_value)

      plot_df <- grouped_df %>%
        filter(.group_value %in% top_groups)

      ggplot(plot_df, aes(y = .group_value, x = surgical_duration_mins)) +
        geom_boxplot() +
        labs(title = paste("Duration by", friendly_group_label(compare_col)), y = NULL, x = "Minutes") +
        theme_minimal(base_size = 12)
    }
  })

  output$surg_summary_table <- renderDT({
    message <- if (identical(input$surg_compare, "overall")) {
      "No surgical timing rows are available for the current filters."
    } else {
      "No linked timing rows are available for the selected comparison."
    }
    datatable_or_message(surg_compare_summary(), message)
  })

  output$surg_raw_table <- renderDT({
    df <- filtered_surgical() %>%
      transmute(
        FORM_RESPONSE_GROUP_ID,
        `MRN Number`,
        `First Name`,
        `Last Name`,
        Joint,
        Laterality,
        `Procedure type`,
        `Procedure date` = as.character(analysis_date),
        Consultant = consultant,
        `Surgeon grade` = surgeon_grade,
        `Implant fixation type` = fixation_type,
        `Knee system` = knee_system,
        `Femoral component` = femoral_component,
        `Acetabular component` = acetabular_component,
        `Knife to skin` = knife_time,
        `Wound closure` = closure_time,
        `Surgical duration (mins)` = surgical_duration_mins,
        Hospital = ACCESS_POINT_NAME,
        source
      )

    datatable_or_message(df)
  })

  output$download_surgical <- downloadHandler(
    filename = function() paste0("inor_surgical_times_", Sys.Date(), ".csv"),
    content = function(file) write_export(filtered_surgical(), file)
  )

  filtered_implants <- reactive({
    df <- implant_joined

    df <- apply_date_filter(df, "analysis_date", input$impl_date_range)
    df <- apply_single_filter(df, "source", input$impl_source)
    df <- apply_single_filter(df, "joint_type", input$impl_joint)
    df <- apply_single_filter(df, "consultant", input$impl_consultant)
    df <- apply_single_filter(df, "fixation_type", input$impl_fixation)
    df <- apply_component_filter(df, "knee_system", input$impl_knee_system)
    df <- apply_component_filter(df, "femoral_component", input$impl_femoral_component)
    df <- apply_component_filter(df, "acetabular_component", input$impl_acetabular_component)
    df <- apply_single_filter(df, "MANUFACTURER_NAME", input$impl_manufacturer)

    search_value <- trimws(input$impl_search %||% "")
    if (!identical(search_value, "")) {
      search_blob <- paste(
        df$DESCRIPTION,
        df$`Brand Name`,
        df$`Brand Model`,
        df$CATALOGUE_NUMBER,
        sep = " | "
      )
      df <- df[grepl(search_value, search_blob, ignore.case = TRUE), , drop = FALSE]
    }

    df
  })

  output$impl_link_note <- renderUI({
    implant_to_outcomes <- c(
      count_overlap_ids(implant_classification, oks),
      count_overlap_ids(implant_classification, ohs),
      count_overlap_ids(implant_classification, eq5d),
      count_overlap_ids(implant_classification, all_periop_complications),
      count_overlap_ids(implant_classification, all_postop_complications),
      count_overlap_ids(implant_classification, surgical_data)
    )

    if (all(implant_to_outcomes == 0)) {
      tags$div(
        class = "notice-box",
        HTML(
          "The dummy component extracts classify implants correctly within their own files, but they do not share <code>FORM_RESPONSE_GROUP_ID</code> values with PROMs, peri-op complications, post-op complications, or surgical timing rows. 
          Implant browsing still works; implant-outcome linkage will populate once you use linked registry extracts."
        )
      )
    } else {
      tags$div(class = "success-box", "Implant cases overlap with outcomes, so linked implant analyses are enabled.")
    }
  })

  output$impl_metrics <- renderUI({
    df <- filtered_implants()
    fluidRow(
      column(3, metric_box("Component rows", nrow(df), "Filtered implant records")),
      column(3, metric_box("Distinct cases", count_unique_ids(df), "Classified case IDs")),
      column(3, metric_box("Manufacturers", length(unique(stats::na.omit(df$MANUFACTURER_NAME))), "Distinct names")),
      column(3, metric_box("Catalogue numbers", length(unique(stats::na.omit(df$CATALOGUE_NUMBER))), "Distinct catalogue codes"))
    )
  })

  output$impl_manufacturer_plot <- renderPlot({
    df <- filtered_implants() %>%
      filter(!is.na(MANUFACTURER_NAME), MANUFACTURER_NAME != "") %>%
      count(MANUFACTURER_NAME, sort = TRUE) %>%
      slice_head(n = 15)

    validate(need(nrow(df) > 0, "No manufacturer values available for the current filters."))

    ggplot(df, aes(x = reorder(MANUFACTURER_NAME, n), y = n)) +
      geom_col() +
      coord_flip() +
      labs(title = "Top manufacturers", x = NULL, y = "Component rows") +
      theme_minimal(base_size = 12)
  })

  output$impl_fixation_plot <- renderPlot({
    df <- filtered_implants() %>%
      distinct(FORM_RESPONSE_GROUP_ID, joint_type, fixation_type)

    validate(need(nrow(df) > 0, "No classified implant cases are available for the current filters."))

    plot_df <- df %>% count(fixation_type, sort = TRUE)
    ggplot(plot_df, aes(x = fixation_type, y = n)) +
      geom_col() +
      labs(title = "Fixation mix", x = NULL, y = "Cases") +
      theme_minimal(base_size = 12)
  })

  output$impl_classification_table <- renderDT({
    df <- filtered_implants() %>%
      distinct(
        FORM_RESPONSE_GROUP_ID,
        consultant,
        joint_type,
        fixation_type,
        knee_system,
        femoral_component,
        acetabular_component,
        analysis_date
      ) %>%
      mutate(`Procedure date` = as.character(analysis_date)) %>%
      select(-analysis_date) %>%
      arrange(joint_type, fixation_type)

    datatable_or_message(df)
  })

  output$impl_types_table <- renderDT({
    df <- filtered_implants() %>%
      filter(!is.na(DESCRIPTION), DESCRIPTION != "") %>%
      count(MANUFACTURER_NAME, CATALOGUE_NUMBER, DESCRIPTION, sort = TRUE) %>%
      rename(times_used = n)

    datatable_or_message(df)
  })

  output$impl_raw_table <- renderDT({
    df <- filtered_implants() %>%
      mutate(`Procedure date` = as.character(analysis_date)) %>%
      select(
        FORM_RESPONSE_GROUP_ID,
        `Patient Id`,
        `MRN Number`,
        `First Name`,
        `Last Name`,
        consultant,
        joint_type,
        fixation_type,
        knee_system,
        femoral_component,
        acetabular_component,
        `Procedure date`,
        QUESTION_NAME,
        MANUFACTURER_NAME,
        CATALOGUE_NUMBER,
        DESCRIPTION,
        `Anatomical Side`,
        `Brand Name`,
        `Brand Model`,
        ACCESS_POINT_NAME,
        source
      )

    datatable_or_message(df)
  })

  output$download_implants <- downloadHandler(
    filename = function() paste0("inor_implants_", Sys.Date(), ".csv"),
    content = function(file) write_export(filtered_implants(), file)
  )

  lookup_value <- reactive(trimws(input$lookup_value %||% ""))

  lookup_results <- reactive({
    value <- lookup_value()
    field <- lookup_field_map[[input$lookup_mode]]

    if (identical(value, "")) {
      return(list(
        cases = operative_cases_enriched[0, , drop = FALSE],
        knee = prepare_prom_data("knee")[0, , drop = FALSE],
        hip = prepare_prom_data("hip")[0, , drop = FALSE],
        eq5d = prepare_prom_data("eq5d")[0, , drop = FALSE],
        complications = bind_rows(all_periop_complications, all_postop_complications)[0, , drop = FALSE],
        surgical = surgical_enriched[0, , drop = FALSE],
        implants = implant_joined[0, , drop = FALSE],
        classification = implant_classification[0, , drop = FALSE]
      ))
    }

    class_df <- if (identical(input$lookup_mode, "case_id")) {
      implant_classification %>% filter(FORM_RESPONSE_GROUP_ID == value)
    } else {
      implant_case_ids <- filter_exact_field(implant_joined, field, value) %>%
        pull(FORM_RESPONSE_GROUP_ID) %>%
        unique()
      implant_classification %>% filter(FORM_RESPONSE_GROUP_ID %in% implant_case_ids)
    }

    list(
      cases = filter_exact_field(operative_cases_enriched, field, value),
      knee = filter_exact_field(prepare_prom_data("knee"), field, value),
      hip = filter_exact_field(prepare_prom_data("hip"), field, value),
      eq5d = filter_exact_field(prepare_prom_data("eq5d"), field, value),
      complications = filter_exact_field(bind_rows(all_periop_complications, all_postop_complications), field, value),
      surgical = filter_exact_field(surgical_enriched, field, value),
      implants = filter_exact_field(implant_joined, field, value),
      classification = class_df
    )
  })

  output$lookup_note <- renderUI({
    if (identical(lookup_value(), "")) {
      tags$div(class = "notice-box", "Enter a case ID, MRN, or patient ID to search across all datasets.")
    } else {
      tags$div(class = "success-box", paste("Lookup value:", lookup_value()))
    }
  })

  output$lookup_cases <- renderDT({ datatable_or_message(lookup_results()$cases) })
  output$lookup_knee <- renderDT({ datatable_or_message(lookup_results()$knee) })
  output$lookup_hip <- renderDT({ datatable_or_message(lookup_results()$hip) })
  output$lookup_eq5d <- renderDT({ datatable_or_message(lookup_results()$eq5d) })
  output$lookup_complications <- renderDT({ datatable_or_message(lookup_results()$complications) })
  output$lookup_surgical <- renderDT({ datatable_or_message(lookup_results()$surgical) })
  output$lookup_implants <- renderDT({ datatable_or_message(lookup_results()$implants) })
  output$lookup_classification <- renderDT({ datatable_or_message(lookup_results()$classification) })

  output$documentation_inventory <- renderDT({
    datatable_or_message(inventory_table)
  })
}

app <- shinyApp(ui, server)
