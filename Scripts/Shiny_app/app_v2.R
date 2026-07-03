
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

  result %>%
    filter(!is.na(.group_value) & .group_value != "")
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

summarise_scores <- function(df, group_cols = NULL, score_col = "Score") {
  if (is.null(df) || nrow(df) == 0 || !score_col %in% names(df)) {
    return(data.frame())
  }

  df <- df %>% filter(!is.na(.data[[score_col]]))
  if (nrow(df) == 0) return(data.frame())

  if (!is.null(group_cols) && length(group_cols) > 0) {
    df <- df %>% group_by(across(all_of(group_cols)))
  }

  df %>%
    summarise(
      n = n(),
      mean = round(mean(.data[[score_col]], na.rm = TRUE), 1),
      median = median(.data[[score_col]], na.rm = TRUE),
      sd = round(sd(.data[[score_col]], na.rm = TRUE), 1),
      min = min(.data[[score_col]], na.rm = TRUE),
      max = max(.data[[score_col]], na.rm = TRUE),
      .groups = "drop"
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

preop_enriched <- preop %>%
  mutate(
    dob_date = parse_date_flexible(`Date of Birth`),
    proc_date = parse_date_flexible(`Procedure date`),
    ref_date = if_else(!is.na(proc_date), proc_date, Sys.Date()),
    age = if_else(!is.na(dob_date), as.numeric(calc_age(dob_date, ref_date)), NA_real_),
    procedure_year = if_else(!is.na(proc_date), format(proc_date, "%Y"), NA_character_)
  )

case_core <- preop_enriched %>%
  transmute(
    FORM_RESPONSE_GROUP_ID,
    consultant = `Admitting consultant`,
    surgeon_grade = `Surgeon grade`,
    case_joint = Joint,
    case_laterality = Laterality,
    case_procedure_type = `Procedure type`,
    case_proc_date = proc_date,
    case_hospital = ACCESS_POINT_NAME,
    bmi = BMI
  ) %>%
  distinct()

case_cohort <- case_core %>%
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

  if ("Stage" %in% names(df)) {
    df$Stage <- factor(as.character(df$Stage), levels = stage_order, ordered = TRUE)
  }

  df
}

prepare_complication_data <- function(source_choice = "all") {
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
      analysis_date = dplyr::coalesce(case_proc_date, complication_date_parsed, assessment_date_parsed)
    )
}

all_complications <- prepare_complication_data("all")

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
    "Pre-op assessment",
    "Peri-op assessment V1",
    "Peri-op assessment V2",
    "Post-op assessment V1",
    "Post-op assessment V2",
    "Oxford Knee Score",
    "Oxford Hip Score",
    "EQ-5D",
    "Combined complications (long format)",
    "Component log V1 (surgical times)",
    "Component log V2 (surgical times)",
    "Component log V1 (implant components)",
    "Component log V2 (implant components)",
    "Combined implant classification"
  ),
  Rows = c(
    nrow(preop),
    nrow(periop_v1),
    nrow(periop_v2),
    nrow(postop_v1),
    nrow(postop_v2),
    nrow(oks),
    nrow(ohs),
    nrow(eq5d),
    nrow(all_complications),
    nrow(comp_v1_data),
    nrow(comp_v2_data),
    nrow(comp_v1_comp),
    nrow(comp_v2_comp),
    nrow(implant_classification)
  ),
  Unique_FORM_RESPONSE_GROUP_ID = c(
    count_unique_ids(preop),
    count_unique_ids(periop_v1),
    count_unique_ids(periop_v2),
    count_unique_ids(postop_v1),
    count_unique_ids(postop_v2),
    count_unique_ids(oks),
    count_unique_ids(ohs),
    count_unique_ids(eq5d),
    count_unique_ids(all_complications),
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
    "Pre-op ↔ Peri-op V1",
    "Pre-op ↔ Peri-op V2",
    "Pre-op ↔ Oxford Knee Score",
    "Pre-op ↔ Oxford Hip Score",
    "Pre-op ↔ EQ-5D",
    "Pre-op ↔ Post-op V1",
    "Pre-op ↔ Post-op V2",
    "Pre-op ↔ Combined surgical times",
    "Pre-op ↔ Combined implant components",
    "Combined implant classification ↔ Oxford Knee Score",
    "Combined implant classification ↔ Oxford Hip Score",
    "Combined implant classification ↔ EQ-5D",
    "Combined implant classification ↔ Combined complications",
    "Combined implant classification ↔ Combined surgical times"
  ),
  Shared_FORM_RESPONSE_GROUP_ID = c(
    count_overlap_ids(preop, periop_v1),
    count_overlap_ids(preop, periop_v2),
    count_overlap_ids(preop, oks),
    count_overlap_ids(preop, ohs),
    count_overlap_ids(preop, eq5d),
    count_overlap_ids(preop, postop_v1),
    count_overlap_ids(preop, postop_v2),
    count_overlap_ids(preop, surgical_data),
    count_overlap_ids(preop, implant_data),
    count_overlap_ids(implant_classification, oks),
    count_overlap_ids(implant_classification, ohs),
    count_overlap_ids(implant_classification, eq5d),
    count_overlap_ids(implant_classification, all_complications),
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
    knee = count_overlap_ids(preop, oks) > 0,
    hip = count_overlap_ids(preop, ohs) > 0,
    eq5d = count_overlap_ids(preop, eq5d) > 0
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
  if (identical(source_choice, "V1")) return(count_overlap_ids(preop, postop_v1) > 0)
  if (identical(source_choice, "V2")) return(count_overlap_ids(preop, postop_v2) > 0)
  count_overlap_ids(preop, postop_v1) > 0 || count_overlap_ids(preop, postop_v2) > 0
}

complication_has_implant_link <- function(source_choice) {
  if (identical(source_choice, "V1")) return(count_overlap_ids(implant_classification, get_complications_v1()) > 0)
  if (identical(source_choice, "V2")) return(count_overlap_ids(implant_classification, get_complications_v2()) > 0)
  count_overlap_ids(implant_classification, all_complications) > 0
}

surgical_has_preop_link <- count_overlap_ids(preop, surgical_data) > 0
surgical_has_implant_link <- count_overlap_ids(implant_classification, surgical_data) > 0

key_link_counts <- c(
  count_overlap_ids(preop, oks),
  count_overlap_ids(preop, ohs),
  count_overlap_ids(preop, eq5d),
  count_overlap_ids(preop, postop_v1),
  count_overlap_ids(preop, postop_v2),
  count_overlap_ids(preop, surgical_data),
  count_overlap_ids(preop, implant_data)
)

no_shared_links <- all(key_link_counts == 0)

consultant_choices <- make_choice_vector(preop_enriched$`Admitting consultant`)
surgeon_grade_choices <- make_choice_vector(preop_enriched$`Surgeon grade`)
fixation_choices <- make_choice_vector(implant_classification$fixation_type)
knee_system_choices <- split_delimited_choices(implant_classification$knee_system)
femoral_component_choices <- split_delimited_choices(implant_classification$femoral_component)
acetabular_component_choices <- split_delimited_choices(implant_classification$acetabular_component)

cases_date_bounds <- date_bounds(preop_enriched$proc_date)
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
        selectInput("cases_joint", "Joint", choices = make_choice_vector(preop_enriched$Joint)),
        selectInput("cases_consultant", "Consultant", choices = consultant_choices),
        selectInput("cases_hospital", "Hospital", choices = make_choice_vector(preop_enriched$ACCESS_POINT_NAME)),
        selectInput("cases_laterality", "Laterality", choices = make_choice_vector(preop_enriched$Laterality)),
        selectInput("cases_proc", "Procedure type", choices = make_choice_vector(preop_enriched$`Procedure type`)),
        textInput("cases_search", "Search name / MRN / case ID"),
        downloadButton("download_cases", "Download filtered cases")
      ),
      mainPanel(
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
            "Acetabular component" = "acetabular_component"
          )
        ),
        selectizeInput(
          "prom_case_id",
          "Case trajectory lookup (FORM_RESPONSE_GROUP_ID)",
          choices = NULL,
          options = list(placeholder = "Select a case ID")
        ),
        tags$p(
          class = "help-note",
          "Date filtering uses procedure date when the PROM record links back to the case table; otherwise it falls back to the PROM event date."
        ),
        downloadButton("download_proms", "Download filtered PROMs")
      ),
      mainPanel(
        uiOutput("prom_link_note"),
        uiOutput("prom_metrics"),
        fluidRow(
          column(6, plotOutput("prom_stage_plot", height = 300)),
          column(6, plotOutput("prom_compare_plot", height = 300))
        ),
        h4("Stage summary"),
        DTOutput("prom_stage_summary_table"),
        h4("Comparison summary"),
        DTOutput("prom_compare_summary_table"),
        h4("Filtered PROM records"),
        DTOutput("prom_raw_table"),
        h4("Trajectory for selected case"),
        DTOutput("prom_trajectory_table")
      )
    )
  ),

  tabPanel(
    "Complications",
    sidebarLayout(
      sidebarPanel(
        selectInput("comp_source", "Source", choices = c("All" = "all", "Post-op V1" = "V1", "Post-op V2" = "V2")),
        uiOutput("comp_date_ui"),
        selectInput("comp_joint", "Joint", choices = make_choice_vector(all_complications$analysis_joint)),
        selectInput("comp_type", "Complication type", choices = make_choice_vector(all_complications$complication)),
        selectInput("comp_presentation", "Presentation", choices = make_choice_vector(all_complications$presentation)),
        selectInput("comp_side", "Side", choices = make_choice_vector(all_complications$side)),
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
        downloadButton("download_complications", "Download filtered complications")
      ),
      mainPanel(
        uiOutput("comp_link_note"),
        uiOutput("comp_metrics"),
        fluidRow(
          column(6, plotOutput("comp_type_plot", height = 300)),
          column(6, plotOutput("comp_compare_plot", height = 300))
        ),
        h4("Complication summary"),
        DTOutput("comp_summary_table"),
        h4("Filtered complication records"),
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
        downloadButton("download_surgical", "Download filtered surgical times")
      ),
      mainPanel(
        uiOutput("surg_link_note"),
        uiOutput("surg_metrics"),
        fluidRow(
          column(6, plotOutput("surg_hist_plot", height = 300)),
          column(6, plotOutput("surg_compare_plot", height = 300))
        ),
        h4("Duration summary"),
        DTOutput("surg_summary_table"),
        h4("Filtered surgical timing records"),
        DTOutput("surg_raw_table")
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
          tabPanel("Pre-op cases", DTOutput("lookup_cases")),
          tabPanel("Knee PROMs", DTOutput("lookup_knee")),
          tabPanel("Hip PROMs", DTOutput("lookup_hip")),
          tabPanel("EQ-5D", DTOutput("lookup_eq5d")),
          tabPanel("Complications", DTOutput("lookup_complications")),
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
      tags$li("Procedure-date filtering across the case registry, surgical times, and implant tables."),
      tags$li("PROMs and complication date filters that use procedure date when linked, otherwise the native event / assessment date."),
      tags$li("Consultant is now sourced from the peri-op assessment where linked to the case."),
      tags$li("Outcome comparison by consultant, surgeon grade, implant fixation type, knee system, femoral component, and acetabular component."),
      tags$li("Complication outputs as either absolute numbers or rates."),
      tags$li("A BMI histogram in the demographics tab.")
    ),
    tags$h4("Using the dummy files"),
    tags$ul(
      tags$li("The dummy extracts still demonstrate the UI, plots, and summaries."),
      tags$li("Some consultant-linked or implant-linked outcome analyses stay empty because the dummy files do not consistently share FORM_RESPONSE_GROUP_ID values across datasets."),
      tags$li("Once your real extracts are linked, those comparison sections should populate automatically.")
    ),
    tags$h4("Dataset inventory"),
    DTOutput("documentation_inventory")
  )
)

server <- function(input, output, session) {

  output$overview_notice <- renderUI({
    if (no_shared_links) {
      tags$div(
        class = "notice-box",
        HTML(
          "These dummy datasets do not share <code>FORM_RESPONSE_GROUP_ID</code> values across the main files, so the app focuses on within-dataset exploration and suppresses linked analyses when required."
        )
      )
    } else {
      tags$div(
        class = "success-box",
        HTML("Shared <code>FORM_RESPONSE_GROUP_ID</code> values were detected across key datasets, so linked analyses can run.")
      )
    }
  })

  output$overview_metrics <- renderUI({
    joint_counts <- preop %>% count(Joint)
    hip_cases <- joint_counts %>% filter(Joint == "Hip") %>% pull(n)
    knee_cases <- joint_counts %>% filter(Joint == "Knee") %>% pull(n)
    hip_cases <- ifelse(length(hip_cases) == 0, 0, hip_cases)
    knee_cases <- ifelse(length(knee_cases) == 0, 0, knee_cases)

    fluidRow(
      column(3, metric_box("Pre-op cases", format(nrow(preop), big.mark = ","), paste("Hip:", hip_cases, "| Knee:", knee_cases))),
      column(3, metric_box("PROM records", format(nrow(oks) + nrow(ohs) + nrow(eq5d), big.mark = ","), "OKS + OHS + EQ-5D")),
      column(3, metric_box("Complication rows", format(nrow(all_complications), big.mark = ","), "Long-format V1 + V2")),
      column(3, metric_box("Implant components", format(nrow(implant_data), big.mark = ","), paste("Classified cases:", nrow(implant_classification))))
    )
  })

  output$overview_joint_plot <- renderPlot({
    validate(need(nrow(preop) > 0, "No pre-op records available."))
    plot_df <- preop %>% count(Joint)
    ggplot(plot_df, aes(x = Joint, y = n)) +
      geom_col() +
      labs(title = "Pre-op case mix", x = NULL, y = "Cases") +
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
    df <- preop_enriched
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

  output$cases_metrics <- renderUI({
    df <- filtered_cases()
    female_pct <- if (nrow(df) > 0) round(mean(df$Sex == "Female", na.rm = TRUE) * 100, 1) else NA_real_
    comorb_pct <- if (nrow(df) > 0) round(mean(df$`Co-morbidities` == "Yes", na.rm = TRUE) * 100, 1) else NA_real_

    fluidRow(
      column(3, metric_box("Filtered cases", nrow(df), "Pre-op registry rows")),
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
        BMI,
        `Co-morbidities`,
        `Admitting consultant`,
        `Surgeon grade`,
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

  filtered_prom <- reactive({
    req(input$prom_type)
    df <- prepare_prom_data(input$prom_type)

    df <- apply_date_filter(df, "analysis_date", input$prom_date_range)

    stages <- input$prom_stage
    if (!is.null(stages) && length(stages) > 0) {
      df <- df %>% filter(as.character(Stage) %in% stages)
    }

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

    df %>% arrange(Stage, analysis_date, event_date_parsed)
  })

  output$prom_link_note <- renderUI({
    req(input$prom_type)

    messages <- character()
    if (!has_preop_link_for_prom(input$prom_type)) {
      messages <- c(
        messages,
        "This PROM extract does not link back to the pre-op case table in the dummy data, so consultant filters and procedure-date based cohorting may stay empty."
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
    df <- filtered_prom()
    fluidRow(
      column(3, metric_box("Filtered records", nrow(df), "PROM rows after filters")),
      column(3, metric_box("Mean score", ifelse(nrow(df) == 0 || all(is.na(df$Score)), "NA", round(mean(df$Score, na.rm = TRUE), 1)), "Average score")),
      column(3, metric_box("Median score", ifelse(nrow(df) == 0 || all(is.na(df$Score)), "NA", median(df$Score, na.rm = TRUE)), "Middle score")),
      column(3, metric_box("Unique cases", count_unique_ids(df), "Distinct FORM_RESPONSE_GROUP_ID"))
    )
  })

  output$prom_stage_plot <- renderPlot({
    df <- summarise_scores(filtered_prom(), group_cols = c("Stage"))
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
    if (identical(compare_col, "overall")) {
      return(summarise_scores(df))
    }

    grouped_df <- expand_group_column(df, compare_col)
    if (nrow(grouped_df) == 0) return(data.frame())

    group_cols <- ".group_value"
    if (length(unique(stats::na.omit(as.character(grouped_df$Stage)))) > 1) {
      group_cols <- c(group_cols, "Stage")
    }

    summary_df <- summarise_scores(grouped_df, group_cols = group_cols)
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
    datatable_or_message(summarise_scores(filtered_prom(), group_cols = c("Stage")))
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
        "The surgical timing extract does not link back to the pre-op case table in the dummy data, so consultant-linked filtering may stay empty."
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
      count_overlap_ids(implant_classification, all_complications),
      count_overlap_ids(implant_classification, surgical_data)
    )

    if (all(implant_to_outcomes == 0)) {
      tags$div(
        class = "notice-box",
        HTML(
          "The dummy component extracts classify implants correctly within their own files, but they do not share <code>FORM_RESPONSE_GROUP_ID</code> values with PROMs, complications, or surgical timing rows. 
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
        cases = preop_enriched[0, , drop = FALSE],
        knee = prepare_prom_data("knee")[0, , drop = FALSE],
        hip = prepare_prom_data("hip")[0, , drop = FALSE],
        eq5d = prepare_prom_data("eq5d")[0, , drop = FALSE],
        complications = all_complications[0, , drop = FALSE],
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
      cases = filter_exact_field(preop_enriched, field, value),
      knee = filter_exact_field(prepare_prom_data("knee"), field, value),
      hip = filter_exact_field(prepare_prom_data("hip"), field, value),
      eq5d = filter_exact_field(prepare_prom_data("eq5d"), field, value),
      complications = filter_exact_field(all_complications, field, value),
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
