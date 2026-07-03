get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = FALSE)))
  }
  getwd()
}

app_dir <- get_script_dir()
if (!(file.exists(file.path(app_dir, "app.R")) &&
      file.exists(file.path(app_dir, "INOR_query_tool.R")) &&
      dir.exists(file.path(app_dir, "RawData")))) {
  app_dir <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

options(inor_app_base_dir = app_dir)
shiny::runApp(appDir = app_dir, launch.browser = TRUE)
