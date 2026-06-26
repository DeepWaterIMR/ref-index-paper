# Shared setup for the public redfish index showcase.

required_packages <- c(
  "dplyr",
  "readr",
  "tidyr",
  "purrr",
  "stringr",
  "ggplot2",
  "ggOceanMaps",
  "sf",
  "lubridate"
)

optional_packages <- c("DBI", "duckdb", "sdmTMB")

require_packages <- function(packages) {
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop(
      "Missing required R packages: ",
      paste(missing, collapse = ", "),
      ". Restore/install dependencies before running the workflow.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

load_public_packages <- function() {
  require_packages(required_packages)
  invisible(lapply(required_packages, function(pkg) {
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  }))
}

project_path <- function(...) {
  file.path(getwd(), ...)
}

ensure_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  invisible(path)
}

station_key_columns <- c(
  "startyear",
  "cruise",
  "platformname",
  "callsignal",
  "serialnumber"
)

species_labels <- c(
  "snabeluer" = "Beaked redfish",
  "vanlig uer" = "Golden redfish",
  "uerslekten" = "Redfish unidentified"
)

normalize_text <- function(x) {
  stringr::str_to_lower(stringr::str_squish(as.character(x)))
}
