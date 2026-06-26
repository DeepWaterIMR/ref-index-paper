# Audit the candidate public surface for private paths and restricted data leaks.

source("R/00_public_setup.R")
load_public_packages()

if (!exists("flag_russian_eez", mode = "function")) {
  source("R/01_prepare_public_data.R")
}

audit_public_surface <- function(write_outputs = TRUE) {
  ensure_dir(project_path("review"))
  ensure_dir(project_path("review", "tables"))

  candidate_files <- candidate_public_files()

  patterns <- tibble::tribble(
    ~check, ~pattern,
    "local_user_path", "/Users/|/home/|CloudStorage|OneDrive|ownCloud|Zotero/storage",
    "private_share", "owncloud\\.imr\\.no/index\\.php/s/",
    "internal_database_path", "IMR_biotic_BES_database|bioticexplorer\\.duckdb",
    "internal_api", "tomcat7\\.imr\\.no|localhost:23119",
    "credential_words", "password|passwd|api[_-]?key|secret|token"
  )

  hits <- purrr::map_dfr(candidate_files, function(path) {
    scan_file(path, patterns)
  })

  data_checks <- audit_public_data()

  if (write_outputs) {
    readr::write_csv(hits, project_path("review", "tables", "public_surface_text_audit.csv"))
    readr::write_csv(data_checks, project_path("review", "tables", "public_data_audit.csv"))
  }

  if (nrow(hits) > 0) {
    stop("Public-surface audit found text hits. See review/tables/public_surface_text_audit.csv.", call. = FALSE)
  }

  if (any(!data_checks$passed)) {
    stop("Public-data audit failed. See review/tables/public_data_audit.csv.", call. = FALSE)
  }

  list(text_hits = hits, data_checks = data_checks)
}

candidate_public_files <- function() {
  roots <- c("README.md", "run_analysis.R", "renv.lock", "AGENTS.md", "CLAUDE.md", "memory", "data/public", "figures")
  files <- unlist(lapply(roots, function(root) {
    if (file.exists(root) && !dir.exists(root)) {
      root
    } else if (dir.exists(root)) {
      list.files(root, recursive = TRUE, full.names = TRUE, all.files = FALSE)
    } else {
      character()
    }
  }))

  public_r <- c(
    "R/00_public_setup.R",
    "R/01_prepare_public_data.R",
    "R/02_fit_showcase_model.R",
    "R/03_public_audit.R",
    "R/update_bes_data.R",
    "data/gis/study area.gpkg",
    "data/gis/russian_eez.dbf",
    "data/gis/russian_eez.prj",
    "data/gis/russian_eez.shp",
    "data/gis/russian_eez.shx"
  )

  files <- c(files, public_r[file.exists(public_r)])
  files[file.info(files)$isdir == FALSE]
}

scan_file <- function(path, patterns) {
  ext <- tools::file_ext(path)
  text_ext <- c("", "R", "r", "md", "Rmd", "qmd", "csv", "yml", "yaml", "json", "txt", "lock")
  if (!ext %in% text_ext) {
    return(tibble::tibble())
  }

  if (basename(path) == "03_public_audit.R") {
    return(tibble::tibble())
  }

  lines <- tryCatch(readLines(path, warn = FALSE), error = function(e) character())
  if (length(lines) == 0) {
    return(tibble::tibble())
  }

  purrr::map_dfr(seq_len(nrow(patterns)), function(i) {
    hit <- grep(patterns$pattern[[i]], lines, ignore.case = TRUE)
    if (length(hit) == 0) {
      return(tibble::tibble())
    }
    tibble::tibble(
      file = path,
      line = hit,
      check = patterns$check[[i]],
      text = substr(lines[hit], 1, 220)
    )
  })
}

audit_public_data <- function() {
  station_path <- project_path("data", "public", "redfish_station_data.rds")
  cruise_path <- project_path("data", "public", "redfish_cruise_summary.csv")
  russian_eez_path <- project_path("data", "gis", "russian_eez.shp")
  study_area_path <- project_path("data", "gis", "study area.gpkg")

  checks <- list()

  if (file.exists(station_path)) {
    stn <- readRDS(station_path)
    checks$station_has_rows <- nrow(stn) > 0
    checks$station_has_coordinates <- all(c("longitudestart", "latitudestart") %in% names(stn))
    checks$station_inside_study_area <- {
      if (file.exists(study_area_path)) {
        all(flag_study_area(stn, study_area_path), na.rm = TRUE)
      } else {
        FALSE
      }
    }
    checks$station_outside_russian_eez <- {
      if (file.exists(russian_eez_path)) {
        !any(flag_russian_eez(stn, russian_eez_path), na.rm = TRUE)
      } else {
        FALSE
      }
    }
  } else {
    checks$station_public_data_exists <- FALSE
  }

  if (file.exists(cruise_path)) {
    cruise <- readr::read_csv(cruise_path, show_col_types = FALSE)
    checks$cruise_summary_has_included <- "included" %in% names(cruise)
  } else {
    checks$cruise_summary_exists <- FALSE
  }

  tibble::tibble(check = names(checks), passed = unlist(checks))
}
