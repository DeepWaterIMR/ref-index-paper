# Prepare sanitized public data and review products.

source("R/00_public_setup.R")
load_public_packages()

prepare_public_data <- function(
  update_from_bes = identical(Sys.getenv("UPDATE_BES"), "true"),
  include_russian_data = identical(Sys.getenv("INCLUDE_RUSSIAN_DATA"), "true"),
  include_length_data = identical(Sys.getenv("INCLUDE_LENGTH_DATA", "true"), "true"),
  bes_database_path = Sys.getenv("BES_DATABASE_PATH", "paste your BES database path here"),
  compiled_data_path = Sys.getenv(
    "COMPILED_SURVEY_DATA_PATH",
    project_path("data", "survey data", "Compiled_redfish_survey_data.rda")
  ),
  russian_eez_path = Sys.getenv("RUSSIAN_EEZ_PATH", project_path("data", "gis", "russian_eez.shp")),
  study_area_path = Sys.getenv("STUDY_AREA_PATH", project_path("data", "gis", "study area.gpkg")),
  write_outputs = TRUE
) {
  ensure_dir(project_path("data", "public"))
  ensure_dir(project_path("figures"))
  ensure_dir(project_path("review"))
  ensure_dir(project_path("review", "tables"))

  if (update_from_bes) {
    source("R/update_bes_data.R")
    update_bes_data(bes_database_path = bes_database_path)
  }

  station_path <- project_path("data", "survey data", "stn_Database.rds")
  length_path <- project_path("data", "survey data", "stn_ldist_Database.rds")

  if (!file.exists(station_path) || !file.exists(length_path)) {
    stop(
      "Expected source data were not found. Provide data/survey data/stn_Database.rds ",
      "and data/survey data/stn_ldist_Database.rds, or run with UPDATE_BES=true.",
      call. = FALSE
    )
  }

  compiled_data <- if (!update_from_bes && nzchar(compiled_data_path) && file.exists(compiled_data_path)) {
    read_compiled_survey_data(compiled_data_path)
  } else {
    NULL
  }

  stn_raw <- if (!is.null(compiled_data)) {
    compiled_data$station_data
  } else {
    readRDS(station_path)
  }
  if (!"source" %in% names(stn_raw)) {
    stn_raw$source <- "BioticExplorer"
  }

  len_raw <- if (!include_length_data) {
    tibble::tibble()
  } else if (!is.null(compiled_data) && nrow(compiled_data$length_data) > 0) {
    compiled_data$length_data
  } else if (!update_from_bes && file.exists(length_path)) {
    readRDS(length_path)
  } else {
    tibble::tibble()
  }
  if (nrow(len_raw) > 0 && !"source" %in% names(len_raw)) {
    len_raw$source <- "BioticExplorer"
  }

  stn_flagged <- add_exclusion_flags(
    stn_raw,
    russian_eez_path = russian_eez_path,
    study_area_path = study_area_path,
    include_russian_data = include_russian_data
  )

  cruise_summary <- make_cruise_summary(stn_flagged)

  len_flagged <- if (nrow(len_raw) > 0) {
    len_raw |>
      dplyr::left_join(
        stn_flagged |>
          dplyr::distinct(
            dplyr::across(dplyr::all_of(intersect(station_key_columns, names(stn_flagged)))),
            exclude_russian,
            exclude_study_area,
            exclusion_reason,
            included
          ),
        by = intersect(station_key_columns, names(len_raw))
      ) |>
      dplyr::mutate(
        exclude_russian = dplyr::coalesce(.data$exclude_russian, FALSE),
        exclude_study_area = dplyr::coalesce(.data$exclude_study_area, FALSE),
        exclusion_reason = dplyr::coalesce(.data$exclusion_reason, ""),
        included = dplyr::coalesce(
          .data$included,
          !.data$exclude_study_area & (include_russian_data | !.data$exclude_russian)
        )
      )
  } else {
    tibble::tibble(included = logical())
  }

  stn_public <- stn_flagged |>
    dplyr::filter(.data$included) |>
    dplyr::select(-dplyr::any_of(c(
      "exclude_russian",
      "exclude_study_area",
      "exclusion_reason",
      "in_russian_eez",
      "in_study_area",
      "nation"
    )))

  len_public <- if (nrow(len_flagged) > 0) {
    len_flagged |>
      dplyr::filter(.data$included) |>
      dplyr::select(-dplyr::any_of(c(
        "exclude_russian",
        "exclude_study_area",
        "exclusion_reason",
        "in_russian_eez",
        "in_study_area",
        "included",
        "nation"
      )))
  } else {
    tibble::tibble()
  }

  duplicate_station_report <- duplicate_report(
    stn_public,
    c(intersect(station_key_columns, names(stn_public)), "commonname")
  )

  duplicate_length_report <- duplicate_report(
    len_public,
    c(intersect(station_key_columns, names(len_public)), "commonname", "length_group")
  )

  if (write_outputs) {
    saveRDS(stn_public, project_path("data", "public", "redfish_station_data.rds"), compress = "xz")
    saveRDS(len_public, project_path("data", "public", "redfish_length_data.rds"), compress = "xz")
    readr::write_csv(cruise_summary, project_path("data", "public", "redfish_cruise_summary.csv"))
    readr::write_csv(duplicate_station_report, project_path("review", "tables", "duplicate_station_report.csv"))
    readr::write_csv(duplicate_length_report, project_path("review", "tables", "duplicate_length_report.csv"))
    make_filtered_station_map(stn_public, project_path("figures", "filtered_redfish_station_map.png"))
  }

  list(
    station_data = stn_public,
    length_data = len_public,
    cruise_summary = cruise_summary,
    duplicate_station_report = duplicate_station_report,
    duplicate_length_report = duplicate_length_report
  )
}

read_compiled_survey_data <- function(compiled_data_path) {
  env <- new.env(parent = emptyenv())
  load(compiled_data_path, envir = env)

  if (!exists("stn", envir = env, inherits = FALSE)) {
    stop("Compiled survey data did not contain an object named 'stn'.", call. = FALSE)
  }

  stn <- get("stn", envir = env, inherits = FALSE)
  dt <- if (exists("dt", envir = env, inherits = FALSE)) {
    get("dt", envir = env, inherits = FALSE)
  } else {
    tibble::tibble()
  }

  length_data <- if (nrow(dt) > 0) {
    normalize_compiled_length_data(dt, stn)
  } else {
    tibble::tibble()
  }

  list(station_data = stn, length_data = length_data)
}

normalize_compiled_length_data <- function(length_data, station_data) {
  station_totals <- station_data |>
    dplyr::distinct(
      dplyr::across(dplyr::all_of(intersect(
        c("startyear", "cruise", "platformname", "callsignal", "serialnumber", "commonname"),
        names(station_data)
      ))),
      total_n = .data$count,
      total_weight = .data$biomass,
      source = .data$source
    )

  length_data |>
    dplyr::left_join(
      station_totals,
      by = intersect(
        c("startyear", "cruise", "platformname", "callsignal", "serialnumber", "commonname"),
        names(length_data)
      )
    ) |>
    dplyr::select(-dplyr::any_of("id"))
}

add_exclusion_flags <- function(data, russian_eez_path, study_area_path, include_russian_data) {
  if (!nzchar(study_area_path) || !file.exists(study_area_path)) {
    stop("Study area geometry was not found at ", study_area_path, call. = FALSE)
  }

  data <- data |>
    dplyr::mutate(
      in_study_area = flag_study_area(data, study_area_path),
      in_russian_eez = flag_russian_eez(data, russian_eez_path),
      exclude_study_area = !.data$in_study_area,
      exclude_russian = .data$in_russian_eez,
      exclusion_reason = dplyr::case_when(
        !.data$in_study_area & .data$in_russian_eez ~ "Outside study area; Russian EEZ",
        !.data$in_study_area ~ "Outside study area",
        .data$in_russian_eez ~ "Russian EEZ",
        TRUE ~ ""
      ),
      included = !.data$exclude_study_area & (include_russian_data | !.data$exclude_russian)
    )

  data
}

flag_points_in_polygon <- function(data, polygon_path) {
  if (!nzchar(polygon_path) || !file.exists(polygon_path)) {
    return(rep(FALSE, nrow(data)))
  }

  if (!all(c("longitudestart", "latitudestart") %in% names(data))) {
    return(rep(FALSE, nrow(data)))
  }

  polygon <- sf::st_read(polygon_path, quiet = TRUE) |>
    sf::st_make_valid() |>
    sf::st_transform(4326)

  coord_key <- paste(data$longitudestart, data$latitudestart, sep = "_")
  unique_coords <- data |>
    dplyr::distinct(.data$longitudestart, .data$latitudestart) |>
    dplyr::mutate(
      coord_key = paste(.data$longitudestart, .data$latitudestart, sep = "_"),
      in_polygon = FALSE
    )

  valid_coords <- stats::complete.cases(unique_coords[, c("longitudestart", "latitudestart")])
  if (!any(valid_coords)) {
    return(rep(FALSE, nrow(data)))
  }

  bbox <- sf::st_bbox(
    c(
      xmin = min(unique_coords$longitudestart[valid_coords], na.rm = TRUE) - 2,
      ymin = min(unique_coords$latitudestart[valid_coords], na.rm = TRUE) - 2,
      xmax = max(unique_coords$longitudestart[valid_coords], na.rm = TRUE) + 2,
      ymax = max(unique_coords$latitudestart[valid_coords], na.rm = TRUE) + 2
    ),
    crs = sf::st_crs(4326)
  )
  polygon <- suppressWarnings(sf::st_crop(polygon, bbox))
  if (nrow(polygon) == 0) {
    return(rep(FALSE, nrow(data)))
  }

  old_s2 <- suppressMessages(sf::sf_use_s2(FALSE))
  on.exit(suppressMessages(sf::sf_use_s2(old_s2)), add = TRUE)

  pts <- sf::st_as_sf(
    unique_coords[valid_coords, ],
    coords = c("longitudestart", "latitudestart"),
    crs = 4326,
    remove = FALSE
  )

  unique_coords$in_polygon[valid_coords] <- lengths(
    suppressMessages(sf::st_intersects(pts, polygon))
  ) > 0
  unique_coords$in_polygon[match(coord_key, unique_coords$coord_key)]
}

flag_russian_eez <- function(data, russian_eez_path) {
  flag_points_in_polygon(data, russian_eez_path)
}

flag_study_area <- function(data, study_area_path) {
  flag_points_in_polygon(data, study_area_path)
}

make_cruise_summary <- function(stn_flagged) {
  group_cols <- intersect(
    c("startyear", "cruiseseries", "cruise", "callsignal", "platformname", "missionid"),
    names(stn_flagged)
  )

  stn_flagged |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::summarise(
      n_stations = dplyr::n_distinct(.data$serialnumber),
      n_records = dplyr::n(),
      included = all(.data$included),
      excluded_records = sum(!.data$included),
      exclusion_reason = dplyr::case_when(
        any(!.data$included & .data$exclude_study_area) & any(!.data$included & .data$exclude_russian) ~ "Outside study area; Russian EEZ",
        any(!.data$included & .data$exclude_study_area) ~ "Outside study area",
        any(!.data$included & .data$exclude_russian) ~ "Russian EEZ",
        TRUE ~ NA_character_
      ),
      .groups = "drop"
    )
}

duplicate_report <- function(data, columns) {
  columns <- intersect(columns, names(data))
  if (length(columns) == 0) {
    return(tibble::tibble())
  }

  data |>
    dplyr::count(dplyr::across(dplyr::all_of(columns)), name = "n_rows") |>
    dplyr::filter(.data$n_rows > 1) |>
    dplyr::arrange(dplyr::desc(.data$n_rows))
}

make_filtered_station_map <- function(stn_public, output_path) {
  if (!all(c("longitudestart", "latitudestart") %in% names(stn_public))) {
    return(invisible(FALSE))
  }

  plot_data <- stn_public |>
    dplyr::filter(!is.na(.data$longitudestart), !is.na(.data$latitudestart)) |>
    dplyr::distinct(
      .data$startyear,
      .data$cruiseseries,
      .data$cruise,
      .data$platformname,
      .data$callsignal,
      .data$serialnumber,
      .data$longitudestart,
      .data$latitudestart
    )

  plot_sf <- sf::st_as_sf(
    plot_data,
    coords = c("longitudestart", "latitudestart"),
    crs = 4326,
    remove = FALSE
  )

  p <- ggOceanMaps::basemap(
    data = plot_sf,
    bathymetry = TRUE,
    legends = FALSE,
    land.col = "grey92",
    land.border.col = "grey45",
    grid.col = "white",
    base_size = 11
  ) +
    ggplot2::geom_sf(
      data = plot_sf,
      ggplot2::aes(color = .data$cruiseseries),
      alpha = 0.7,
      size = 0.6,
      show.legend = TRUE
    ) +
    ggplot2::scale_color_viridis_d(option = "C", end = 0.9, na.value = "grey55") +
    ggplot2::labs(
      color = "Survey series",
      title = "Filtered public redfish survey stations"
    ) +
    ggplot2::guides(color = ggplot2::guide_legend(nrow = 2, byrow = TRUE)) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.title = ggplot2::element_text(size = 10)
    )

  ggplot2::ggsave(output_path, p, width = 8, height = 6, dpi = 180)
  invisible(TRUE)
}
