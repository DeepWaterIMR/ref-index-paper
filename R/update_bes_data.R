# Refresh source survey data from a local BioticExplorerServer DuckDB database.
# The database path must be supplied by the user and is not stored in Git.

source("R/00_public_setup.R")

update_bes_data <- function(
  bes_database_path = Sys.getenv("BES_DATABASE_PATH", "paste your BES database path here"),
  output_dir = project_path("data", "survey data")
) {
  if (!nzchar(bes_database_path) || bes_database_path == "paste your BES database path here") {
    stop("Set BES_DATABASE_PATH to your local BioticExplorerServer DuckDB database.", call. = FALSE)
  }

  require_packages(c("DBI", "duckdb", "dplyr", "readr", "lubridate"))
  ensure_dir(output_dir)

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = bes_database_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  stnall <- dplyr::tbl(con, "stnall")
  metadata <- dplyr::tbl(con, "metadata") |>
    dplyr::collect()

  updated <- as.Date(metadata$timestart[[1]])
  writeLines(as.character(updated), file.path(output_dir, "biotic_database_updated.txt"))

  species_map <- c(
    "snabeluer" = "snabeluer",
    "vanlig uer" = "vanlig uer",
    "uerslekten" = "uerslekten",
    "uerfamilien" = "uerslekten",
    "uerfamilie" = "uerslekten",
    "uerfam" = "uerslekten"
  )

  station_columns <- c(
    "startyear",
    "cruiseseries",
    "cruise",
    "callsignal",
    "platformname",
    "missionid",
    "stationstartdate",
    "stationstopdate",
    "bottomdepthstart",
    "gear",
    "gearname",
    "trawldoorspread",
    "verticaltrawlopening",
    "wirelength",
    "vesselspeed",
    "serialnumber",
    "longitudestart",
    "latitudestart",
    "distance",
    "geartype",
    "gear_width",
    "duration",
    "source"
  )

  stn_filtered <- stnall |>
    dplyr::filter(
      .data$gearcategory == "Bottom trawls",
      !is.na(.data$distance),
      !is.na(.data$latitudestart),
      !is.na(.data$longitudestart)
    ) |>
    dplyr::collect() |>
    dplyr::select(where(~ !all(is.na(.x)))) |>
    dplyr::filter(
      .data$startyear > 1980,
      .data$startyear < lubridate::year(Sys.Date()),
      !is.na(.data$catchweight),
      .data$missiontypename %in% c("Forskningsfartøy", "Leiefartøy"),
      .data$gearcondition %in% 1:2,
      .data$latitudestart > 0,
      !.data$stationtype %in% c("C", "2", "A", "E"),
      !.data$gear %in% c(3136, 3134),
      .data$distance <= 5,
      .data$distance >= 0.3
    ) |>
    dplyr::filter(
      dplyr::if_else(
        .data$cruiseseriescode == "23",
        .data$samplequality %in% 1:2,
        .data$samplequality %in% 1
      )
    ) |>
    dplyr::mutate(
      cruiseseries = dplyr::recode(
        as.character(.data$cruiseseriescode),
        `2` = "BSS",
        `3` = "BSS",
        `2,4` = "BSS",
        `4` = "BSS",
        `5` = "WinterS",
        `6` = "EcoS",
        `7` = "Other",
        `8` = "Other",
        `9` = "Other",
        `10` = "Other",
        `11` = "Other",
        `15` = "ShrimpS",
        `16` = "EggaN",
        `23` = "CoastalS",
        `24` = "Other",
        `24,8` = "Other",
        `25` = "EggaS",
        `26` = "Other",
        `31` = "BSS",
        `NA` = "Other",
        "JuvenileS" = "BSS",
        "SvalbardS" = "BSS",
        "BSshrimpS" = "BSS",
        "TrawlMay" = "EggaMay",
        .default = "Other"
      ),
      commonname = dplyr::recode(.data$commonname, !!!species_map, .default = "other"),
      geartype = dplyr::case_when(
        grepl("^32", .data$gear) ~ "Shrimp",
        grepl("^31", .data$gear) ~ "Cod",
        TRUE ~ "Other"
      ),
      gear_width = dplyr::if_else(.data$geartype == "Cod", 25 * 3.746, 25),
      duration = as.numeric(difftime(.data$stationstopdate, .data$stationstartdate, units = "mins")),
      source = "BioticExplorer"
    )

  station_base <- stn_filtered |>
    dplyr::distinct(dplyr::across(dplyr::all_of(intersect(station_columns, names(stn_filtered)))))

  catches <- stn_filtered |>
    dplyr::filter(.data$commonname %in% names(species_labels)) |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(intersect(station_columns, names(stn_filtered)))),
      .data$commonname,
    ) |>
    dplyr::summarise(
      biomass = sum(.data$catchweight, na.rm = TRUE),
      count = sum(.data$catchcount, na.rm = TRUE),
      .groups = "drop"
    )

  stn <- tidyr::crossing(
    station_base,
    commonname = names(species_labels)
  ) |>
    dplyr::left_join(
      catches,
      by = c(intersect(station_columns, names(station_base)), "commonname")
    ) |>
    dplyr::mutate(
      biomass = tidyr::replace_na(.data$biomass, 0),
      count = tidyr::replace_na(.data$count, 0),
      rho_biomass = .data$biomass / (.data$distance * .data$gear_width / 1852),
      rho_abundance = .data$count / (.data$distance * .data$gear_width / 1852)
    )

  saveRDS(stn, file.path(output_dir, "stn_Database.rds"), compress = "xz")

  cruise_summary <- stn |>
    dplyr::group_by(
      .data$startyear,
      .data$cruiseseries,
      .data$cruise,
      .data$callsignal,
      .data$platformname,
      .data$missionid
    ) |>
    dplyr::summarise(
      n = dplyr::n_distinct(.data$serialnumber),
      catchcount = sum(.data$count, na.rm = TRUE),
      catchweight = sum(.data$biomass, na.rm = TRUE),
      .groups = "drop"
    )

  readr::write_csv(
    cruise_summary,
    file.path(output_dir, paste0(updated, " redfish bottom trawl cruises.csv"))
  )

  invisible(stn)
}
