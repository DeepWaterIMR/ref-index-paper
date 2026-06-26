# Fit a minimal sdmTMB showcase model from the sanitized public data.

source("R/00_public_setup.R")
load_public_packages()

fit_showcase_model <- function(
  species = Sys.getenv("SHOWCASE_SPECIES", "snabeluer"),
  smoke_test = !identical(Sys.getenv("SMOKE_TEST"), "false"),
  max_rows = as.integer(Sys.getenv("MAX_MODEL_ROWS", ifelse(smoke_test, "2000", "0"))),
  write_outputs = TRUE
) {
  if (!requireNamespace("sdmTMB", quietly = TRUE)) {
    stop("Package sdmTMB is required to fit the showcase model.", call. = FALSE)
  }

  ensure_dir(project_path("output"))
  ensure_dir(project_path("output", "tables"))
  ensure_dir(project_path("output", "figures"))
  ensure_dir(project_path("output", "models"))

  data_path <- project_path("data", "public", "redfish_station_data.rds")
  if (!file.exists(data_path)) {
    stop("Run prepare_public_data() before fitting the showcase model.", call. = FALSE)
  }

  model_data <- readRDS(data_path) |>
    dplyr::filter(.data$commonname == species) |>
    dplyr::filter(!is.na(.data$rho_biomass), !is.na(.data$longitudestart), !is.na(.data$latitudestart)) |>
    dplyr::mutate(
      value = pmax(.data$rho_biomass, 0),
      year_factor = factor(.data$startyear)
    )

  if (smoke_test) {
    recent_years <- tail(sort(unique(model_data$startyear)), 5)
    model_data <- model_data |>
      dplyr::filter(.data$startyear %in% recent_years)
  }

  if (!is.na(max_rows) && max_rows > 0 && nrow(model_data) > max_rows) {
    set.seed(20260609)
    model_data <- dplyr::slice_sample(model_data, n = max_rows)
  }

  model_sf <- sf::st_as_sf(
    model_data,
    coords = c("longitudestart", "latitudestart"),
    crs = 4326,
    remove = FALSE
  ) |>
    sf::st_transform(32633)

  coords <- sf::st_coordinates(model_sf)
  model_data$X <- coords[, 1] / 1000
  model_data$Y <- coords[, 2] / 1000

  mesh <- sdmTMB::make_mesh(model_data, xy_cols = c("X", "Y"), cutoff = ifelse(smoke_test, 60, 35))

  fit <- sdmTMB::sdmTMB(
    value ~ 0 + year_factor + s(bottomdepthstart, k = 5),
    data = model_data,
    mesh = mesh,
    family = sdmTMB::delta_lognormal(),
    spatial = "on",
    spatiotemporal = "off",
    silent = TRUE
  )

  index <- model_data |>
    dplyr::group_by(.data$startyear) |>
    dplyr::summarise(
      n_stations = dplyr::n_distinct(.data$serialnumber),
      mean_biomass_density = mean(.data$value, na.rm = TRUE),
      .groups = "drop"
    )

  if (write_outputs) {
    saveRDS(fit, project_path("output", "models", paste0("showcase_", species, "_sdmtmb.rds")), compress = "xz")
    readr::write_csv(index, project_path("output", "tables", paste0("showcase_", species, "_nominal_index.csv")))

    p <- ggplot2::ggplot(index, ggplot2::aes(.data$startyear, .data$mean_biomass_density)) +
      ggplot2::geom_line() +
      ggplot2::geom_point() +
      ggplot2::labs(x = "Year", y = "Mean biomass density", title = species_labels[[species]]) +
      ggplot2::theme_minimal(base_size = 11)

    ggplot2::ggsave(
      project_path("output", "figures", paste0("showcase_", species, "_nominal_index.png")),
      p,
      width = 7,
      height = 4,
      dpi = 180
    )
  }

  list(fit = fit, nominal_index = index, model_data = model_data)
}
