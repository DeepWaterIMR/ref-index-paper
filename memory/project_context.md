# Project Context

The project is a public reproducibility showcase for a redfish survey-index model inspired by Vihtakari et al. (2026). It is intentionally not a faithful reconstruction of the paper results because the original workflow used internal and restricted data.

The candidate public workflow starts from sanitized data in `data/public/` and is designed to be rerunnable with `source("run_analysis.R")`.

Public filtering is spatial:

- keep only stations inside `data/gis/study area.gpkg`
- exclude stations inside `data/gis/russian_eez.shp` by default
- allow authorized IMR reruns with `INCLUDE_RUSSIAN_DATA=true`

For IMR-internal refreshes from BioticExplorerServer DuckDB, tracked files must use the placeholder `BES_DATABASE_PATH="paste your BES database path here"` rather than a local path.

The public workflow should remain minimal:

- `R/01_prepare_public_data.R` prepares sanitized public data and review artifacts
- `R/02_fit_showcase_model.R` runs the showcase model
- `R/03_public_audit.R` audits the candidate public surface
- `run_analysis.R` runs the end-to-end workflow

Before suggesting publication, rerun `source("run_analysis.R")` and `audit_public_surface()`.

Current expectations:

- `data/public/` is the reproducibility surface
- `figures/` holds shareable figure artifacts generated from the public workflow
- `review/` and `output/` are local QA products and stay ignored unless explicitly reviewed
- duplicate review tables should stay empty
- the public audit should pass with stations inside the study area and outside the Russian EEZ

NMDC upload notes:

- draft NMDC metadata for the public dataset is in `review/nmdc_metadata_draft.md`
- intended upload files are `data/public/redfish_station_data.rds`, `data/public/redfish_length_data.rds`, and `data/public/redfish_cruise_summary.csv`
- do not upload `.DS_Store` from `data/public/`
- the draft uses observed public-data coverage 1981-01-22T22:30:00 to 2025-12-12T08:43:49, latitude 62.00833 to 81.69333, and longitude 0.24167 to 43.41498
- confirm the final NMDC DOI, license, PI list, and exact GCMD controlled keywords before submission
