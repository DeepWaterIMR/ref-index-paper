# Redfish Survey Index Showcase

Code repository for Vihtakari et al. (2026): Demersal distribution and fisheries‑independent trends of beaked and golden redfish in the Barents and Norwegian Seas. ICES Journal of Marine Science. https://doi.org/10.1093/icesjms/fsag009

The goal is not to reconstruct the published paper results exactly. The paper used internal and partly restricted data. This public workflow demonstrates the model structure on an updated, curated dataset with Russian data excluded by default.

## Data

The public analysis starts from sanitized station-level files in `data/public/`. A matching public dataset is intended to be available from NMDC: <https://doi.org/10.21335/NMDC-999805712>.

When `data/survey data/Compiled_redfish_survey_data.rda` is present, the public-prep step uses that compiled survey source directly before applying the spatial public filter.

Russian data are excluded by default using:

- a study-area polygon stored at `data/gis/study area.gpkg`,
- a Russian EEZ polygon stored at `data/gis/russian_eez.shp`.

IMR users who are allowed to work with the restricted data can rerun the pipeline with `INCLUDE_RUSSIAN_DATA=true`. This switch is intentionally opt-in.

## Run

Install dependencies from `renv.lock` when available, then run:

```r
source("run_analysis.R")
```

The default model run is a quick, rerunnable showcase. To attempt the larger model fit, set `SMOKE_TEST=false`.

For IMR-internal data refreshes, set the path to a local BioticExplorerServer DuckDB database before running:

```sh
export UPDATE_BES=true
export BES_DATABASE_PATH="paste your BES database path here"
```

The local DuckDB path is deliberately not stored in the repository.

## Outputs

The workflow writes:

- sanitized station data to `data/public/`,
- sanitized length data unless `INCLUDE_LENGTH_DATA=false`,
- a cruise summary with an automatically updated `included` column,
- a filtered station map to `figures/`,
- duplicate and exclusion review tables,
- showcase model outputs under `output/`.

`review/` and `output/` are local QA products and are ignored by Git unless explicitly reviewed for public release.

Legacy manuscript and SI helpers live under `R/legacy/`. They support the local `docs/` workflows but are not part of the minimal public showcase path.
