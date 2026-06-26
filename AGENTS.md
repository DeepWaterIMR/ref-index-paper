# Agent Notes

This repository is intended to become a public, minimal redfish survey-index showcase.

- Do not commit or push unless the user explicitly asks.
- Keep private IMR extraction material out of the public surface.
- The workflow is a rerunnable showcase, not a faithful reconstruction of the published paper results.
- Public filtering is spatial. Keep only stations inside `data/gis/study area.gpkg`, and exclude stations in `data/gis/russian_eez.shp` by default.
- Russian data are excluded by default. The opt-in switch for authorized IMR users is `INCLUDE_RUSSIAN_DATA=true`.
- Local BioticExplorerServer DuckDB paths must never be written into tracked files. Use `BES_DATABASE_PATH="paste your BES database path here"` in examples.
- `data/public/` is the reproducibility surface. `review/` and `output/` are local QA products and stay ignored unless explicitly reviewed.
- Keep the public workflow minimal: prepare sanitized data, fit the showcase model, write review artifacts, and run the audit. Do not restore legacy private notebooks or manuscript-era scripts into the public candidate tree.
- Run `source("run_analysis.R")` and `audit_public_surface()` before suggesting publication.
