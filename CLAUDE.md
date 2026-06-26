# Claude Notes

Follow the same public-release rules as `AGENTS.md`.

This repo is a public redfish survey-index showcase, not a faithful reproduction of the paper results.

The workflow should stay minimal: prepare sanitized data, fit the showcase model, write review artifacts, and run the audit. Avoid restoring the legacy private notebooks or manuscript-era scripts into the public candidate tree.

Public filtering is spatial:

- keep only stations inside `data/gis/study area.gpkg`
- exclude stations inside `data/gis/russian_eez.shp` by default
- allow authorized IMR reruns with `INCLUDE_RUSSIAN_DATA=true`

Never write a local BES database path into tracked files. Use `BES_DATABASE_PATH="paste your BES database path here"` in examples.

Before suggesting publication, run `source("run_analysis.R")` and `audit_public_surface()`.
