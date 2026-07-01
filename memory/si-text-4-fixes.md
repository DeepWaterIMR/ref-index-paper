---
name: si-text-4-fixes
description: Summary of all changes made to SI-Text-4 to make it render from public data
metadata:
  type: project
---

`docs/SI files/SI-Text-4-length-based-models.qmd` — made renderable from public data (Jul 2026).

**Key changes:**
- Public length data source: `data/public/redfish_length_data.rds` (replaces private DB load)
- Model output path: `data/model_output/length_based/` → `output/si-text-4/` (37 files, all occurrences replaced)
- `cache_dir` redefined after `rm(list=ls())` in packages chunk — `rm()` wiped the value set in setup chunk
- Spatial filtering added to data and prediction-grid chunks using `data/gis/study area.gpkg` and `data/gis/russian_eez.shp`
- `fig-resid-qq`: NA residuals render a blank panel labelled "NA residuals"; `filter(!is.na(x))` added to normal path; `cl=1`
- `fig-pred-scatter`, `fig-pred-hist`, `fig-simulated-effects`: wrapped in `if(FALSE)` — saved models predate sdmTMB 1.0.0
- `region_levels`: "Eastern Barents Sea" excluded when `!include_russian_data`
- GEBCO high-res bathymetry: portable check via `getOption("ggOceanMaps.userpath")`

Rendered HTML: `docs/SI files/SI-Text-4-length-based-models.html` (20 MB)

See [[segmental-save-asymmetry]] and [[sdmtmb-segfault-notes]] for related patterns.
