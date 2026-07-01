---
name: segmental-save-asymmetry
description: Known asymmetry between segmental_save=TRUE write path and recompile_models=FALSE load path in SI-Text-3
metadata:
  type: project
---

In `docs/SI files/SI-Text-3-model-selection.qmd`, the model fitting uses `segmental_save = TRUE` which saves each CV model as an individual RDS file. However, the `saveRDS(component_ranking_mesh, ...)` call is inside the `else { # segmental_save=FALSE }` block, so **the consolidated mesh files are never written during a segmental-save run**.

The `recompile_models = FALSE` fast-rerender path reads these consolidated mesh files, so they must be created externally if the production run used `segmental_save = TRUE`.

**Fix (Jul 2026):** Reconstruct each missing mesh file with R:
```r
model_family_name <- "delta_lognormal"
species <- c("snabeluer", "vanlig uer")
prefix <- "Component-ranking"  # or "Model-selection" or "Spatiotemporal-selection"
base_dir <- "output/si-text-3/model_selection/"

mesh <- lapply(species, function(k) {
  f <- dir(base_dir,
    pattern = paste0(prefix, "-models-", gsub("\\(.*\\)", "", model_family_name), "_", k),
    full.names = TRUE)[1]
  readRDS(f)$mesh
})
saveRDS(mesh,
  file = paste0(base_dir, prefix, "-meshes-", gsub("\\(.*\\)", "", model_family_name), ".rds"),
  compress = "xz")
```

The three files required for `recompile_models=FALSE`:
- `output/si-text-3/model_selection/Component-ranking-meshes-delta_lognormal.rds`
- `output/si-text-3/model_selection/Model-selection-meshes-delta_lognormal.rds`
- `output/si-text-3/model_selection/Spatiotemporal-selection-meshes-delta_lognormal.rds`

**How to apply:** If SI-Text-3 is ever rerun from scratch with `segmental_save=TRUE`, regenerate these three files before attempting a fast rerender.
