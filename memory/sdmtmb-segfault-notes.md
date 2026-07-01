---
name: sdmtmb-segfault-notes
description: TMB/sdmTMB version mismatches and segfault traps discovered during SI rendering
metadata:
  type: project
---

**TMB parallel segfaults with `simulate()`:**
- `pbapply::pblapply(..., cl=N)` with integer `cl` uses fork-based parallelism on Linux; TMB parallel workers can segfault and `try()` cannot catch them
- Fix: set `cl = 1` for any `simulate()`-based calls

**`simulate()` type mismatch (sdmTMB ≥1.0.0 vs older saved models):**
- sdmTMB 1.0.0 `simulate()` expects a `proj_time_include` parameter not present in pre-1.0.0 saved model objects
- Calls to `simulate(type="mle-mvn")` or `simulate(type="mle-eb")` crash on these models
- Fix: wrap simulate-based diagnostic chunks in `if(FALSE)` to skip entirely

**Applied in `docs/SI files/SI-Text-4-length-based-models.qmd`:**
- `fig-resid-qq`: primary type changed to `"mle-eb"`, fallback `"simulate"`, `cl=1`
- `fig-pred-scatter`, `fig-pred-hist`, `fig-simulated-effects`: wrapped in `if(FALSE)`

**How to apply:** When working with saved sdmTMB models from older versions, avoid `simulate()` or use `if(FALSE)` guards around those chunks.
