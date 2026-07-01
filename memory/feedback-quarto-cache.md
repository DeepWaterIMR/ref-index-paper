---
name: feedback-quarto-cache
description: Do not delete .quarto/ cache between runs — it holds knitr chunk cache
metadata:
  type: feedback
---

Do not delete `.quarto/` or knitr cache directories between render attempts.

**Why:** The user explicitly corrected this. Deleting `.quarto/` wipes the knitr intermediate chunk cache, forcing a full model recompilation that takes hours. The cached output files in `output/` can be reused instead.

**How to apply:** When a render crashes partway through, diagnose the failing chunk and fix the load path for the missing cached file. Set `recompile_models=FALSE` and `recompile_data=FALSE` to skip recomputation. Never suggest `rm -rf .quarto/` as a fix.
