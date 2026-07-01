---
name: si-render-workflow
description: How to render SI Quarto files and where outputs go
metadata:
  type: project
---

Render command for SI files (run from repo root):

```bash
screen -S <session-name> -dm bash -c '
  cd "<repo root>"
  /usr/lib/rstudio-server/bin/quarto/bin/quarto render \
    "docs/SI files/<file>.qmd" --output-dir "." >> output/<log>.log 2>&1
  echo "Exit code: $?" >> output/<log>.log
'
```

`--output-dir "."` is relative to the qmd file location, so output lands in `docs/SI files/`. Using a path relative to the repo root instead causes a nested-directory bug (e.g. `docs/SI files/docs/SI files/`).

**Why:** `knitr::opts_knit$set(root.dir = '../../')` sets the R working directory to the repo root for chunk execution, but Quarto places output relative to the qmd file.

SI files rendered so far (Jul 2026):
- `docs/SI files/SI-Text-3-model-selection.html` (4.4 MB)
- `docs/SI files/SI-Text-4-length-based-models.html` (20 MB)
