# EFAC Article Replication Code

This folder contains the dataset article materials:

- Article: "Declassified transcripts from Norway's Extended Foreign Affairs Committee 1924-1992"
- Dataset DOI: https://doi.org/10.5281/zenodo.19052379

- `manuscript.qmd`
- `refs.bib`
- `figs/`
- `scripts/`
- `csv/` (replication-facing data snapshot used by the manuscript)

Package citations can be regenerated with `scripts/build_package_citations.R`, which uses `renv::dependencies()` across `article/` and `data_generation/scripts/` for consistency.

Run article scripts and rendering commands from `article/` so relative paths resolve correctly.
