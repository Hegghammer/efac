# Data Generation Scripts

This folder contains pipeline/provenance scripts used to build and quality-check the EFAC dataset.

- Article: "Declassified transcripts from Norway's Extended Foreign Affairs Committee 1924-1992"
- Dataset DOI: https://doi.org/10.5281/zenodo.19052379

- Scripts are in `data_generation/scripts/`.
- Run commands from `data_generation/` so script-relative paths resolve correctly.

## Important reproducibility note

The scripts in `data_generation/scripts/` do not provide a full one-click regeneration of the published EFAC dataset.

During dataset construction, substantial manual processing was also performed (for example manual inspection, manual corrections, and manual resolution of ambiguous identities/metadata). The scripts are therefore provided primarily for computational provenance and partial reproducibility, not as a complete automated pipeline from source PDFs to the final Zenodo release.

The authoritative published dataset is the Zenodo release at https://doi.org/10.5281/zenodo.19052379.

## Path conventions

- Core CSV inputs for auxiliary scripts default to `../article/csv`.
- Override with `EFAC_CORE_CSV_DIR` if needed.
- Some scripts expect optional local folders such as `data/`, `out/`, `temp/`, or `validate/` depending on task.

## Example

- `Rscript scripts/18_check_data_consistency.R`
