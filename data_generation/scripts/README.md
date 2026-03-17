# Data Generation Scripts

Run these scripts from `data_generation/`.

These scripts document the data-generation workflow but do not, by themselves, fully reproduce the final published dataset without manual intervention.

## Core pipeline (required)

1. `01_download.R`
2. `02_get_metadata.R`
3. `03_ocr.R`
4. `04_get_bold.R`
5. `05_collate_post75.R`
6. `06_collate_pre76.R`
7. `07_add_missing_content.R`
8. `08_clean.R`
9. `09_identify_speakers.R`
10. `10_attendance.R`
11. `11_attendance_early_years.R`
12. `12_later_additions.R` (only when incorporating newly released documents)
13. `13_consolidate.R`

## Auxiliary scripts (optional)

- `14_validate.R`: build human-validation sample files.
- `15_verify_similarity.R`: similarity-based OCR/parsing QA ranking.
- `16_classify_lang.py`: optional language-label enrichment (`nb`/`nn`).
- `17_spellcheck.R`: auxiliary OCR-cleaning helper that generates manual-review candidates.
- `18_check_data_consistency.R`: manuscript-referenced integrity validation script (40 checks).

## Notes

- Several steps include intentional manual review/edit checkpoints.
- Auxiliary scripts `14`, `17`, and `18` default to core CSV inputs at `../article/csv`.
- Override core CSV location with `EFAC_CORE_CSV_DIR` when needed.
