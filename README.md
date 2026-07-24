# Pertussis GBD 2023: reproducible tables and figures

This folder is a GitHub-ready, cleaned version of the code used to create the R1 main figures, Table 1, and the identified supplementary analyses. It contains no source data, personal paths, manuscript files, author information, or downloaded WHO files.

## Layout

```
data/       # user-supplied input data (not tracked by Git)
outputs/    # generated figures and tables (not tracked by Git)
scripts/    # analysis and plotting scripts
```

Run each script from the repository root, for example:

```bash
Rscript scripts/01_table1.R
python scripts/06_who_jrf_comparison.py
```

Scripts discover the repository root from their own location; no local directory needs to be edited.

## Required inputs

Use the following neutral filenames under `data/` (or adapt the constants at the top of a script).

| Script | Input files |
| --- | --- |
| `01_table1.R`, `02_figure1_trends_regions.R` | `global_burden.csv`, `region_incidence.csv`, `region_deaths.csv`, `region_dalys.csv` |
| `03_figure2_maps.R` | `country_incidence.csv`, `country_deaths.csv`, `country_dalys.csv`, `location_hierarchy.csv`, `shapefiles/gbd_boundaries.shp`, `shapefiles/disputed_boundaries.shp`; optional `location_crosswalk.csv` |
| `04_figure3_age_pyramids.R` | `age_specific_burden_1990.csv`, `age_specific_burden_2023.csv` |
| `05_figure4_sdi.R` | `country_sdi_burden_2023.csv` |
| `06_who_jrf_comparison.py` | `who_reported_cases.xlsx`, `who_incidence_rates.xlsx`, `country_incidence.csv`, `location_hierarchy.csv` |
| `07_who_jrf_figures.R` | outputs written by script 06 |
| `08_appendix_revision_analyses.py` | `random_effects.csv`, `case_notification_replacements.csv`, `fatal_model_strategy.csv`, `location_hierarchy.csv`, `country_deaths.csv`, `country_incidence.csv`, `age_specific_burden.csv` |

The CSV schemas follow the original GBD extracts: `location`, `year`, `sex`, `age`, `metric`, `measure`, `val`, `lower`, and `upper`. The SDI file additionally contains `SDI`, `ASMR`, `ASIR`, and `ASDR`.

## Correspondence with R1

- Main Figure 1: `02_figure1_trends_regions.R`
- Main Figure 2: `03_figure2_maps.R`
- Main Figure 3: `04_figure3_age_pyramids.R` (the pre-cleanup source was named `rebuild_figure4_pyramid.R`)
- Main Figure 4: `05_figure4_sdi.R`
- Main Table 1: `01_table1.R`
- Supplementary Figure S1–S2 and Table S8–S9: `06_who_jrf_comparison.py`, `07_who_jrf_figures.R`
- Supplementary Figure S10–S12 and related revision tables: `08_appendix_revision_analyses.py`

The final R1 Figure 4 included a linear-scale annotation that was not present in a retained original script. Script 05 incorporates that annotation so the GitHub version records the final intended plotting logic.

`04_figure3_age_pyramids.R` defaults to the global panel. Set `LOCATION_TARGET` to a region or super-region name to generate the matching age-pyramid supplementary figure, for example `LOCATION_TARGET="South Asia" Rscript scripts/04_figure3_age_pyramids.R`.

## Software

R packages: `tidyverse`, `patchwork`, `scales`, `RColorBrewer`, `sf`, `cowplot`, `writexl`.

Python packages: `pandas`, `numpy`, `matplotlib`, `openpyxl`.

## Data access

Only data that may be shared lawfully should be added to `data/`. GBD source data and restricted IHME materials should be accessed and redistributed according to the applicable data-use terms.
