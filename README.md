# Pertussis GBD 2023: manuscript-specific analysis code

This repository contains the R and Python code used for manuscript-specific
data processing, secondary analyses, and generation of the main figures,
Extended Data figures, and Table 1 for the R2 submission. It does not contain
source data, restricted IHME materials, map layers, manuscript files, or local
computer paths.

The scripts operate on final GBD estimates and public WHO/UNICEF Joint
Reporting Form (JRF) data. They do not reproduce the full GBD production
pipeline. Code and documentation for the broader GBD 2023 estimation framework
are available from the GBD 2023 code portal.

## Repository layout

```text
scripts/       Analysis and plotting scripts
outputs/       Generated files (created locally and excluded from the archive)
.env.example   Placeholder-only input configuration template
```

## Script-to-manuscript correspondence

| Script | Output or analysis |
| --- | --- |
| `01_table1.R` | Main Table 1 |
| `02_main_figures.R` | Main Figs. 1, 3 and 4 |
| `03_figure2_maps.R` | Main Fig. 2, including the final three-panel grid layout |
| `04_who_jrf_comparison.py` | Country-level GBD–WHO/JRF comparison datasets |
| `05_extended_data_figure1.R` | Extended Data Fig. 1 |
| `06_revision_sensitivity_analyses.py` | Random-effect, mortality-pathway and infant-mortality sensitivity outputs |
| `07_extended_data_figures_2_10.R` | Extended Data Figs. 2–10 |

## Input configuration

No actual filenames are embedded in the public code. Copy `.env.example` to a
private shell configuration, replace each angle-bracket placeholder with the
corresponding local input file, and export the variables before running a
script. Do not commit that private configuration.

Example:

```bash
export GLOBAL_BURDEN_INPUT="<PATH_TO_GLOBAL_BURDEN_CSV>"
export COUNTRY_BOUNDARIES_SHP="<PATH_TO_COUNTRY_BOUNDARY_VECTOR>"
Rscript scripts/02_main_figures.R
Rscript scripts/03_figure2_maps.R
```

The map script expects a country-boundary vector layer with a location identifier field and a separate disputed-boundary vector layer. The map shapefiles used for Fig. 2 were sourced from Global Administrative Areas—GADM maps and data 2019 (version 3.6; https://gadm.org/) and are subject to the IHME Free-of-Charge Non-Commercial User Agreement (https://www.healthdata.org/Data-tools-practices/data-practices/ihme-free-charge-non-commercial-user-agreement). The shapefile layers are not redistributed in this repository.

Core burden files use the columns `location`, `year`, `sex`, `age`, `metric`,
`measure`, `val`, `lower`, and `upper`. The country SDI input additionally uses
`SDI`, `ASMR`, `ASIR`, and `ASDR`. The scripts validate required variables and
stop with a clear error when an input has not been configured.

## Running the analyses

Run scripts from any working directory; outputs are written to `outputs/`.

```bash
Rscript scripts/01_table1.R
Rscript scripts/02_main_figures.R
Rscript scripts/03_figure2_maps.R
python3 scripts/04_who_jrf_comparison.py
Rscript scripts/05_extended_data_figure1.R
python3 scripts/06_revision_sensitivity_analyses.py
Rscript scripts/07_extended_data_figures_2_10.R
```

The comparison and sensitivity scripts write derived files that can be assigned
to the corresponding placeholder variables before the Extended Data scripts are
run.

## Software

- R 4.3.2 with `tidyverse` 2.0.0, `dplyr` 1.1.4, `ggplot2` 3.5.2,
  `readr` 2.1.5, `patchwork` 1.3.0, `scales` 1.4.0,
  `RColorBrewer` 1.1-3, `sf` 1.0-19, `cowplot` 1.1.3 and `writexl` 1.5.0.
- Python 3.9.6 with the packages listed in `requirements.txt`.

## Data access and restrictions

Final GBD 2023 estimates can be obtained through the GBD Results Tool following
free user registration. WHO/JRF pertussis data are publicly available through
the WHO Immunization Data Portal. Some underlying GBD input data are owned by
third-party providers and cannot be redistributed. Only data that may lawfully
be shared should be supplied to these scripts.

Code in this repository is released under the MIT License. See the LICENSE file for details. Map layers and source data are not included in this repository and remain subject to their respective terms of use.
