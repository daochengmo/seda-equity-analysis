# SEDA Equity Analysis

Descriptive analysis of post-pandemic achievement gaps in U.S. school districts, using the Stanford Education Data Archive (SEDA) v2024.3.

## Overview

This pipeline characterizes the geographic and demographic structure of academic achievement gaps across approximately 11,000 U.S. school districts in 2024. It produces five publication-quality figures and a 12-page technical report.

## Key findings

- The median district White-Black math gap is 0.65 SD (~two grade levels of learning).
- Gaps are concentrated in the Upper Midwest; Wisconsin shows the largest within-state district gaps in the country (median 1.15 SD).
- District poverty and racial gap size are *negatively* correlated (r = -0.24), consistent with the segregation-driven account of inequality (Reardon & Owens, 2014).
- 62-65% of districts saw racial achievement gaps widen between 2019 and 2024.

## Pipeline

| Script | Purpose |
|---|---|
| `01_setup_and_load.R` | Install packages, load SEDA achievement and covariate files |
| `02_clean_and_merge.R` | Build district-year panel; compute within-district gaps |
| `03_figure1_gap_distributions.R` | National distribution of district-level gaps |
| `04_figure2_state_rankings.R` | State-level dot-and-whisker ranking |
| `05_figure3_county_map.R` | National choropleth of state-level gaps |
| `06_figure4_ses_vs_gap.R` | District poverty vs. racial gap |
| `07_figure5_pre_post_pandemic.R` | Within-district 2019-2024 gap change |
| `report.Rmd` | Final write-up |

## Data

SEDA v2024.3 is publicly available from the Educational Opportunity Project at Stanford University (https://edopportunity.org/get-the-data/) and is not redistributed in this repository.

## Software

R 4.6 with `tidyverse`, `sf`, `tigris`, and `patchwork`. Analysis run on macOS, May 2026.

## Author

Daocheng Mo · M.S. Learning Analytics, Teachers College, Columbia University
