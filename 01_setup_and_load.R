# =============================================================================
# 01_setup_and_load.R
# Project: Mapping Educational Opportunity in U.S. School Districts
# Author: Daocheng (Jacky) Mo
# Date: May 2026
# Purpose: Install packages, load SEDA 2024.3 data, initial inspection
# =============================================================================

# ---- 1. Install required packages (only first time) -------------------------
# install.packages() downloads packages from CRAN. The `if(!require())` pattern
# checks if a package is already installed and only installs if needed.

required_packages <- c(
  "tidyverse",   # data wrangling + ggplot2 (the standard for modern R analysis)
  "readxl",      # read Excel codebooks
  "scales",      # nicer axis formatting on plots
  "knitr",       # for tables in R Markdown
  "sf",          # spatial data for the choropleth map
  "tigris",      # download US state shapefiles
  "patchwork"    # combine multiple ggplots into one figure
)

install_if_missing <- function(pkgs) {
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) {
      install.packages(p)
    }
  }
}
install_if_missing(required_packages)

# ---- 2. Load libraries ------------------------------------------------------
library(tidyverse)
library(readxl)
library(scales)

# ---- 3. Define paths --------------------------------------------------------
# Using here-style relative paths from the project root.
data_dir   <- "data"
fig_dir    <- "figures"
out_dir    <- "output"

# ---- 4. Load the achievement data -------------------------------------------
# The 'annualsub' file has one row per (district × year × subject), with
# achievement estimates for All students plus subgroups by race/gender/ECD.
# We use the Cohort Scale (CS) metric, which is the standard SEDA scale for
# cross-district comparison.

ach_raw <- read_csv(file.path(data_dir, "seda_admindist_annualsub_cs_2024.3.csv"))

# ---- 5. Load the covariates -------------------------------------------------
# District-year covariates: % FRL (free/reduced lunch), racial composition,
# urbanicity, segregation indices, etc.

cov_raw <- read_csv(file.path(data_dir, "seda_cov_admindist_annual_2024.3.csv"))

# ---- 6. Inspect ------------------------------------------------------------
# Always look at the data before analyzing it.

cat("=== Achievement file ===\n")
cat("Rows:", nrow(ach_raw), " Cols:", ncol(ach_raw), "\n")
glimpse(ach_raw)

cat("\n=== Covariates file ===\n")
cat("Rows:", nrow(cov_raw), " Cols:", ncol(cov_raw), "\n")
glimpse(cov_raw)