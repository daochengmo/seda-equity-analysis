# =============================================================================
# 02_clean_and_merge.R
# Purpose: Build analysis-ready datasets from raw SEDA files.
# Outputs:
#   - district_recent: one row per district, most recent year (2024 if available)
#                      with achievement by subgroup + covariates
#   - gap_recent: one row per district with white-Black and white-Hispanic gaps
# =============================================================================

# Assumes 01_setup_and_load.R has been run, so ach_raw and cov_raw are in memory.
# If not, source that script first:
# source("01_setup_and_load.R")

library(tidyverse)

# ---- 1. Inspect what subgroups and years are available ----------------------
ach_raw |> count(subcat, subgroup)   # see all subgroup categories
ach_raw |> count(year)               # see year range

# ---- 2. Reshape: pivot achievement to one row per district-year-subgroup ----
# The raw file has both math (cs_mn_avg_mth_eb) and RLA (cs_mn_avg_rla_eb)
# achievement on the same row. We'll keep both side by side but rename cleanly.

ach_clean <- ach_raw |>
  select(
    district_id   = sedaadmin,
    district_name = sedaadminname,
    state         = stateabb,
    fips,
    year,
    subcat,
    subgroup,
    math_score    = cs_mn_avg_mth_eb,
    rla_score     = cs_mn_avg_rla_eb,
    n_math        = tot_asmts_mth,
    n_rla         = tot_asmts_rla
  )

cat("Cleaned achievement dim:", dim(ach_clean), "\n")

# ---- 3. Pick the most recent year available for each district ---------------
# SEDA 2024.3 spans 2009-2024 (with a gap during COVID). For descriptive
# snapshots, we want the most recent reliable year per district.

latest_year_per_district <- ach_clean |>
  filter(!is.na(math_score) | !is.na(rla_score)) |>
  group_by(district_id) |>
  summarise(latest_year = max(year, na.rm = TRUE), .groups = "drop")

cat("Districts with at least one valid score:", nrow(latest_year_per_district), "\n")
cat("Most common 'latest year':\n")
print(latest_year_per_district |> count(latest_year) |> arrange(desc(latest_year)))

# ---- 4. Build snapshot: 2024 (or latest available) by subgroup --------------
# Filter to the most recent year per district where 'all students' has a score
# This gives us a clean cross-section for the descriptive analysis.

target_year <- 2024  # most recent year in SEDA 2024.3

snapshot_all <- ach_clean |>
  filter(year == target_year, subgroup == "all") |>
  select(district_id, district_name, state, fips,
         math_all = math_score, rla_all = rla_score,
         n_math_all = n_math, n_rla_all = n_rla)

cat("Districts with 'all students' data in", target_year, ":", nrow(snapshot_all), "\n")

# Subgroup scores in target_year (one column per subgroup x subject)
snapshot_subgroups <- ach_clean |>
  filter(year == target_year, subgroup %in% c("wht", "blk", "hsp", "asn", "ecd", "nec")) |>
  select(district_id, subgroup, math_score, rla_score) |>
  pivot_wider(
    names_from  = subgroup,
    values_from = c(math_score, rla_score),
    names_glue  = "{.value}_{subgroup}"
  )

cat("Districts with any subgroup data in", target_year, ":", nrow(snapshot_subgroups), "\n")

# ---- 5. Build covariates snapshot for target_year ---------------------------
# Add a single 'urbanicity' factor from the four 0/1 dummies for easier plotting.

cov_snapshot <- cov_raw |>
  filter(year == target_year) |>
  mutate(
    urbanicity = case_when(
      urban  == 1 ~ "Urban",
      suburb == 1 ~ "Suburban",
      town   == 1 ~ "Town",
      rural  == 1 ~ "Rural",
      TRUE        ~ NA_character_
    ),
    urbanicity = factor(urbanicity, levels = c("Urban", "Suburban", "Town", "Rural"))
  ) |>
  select(district_id = sedaadmin, fips, year,
         total_enrollment = totenrl,
         pct_frl  = perfrl,
         pct_blk  = perblk, pct_hsp = perhsp, pct_wht = perwht, pct_asn = perasn,
         ses_index = sesall,
         pct_poverty = povertyall,
         pct_baplus  = baplusall,
         urbanicity)

cat("Covariates rows for", target_year, ":", nrow(cov_snapshot), "\n")

# ---- 6. Merge everything ----------------------------------------------------
district_recent <- snapshot_all |>
  left_join(snapshot_subgroups, by = "district_id") |>
  left_join(cov_snapshot, by = c("district_id", "fips"))

cat("Final merged dataset:", nrow(district_recent), "districts x",
    ncol(district_recent), "vars\n")

# ---- 7. Compute achievement gaps (white - Black, white - Hispanic) ----------
# Positive value = white students score higher. We compute math gap (most
# commonly studied) for each district where both groups have data.

gap_recent <- district_recent |>
  mutate(
    gap_wht_blk_math = math_score_wht - math_score_blk,
    gap_wht_hsp_math = math_score_wht - math_score_hsp,
    gap_wht_blk_rla  = rla_score_wht  - rla_score_blk,
    gap_wht_hsp_rla  = rla_score_wht  - rla_score_hsp,
    gap_nec_ecd_math = math_score_nec - math_score_ecd,  # non-ECD vs ECD
    gap_nec_ecd_rla  = rla_score_nec  - rla_score_ecd
  )

# Quick sanity check: how many districts have a computable W-B gap?
cat("Districts with computable W-B math gap:",
    sum(!is.na(gap_recent$gap_wht_blk_math)), "\n")
cat("Districts with computable W-H math gap:",
    sum(!is.na(gap_recent$gap_wht_hsp_math)), "\n")
cat("Districts with computable nonECD-ECD math gap:",
    sum(!is.na(gap_recent$gap_nec_ecd_math)), "\n")

# ---- 8. Save cleaned datasets for later use ---------------------------------
saveRDS(district_recent, file.path(out_dir, "district_recent.rds"))
saveRDS(gap_recent,      file.path(out_dir, "gap_recent.rds"))

cat("\n✓ Cleaning complete. Saved district_recent.rds and gap_recent.rds.\n")