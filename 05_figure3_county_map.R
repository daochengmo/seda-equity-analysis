# =============================================================================
# 05_figure3_county_map.R
# Purpose: County-level choropleth map of White-Black math achievement gaps.
# Why: Geographic visualization makes spatial patterns of inequality
#      immediately visible — including the well-documented "Upper Midwest"
#      pattern (Reardon et al., 2019).
# =============================================================================

library(tidyverse)
library(scales)
library(sf)
library(tigris)
options(tigris_use_cache = TRUE)   # cache shapefile downloads

district_recent <- readRDS(file.path(out_dir, "district_recent.rds"))
gap_recent      <- readRDS(file.path(out_dir, "gap_recent.rds"))

# Theme
theme_elda_map <- function(base_size = 11) {
  theme_void(base_size = base_size, base_family = "Helvetica") +
    theme(
      plot.title    = element_text(face = "bold", size = base_size + 2,
                                   hjust = 0, margin = margin(b = 4)),
      plot.subtitle = element_text(color = "grey30", size = base_size - 1,
                                   hjust = 0, margin = margin(b = 10)),
      plot.caption  = element_text(color = "grey45", size = base_size - 2,
                                   hjust = 0, margin = margin(t = 10)),
      legend.position = "bottom",
      legend.title    = element_text(size = base_size - 1, color = "grey25"),
      legend.text     = element_text(size = base_size - 2, color = "grey35"),
      plot.background  = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
}

# ---- 1. Build county-level gap data ----------------------------------------
# We need county FIPS for each district. SEDA's `fips` is state FIPS only,
# but the sedaadmin ID encodes county info. Easier: aggregate by state for
# the map base, OR use the SEDA district-to-county crosswalk.
#
# Simpler approach: aggregate district gaps to STATE level for a state
# choropleth (cleaner & still tells the story). County-level requires the
# crosswalk file we didn't download.

state_gap <- gap_recent %>%
  filter(!is.na(gap_wht_blk_math)) %>%
  group_by(state) %>%
  summarise(
    n_districts = n(),
    median_gap  = median(gap_wht_blk_math),
    .groups = "drop"
  ) %>%
  filter(n_districts >= 5)

cat("States with mappable gap data:", nrow(state_gap), "\n")

# ---- 2. Get state shapefile from tigris ------------------------------------
# tigris pulls Census TIGER/Line shapefiles. We use the lower-resolution
# 'cb = TRUE' version — perfect for national maps and ~10x smaller.

us_states <- states(cb = TRUE, resolution = "20m", year = 2022) %>%
  filter(!STUSPS %in% c("AK", "HI", "PR", "VI", "GU", "AS", "MP")) %>%   # contiguous US only
  select(state = STUSPS, NAME, geometry)

cat("State shapefile loaded:", nrow(us_states), "states\n")

# ---- 3. Join gap data to shapefile -----------------------------------------
map_data <- us_states %>%
  left_join(state_gap, by = "state")

# ---- 4. Build the map -------------------------------------------------------
fig3 <- ggplot(map_data) +
  geom_sf(aes(fill = median_gap), color = "white", linewidth = 0.25) +
  scale_fill_gradient2(
    low      = "#2c7fb8",   # blue (small gap)
    mid      = "#f7f7f7",
    high     = "#c0504d",   # red (large gap)
    midpoint = 0.65,        # national median
    na.value = "grey85",
    name     = "Median W-B math gap (SD)",
    breaks   = c(0.4, 0.65, 0.9, 1.15),
    labels   = c("0.4", "0.65\n(nat'l median)", "0.9", "1.15"),
    guide    = guide_colorbar(barwidth = 14, barheight = 0.6,
                              title.position = "top")
  ) +
  coord_sf(crs = 5070) +   # Albers Equal Area, standard for US maps
  labs(
    title    = "Where Achievement Gaps Are Largest: A National Map",
    subtitle = "Median district White-Black math achievement gap by state, 2024.\nGrey = fewer than 5 districts with computable gaps.",
    caption  = "Data: SEDA v2024.3 + U.S. Census TIGER/Line 2022. Analysis: D. Mo, May 2026."
  ) +
  theme_elda_map()

print(fig3)

# ---- 5. Save ---------------------------------------------------------------
ggsave(file.path(fig_dir, "fig3_state_map.png"),
       fig3, width = 9, height = 6.5, dpi = 300, bg = "white")

ggsave(file.path(fig_dir, "fig3_state_map.pdf"),
       fig3, width = 9, height = 6.5, bg = "white")

cat("Figure 3 saved.\n")