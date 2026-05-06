# =============================================================================
# 07_figure5_pre_post_pandemic.R
# Purpose: Compare district achievement gaps before (2019) and after (2024)
#          the COVID-19 disruption.
# Why: SEDA 2024.3's headline contribution is enabling pre/post comparison.
#      This figure speaks directly to the Education Recovery research agenda.
# =============================================================================

library(tidyverse)
library(scales)

# We need data from BOTH 2019 and 2024 — go back to ach_clean
# (still in memory from script 02; if not, source 02 first)

theme_elda <- function(base_size = 11) {
  theme_minimal(base_size = base_size, base_family = "Helvetica") +
    theme(
      plot.title       = element_text(face = "bold", size = base_size + 2),
      plot.subtitle    = element_text(color = "grey30", size = base_size - 1),
      plot.caption     = element_text(color = "grey45", size = base_size - 2, hjust = 0),
      axis.title       = element_text(color = "grey25"),
      axis.text        = element_text(color = "grey35"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey92"),
      legend.position  = "bottom",
      legend.title     = element_text(size = base_size - 1, color = "grey25"),
      legend.text      = element_text(size = base_size - 2, color = "grey35"),
      plot.background  = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
}

elda_blue <- "#1f4e79"
elda_red  <- "#c0504d"
elda_gold <- "#e8a33d"

# ---- 1. Build a 2019-vs-2024 gap dataset ------------------------------------
build_gap_year <- function(yr) {
  ach_clean %>%
    filter(year == yr,
           subgroup %in% c("wht", "blk", "hsp", "ecd", "nec")) %>%
    select(district_id, state, subgroup, math_score) %>%
    pivot_wider(names_from = subgroup, values_from = math_score) %>%
    transmute(
      district_id,
      state,
      year = yr,
      gap_wb_math  = wht - blk,
      gap_wh_math  = wht - hsp,
      gap_nec_ecd  = nec - ecd
    )
}

gap_2019 <- build_gap_year(2019)
gap_2024 <- build_gap_year(2024)

# Match districts present in BOTH years for a clean within-district comparison
panel <- inner_join(gap_2019, gap_2024,
                    by = c("district_id", "state"),
                    suffix = c("_2019", "_2024"))

cat("Districts present in BOTH 2019 and 2024:", nrow(panel), "\n")

# ---- 2. Reshape for the figure ---------------------------------------------
# We compute the change (2024 - 2019) for each gap type
panel_long <- panel %>%
  transmute(
    district_id, state,
    `White - Black`    = gap_wb_math_2024  - gap_wb_math_2019,
    `White - Hispanic` = gap_wh_math_2024  - gap_wh_math_2019,
    `Non-ECD - ECD`    = gap_nec_ecd_2024  - gap_nec_ecd_2019
  ) %>%
  pivot_longer(-c(district_id, state),
               names_to = "gap_type", values_to = "gap_change") %>%
  filter(!is.na(gap_change)) %>%
  mutate(gap_type = factor(gap_type,
                           levels = c("White - Black",
                                      "White - Hispanic",
                                      "Non-ECD - ECD")))

# Summary stats
change_summary <- panel_long %>%
  group_by(gap_type) %>%
  summarise(
    n          = n(),
    mean_chg   = mean(gap_change, na.rm = TRUE),
    median_chg = median(gap_change, na.rm = TRUE),
    pct_widened = mean(gap_change > 0, na.rm = TRUE),
    .groups = "drop"
  )

print(change_summary)

# ---- 3. The figure ---------------------------------------------------------
fig5 <- ggplot(panel_long, aes(x = gap_change, fill = gap_type)) +
  geom_vline(xintercept = 0, color = "grey50", linewidth = 0.5) +
  geom_histogram(bins = 40, color = "white", linewidth = 0.2, alpha = 0.85) +
  geom_vline(data = change_summary,
             aes(xintercept = median_chg),
             color = elda_gold, linewidth = 0.6, linetype = "dashed") +
  geom_text(data = change_summary,
            aes(x = median_chg, y = Inf,
                label = sprintf("Median change: %+.2f SD\n%.0f%% widened", median_chg, 100 * pct_widened)),
            vjust = 1.6, hjust = -0.05, size = 3,
            color = "grey20", fontface = "bold",
            inherit.aes = FALSE) +
  geom_text(data = change_summary,
            aes(x = -Inf, y = Inf,
                label = sprintf("n = %s districts", comma(n))),
            vjust = 1.6, hjust = -0.1, size = 3,
            color = "grey30", inherit.aes = FALSE) +
  facet_wrap(~ gap_type, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = c(elda_blue, elda_red, elda_gold), guide = "none") +
  scale_x_continuous(breaks = seq(-1, 1, 0.25),
                     labels = function(x) sprintf("%+.2f", x)) +
  labs(
    title    = "Did Achievement Gaps Widen After the Pandemic?",
    subtitle = "Within-district change in math achievement gap, 2019 to 2024 (SD units).\nPositive = gap grew; negative = gap shrank.",
    x        = "Change in gap, 2024 minus 2019 (SD units)",
    y        = "Number of districts",
    caption  = "Includes districts with computable gaps in both 2019 and 2024. Data: SEDA v2024.3. Analysis: D. Mo, May 2026."
  ) +
  theme_elda()

print(fig5)

# ---- 4. Save ---------------------------------------------------------------
ggsave(file.path(fig_dir, "fig5_pre_post_pandemic.png"),
       fig5, width = 7.5, height = 8, dpi = 300, bg = "white")

ggsave(file.path(fig_dir, "fig5_pre_post_pandemic.pdf"),
       fig5, width = 7.5, height = 8, bg = "white")

cat("Figure 5 saved.\n")