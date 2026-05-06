# =============================================================================
# 03_figure1_gap_distributions.R
# Purpose: National distribution of district-level achievement gaps (math).
# =============================================================================

library(tidyverse)
library(scales)

gap_recent <- readRDS(file.path(out_dir, "gap_recent.rds"))

# ---- Custom theme ----------------------------------------------------------
theme_elda <- function(base_size = 11) {
  theme_minimal(base_size = base_size, base_family = "Helvetica") +
    theme(
      plot.title         = element_text(face = "bold", size = base_size + 2),
      plot.subtitle      = element_text(color = "grey30", size = base_size - 1),
      plot.caption       = element_text(color = "grey45", size = base_size - 2, hjust = 0),
      axis.title         = element_text(color = "grey25"),
      axis.text          = element_text(color = "grey35"),
      panel.grid.minor   = element_blank(),
      panel.grid.major   = element_line(color = "grey92"),
      strip.text         = element_text(face = "bold", color = "grey20"),
      strip.background   = element_rect(fill = "grey95", color = NA),
      plot.background    = element_rect(fill = "white", color = NA),
      panel.background   = element_rect(fill = "white", color = NA)
    )
}

elda_blue   <- "#1f4e79"
elda_red    <- "#c0504d"
elda_gold   <- "#e8a33d"

# ---- Reshape to long format ------------------------------------------------
gap_long <- gap_recent %>%
  select(district_id, district_name, state,
         `White - Black`     = gap_wht_blk_math,
         `White - Hispanic`  = gap_wht_hsp_math,
         `Non-ECD - ECD`     = gap_nec_ecd_math) %>%
  pivot_longer(cols = -c(district_id, district_name, state),
               names_to  = "gap_type",
               values_to = "gap_sd") %>%
  filter(!is.na(gap_sd)) %>%
  mutate(gap_type = factor(gap_type,
                           levels = c("White - Black",
                                      "White - Hispanic",
                                      "Non-ECD - ECD")))

# ---- Summary stats ---------------------------------------------------------
gap_summary <- gap_long %>%
  group_by(gap_type) %>%
  summarise(n      = n(),
            median = median(gap_sd, na.rm = TRUE),
            p10    = quantile(gap_sd, 0.10, na.rm = TRUE),
            p90    = quantile(gap_sd, 0.90, na.rm = TRUE),
            .groups = "drop")

print(gap_summary)

# ---- Build the figure ------------------------------------------------------
fig1 <- ggplot(gap_long, aes(x = gap_sd)) +
  geom_vline(xintercept = 0, color = "grey60", linewidth = 0.4) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 50, fill = elda_blue,
                 color = "white", alpha = 0.85, linewidth = 0.2) +
  geom_density(color = elda_red, linewidth = 0.7) +
  geom_vline(data = gap_summary, aes(xintercept = median),
             color = elda_gold, linewidth = 0.6, linetype = "dashed") +
  geom_text(data = gap_summary,
            aes(x = median, y = Inf,
                label = sprintf("Median = %.2f SD", median)),
            vjust = 1.8, hjust = -0.05,
            size = 3, color = elda_gold, fontface = "bold") +
  geom_text(data = gap_summary,
            aes(x = -Inf, y = Inf,
                label = sprintf("n = %s districts", comma(n))),
            vjust = 1.8, hjust = -0.1,
            size = 3, color = "grey30") +
  facet_wrap(~ gap_type, ncol = 1, scales = "free_y") +
  scale_x_continuous(breaks = seq(-2, 3, 0.5)) +
  labs(
    title    = "Achievement Gaps Vary Widely Across U.S. School Districts",
    subtitle = "District-level math achievement gaps, 2024 (SD units). Positive = advantaged group higher.",
    x        = "Achievement gap (SD units)",
    y        = "Density",
    caption  = "Data: Stanford Education Data Archive (SEDA) v2024.3. Analysis: D. Mo, May 2026."
  ) +
  theme_elda()

print(fig1)

# ---- Save ------------------------------------------------------------------
ggsave(file.path(fig_dir, "fig1_gap_distributions.png"),
       fig1, width = 7.5, height = 8, dpi = 300, bg = "white")

ggsave(file.path(fig_dir, "fig1_gap_distributions.pdf"),
       fig1, width = 7.5, height = 8, bg = "white")

cat("Figure 1 saved.\n")
                                  
                                  
                                  
                                  
                                  
                                  