# =============================================================================
# 06_figure4_ses_vs_gap.R
# Purpose: Relationship between district SES (poverty) and W-B math gap.
# Why: One of the most-cited findings in Reardon's work is that achievement
#      gaps grow with district SES segregation. This figure replicates the
#      basic pattern at district level using % FRL as a proxy for SES.
# =============================================================================

library(tidyverse)
library(scales)

gap_recent <- readRDS(file.path(out_dir, "gap_recent.rds"))

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
      legend.position    = "bottom",
      legend.title       = element_text(size = base_size - 1, color = "grey25"),
      legend.text        = element_text(size = base_size - 2, color = "grey35"),
      plot.background    = element_rect(fill = "white", color = NA),
      panel.background   = element_rect(fill = "white", color = NA)
    )
}

elda_blue <- "#1f4e79"
elda_red  <- "#c0504d"
elda_gold <- "#e8a33d"

# ---- 1. Prepare data --------------------------------------------------------
plot_df <- gap_recent %>%
  filter(!is.na(gap_wht_blk_math),
         !is.na(pct_frl),
         !is.na(urbanicity)) %>%
  mutate(pct_frl_pct = pct_frl * 100)   # express as %

cat("Districts plotted:", nrow(plot_df), "\n")

# Compute correlation for annotation
cor_frl_gap <- cor(plot_df$pct_frl, plot_df$gap_wht_blk_math, use = "complete.obs")
cat("Correlation (FRL%, W-B gap):", round(cor_frl_gap, 3), "\n")

# ---- 2. The figure ----------------------------------------------------------
fig4 <- ggplot(plot_df, aes(x = pct_frl_pct, y = gap_wht_blk_math)) +
  geom_hline(yintercept = 0, color = "grey60", linewidth = 0.4) +
  geom_point(aes(color = urbanicity, size = total_enrollment),
             alpha = 0.45, stroke = 0) +
  geom_smooth(method = "loess", se = TRUE, color = elda_red,
              fill = elda_red, alpha = 0.15, linewidth = 0.8) +
  scale_color_manual(
    values = c("Urban"    = "#c0504d",
               "Suburban" = "#1f4e79",
               "Town"     = "#e8a33d",
               "Rural"    = "#6b8e6b"),
    name = "Urbanicity"
  ) +
  scale_size_continuous(
    range  = c(0.5, 5),
    breaks = c(1000, 10000, 50000),
    labels = comma,
    name   = "District enrollment"
  ) +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  annotate("text", x = 5, y = max(plot_df$gap_wht_blk_math) - 0.1,
           label = sprintf("Correlation: r = %.2f\nn = %s districts",
                           cor_frl_gap, comma(nrow(plot_df))),
           hjust = 0, size = 3.2, color = "grey25", fontface = "italic") +
  labs(
    title    = "The Paradox of District Poverty: Where Are Racial Gaps Largest?",
    subtitle = "District White-Black math gap (2024) by share of students on free/reduced-price lunch (dot size = enrollment).\nDescriptive pattern consistent with segregation-focused accounts of achievement gaps.",
    x        = "Share of students eligible for free/reduced-price lunch",
    y        = "White-Black math gap (SD units)",
    caption  = "Data: SEDA v2024.3. LOESS smoother with 95% CI. Analysis: D. Mo, May 2026."
  ) +
  guides(color = guide_legend(override.aes = list(size = 3, alpha = 0.8))) +
  theme_elda()

print(fig4)

# ---- 3. Save ----------------------------------------------------------------
ggsave(file.path(fig_dir, "fig4_ses_vs_gap.png"),
       fig4, width = 8, height = 6, dpi = 300, bg = "white")

ggsave(file.path(fig_dir, "fig4_ses_vs_gap.pdf"),
       fig4, width = 8, height = 6, bg = "white")

cat("Figure 4 saved.\n")