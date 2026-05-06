# =============================================================================
# 04_figure2_state_rankings.R
# Purpose: State-level ranking of district White-Black achievement gaps.
#          Shows median + interquartile range per state.
# Why: Highlights geographic concentration of inequality — a key theme
#      in equity research and a natural lens for state-level policy.
# =============================================================================

library(tidyverse)
library(scales)

gap_recent <- readRDS(file.path(out_dir, "gap_recent.rds"))

# Reuse the theme from figure 1 (re-define here for portability)
theme_elda <- function(base_size = 11) {
  theme_minimal(base_size = base_size, base_family = "Helvetica") +
    theme(
      plot.title       = element_text(face = "bold", size = base_size + 2),
      plot.subtitle    = element_text(color = "grey30", size = base_size - 1),
      plot.caption     = element_text(color = "grey45", size = base_size - 2, hjust = 0),
      axis.title       = element_text(color = "grey25"),
      axis.text        = element_text(color = "grey35"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "grey92"),
      panel.grid.major.y = element_blank(),
      plot.background  = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
}

elda_blue <- "#1f4e79"
elda_red  <- "#c0504d"
elda_gold <- "#e8a33d"

# ---- 1. Per-state summaries of W-B math gap --------------------------------
# Only include states with >= 5 districts having a computable gap
state_wb <- gap_recent %>%
  filter(!is.na(gap_wht_blk_math)) %>%
  group_by(state) %>%
  summarise(
    n_districts = n(),
    median_gap  = median(gap_wht_blk_math),
    p25         = quantile(gap_wht_blk_math, 0.25),
    p75         = quantile(gap_wht_blk_math, 0.75),
    .groups = "drop"
  ) %>%
  filter(n_districts >= 5) %>%
  arrange(median_gap) %>%
  mutate(state = factor(state, levels = state))   # lock the ordering

cat("States included:", nrow(state_wb), "\n")
print(state_wb, n = 50)

# National median for reference line
nat_median <- median(gap_recent$gap_wht_blk_math, na.rm = TRUE)

# ---- 2. The figure ---------------------------------------------------------
fig2 <- ggplot(state_wb, aes(y = state)) +
  # Reference: national median
  geom_vline(xintercept = nat_median, color = elda_gold,
             linewidth = 0.5, linetype = "dashed") +
  # IQR bar
  geom_segment(aes(x = p25, xend = p75, yend = state),
               color = elda_blue, linewidth = 2.5, alpha = 0.5,
               lineend = "round") +
  # Median dot
  geom_point(aes(x = median_gap), color = elda_blue, size = 2.5) +
  # Sample size annotation
  geom_text(aes(x = p75, label = sprintf(" n=%d", n_districts)),
            hjust = 0, size = 2.6, color = "grey45") +
  # Annotate the national median line
  annotate("text", x = nat_median, y = Inf,
           label = sprintf("National median = %.2f SD", nat_median),
           vjust = 1.2, hjust = -0.05, color = elda_gold,
           fontface = "bold", size = 3) +
  scale_x_continuous(breaks = seq(0, 1.5, 0.25),
                     limits = c(min(state_wb$p25) - 0.05,
                                max(state_wb$p75) + 0.30)) +
  labs(
    title    = "Where Are Achievement Gaps Largest? State-Level Ranking",
    subtitle = "Median (dot) and interquartile range (bar) of district White-Black math gaps within each state, 2024.",
    x        = "District White-Black math gap (SD units)",
    y        = NULL,
    caption  = "Includes states with >=5 districts having computable gaps. Data: SEDA v2024.3. Analysis: D. Mo, May 2026."
  ) +
  theme_elda()

print(fig2)

# ---- 3. Save ---------------------------------------------------------------
ggsave(file.path(fig_dir, "fig2_state_rankings.png"),
       fig2, width = 8, height = 9, dpi = 300, bg = "white")

ggsave(file.path(fig_dir, "fig2_state_rankings.pdf"),
       fig2, width = 8, height = 9, bg = "white")

cat("Figure 2 saved.\n")