# Bar plots of the studies retained after abstract screening:
#   1. number of studies by publication year
#   2. number of studies by journal (most frequent journals, plus "Other")

library(ggplot2)
library(patchwork)

# creates `screening` and `journal_counts`
source("R/process_screening_results.R")

# studies by year ---------------------------------------------------------

year_plot <- ggplot(screening, aes(x = year)) +
  geom_bar() +
  # label every year so that years with no studies are still shown
  scale_x_continuous(
    breaks = seq(min(screening$year), max(screening$year), by = 1)
  ) +
  scale_y_continuous(
    breaks = scales::breaks_width(2),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = "Publication year",
    y = "Number of studies"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

year_plot

ggsave(
  filename = "outputs/studies_by_year.png",
  plot = year_plot,
  width = 20,
  height = 12,
  units = "cm",
  dpi = 300,
  bg = "white"
)

# studies by journal ------------------------------------------------------

# "Other" is much larger than any single journal, so the y-axis is broken to
# keep the individual journals readable. ggplot2 has no broken axes, so the
# plot is drawn as two panels, below and above the break, stacked together
# with a narrow strip between them that marks the break.

# the y-axis jumps from the first value to the second
y_break <- c(10, 32)

# top of the y-axis, rounded up to the next gridline
y_step <- 2
y_max <- ceiling(max(journal_counts$n) / y_step) * y_step

# every bar must end below or above the break, not inside it
stopifnot(
  all(journal_counts$n <= y_break[1] | journal_counts$n >= y_break[2]),
  y_max > y_break[2]
)

journal_base <- ggplot(journal_counts, aes(x = journal, y = n)) +
  geom_col() +
  scale_x_discrete(labels = scales::label_wrap(14)) +
  scale_y_continuous(
    breaks = scales::breaks_width(y_step),
    expand = expansion(0)
  ) +
  labs(
    x = "Journal",
    y = "Number of studies"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.y = element_line(linewidth = 0.4),
    # keep the y-axis labels clear of the break marks
    axis.text.y.left = element_text(margin = margin(r = 8))
  )

# no margin on the sides facing the break, so the strip between the panels
# sets the size of the gap
journal_lower <- journal_base +
  coord_cartesian(ylim = c(0, y_break[1])) +
  theme(plot.margin = margin(t = 0, r = 5.5, b = 5.5, l = 5.5))

journal_upper <- journal_base +
  coord_cartesian(ylim = c(y_break[2], y_max)) +
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    plot.margin = margin(t = 5.5, r = 5.5, b = 0, l = 5.5)
  )

# break marks: a short diagonal stroke across each cut end of the y-axis.
# The strip's panel lines up with the panels above and below, so its left edge
# is the y-axis and its bottom and top edges are the two sides of the break.
break_marks <- grid::segmentsGrob(
  x0 = unit(0, "npc") - unit(4, "pt"),
  x1 = unit(0, "npc") + unit(4, "pt"),
  y0 = unit(c(0, 1), "npc") - unit(3, "pt"),
  y1 = unit(c(0, 1), "npc") + unit(3, "pt"),
  gp = grid::gpar(lwd = 1.5)
)

# the strip is otherwise empty, but carries the same y-axis title as the panels
# so that patchwork merges the three into a single title
journal_break <- ggplot() +
  annotation_custom(break_marks) +
  coord_cartesian(clip = "off") +
  labs(y = "Number of studies") +
  theme_minimal() +
  theme(plot.margin = margin(t = 0, r = 5.5, b = 0, l = 5.5))

# the two panels' heights are proportional to the range each covers, so one
# study is the same height either side of the break
journal_plot <- journal_upper / journal_break / journal_lower +
  plot_layout(
    heights = unit(
      c(y_max - y_break[2], 14, y_break[1]),
      c("null", "pt", "null")
    ),
    axis_titles = "collect"
  )

journal_plot

ggsave(
  filename = "outputs/studies_by_journal.png",
  plot = journal_plot,
  width = 20,
  height = 12,
  units = "cm",
  dpi = 300,
  bg = "white"
)
