# Export publication-sized, editable vector PDFs for main Figs. 1, 3 and 4.
# Inputs are configured with environment variables documented in README.md.

suppressPackageStartupMessages({
  library(tidyverse)
  library(patchwork)
  library(scales)
  library(RColorBrewer)
  library(cowplot)
  library(sf)
})

source(file.path(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))), "utils.R"))
paths <- repository_paths()
final_dir <- ensure_output_dir(paths$output)

pdf.options(useDingbats = FALSE)
font_family <- "Helvetica"
pt_per_mm <- 72.27 / 25.4

save_vector_pdf <- function(plot, filename, width_mm, height_mm) {
  ggsave(
    filename = file.path(final_dir, filename),
    plot = plot,
    device = grDevices::pdf,
    width = width_mm,
    height = height_mm,
    units = "mm",
    bg = "white",
    colormodel = "srgb",
    limitsize = FALSE
  )
}

panel_theme <- theme(
  plot.tag = element_text(
    family = font_family, face = "bold", size = 7,
    hjust = 0, vjust = 1
  ),
  plot.tag.position = c(0.01, 0.99)
)

# -----------------------------------------------------------------------------
# Figure 1: six panels at 180 mm wide, with one shared regional legend.
# -----------------------------------------------------------------------------
shade_colour <- "#F4B6B6"
line_colour <- "#0B3FB3"
ribbon_colour <- "#A9BCEB"

trend_data <- read_csv(require_input("GLOBAL_BURDEN_INPUT"), show_col_types = FALSE) |>
  filter(sex == "Both", age == "Age-standardized", metric == "Rate")

make_rate_panel <- function(measure_name, panel_title, panel_tag) {
  panel_data <- filter(trend_data, measure == measure_name)
  ggplot(panel_data, aes(year, val)) +
    annotate(
      "rect", xmin = 2019, xmax = 2022, ymin = -Inf, ymax = Inf,
      fill = shade_colour, alpha = 0.25
    ) +
    geom_ribbon(
      aes(ymin = lower, ymax = upper),
      fill = ribbon_colour, alpha = 0.55
    ) +
    geom_line(colour = line_colour, linewidth = 0.45) +
    geom_point(colour = line_colour, size = 0.55) +
    scale_x_continuous(breaks = c(1990, 2000, 2010, 2020)) +
    scale_y_continuous(labels = comma) +
    labs(
      tag = panel_tag, title = panel_title, x = "Year",
      y = "Age-standardized rate\nper 100,000"
    ) +
    theme_minimal(base_family = font_family, base_size = 6) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 7),
      axis.title = element_text(face = "bold", size = 6),
      axis.text = element_text(size = 5),
      panel.grid.minor = element_blank(),
      plot.margin = margin(2, 2, 2, 2)
    ) +
    panel_theme
}

regional_data <- bind_rows(
  read_csv(require_input("REGIONAL_INCIDENCE_INPUT"), show_col_types = FALSE) |>
    mutate(measure = "Incidence"),
  read_csv(require_input("REGIONAL_DEATH_INPUT"), show_col_types = FALSE) |>
    mutate(measure = "Deaths"),
  read_csv(require_input("REGIONAL_DALY_INPUT"), show_col_types = FALSE) |>
    mutate(measure = "DALYs")
) |>
  filter(
    sex == "Both", age == "All ages", metric == "Number",
    between(year, 1990, 2023)
  )

region_order <- regional_data |>
  group_by(location) |>
  summarise(mean_value = mean(val), .groups = "drop") |>
  arrange(desc(mean_value)) |>
  pull(location)

region_colours <- setNames(
  colorRampPalette(brewer.pal(12, "Set3"))(length(region_order)),
  region_order
)

make_number_panel <- function(measure_name, panel_title, y_label, panel_tag) {
  panel_data <- regional_data |>
    filter(measure == measure_name) |>
    mutate(location = factor(location, levels = region_order))
  ggplot(panel_data, aes(year, val, fill = location)) +
    geom_col(width = 0.8) +
    scale_fill_manual(values = region_colours, name = "GBD region") +
    scale_x_continuous(
      breaks = c(1990, 2000, 2010, 2020),
      expand = c(0.01, 0.01)
    ) +
    scale_y_continuous(labels = comma) +
    labs(tag = panel_tag, title = panel_title, x = "Year", y = y_label) +
    guides(
      fill = guide_legend(
        ncol = 1, title.position = "top",
        keyheight = grid::unit(2.5, "mm"),
        keywidth = grid::unit(2.5, "mm")
      )
    ) +
    theme_minimal(base_family = font_family, base_size = 6) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 7),
      axis.title = element_text(face = "bold", size = 6),
      axis.text = element_text(size = 5),
      legend.title = element_text(face = "bold", size = 6),
      legend.text = element_text(size = 5),
      legend.spacing.y = grid::unit(0, "pt"),
      legend.margin = margin(0, 0, 0, 0),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.25),
      plot.margin = margin(2, 2, 2, 2)
    ) +
    panel_theme
}

figure1 <- (
  make_rate_panel("Incidence", "Incidence", "A") /
    make_rate_panel("Deaths", "Deaths", "B") /
    make_rate_panel("DALYs (Disability-Adjusted Life Years)", "DALYs", "C")
) | (
  make_number_panel("Incidence", "Incidence", "Incident cases", "D") /
    make_number_panel("Deaths", "Deaths", "Deaths", "E") /
    make_number_panel("DALYs", "DALYs", "DALYs", "F")
)
figure1 <- figure1 +
  plot_layout(guides = "collect", widths = c(1, 1.38)) &
  theme(legend.position = "right")

save_vector_pdf(figure1, "Figure1_final.pdf", 180, 125)

# -----------------------------------------------------------------------------
# Figure 3: age-specific pyramids at 180 mm wide.
# -----------------------------------------------------------------------------
age_1990 <- read_csv(require_input("AGE_BURDEN_1990_INPUT"), show_col_types = FALSE)
age_2023 <- read_csv(require_input("AGE_BURDEN_2023_INPUT"), show_col_types = FALSE)

age_levels <- c(
  "<28 days", "1-5 months", "6-11 months", "12-23 months", "2-4 years",
  "5-9 years", "10-14 years", "15-19 years", "20-24 years", "25-29 years",
  "30-34 years", "35-39 years", "40-44 years", "45-49 years", "50-54 years",
  "55-59 years", "60-64 years", "65-69 years", "70-74 years", "75-79 years",
  "80+ years"
)

age_data <- bind_rows(age_1990, age_2023) |>
  filter(location == "Global", sex == "Both") |>
  complete(
    measure, metric, sex, year, location, cause, age = age_levels,
    fill = list(val = 0, upper = 0, lower = 0)
  ) |>
  mutate(age = factor(age, levels = age_levels))

make_pyramid_panel <- function(
    measure_name, metric_name, panel_title, panel_tag, show_age = TRUE) {
  panel_data <- age_data |>
    filter(measure == measure_name, metric == metric_name) |>
    transmute(
      age, year,
      value = if_else(year == 1990, -val, val),
      lower = if_else(year == 1990, -upper, lower),
      upper = if_else(year == 1990, -lower, upper),
      year = factor(year)
    )
  axis_limit <- max(abs(c(panel_data$lower, panel_data$upper)), na.rm = TRUE) * 1.1
  x_label <- if (metric_name == "Number") "Number" else "Rate per 100,000"

  result <- ggplot(panel_data, aes(value, age, fill = year)) +
    geom_col(width = 0.7) +
    geom_errorbarh(
      aes(xmin = lower, xmax = upper), height = 0.28,
      linewidth = 0.18, colour = "grey40"
    ) +
    scale_fill_manual(values = c("1990" = "#E8873D", "2023" = "#5B9BD5")) +
    scale_x_continuous(
      limits = c(-axis_limit, axis_limit),
      labels = function(x) comma(abs(x)),
      breaks = pretty_breaks(n = 4)
    ) +
    labs(tag = panel_tag, title = panel_title, x = x_label, y = NULL) +
    theme_minimal(base_family = font_family, base_size = 6) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 7),
      axis.text.y = element_text(size = 5),
      axis.text.x = element_text(size = 5),
      axis.title.x = element_text(size = 6, face = "bold"),
      legend.position = "none",
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      plot.margin = margin(2, 3, 2, 3)
    ) +
    panel_theme

  if (!show_age) {
    result <- result + theme(axis.text.y = element_blank())
  }
  result
}

age_legend <- ggplot() +
  annotate(
    "rect", xmin = 0.02, xmax = 0.035, ymin = 0.22, ymax = 0.78,
    fill = "#E8873D", colour = NA
  ) +
  annotate(
    "text", x = 0.041, y = 0.50, label = "1990", hjust = 0,
    size = 6 / pt_per_mm, family = font_family, fontface = "bold"
  ) +
  annotate(
    "rect", xmin = 0.10, xmax = 0.115, ymin = 0.22, ymax = 0.78,
    fill = "#5B9BD5", colour = NA
  ) +
  annotate(
    "text", x = 0.121, y = 0.50, label = "2023", hjust = 0,
    size = 6 / pt_per_mm, family = font_family, fontface = "bold"
  ) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  theme_void(base_family = font_family) +
  theme(plot.margin = margin(0, 0, 0, 0))

figure3 <- age_legend / ((
  make_pyramid_panel("Incidence", "Number", "Incidence", "A", TRUE) /
    make_pyramid_panel("Deaths", "Number", "Deaths", "B", TRUE) /
    make_pyramid_panel(
      "DALYs (Disability-Adjusted Life Years)", "Number", "DALYs", "C", TRUE
    )
) | (
  make_pyramid_panel("Incidence", "Rate", "Incidence", "D", FALSE) /
    make_pyramid_panel("Deaths", "Rate", "Deaths", "E", FALSE) /
    make_pyramid_panel(
      "DALYs (Disability-Adjusted Life Years)", "Rate", "DALYs", "F", FALSE
    )
)) +
  plot_layout(heights = c(0.045, 1))

save_vector_pdf(figure3, "Figure3_final.pdf", 180, 155)

# -----------------------------------------------------------------------------
# Figure 4: three SDI panels at 180 mm wide and within the 210 mm height limit.
# -----------------------------------------------------------------------------
sdi_data <- read_csv(require_input("COUNTRY_SDI_INPUT"), show_col_types = FALSE) |>
  filter(if_all(c(SDI, ASMR, ASIR, ASDR), ~ !is.na(.x)))

format_p <- function(p_value) {
  exponent <- floor(log10(p_value))
  coefficient <- p_value / (10 ^ exponent)
  sprintf("%.2f × 10^%d", coefficient, exponent)
}

make_sdi_panel <- function(variable, panel_title, panel_tag) {
  pearson <- cor.test(sdi_data$SDI, sdi_data[[variable]], method = "pearson")
  statistic_label <- sprintf(
    "Pearson r = %.3f\n95%% CI %.3f to %.3f\nTwo-sided P = %s",
    unname(pearson$estimate), pearson$conf.int[[1]], pearson$conf.int[[2]],
    format_p(pearson$p.value)
  )

  ggplot(sdi_data, aes(SDI, .data[[variable]])) +
    geom_point(alpha = 0.60, colour = "#2166AC", size = 1.05) +
    geom_smooth(
      method = "loess", colour = "#D62728", fill = "#D62728",
      alpha = 0.20, linewidth = 0.5, se = TRUE
    ) +
    annotate(
      "label", x = -Inf, y = Inf, label = statistic_label,
      hjust = -0.05, vjust = 1.08, size = 5 / pt_per_mm,
      family = font_family, fontface = "bold", fill = "white",
      label.size = 0.18, label.padding = grid::unit(1.2, "mm")
    ) +
    scale_y_continuous(labels = comma, expand = expansion(mult = c(0.05, 0.08))) +
    labs(
      tag = panel_tag,
      title = panel_title,
      x = "Socio-demographic Index (SDI)",
      y = paste0(
        "Age-standardized ", tolower(panel_title),
        " rate (per 100,000)"
      )
    ) +
    theme_minimal(base_family = font_family, base_size = 6) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 7),
      axis.title = element_text(face = "bold", size = 6),
      axis.text = element_text(size = 5),
      panel.grid.minor = element_blank(),
      plot.margin = margin(3, 3, 3, 3)
    ) +
    panel_theme
}

figure4 <-
  make_sdi_panel("ASMR", "Mortality", "A") /
  make_sdi_panel("ASIR", "Incidence", "B") /
  make_sdi_panel("ASDR", "DALYs", "C")

save_vector_pdf(figure4, "Figure4_final.pdf", 180, 165)

message("Main Figs. 1, 3 and 4 created in: ", final_dir)
