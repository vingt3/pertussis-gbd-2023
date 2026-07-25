suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(patchwork)
  library(scales)
  library(tidyr)
})

source(file.path(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))), "utils.R"))
paths <- repository_paths()
output_dir <- ensure_output_dir(paths$output)

data_2023 <- read.csv(
  require_input("AGE_BURDEN_2023_INPUT"),
  check.names = FALSE,
  stringsAsFactors = FALSE
)
data_1990 <- read.csv(
  require_input("AGE_BURDEN_1990_INPUT"),
  check.names = FALSE,
  stringsAsFactors = FALSE
)

age_order <- c(
  "<28 days", "1-5 months", "6-11 months", "12-23 months",
  "2-4 years", "5-9 years", "10-14 years", "15-19 years",
  "20-24 years", "25-29 years", "30-34 years", "35-39 years",
  "40-44 years", "45-49 years", "50-54 years", "55-59 years",
  "60-64 years", "65-69 years", "70-74 years", "75-79 years",
  "80+ years"
)

regions <- c(
  "Central Europe, Eastern Europe, and Central Asia",
  "High-income",
  "Latin America and Caribbean",
  "North Africa and Middle East",
  "South Asia",
  "Sub-Saharan Africa",
  "Southeast Asia, East Asia, and Oceania"
)

measures <- c(
  "Deaths",
  "DALYs (Disability-Adjusted Life Years)",
  "Incidence"
)

time_colours <- c("1990" = "#F4A261", "2023" = "#2F7FB8")
sex_colours <- c("Female" = "#9E2042", "Male" = "#174A9C")

display_measure <- function(x) {
  ifelse(x == "DALYs (Disability-Adjusted Life Years)", "DALYs", x)
}

save_vector_pdf <- function(plot, filename, width_mm, height_mm) {
  path <- file.path(output_dir, filename)
  pdf(
    path,
    width = width_mm / 25.4,
    height = height_mm / 25.4,
    family = "Helvetica",
    colormodel = "srgb",
    useDingbats = FALSE,
    onefile = TRUE
  )
  print(plot)
  dev.off()
  invisible(path)
}

prepare_panel_data <- function(region, measure, comparison) {
  if (comparison == "time") {
    raw <- bind_rows(data_1990, data_2023) |>
      filter(
        location == region,
        measure == !!measure,
        sex == "Both",
        year %in% c(1990, 2023)
      ) |>
      mutate(group = as.character(year))
    groups <- c("1990", "2023")
  } else {
    raw <- data_2023 |>
      filter(
        location == region,
        measure == !!measure,
        sex %in% c("Female", "Male"),
        year == 2023
      ) |>
      mutate(group = sex)
    groups <- c("Female", "Male")
  }

  number <- raw |>
    filter(metric == "Number") |>
    select(age, group, val_number = val, lower_number = lower, upper_number = upper)
  rate <- raw |>
    filter(metric == "Rate") |>
    select(age, group, val_rate = val, lower_rate = lower, upper_rate = upper)

  expanded <- expand_grid(age = age_order, group = groups) |>
    left_join(number, by = c("age", "group")) |>
    left_join(rate, by = c("age", "group")) |>
    mutate(
      across(
        c(val_number, lower_number, upper_number, val_rate, lower_rate, upper_rate),
        ~replace_na(.x, 0)
      ),
      age = factor(age, levels = age_order),
      group = factor(group, levels = groups)
    )
  expanded
}

make_age_panel <- function(region, measure, comparison) {
  d <- prepare_panel_data(region, measure, comparison)
  measure_short <- display_measure(measure)
  groups <- levels(d$group)
  palette <- if (comparison == "time") time_colours else sex_colours
  subtitle <- if (comparison == "time") "1990 vs 2023" else "Female vs male (2023)"

  number_max <- max(d$upper_number, d$val_number, na.rm = TRUE)
  rate_max <- max(d$upper_rate, d$val_rate, na.rm = TRUE)
  scale_factor <- ifelse(rate_max > 0, number_max / rate_max, 1)
  dodge <- position_dodge(width = 0.72)

  ggplot(d, aes(x = age, group = group)) +
    geom_col(
      aes(y = val_number, fill = group),
      position = dodge, width = 0.62, alpha = 0.72
    ) +
    geom_errorbar(
      aes(ymin = lower_number, ymax = upper_number, colour = group),
      position = dodge, width = 0.18, linewidth = 0.24, alpha = 0.55
    ) +
    geom_errorbar(
      aes(
        ymin = lower_rate * scale_factor,
        ymax = upper_rate * scale_factor,
        colour = group
      ),
      position = dodge, width = 0.12, linewidth = 0.20, alpha = 0.45
    ) +
    geom_line(
      aes(y = val_rate * scale_factor, colour = group),
      position = dodge, linewidth = 0.48
    ) +
    geom_point(
      aes(y = val_rate * scale_factor, colour = group),
      position = dodge, size = 0.75
    ) +
    scale_fill_manual(values = palette, breaks = groups) +
    scale_colour_manual(values = palette, breaks = groups) +
    scale_y_continuous(
      name = paste0(measure_short, " - number"),
      labels = label_number(big.mark = ",", accuracy = 1),
      expand = expansion(mult = c(0, 0.10)),
      sec.axis = sec_axis(
        ~ . / scale_factor,
        name = paste0(measure_short, " - rate per 100,000"),
        labels = label_number(big.mark = ",", accuracy = 0.1)
      )
    ) +
    labs(
      title = measure_short,
      subtitle = subtitle,
      x = "Age group",
      fill = NULL,
      colour = NULL
    ) +
    guides(
      fill = guide_legend(override.aes = list(alpha = 0.72)),
      colour = "none"
    ) +
    theme_minimal(base_family = "Helvetica", base_size = 5.2) +
    theme(
      plot.title = element_text(face = "bold", size = 6.2, hjust = 0.5),
      plot.subtitle = element_text(size = 5.0, hjust = 0.5),
      plot.tag = element_text(face = "bold", size = 7.0),
      axis.title = element_text(face = "bold", size = 5.1),
      axis.text.x = element_text(
        size = 5.0, angle = 48, hjust = 1, vjust = 1, colour = "black"
      ),
      axis.text.y = element_text(size = 5.0, colour = "black"),
      legend.position = "top",
      legend.text = element_text(size = 5.0),
      legend.key.height = unit(2.2, "mm"),
      legend.key.width = unit(3.5, "mm"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour = "grey88", linewidth = 0.22),
      plot.margin = margin(2.0, 4.5, 2.0, 4.5)
    )
}

make_age_figure <- function(region) {
  plots <- list()
  for (measure in measures) {
    plots[[length(plots) + 1]] <- make_age_panel(region, measure, "time")
    plots[[length(plots) + 1]] <- make_age_panel(region, measure, "sex")
  }

  wrap_plots(plots, ncol = 2, guides = "keep") +
    plot_annotation(
      title = paste0(region, ": temporal and sex comparisons"),
      subtitle = "Bars show numbers; lines and points show rates per 100,000; error bars show 95% uncertainty intervals",
      tag_levels = "a",
      theme = theme(
        plot.title = element_text(
          family = "Helvetica", face = "bold", size = 7.0, hjust = 0.5
        ),
        plot.subtitle = element_text(
          family = "Helvetica", size = 5.2, hjust = 0.5
        ),
        plot.margin = margin(3, 3, 3, 3)
      )
    )
}

for (i in seq_along(regions)) {
  fig <- make_age_figure(regions[[i]])
  save_vector_pdf(
    fig,
    sprintf("Extended_Data_Fig_%d.pdf", i + 1),
    width_mm = 180,
    height_mm = 162
  )
}

# ED Fig. 9: mortality-to-incidence ratio and infant mortality trends.
ratio <- read.csv(
  require_input("MORTALITY_INCIDENCE_RATIO_INPUT"),
  check.names = FALSE,
  stringsAsFactors = FALSE
) |>
  arrange(deaths_per_1000_cases) |>
  mutate(location = factor(location, levels = location))

infant <- read.csv(
  require_input("INFANT_MORTALITY_TRENDS_INPUT"),
  check.names = FALSE,
  stringsAsFactors = FALSE
)

infant_colours <- c(
  "Central Europe, Eastern Europe, and Central Asia" = "#2F7FB8",
  "Global" = "#F07C00",
  "High-income" = "#3BA44D",
  "Latin America and Caribbean" = "#D64545",
  "North Africa and Middle East" = "#8B66C2",
  "South Asia" = "#9A675A",
  "Southeast Asia, East Asia, and Oceania" = "#E27AC3",
  "Sub-Saharan Africa" = "#8F8F8F"
)

p9a <- ggplot(ratio, aes(x = deaths_per_1000_cases, y = location)) +
  geom_col(fill = "#C44E52", width = 0.72) +
  geom_text(
    aes(label = number(deaths_per_1000_cases, accuracy = 0.01)),
    hjust = -0.12, size = 1.8, family = "Helvetica"
  ) +
  scale_x_continuous(
    limits = c(0, max(ratio$deaths_per_1000_cases) * 1.12),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "Mortality-to-incidence ratio by GBD super-region, 2023",
    x = "Deaths per 1,000 estimated cases",
    y = NULL
  ) +
  theme_classic(base_family = "Helvetica", base_size = 6.0) +
  theme(
    plot.title = element_text(face = "bold", size = 7.0, hjust = 0.5),
    plot.tag = element_text(face = "bold", size = 7.0),
    axis.title = element_text(size = 6.0),
    axis.text = element_text(size = 5.4, colour = "black"),
    plot.margin = margin(4, 8, 4, 4)
  )

p9b <- ggplot(infant, aes(x = year, y = val, colour = location, group = location)) +
  geom_line(aes(linewidth = location == "Global"), alpha = 0.92) +
  geom_point(size = 1.25, alpha = 0.92) +
  scale_linewidth_manual(values = c("TRUE" = 0.75, "FALSE" = 0.42), guide = "none") +
  scale_colour_manual(values = infant_colours) +
  scale_x_continuous(breaks = 2019:2023) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.08))) +
  labs(
    title = "Mortality rate among infants aged 1-5 months, 2019-2023",
    x = "Year",
    y = "Deaths per 100,000",
    colour = NULL
  ) +
  theme_classic(base_family = "Helvetica", base_size = 6.0) +
  theme(
    plot.title = element_text(face = "bold", size = 7.0, hjust = 0.5),
    plot.tag = element_text(face = "bold", size = 7.0),
    axis.title = element_text(size = 6.0),
    axis.text = element_text(size = 5.4, colour = "black"),
    panel.grid.major.y = element_line(colour = "grey88", linewidth = 0.22),
    legend.position = "bottom",
    legend.text = element_text(size = 5.0),
    legend.key.width = unit(4.5, "mm"),
    plot.margin = margin(4, 4, 4, 4)
  ) +
  guides(colour = guide_legend(ncol = 2, byrow = TRUE))

fig9 <- p9a / p9b +
  plot_layout(heights = c(0.92, 1.08)) +
  plot_annotation(tag_levels = "a")
save_vector_pdf(fig9, "Extended_Data_Fig_9.pdf", 180, 210)

# ED Fig. 10: top 10 location-specific random effects.
random_effects <- read.csv(
  require_input("TOP_RANDOM_EFFECTS_INPUT"),
  check.names = FALSE,
  stringsAsFactors = FALSE
) |>
  arrange(random_effect) |>
  mutate(location_name = factor(location_name, levels = location_name))

fig10 <- ggplot(random_effects, aes(x = random_effect, y = location_name)) +
  geom_col(fill = "#4E79A7", width = 0.72) +
  geom_text(
    aes(label = number(random_effect, accuracy = 0.01)),
    hjust = -0.12, size = 2.1, family = "Helvetica"
  ) +
  scale_x_continuous(
    limits = c(0, max(random_effects$random_effect) * 1.10),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "Top 10 location-specific random effects in the pertussis incidence model",
    x = "Location-specific random effect",
    y = NULL
  ) +
  theme_classic(base_family = "Helvetica", base_size = 6.0) +
  theme(
    plot.title = element_text(face = "bold", size = 7.0, hjust = 0.5),
    axis.title = element_text(size = 6.0),
    axis.text = element_text(size = 5.5, colour = "black"),
    plot.margin = margin(5, 8, 5, 5)
  )

save_vector_pdf(fig10, "Extended_Data_Fig_10.pdf", 180, 110)

message("Created vector Extended Data figures in: ", output_dir)
