source(file.path(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))), "utils.R"))
require_packages(c("tidyverse", "patchwork", "scales", "RColorBrewer"))
paths <- repository_paths(); ensure_output_dir(paths$output)

trend <- readr::read_csv(file.path(paths$data, "global_burden.csv"), show_col_types = FALSE) |>
  filter(sex == "Both", age == "Age-standardized", metric == "Rate")
make_trend <- function(measure, label) {
  d <- filter(trend, measure == !!measure)
  ggplot(d, aes(year, val)) +
    annotate("rect", xmin = 2019, xmax = 2022, ymin = -Inf, ymax = Inf, fill = "#F4B6B6", alpha = .25) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#A9BCEB", alpha = .55) +
    geom_line(colour = "#0B3FB3", linewidth = 1) + geom_point(colour = "#0B3FB3", size = 1.3) +
    scale_x_continuous(breaks = seq(1990, 2023, 5)) + scale_y_continuous(labels = comma) +
    labs(title = label, x = "Year", y = "Age-standardized rate per 100,000") +
    theme_minimal(base_size = 11) + theme(plot.title = element_text(face = "bold", hjust = .5), panel.grid.minor = element_blank())
}
rate_panels <- make_trend("Incidence", "Incidence") / make_trend("Deaths", "Deaths") / make_trend("DALYs (Disability-Adjusted Life Years)", "DALYs")

regional <- bind_rows(
  readr::read_csv(file.path(paths$data, "region_incidence.csv"), show_col_types = FALSE) |> mutate(measure = "Incidence"),
  readr::read_csv(file.path(paths$data, "region_deaths.csv"), show_col_types = FALSE) |> mutate(measure = "Deaths"),
  readr::read_csv(file.path(paths$data, "region_dalys.csv"), show_col_types = FALSE) |> mutate(measure = "DALYs")
) |> filter(sex == "Both", age == "All ages", metric == "Number", between(year, 1990, 2023))
order <- regional |> group_by(location) |> summarise(mean_value = mean(val), .groups = "drop") |> arrange(desc(mean_value)) |> pull(location)
colours <- setNames(colorRampPalette(brewer.pal(12, "Set3"))(length(order)), order)
make_bars <- function(measure, label) {
  ggplot(filter(regional, measure == !!measure) |> mutate(location = factor(location, levels = order)), aes(year, val, fill = location)) +
    geom_col(width = .8) + scale_fill_manual(values = colours, name = "Region") +
    scale_x_continuous(breaks = seq(1990, 2023, 5)) + scale_y_continuous(labels = comma) +
    labs(title = label, x = "Year", y = paste("Number of", label)) + theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold", hjust = .5), axis.text.x = element_text(angle = 45, hjust = 1), legend.text = element_text(size = 7), panel.grid.major.x = element_blank())
}
absolute_panels <- make_bars("Incidence", "Incident cases") / make_bars("Deaths", "Deaths") / make_bars("DALYs", "DALYs")
figure <- rate_panels | absolute_panels
ggsave(file.path(paths$output, "figure_1.png"), figure, width = 18, height = 12, dpi = 300, bg = "white")
ggsave(file.path(paths$output, "figure_1.pdf"), figure, width = 18, height = 12, bg = "white")
