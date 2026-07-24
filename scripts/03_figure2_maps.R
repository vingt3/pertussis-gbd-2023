source(file.path(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))), "utils.R"))
require_packages(c("tidyverse", "sf", "patchwork", "cowplot"))
paths <- repository_paths(); ensure_output_dir(paths$output)

# Edit these neutral labels and break points for another cause or estimation round.
disease_title <- "Pertussis burden"
shapes <- file.path(paths$data, "shapefiles")
hierarchy <- readr::read_csv(file.path(paths$data, "location_hierarchy.csv"), show_col_types = FALSE) |>
  select(location_id, location_name) |> distinct(location_name, .keep_all = TRUE)
crosswalk_file <- file.path(paths$data, "location_crosswalk.csv")
crosswalk <- if (file.exists(crosswalk_file)) readr::read_csv(crosswalk_file, show_col_types = FALSE) else tibble(source_location = character(), mapped_location = character())
boundaries <- sf::st_read(file.path(shapes, "gbd_boundaries.shp"), quiet = TRUE)
disputed <- sf::st_read(file.path(shapes, "disputed_boundaries.shp"), quiet = TRUE)

read_rate <- function(filename, label) {
  readr::read_csv(file.path(paths$data, filename), show_col_types = FALSE) |>
    filter(year == 2023, sex == "Both", age == "Age-standardized", metric == "Rate") |>
    transmute(location, rate = val, measure = label)
}
rates <- bind_rows(
  read_rate("country_incidence.csv", "Incidence"),
  read_rate("country_deaths.csv", "Deaths"),
  read_rate("country_dalys.csv", "DALYs")
) |>
  left_join(crosswalk, by = c("location" = "source_location")) |>
  mutate(location_for_join = coalesce(mapped_location, location)) |>
  left_join(hierarchy, by = c("location_for_join" = "location_name"))

bin_spec <- list(
  Incidence = list(breaks = c(0, 50, 100, 200, 350, 550, Inf), labels = c("0–50", "50–100", "100–200", "200–350", "350–550", ">550"), colours = c("#eff3ff", "#c6dbef", "#9ecae1", "#6baed6", "#3182bd", "#08519c")),
  Deaths = list(breaks = c(0, .01, .5, 1.5, 3, 6, Inf), labels = c("0–0.01", "0.01–0.5", "0.5–1.5", "1.5–3", "3–6", ">6"), colours = c("#fee8c8", "#fdbb84", "#fc8d59", "#ef6548", "#d7301f", "#990000")),
  DALYs = list(breaks = c(0, 5, 30, 100, 250, 500, Inf), labels = c("0–5", "5–30", "30–100", "100–250", "250–500", ">500"), colours = c("#f2e5ff", "#e1c8f5", "#c99eea", "#b276db", "#8f4ec6", "#6a1b9a"))
)
insets <- tribble(
  ~name, ~xmin, ~xmax, ~ymin, ~ymax,
  "Caribbean and Central America", -95, -55, 5, 30,
  "Persian Gulf", 44, 60, 20, 32,
  "Balkan Peninsula", 16, 32, 35, 48,
  "Southeast Asia", 93, 128, -12, 25,
  "West Africa", -20, 16, -2, 26,
  "Eastern Mediterranean", 24, 46, 27, 42,
  "Northern Europe", -12, 35, 53, 72
)
make_shape <- function(measure) {
  spec <- bin_spec[[measure]]
  boundaries |> left_join(filter(rates, measure == !!measure) |> select(location_id, rate), by = c("loc_id" = "location_id")) |>
    mutate(rate_bin = cut(pmax(rate, 0), breaks = spec$breaks, labels = spec$labels, include.lowest = TRUE), rate_bin = factor(rate_bin, levels = spec$labels))
}
base_map <- function(shape, measure, panel) {
  spec <- bin_spec[[measure]]
  ggplot(shape) +
    geom_sf(aes(fill = rate_bin), colour = "black", linewidth = .05) +
    geom_sf(data = disputed, linetype = 2, fill = NA, colour = "grey50", linewidth = .25, show.legend = FALSE) +
    scale_fill_manual(values = setNames(spec$colours, spec$labels), na.value = "grey95", name = "Rate per 100,000", drop = FALSE, guide = guide_legend(nrow = 1)) +
    labs(title = paste(panel, measure, "(rate per 100,000)")) + theme_void() +
    theme(plot.title = element_text(face = "bold", size = 13, hjust = 0), legend.position = "bottom", legend.text = element_text(size = 8))
}
inset_map <- function(shape, row) {
  spec <- bin_spec[[unique(shape$measure)[1]]]
  ggplot(shape) + geom_sf(aes(fill = rate_bin), colour = "black", linewidth = .03, show.legend = FALSE) +
    geom_sf(data = disputed, linetype = 2, fill = NA, colour = "grey55", linewidth = .2, show.legend = FALSE) +
    scale_fill_manual(values = setNames(spec$colours, spec$labels), na.value = "grey95", drop = FALSE) +
    coord_sf(xlim = c(row$xmin, row$xmax), ylim = c(row$ymin, row$ymax), expand = FALSE) + labs(title = row$name) + theme_void() +
    theme(plot.title = element_text(size = 7, hjust = .5), panel.border = element_rect(colour = "grey50", fill = NA, linewidth = .35))
}
make_row <- function(measure, panel) {
  shape <- make_shape(measure) |> mutate(measure = measure)
  inset_plots <- purrr::map(seq_len(nrow(insets)), ~inset_map(shape, insets[.x, ]))
  inset_grid <- wrap_plots(inset_plots, ncol = 2)
  base_map(shape, measure, panel) + inset_grid + plot_layout(widths = c(3.5, 1))
}
figure <- make_row("Incidence", "A") / make_row("Deaths", "B") / make_row("DALYs", "C") +
  plot_annotation(title = paste("Global distribution of", disease_title, ", 2023"), caption = "Panels show age-standardized rates per 100,000 population. Insets show selected small geographic areas.")
ggsave(file.path(paths$output, "figure_2.png"), figure, width = 16, height = 24, dpi = 300, bg = "white")
ggsave(file.path(paths$output, "figure_2.pdf"), figure, width = 16, height = 24, bg = "white")
