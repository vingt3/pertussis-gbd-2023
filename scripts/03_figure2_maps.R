# Create the final editable vector PDF for main Fig. 2.
# Input files, including map layers, are configured through environment
# variables documented in README.md; no source data are included here.

suppressPackageStartupMessages({
  library(tidyverse)
  library(sf)
  library(cowplot)
})

source(file.path(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))), "utils.R"))
paths <- repository_paths()
output_dir <- ensure_output_dir(paths$output)
grid_output_file <- file.path(output_dir, "Figure2_final.pdf")

font_family <- "Helvetica"
sf::sf_use_s2(FALSE)
read_and_simplify <- function(path) {
  sf::st_read(path, quiet = TRUE) |>
    sf::st_make_valid() |>
    sf::st_simplify(dTolerance = 0.025, preserveTopology = TRUE)
}

gbd_shapefile <- read_and_simplify(require_input("COUNTRY_BOUNDARIES_SHP"))
disputed_shapefile <- read_and_simplify(require_input("DISPUTED_BOUNDARIES_SHP"))

death_data <- read_csv(require_input("COUNTRY_DEATH_INPUT"), show_col_types = FALSE)
incidence_data <- read_csv(require_input("COUNTRY_INCIDENCE_INPUT"), show_col_types = FALSE)
daly_data <- read_csv(require_input("COUNTRY_DALY_INPUT"), show_col_types = FALSE)

extract_rate <- function(data, measure_label) {
  data |>
    filter(year == 2023, sex == "Both", age == "Age-standardized", metric == "Rate") |>
    transmute(location, rate = val, measure = measure_label)
}

rate_data <- bind_rows(
  extract_rate(incidence_data, "Incidence"),
  extract_rate(death_data, "Deaths"),
  extract_rate(daly_data, "DALYs")
)

location_map <- read_csv(require_input("LOCATION_HIERARCHY_INPUT"), show_col_types = FALSE) |>
  select(location_id, location_name) |>
  filter(!is.na(location_id), !is.na(location_name)) |>
  group_by(location_name) |>
  slice(1) |>
  ungroup()

rate_data <- rate_data |>
  left_join(location_map, by = c("location" = "location_name"))

manual_corrections <- tibble(
  location = c(
    "Türkiye", "Taiwan (Province of China)", "Palestine",
    "Democratic People's Republic of Korea", "Republic of Korea",
    "United States of America", "United Kingdom",
    "Bolivia (Plurinational State of)", "Venezuela (Bolivarian Republic of)",
    "Iran (Islamic Republic of)", "Viet Nam", "Russian Federation",
    "Czechia", "Republic of Moldova", "Syrian Arab Republic",
    "Lao People's Democratic Republic", "United Republic of Tanzania",
    "Cabo Verde", "Côte d'Ivoire", "Democratic Republic of the Congo",
    "Republic of the Congo", "Brunei Darussalam",
    "Micronesia (Federated States of)", "Timor-Leste", "Eswatini",
    "São Tomé and Príncipe"
  ),
  correct_name = c(
    "Turkey", "Taiwan", "Palestine", "North Korea", "South Korea",
    "United States", "United Kingdom", "Bolivia", "Venezuela", "Iran",
    "Vietnam", "Russia", "Czech Republic", "Moldova", "Syria", "Laos",
    "Tanzania", "Cape Verde", "Ivory Coast",
    "Democratic Republic of the Congo", "Congo", "Brunei", "Micronesia",
    "East Timor", "Swaziland", "Sao Tome and Principe"
  )
)

unmatched_locations <- rate_data |>
  filter(is.na(location_id)) |>
  distinct(location)

for (unmatched_name in unmatched_locations$location) {
  correction <- manual_corrections |>
    filter(location == unmatched_name)
  if (nrow(correction) > 0) {
    corrected_match <- location_map |>
      filter(location_name == correction$correct_name[[1]]) |>
      slice(1)
    if (nrow(corrected_match) > 0) {
      rate_data$location_id[
        rate_data$location == unmatched_name & is.na(rate_data$location_id)
      ] <- corrected_match$location_id[[1]]
      next
    }
  }

  partial_match <- location_map |>
    filter(
      str_detect(location_name, regex(unmatched_name, ignore_case = TRUE)) |
        str_detect(unmatched_name, regex(location_name, ignore_case = TRUE))
    ) |>
    slice(1)
  if (nrow(partial_match) > 0) {
    rate_data$location_id[
      rate_data$location == unmatched_name & is.na(rate_data$location_id)
    ] <- partial_match$location_id[[1]]
  }
}

bin_specs <- list(
  Incidence = list(
    breaks = c(0, 50, 100, 200, 350, 550, Inf),
    labels = c("0-50", "50-100", "100-200", "200-350", "350-550", ">550"),
    colors = c("#eff3ff", "#c6dbef", "#9ecae1", "#6baed6", "#3182bd", "#08519c")
  ),
  Deaths = list(
    breaks = c(0, 0.01, 0.5, 1.5, 3, 6, Inf),
    labels = c("0-0.01", "0.01-0.5", "0.5-1.5", "1.5-3", "3-6", ">6"),
    colors = c("#fee8c8", "#fdbb84", "#fc8d59", "#ef6548", "#d7301f", "#990000")
  ),
  DALYs = list(
    breaks = c(0, 5, 30, 100, 250, 500, Inf),
    labels = c("0-5", "5-30", "30-100", "100-250", "250-500", ">500"),
    colors = c("#f2e5ff", "#e1c8f5", "#c99eea", "#b276db", "#8f4ec6", "#6a1b9a")
  )
)

inset_regions <- list(
  caribbean = list(
    title = "Caribbean and\nCentral America", xlim = c(-95, -55), ylim = c(5, 30)
  ),
  persian_gulf = list(title = "Persian Gulf", xlim = c(44, 60), ylim = c(20, 32)),
  balkan = list(title = "Balkan Peninsula", xlim = c(16, 32), ylim = c(35, 48)),
  southeast_asia = list(title = "Southeast Asia", xlim = c(93, 128), ylim = c(-12, 25)),
  west_africa = list(title = "West\nAfrica", xlim = c(-20, 16), ylim = c(-2, 26)),
  eastern_mediterranean = list(
    title = "Eastern\nMediterranean", xlim = c(24, 46), ylim = c(27, 42)
  ),
  northern_europe = list(title = "Northern Europe", xlim = c(-12, 35), ylim = c(53, 72))
)

make_binned_shape <- function(measure_name) {
  spec <- bin_specs[[measure_name]]
  measure_data <- rate_data |>
    filter(measure == measure_name, !is.na(location_id))

  gbd_shapefile |>
    left_join(measure_data |> select(location_id, rate), by = c("loc_id" = "location_id")) |>
    mutate(
      rate_bin = cut(
        pmax(rate, 0), breaks = spec$breaks, labels = spec$labels,
        include.lowest = TRUE, right = TRUE
      ),
      rate_bin = forcats::fct_na_value_to_level(rate_bin, level = "No data"),
      rate_bin = factor(rate_bin, levels = c("No data", spec$labels))
    )
}

fill_scale <- function(
  measure_name,
  legend_title = "Age-standardized\nrate\n(per 100,000 population)"
) {
  spec <- bin_specs[[measure_name]]
  scale_fill_manual(
    values = setNames(c("#f2f2f2", spec$colors), c("No data", spec$labels)),
    limits = c("No data", spec$labels), drop = FALSE,
    name = legend_title
  )
}

make_main_map <- function(
  shape, measure_name, panel_label, show_legend = FALSE,
  legend_title = "Age-standardized\nrate\n(per 100,000 population)",
  legend_title_size = 5.5
) {
  ggplot(shape) +
    geom_sf(aes(fill = rate_bin), color = "black", linewidth = 0.045) +
    geom_sf(
      data = disputed_shapefile, linetype = 2, fill = NA,
      color = "gray50", linewidth = 0.22, show.legend = FALSE
    ) +
    fill_scale(measure_name, legend_title) +
    labs(title = paste0(panel_label, "   ", measure_name, " (Rate per 100,000)")) +
    guides(
      fill = guide_legend(
        direction = "vertical", ncol = 1, title.position = "top",
        keyheight = grid::unit(2.35, "mm"),
        keywidth = grid::unit(3.0, "mm")
      )
    ) +
    theme_void(base_family = font_family) +
    theme(
      plot.title = element_text(
        face = "bold", size = 7, hjust = 0,
        margin = margin(b = 1.5)
      ),
      legend.position = if (show_legend) "right" else "none",
      legend.title = element_text(
        face = "bold", size = legend_title_size, lineheight = 0.95
      ),
      legend.text = element_text(size = 5),
      legend.spacing.y = grid::unit(0, "pt"),
      legend.margin = margin(0, 0, 0, 0),
      plot.margin = margin(1, 1, 0, 1)
    )
}

make_inset <- function(shape, measure_name, region) {
  ggplot(shape) +
    geom_sf(aes(fill = rate_bin), color = "black", linewidth = 0.03, show.legend = FALSE) +
    geom_sf(
      data = disputed_shapefile, linetype = 2, fill = NA,
      color = "gray55", linewidth = 0.16, show.legend = FALSE
    ) +
    fill_scale(measure_name) +
    coord_sf(xlim = region$xlim, ylim = region$ylim, expand = FALSE) +
    theme_void(base_family = font_family) +
    theme(
      aspect.ratio = 0.68,
      panel.border = element_rect(color = "gray45", fill = NA, linewidth = 0.25),
      plot.margin = margin(0, 0, 0, 0)
    )
}

make_reference_inset_layout <- function(shape, measure_name) {
  plots <- map(inset_regions, ~ make_inset(shape, measure_name, .x))

  major_gap <- 0.008
  major_width <- (1 - 4 * major_gap) / 5
  major_x <- (0:4) * (major_width + major_gap)
  inner_gap <- 0.006
  small_width <- (major_width - inner_gap) / 2
  right_x <- major_x[[5]]
  west_x <- right_x
  eastern_x <- right_x + small_width + inner_gap
  northern_x <- right_x + (major_width - small_width) / 2
  small_plot_width <- small_width * 0.92
  west_plot_x <- west_x + (small_width - small_plot_width) / 2
  eastern_plot_x <- eastern_x + (small_width - small_plot_width) / 2
  northern_plot_x <- right_x + (major_width - small_plot_width) / 2

  # Five equal major columns, matching the reference artwork. Titles are drawn
  # independently from the maps so the upper title line and all map boundaries
  # can be aligned exactly, irrespective of each geographic bounding box.
  ggdraw() +
    draw_plot(
      plots$caribbean, x = major_x[[1]], y = 0.00,
      width = major_width, height = 0.76
    ) +
    draw_plot(
      plots$persian_gulf, x = major_x[[2]], y = 0.00,
      width = major_width, height = 0.76
    ) +
    draw_plot(
      plots$balkan, x = major_x[[3]], y = 0.00,
      width = major_width, height = 0.76
    ) +
    draw_plot(
      plots$southeast_asia, x = major_x[[4]], y = 0.00,
      width = major_width, height = 0.76
    ) +
    draw_plot(
      plots$west_africa, x = west_plot_x, y = 0.46,
      width = small_plot_width, height = 0.30
    ) +
    draw_plot(
      plots$eastern_mediterranean, x = eastern_plot_x, y = 0.46,
      width = small_plot_width, height = 0.30
    ) +
    draw_plot(
      plots$northern_europe, x = northern_plot_x, y = 0.05,
      width = small_plot_width, height = 0.30
    ) +
    draw_label(
      inset_regions$caribbean$title,
      x = major_x[[1]] + major_width / 2, y = 0.89,
      size = 5, fontfamily = font_family,
      hjust = 0.5, vjust = 0.5, lineheight = 0.90
    ) +
    draw_label(
      inset_regions$persian_gulf$title,
      x = major_x[[2]] + major_width / 2, y = 0.89,
      size = 5, fontfamily = font_family,
      hjust = 0.5, vjust = 0.5
    ) +
    draw_label(
      inset_regions$balkan$title,
      x = major_x[[3]] + major_width / 2, y = 0.89,
      size = 5, fontfamily = font_family,
      hjust = 0.5, vjust = 0.5
    ) +
    draw_label(
      inset_regions$southeast_asia$title,
      x = major_x[[4]] + major_width / 2, y = 0.89,
      size = 5, fontfamily = font_family,
      hjust = 0.5, vjust = 0.5
    ) +
    draw_label(
      inset_regions$west_africa$title,
      x = west_x + small_width / 2, y = 0.89,
      size = 5, fontfamily = font_family,
      hjust = 0.5, vjust = 0.5, lineheight = 0.90
    ) +
    draw_label(
      inset_regions$eastern_mediterranean$title,
      x = eastern_x + small_width / 2, y = 0.89,
      size = 5, fontfamily = font_family,
      hjust = 0.5, vjust = 0.5, lineheight = 0.90
    ) +
    draw_label(
      inset_regions$northern_europe$title,
      x = northern_x + small_width / 2, y = 0.405,
      size = 5, fontfamily = font_family,
      hjust = 0.5, vjust = 0.5
    )
}

build_panel <- function(
  measure_name, panel_label,
  inset_x = 0.205, inset_width = 0.585,
  panel_rel_heights = c(2.85, 1.35), legend_x = 0.16,
  legend_y = 0.01, map_x = 0, map_width = 1,
  legend_title = "Age-standardized\nrate\n(per 100,000 population)",
  legend_title_size = 5.5
) {
  shape <- make_binned_shape(measure_name)
  map_without_legend <- make_main_map(
    shape, measure_name, panel_label, FALSE, legend_title, legend_title_size
  )
  legend_grob <- get_legend(
    make_main_map(
      shape, measure_name, panel_label, TRUE,
      legend_title, legend_title_size
    )
  )

  map_with_legend <- ggdraw() +
    draw_plot(
      map_without_legend, x = map_x, y = 0,
      width = map_width, height = 1
    ) +
    draw_grob(
      legend_grob, x = legend_x, y = legend_y,
      width = 0.225, height = 0.50
    )

  # Align the complete inset band to the left and right boundaries of the
  # geographic map area rather than to the full PDF page.
  aligned_insets <- ggdraw() +
    draw_plot(
      make_reference_inset_layout(shape, measure_name),
      x = inset_x, y = 0, width = inset_width, height = 1
    )

  plot_grid(
    map_with_legend,
    aligned_insets,
    ncol = 1, rel_heights = panel_rel_heights, align = "v"
  )
}

# A is at upper left, B at upper right and C at lower left. The lower-right
# quadrant is intentionally blank, with narrow gutters between panels.
grid_panel_a <- build_panel(
  "Incidence", "A", inset_x = 0.02, inset_width = 0.96,
  panel_rel_heights = c(2.85, 1.35), legend_x = 0.04, legend_y = 0.07,
  map_x = 0.07, map_width = 0.93,
  legend_title = "Age-standardized\nrate\n(per 100,000\npopulation)",
  legend_title_size = 4.5
)
grid_panel_b <- build_panel(
  "Deaths", "B", inset_x = 0.02, inset_width = 0.96,
  panel_rel_heights = c(2.85, 1.35), legend_x = 0.04, legend_y = 0.07,
  map_x = 0.07, map_width = 0.93,
  legend_title = "Age-standardized\nrate\n(per 100,000\npopulation)",
  legend_title_size = 4.5
)
grid_panel_c <- build_panel(
  "DALYs", "C", inset_x = 0.02, inset_width = 0.96,
  panel_rel_heights = c(2.85, 1.35), legend_x = 0.04, legend_y = 0.07,
  map_x = 0.07, map_width = 0.93,
  legend_title = "Age-standardized\nrate\n(per 100,000\npopulation)",
  legend_title_size = 4.5
)

figure2_grid_layout <- ggdraw() +
  draw_plot(grid_panel_a, x = 0.005, y = 0.510, width = 0.485, height = 0.480) +
  draw_plot(grid_panel_b, x = 0.510, y = 0.510, width = 0.485, height = 0.480) +
  draw_plot(grid_panel_c, x = 0.005, y = 0.010, width = 0.485, height = 0.480)

ggsave(
  grid_output_file,
  plot = figure2_grid_layout,
  device = grDevices::pdf,
  width = 180,
  height = 130,
  units = "mm",
  bg = "white",
  colormodel = "srgb",
  limitsize = FALSE
)

message("Created grid-layout Figure 2: ", grid_output_file)
