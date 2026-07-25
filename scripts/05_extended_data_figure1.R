suppressPackageStartupMessages({
  library(ggplot2)
  library(patchwork)
  library(scales)
})

# Rebuild Extended Data Fig. 1 as three panels:
# a, all-age incidence-rate comparison across all 162 matched locations;
# b, log case-count comparison across 116 locations with positive counts;
# c, WHO/JRF reported cases globally and by WHO region in 2023 and 2024.

source(file.path(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))), "utils.R"))
paths <- repository_paths()
output_dir <- ensure_output_dir(paths$output)

comparison <- read.csv(
  require_input("WHO_JRF_COMPARISON_INPUT"),
  stringsAsFactors = FALSE,
  check.names = FALSE
)
who_regions <- read.csv(
  require_input("WHO_JRF_REGION_TOTALS_INPUT"),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

font_family <- "Helvetica"
point_colour <- "#2F6FAE"
year_colours <- c("2023" = "#86B6D9", "2024" = "#D95F02")

base_theme <- theme_classic(base_size = 8, base_family = font_family) +
  theme(
    plot.title = element_text(face = "bold", size = 8.5, hjust = 0),
    plot.subtitle = element_text(size = 6.8, colour = "grey25", hjust = 0),
    plot.tag = element_text(face = "bold", size = 11, hjust = 0, vjust = 1),
    plot.tag.position = "topleft",
    axis.title = element_text(size = 7.2),
    axis.text = element_text(size = 6.5, colour = "black"),
    legend.text = element_text(size = 7),
    legend.title = element_blank(),
    panel.grid.major = element_line(colour = "grey90", linewidth = 0.25),
    panel.grid.minor = element_blank(),
    plot.margin = margin(5, 8, 5, 5)
  )

# Panel a: all 162 matched locations with available incidence rates.
rate_data <- subset(
  comparison,
  !is.na(gbd_all_age_rate_per1m_2023) &
    !is.na(who_incidence_per1m_2023)
)
stopifnot(nrow(rate_data) == 162)

p_rate <- ggplot(
  rate_data,
  aes(x = who_incidence_per1m_2023, y = gbd_all_age_rate_per1m_2023)
) +
  geom_abline(
    slope = 1, intercept = 0,
    linetype = "dashed", colour = "grey45", linewidth = 0.45
  ) +
  geom_point(colour = point_colour, alpha = 0.72, size = 1.35) +
  scale_x_continuous(
    trans = pseudo_log_trans(sigma = 1, base = 10),
    breaks = c(0, 1, 10, 100, 1000),
    labels = label_number(big.mark = ",", accuracy = 1)
  ) +
  scale_y_continuous(
    trans = pseudo_log_trans(sigma = 1, base = 10),
    breaks = c(0, 10, 100, 1000, 10000),
    labels = label_number(big.mark = ",", accuracy = 1)
  ) +
  labs(
    tag = "a",
    title = "All-age incidence rates, 2023",
    subtitle = "GBD versus WHO/JRF (n = 162 matched locations)",
    x = "WHO/JRF reported incidence per 1 million",
    y = "GBD modelled all-age incidence per 1 million"
  ) +
  base_theme

# Panel b: 116 matched locations with positive case counts in both sources.
case_data <- subset(
  comparison,
  !is.na(gbd_estimated_cases_2023) & gbd_estimated_cases_2023 > 0 &
    !is.na(who_cases_2023) & who_cases_2023 > 0
)
stopifnot(nrow(case_data) == 116)

p_cases <- ggplot(
  case_data,
  aes(x = who_cases_2023, y = gbd_estimated_cases_2023)
) +
  geom_abline(
    slope = 1, intercept = 0,
    linetype = "dashed", colour = "grey45", linewidth = 0.45
  ) +
  geom_point(colour = point_colour, alpha = 0.72, size = 1.35) +
  scale_x_log10(labels = label_number(big.mark = ",")) +
  scale_y_log10(labels = label_number(big.mark = ",")) +
  labs(
    tag = "b",
    title = "Case counts, 2023",
    subtitle = "GBD versus WHO/JRF (n = 116 positive-count locations)",
    x = "WHO/JRF reported cases",
    y = "GBD modelled estimated cases"
  ) +
  base_theme

# Panel c: global and WHO regional surveillance totals for 2023 and 2024.
region_plot <- subset(who_regions, GROUP %in% c("GLOBAL", "WHO_REGIONS"))
region_order <- c(
  "Global",
  "Western Pacific Region",
  "European Region",
  "Region of the Americas",
  "South-East Asia Region",
  "Eastern Mediterranean Region",
  "African Region"
)
region_long <- rbind(
  data.frame(
    region = region_plot$NAME,
    year = "2023",
    cases = region_plot$who_cases_2023
  ),
  data.frame(
    region = region_plot$NAME,
    year = "2024",
    cases = region_plot$who_cases_2024
  )
)
region_long$region <- factor(region_long$region, levels = rev(region_order))
region_long$year <- factor(region_long$year, levels = c("2023", "2024"))

p_regions <- ggplot(region_long, aes(x = cases, y = region, fill = year)) +
  geom_col(
    position = position_dodge(width = 0.76),
    width = 0.68
  ) +
  geom_text(
    aes(label = comma(cases)),
    position = position_dodge(width = 0.76),
    hjust = -0.08,
    size = 2.0,
    family = font_family,
    colour = "grey20"
  ) +
  scale_x_continuous(
    labels = label_number(big.mark = ","),
    limits = c(0, 1080000),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_fill_manual(values = year_colours) +
  labs(
    tag = "c",
    title = "WHO/JRF reported cases globally and by WHO region",
    subtitle = "Public WHO/UNICEF Joint Reporting Form surveillance totals, 2023 and 2024",
    x = "WHO/JRF reported pertussis cases",
    y = NULL,
    fill = NULL
  ) +
  base_theme +
  theme(
    legend.position = "top",
    legend.justification = "left",
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 7)
  )

figure <- (p_rate | p_cases) / p_regions +
  plot_layout(heights = c(1, 0.92))

pdf_file <- file.path(output_dir, "Extended_Data_Fig_1.pdf")
png_file <- file.path(output_dir, "Extended_Data_Fig_1.png")

pdf(
  pdf_file,
  width = 180 / 25.4,
  height = 210 / 25.4,
  family = font_family,
  colormodel = "srgb",
  useDingbats = FALSE,
  onefile = TRUE
)
print(figure)
dev.off()

ggsave(
  png_file,
  plot = figure,
  width = 180,
  height = 210,
  units = "mm",
  dpi = 600,
  bg = "white"
)

message("Created: ", pdf_file)
message("Created: ", png_file)
