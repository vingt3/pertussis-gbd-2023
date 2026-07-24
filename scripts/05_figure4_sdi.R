source(file.path(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))), "utils.R"))
require_packages(c("tidyverse", "patchwork", "scales"))
paths <- repository_paths(); ensure_output_dir(paths$output)
data <- readr::read_csv(file.path(paths$data, "country_sdi_burden_2023.csv"), show_col_types = FALSE) |> filter(if_all(c(SDI, ASMR, ASIR, ASDR), ~ !is.na(.x)))
make_plot <- function(y, label) {
  result <- cor.test(data$SDI, data[[y]], method = "pearson")
  note <- sprintf("r = %.3f\np %s", unname(result$estimate), ifelse(result$p.value < .001, "< 0.001", paste0("= ", formatC(result$p.value, format = "f", digits = 3))))
  ggplot(data, aes(SDI, .data[[y]])) + geom_point(alpha = .6, colour = "#2166AC", size = 2.5) +
    geom_smooth(method = "loess", colour = "#D62728", fill = "#D62728", alpha = .2, linewidth = 1.2, se = TRUE) +
    annotate("label", x = min(data$SDI) + .03, y = max(data[[y]]) * .85, label = note, hjust = 0, vjust = 1, fontface = "bold") +
    labs(title = label, x = "Socio-demographic Index (SDI)", y = paste0("Age-standardized ", label, " rate\n(per 100,000 population)")) +
    theme_minimal(base_size = 12) + theme(plot.title = element_text(face = "bold", hjust = .5), axis.title = element_text(face = "bold"))
}
figure <- make_plot("ASMR", "Mortality") / make_plot("ASIR", "Incidence") / make_plot("ASDR", "DALYs") +
  plot_annotation(title = "Relationship between Socio-demographic Index and disease burden, 2023", subtitle = "Each point represents a country; curves show LOESS smoothing with 95% confidence bands.\nLinear Y-axis scale is used to display the SDI gradient.", theme = theme(plot.title = element_text(face = "bold", hjust = .5), plot.subtitle = element_text(hjust = .5)))
ggsave(file.path(paths$output, "figure_4.png"), figure, width = 10, height = 14, dpi = 300, bg = "white")
ggsave(file.path(paths$output, "figure_4.pdf"), figure, width = 10, height = 14, bg = "white")
