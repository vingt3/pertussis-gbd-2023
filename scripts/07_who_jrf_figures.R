source(file.path(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))), "utils.R"))
require_packages(c("ggplot2", "scales"))
paths <- repository_paths(); ensure_output_dir(paths$output)
comparison <- read.csv(file.path(paths$output, "who_jrf_vs_gbd_country_comparison.csv"))
regions <- read.csv(file.path(paths$output, "who_jrf_global_and_region_cases.csv"))

scatter <- subset(comparison, who_cases_2023 > 0 & gbd_cases_2023 > 0)
p1 <- ggplot(scatter, aes(who_cases_2023, gbd_cases_2023)) + geom_point(colour = "#2166AC", alpha = .7) + geom_abline(linetype = "dashed", colour = "grey45") + scale_x_log10(labels = comma) + scale_y_log10(labels = comma) + labs(x = "WHO/JRF reported cases, 2023", y = "GBD modelled cases, 2023", title = "GBD modelled estimates versus WHO/JRF reported cases") + theme_minimal()
ggsave(file.path(paths$output, "supplementary_figure_s1.png"), p1, width = 9.2, height = 6.2, dpi = 300)

selected <- regions[regions$GROUP %in% c("GLOBAL", "WHO_REGIONS"), ]
long <- reshape(selected[, c("NAME", "who_cases_2023", "who_cases_2024")], varying = c("who_cases_2023", "who_cases_2024"), v.names = "cases", timevar = "year", times = c("2023", "2024"), direction = "long")
p2 <- ggplot(long, aes(cases, reorder(NAME, cases), fill = year)) + geom_col(position = position_dodge(.75), width = .68) + scale_x_continuous(labels = comma) + scale_fill_manual(values = c("2023" = "#8FB9DD", "2024" = "#D95F02")) + labs(x = "WHO/JRF reported cases", y = NULL, title = "WHO/JRF reported pertussis cases, 2023 and 2024") + theme_minimal()
ggsave(file.path(paths$output, "supplementary_figure_s2.png"), p2, width = 9.6, height = 5.8, dpi = 300)
