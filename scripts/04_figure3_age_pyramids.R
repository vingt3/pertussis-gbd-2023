source(file.path(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))), "utils.R"))
require_packages(c("tidyverse", "patchwork", "scales"))
paths <- repository_paths(); ensure_output_dir(paths$output)
ages <- c("<28 days", "1-5 months", "6-11 months", "12-23 months", "2-4 years", "5-9 years", "10-14 years", "15-19 years", "20-24 years", "25-29 years", "30-34 years", "35-39 years", "40-44 years", "45-49 years", "50-54 years", "55-59 years", "60-64 years", "65-69 years", "70-74 years", "75-79 years", "80+ years")
location_target <- Sys.getenv("LOCATION_TARGET", "Global")
burden <- bind_rows(readr::read_csv(file.path(paths$data, "age_specific_burden_1990.csv"), show_col_types = FALSE), readr::read_csv(file.path(paths$data, "age_specific_burden_2023.csv"), show_col_types = FALSE)) |>
  filter(location == location_target, sex == "Both") |> mutate(age = factor(age, levels = ages))
plot_pyramid <- function(measure, metric, display) {
  d <- burden |> filter(measure == !!measure, metric == !!metric) |> transmute(year, age, value = if_else(year == 1990, -val, val), lower = if_else(year == 1990, -upper, lower), upper = if_else(year == 1990, -lower, upper), year = factor(year))
  limit <- max(abs(c(d$lower, d$upper)), na.rm = TRUE) * 1.1
  ggplot(d, aes(value, age, fill = year)) + geom_col(width = .7) + geom_errorbarh(aes(xmin = lower, xmax = upper), height = .3, linewidth = .3) +
    scale_fill_manual(values = c("1990" = "#E8873D", "2023" = "#5B9BD5")) +
    scale_x_continuous(limits = c(-limit, limit), labels = function(x) comma(abs(x))) +
    labs(title = display, x = ifelse(metric == "Number", paste(display, "– number"), paste(display, "– rate per 100,000")), y = NULL) +
    theme_minimal(base_size = 11) + theme(plot.title = element_text(face = "bold", hjust = .5), legend.position = "none", panel.grid.major.y = element_blank())
}
measures <- c("Incidence", "Deaths", "DALYs (Disability-Adjusted Life Years)")
labels <- c("Incidence", "Deaths", "DALYs")
number <- purrr::map2(measures, labels, ~plot_pyramid(.x, "Number", .y))
rate <- purrr::map2(measures, labels, ~plot_pyramid(.x, "Rate", .y))
figure <- (number[[1]] / number[[2]] / number[[3]]) | (rate[[1]] / rate[[2]] / rate[[3]])
safe_location <- gsub("[^A-Za-z0-9]+", "_", tolower(location_target))
ggsave(file.path(paths$output, paste0("age_pyramid_", safe_location, ".png")), figure, width = 22, height = 16, dpi = 300, bg = "white")
ggsave(file.path(paths$output, paste0("age_pyramid_", safe_location, ".pdf")), figure, width = 22, height = 16, bg = "white")
