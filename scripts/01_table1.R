source(file.path(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))), "utils.R"))
require_packages(c("dplyr", "writexl", "readr"))
paths <- repository_paths(); ensure_output_dir(paths$output)

format_ui <- function(value, lower, upper, digits = 0) {
  sprintf(paste0("%.", digits, "f (%.", digits, "f to %.", digits, "f)"), value, lower, upper)
}
eapc <- function(df) {
  fit <- lm(log(val) ~ year, data = df)
  beta <- coef(fit)[2]; se <- summary(fit)$coefficients[2, 2]
  sprintf("%.2f (%.2f to %.2f)", (exp(beta)-1)*100, (exp(beta-1.96*se)-1)*100, (exp(beta+1.96*se)-1)*100)
}
summarise_measure <- function(df, measure_name, output_label, source_location = NULL) {
  if (!is.null(source_location)) df <- df |> dplyr::filter(location == source_location)
  number <- df |> dplyr::filter(age == "All ages", metric == "Number") |> dplyr::arrange(year)
  rate <- df |> dplyr::filter(age == "Age-standardized", metric == "Rate") |> dplyr::arrange(year)
  baseline <- number |> dplyr::filter(year == 1990); final <- number |> dplyr::filter(year == 2023)
  final_rate <- rate |> dplyr::filter(year == 2023)
  tibble::tibble(
    Location = output_label,
    measure = measure_name,
    number_2023 = format_ui(final$val, final$lower, final$upper),
    number_change = sprintf("%.2f%%", 100 * (final$val - baseline$val) / baseline$val),
    rate_2023 = format_ui(final_rate$val, final_rate$lower, final_rate$upper, digits = 2),
    eapc = eapc(rate)
  )
}
read_measure <- function(filename, measure_label) {
  readr::read_csv(file.path(paths$data, filename), show_col_types = FALSE) |>
    dplyr::filter(sex == "Both") |>
    dplyr::mutate(measure = measure_label)
}

regional <- dplyr::bind_rows(
  read_measure("region_dalys.csv", "DALYs"),
  read_measure("region_deaths.csv", "Deaths"),
  read_measure("region_incidence.csv", "Incidence")
)
regional_rows <- regional |> dplyr::distinct(location) |> dplyr::pull(location) |>
  lapply(function(loc) dplyr::bind_rows(lapply(unique(regional$measure), function(m) summarise_measure(dplyr::filter(regional, measure == m), m, loc, loc)))) |>
  dplyr::bind_rows()

global <- readr::read_csv(file.path(paths$data, "global_burden.csv"), show_col_types = FALSE)
global_rows <- lapply(c("Both", "Male", "Female"), function(sex_name) {
  d <- global |> dplyr::filter(sex == sex_name) |>
    dplyr::mutate(measure = dplyr::recode(measure, "DALYs (Disability-Adjusted Life Years)" = "DALYs"))
  lapply(unique(d$measure), function(m) summarise_measure(dplyr::filter(d, measure == m), m, paste("Global", sex_name, sep = "_"))) |> dplyr::bind_rows()
}) |> dplyr::bind_rows()

all_rows <- dplyr::bind_rows(global_rows, regional_rows) |>
  tidyr::pivot_wider(names_from = measure, values_from = c(number_2023, number_change, rate_2023, eapc), names_glue = "{measure}_{.value}")
readr::write_csv(all_rows, file.path(paths$output, "table_1.csv"))
writexl::write_xlsx(all_rows, file.path(paths$output, "table_1.xlsx"))
