repository_paths <- function() {
  command_file <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  script_file <- if (length(command_file)) sub("^--file=", "", command_file[[1]]) else getwd()
  root <- normalizePath(file.path(dirname(script_file), ".."), mustWork = FALSE)
  list(root = root, data = file.path(root, "data"), output = file.path(root, "outputs"))
}

require_packages <- function(packages) {
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) stop("Install required R packages: ", paste(missing, collapse = ", "))
  invisible(lapply(packages, library, character.only = TRUE))
}

ensure_output_dir <- function(path) {
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
  path
}
