read_relevant <- function(base, project, verbose = TRUE) {
  UseMethod("read_relevant", base)
}

#' @importFrom dplyr mutate
#' @importFrom n2kanalysis display
#' @importFrom purrr map_dfr
#' @importFrom rlang .data
#' @importFrom stringr str_remove str_subset
read_relevant.character <- function(base, project, verbose = TRUE) {
  file.path(base, project) |>
    list.files(pattern = "rds$", recursive = TRUE, full.names = TRUE) |>
    str_subset("converged") -> todo
  map_dfr(todo, verbose = verbose, function(i, verbose) {
    display(verbose, i)
    readRDS(i) |>
      extract_relevant()
  }) |>
    mutate(analysis = factor(.data$analysis))
}

#' @importFrom dplyr filter transmute
#' @importFrom n2kanalysis display read_manifest
#' @importFrom purrr map_dfr
#' @importFrom stringr str_subset
#' @importFrom rlang .data
read_relevant.manifest <- function(base, project, verbose = TRUE) {
  read_manifest(base = base, project = project) |>
    slot("Manifest") |>
    filter(is.na(.data$parent)) -> todo
  map_dfr(
    todo$fingerprint, verbose = verbose,
    function(i, verbose) {
      display(verbose, i)
      read_model(x = i, base = base, project = project) |>
        extract_relevant()
    }
  ) |>
    mutate(analysis = factor(.data$analysis))
}

#' @importFrom dplyr count group_by inner_join mutate select summarise
#' @importFrom n2kanalysis get_data get_file_fingerprint
#' @importFrom stringr str_remove
#' @importFrom rlang .data
#' @importFrom tidyr complete pivot_wider
extract_relevant <- function(x) {
  if (!inherits(x, "n2kInla")) {
    return(NULL)
  }
  if (!grepl("year:stratum", x@AnalysisMetadata$model_type)) {
    return(NULL)
  }
  get_data(x) |>
    mutate(tmp = sprintf("%s_%0.5f", .data$stratum, .data$weight)) |>
    count(
      .data$tmp, .data$square, .data$point, name = "visits"
    ) |>
    group_by(.data$tmp, .data$square) |>
    summarise(
      points = n(), visits = sum(.data$visits), .groups = "drop"
    ) |>
    group_by(.data$tmp, .data$points) |>
    summarise(
      squares = n(), visits = sum(.data$visits), .groups = "drop"
    ) -> tmp
  tmp |>
    select(-"visits") |>
    complete(
      .data$tmp, points = 1:6, fill = list(squares = 0)
    ) |>
    mutate(points = sprintf("points_%i", .data$points)) |>
    pivot_wider(names_from = "points", values_from = "squares") |>
    inner_join(
      tmp |>
        group_by(.data$tmp) |>
        summarise(
          relevant = sum(.data$squares), visits = sum(.data$visits),
          .groups = "drop"
        ),
      by = "tmp"
    ) |>
    mutate(
      analysis = get_file_fingerprint(x),
      stratum = str_remove(.data$tmp, "_.*"),
      weight = str_remove(.data$tmp, ".*_") |>
        as.numeric()
    ) |>
    select(-"tmp")
}
