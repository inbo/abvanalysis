#' Retrieve the relevant results
#'
#' @inheritParams prepare_dataset
#' @inheritParams n2kanalysis::store_model
#' @param source_repo A [git2rdata::repository()] object containing the raw data
#' @param target_repo A [git2rdata::repository()] object containing the package
#' code.
#' @export
#' @importFrom dplyr %>% count inner_join mutate select
#' @importFrom git2rdata read_vc write_vc
#' @importFrom n2kanalysis get_result
#' @importFrom rlang .data
#' @importFrom stringr str_detect str_replace
#' @importFrom utils file_test
retrieve_results <- function(
    base, project, source_repo, target_repo, verbose = TRUE, strict = TRUE
) {
  if (file_test("-f", file.path(base, project, "results.rds"))) {
    file.path(base, paste(project, "results.rds", sep = "_")) %>%
      readRDS() -> results
  } else {
    results <- get_result(file.path(base, project), verbose = verbose)
    file.path(base, paste(project, "results.rds", sep = "_")) %>%
      saveRDS(object = results)
  }

  # overview of the effort
  read_vc(file.path("observation", "visit"), root = source_repo) %>%
    inner_join(
      read_vc(file.path("location", "point"), root = source_repo),
      by = c("point_id" = "id")
    ) %>%
    inner_join(
      read_vc(file.path("location", "square"), root = source_repo),
      by = c("square_id" = "id")
    ) %>%
    inner_join(
      read_vc(file.path("location", "stratum"), root = source_repo),
      by = c("stratum_id" = "id")
    ) %>%
    count(
      .data$year, location = .data$square_id, stratum = .data$description,
      name = "visits"
    ) %>%
    mutate(stratum = factor(.data$stratum)) %>%
    write_vc(
      file = file.path("inst", "results", "effort"), root = target_repo,
      sorting = c("stratum", "location", "year"), strict = strict
    )

  # analysis metadata
  results@AnalysisMetadata %>%
    inner_join(
      read_vc(file.path("species", "speciesgroup"), source_repo) %>%
        select(.data$id, speciesgroup = .data$description),
      by = c("species_group_id" = "id")
    ) %>%
    transmute(
      analysis = .data$file_fingerprint,
      speciesgroup = factor(.data$speciesgroup),
      composite = str_detect(.data$model_type, "composite"),
      cycle = str_detect(.data$model_type, "cycle"),
      linear = str_detect(.data$model_type, "(RW1|non linear)", TRUE),
      family = ifelse(
        str_detect(.data$model_type, "composite index: .*"),
        NA,
        str_replace(.data$model_type, "inla (.*): .*", "\\1")
      ) %>%
        factor(),
      .data$status_fingerprint, status = factor(.data$status)
    ) %>%
    write_vc(
      file = file.path("inst", "results", "meta"), root = target_repo,
      sorting = "analysis", strict = strict
    )
  # differences among year / cycle
  results@Contrast %>%
    filter(str_detect(.data$description, "^index")) %>%
    mutate(
      reference = str_replace(.data$description, "index_(.*)_.*", "\\1") %>%
        as.integer(),
      alternative = str_replace(.data$description, "index_.*_(.*)", "\\1") %>%
        as.integer()
    ) %>%
    inner_join(results@ContrastEstimate, by = c("fingerprint" = "contrast")) %>%
    transmute(
      analysis = factor(.data$analysis), .data$reference, .data$alternative,
      estimate = round(.data$estimate, 4),
      lower_confidence_limit = round(.data$lower_confidence_limit, 4),
      upper_confidence_limit = round(.data$upper_confidence_limit, 4)
    ) %>%
    write_vc(
      file = file.path("inst", "results", "index"), root = target_repo,
      sorting = c("analysis", "reference", "alternative"), strict = strict
    )
  # estimated average count per year / cycle
  results@Contrast %>%
    filter(str_detect(.data$description, "^estimate")) %>%
    mutate(
      year = str_replace(.data$description, "estimate_(.*)", "\\1") %>%
        as.integer()
    ) %>%
    inner_join(results@ContrastEstimate, by = c("fingerprint" = "contrast")) %>%
    transmute(
      analysis = factor(.data$analysis), .data$year,
      estimate = round(.data$estimate, 4),
      lower_confidence_limit = round(.data$lower_confidence_limit, 4),
      upper_confidence_limit = round(.data$upper_confidence_limit, 4)
    ) %>%
    write_vc(
      file = file.path("inst", "results", "estimate"), root = target_repo,
      sorting = c("analysis", "year"), strict = strict
    )
  # trend in moving window
  results@Contrast %>%
    filter(str_detect(.data$description, "^trend")) %>%
    mutate(
      midpoint = str_replace(.data$description, "trend_(.*)_.*", "\\1") %>%
        as.numeric(),
      window = str_replace(.data$description, "trend_.*_(.*)", "\\1") %>%
        as.integer()
    ) %>%
    inner_join(results@ContrastEstimate, by = c("fingerprint" = "contrast")) %>%
    transmute(
      analysis = factor(.data$analysis), .data$midpoint, .data$window,
      estimate = round(.data$estimate, 4),
      lower_confidence_limit = round(.data$lower_confidence_limit, 4),
      upper_confidence_limit = round(.data$upper_confidence_limit, 4)
    ) %>%
    write_vc(
      file = file.path("inst", "results", "moving_trend"), root = target_repo,
      sorting = c("analysis", "window", "midpoint"), strict = strict
    )
  # linear trend
  results@Contrast %>%
    filter(str_detect(.data$description, "^linear_trend")) %>%
    inner_join(results@ContrastEstimate, by = c("fingerprint" = "contrast")) %>%
    transmute(
      analysis = factor(.data$analysis), estimate = round(.data$estimate, 4),
      lower_confidence_limit = round(.data$lower_confidence_limit, 4),
      upper_confidence_limit = round(.data$upper_confidence_limit, 4)
    ) %>%
    write_vc(
      file = file.path("inst", "results", "linear_trend"), root = target_repo,
      sorting = "analysis", strict = strict
    )
  # estimate along linear trend
  results@Contrast %>%
    filter(str_detect(.data$description, "^linear_estimate")) %>%
    mutate(
      year = str_replace(.data$description, "linear_estimate_(.*)", "\\1") %>%
        as.integer()
    ) %>%
    inner_join(results@ContrastEstimate, by = c("fingerprint" = "contrast")) %>%
    transmute(
      analysis = factor(.data$analysis), .data$year,
      estimate = round(.data$estimate, 4),
      lower_confidence_limit = round(.data$lower_confidence_limit, 4),
      upper_confidence_limit = round(.data$upper_confidence_limit, 4)
    ) %>%
    write_vc(
      file = file.path("inst", "results", "linear_estimate"),
      root = target_repo, sorting = c("analysis", "year"), strict = strict
    )

  # composite index
  results@Parameter %>%
    filter(str_detect(.data$description, "^index")) %>%
    mutate(
      reference = str_replace(.data$description, "index_(.*)_.*", "\\1") %>%
        as.integer(),
      alternative = str_replace(.data$description, "index_.*_(.*)", "\\1") %>%
        as.integer()
    ) %>%
    inner_join(results@ParameterEstimate, by = c("fingerprint" = "parameter")) %>%
    transmute(
      analysis = factor(.data$analysis), .data$reference, .data$alternative,
      estimate = round(.data$estimate, 4),
      lower_confidence_limit = round(.data$lower_confidence_limit, 4),
      upper_confidence_limit = round(.data$upper_confidence_limit, 4)
    ) %>%
    write_vc(
      file = file.path("inst", "results", "composite_index"), strict = strict,
      root = target_repo, sorting = c("analysis", "reference", "alternative")
    )
  # trend in moving window
  results@Parameter %>%
    filter(str_detect(.data$description, "^trend")) %>%
    mutate(
      midpoint = str_replace(.data$description, "trend_(.*)_.*", "\\1") %>%
        as.numeric(),
      window = str_replace(.data$description, "trend_.*_(.*)", "\\1") %>%
        as.integer()
    ) %>%
    inner_join(
      results@ParameterEstimate, by = c("fingerprint" = "parameter")
    ) %>%
    transmute(
      .data$analysis, .data$midpoint, .data$window,
      estimate = round(.data$estimate, 4),
      lower_confidence_limit = round(.data$lower_confidence_limit, 4),
      upper_confidence_limit = round(.data$upper_confidence_limit, 4)
    ) %>%
    write_vc(
      file = file.path("inst", "results", "composite_moving_trend"),
      root = target_repo, sorting = c("analysis", "window", "midpoint"),
      strict = strict
    )

  return(invisible(NULL))
}
