#' Retrieve the relevant results
#'
#' @inheritParams prepare_dataset
#' @inheritParams n2kanalysis::store_model
#' @param source_repo A [git2rdata::repository()] object containing the raw data
#' @param target_repo A [git2rdata::repository()] object containing the package
#' code.
#' @export
#' @importFrom dplyr count inner_join mutate select
#' @importFrom git2rdata read_vc write_vc
#' @importFrom n2kanalysis get_result read_manifest
#' @importFrom rlang .data
#' @importFrom stats qnorm
#' @importFrom stringr str_detect str_replace
#' @importFrom utils file_test
retrieve_results <- function(
  base, project, source_repo, target_repo, verbose = TRUE, strict = TRUE
) {
  file.path(target_repo$path, "..", "inst", "results.rds") |>
    normalizePath() -> target_rds
  if (!file_test("-f", target_rds)) {
    read_manifest(base = base, project = project) |>
      get_result(base = base, project = project, verbose = verbose) |>
      saveRDS(file = target_rds)
  }
  results <- readRDS(target_rds)

  file.path("location", "stratum") |>
    read_vc(root = source_repo) |>
    write_vc(
      file = file.path("inst", "results", "stratum"), root = target_repo,
      sorting = "description", strict = strict
    )

  # overview of the effort
  file.path("observation", "visit") |>
    read_vc(root = source_repo) |>
    inner_join(
      read_vc(file.path("location", "point"), root = source_repo),
      by = c("point_id" = "id")
    ) |>
    inner_join(
      read_vc(file.path("location", "square"), root = source_repo),
      by = c("square_id" = "id")
    ) |>
    inner_join(
      read_vc(file.path("location", "stratum"), root = source_repo),
      by = c("stratum_id" = "id")
    ) |>
    count(
      .data$year, location = .data$square_id, stratum = .data$description,
      name = "visits"
    ) |>
    mutate(stratum = factor(.data$stratum)) |>
    write_vc(
      file = file.path("inst", "results", "effort"), root = target_repo,
      sorting = c("stratum", "location", "year"), strict = strict
    )
  file.path("inst", "results", "effort") |>
    update_metadata(
      root = target_repo, name = "effort", title = "Sampling effort",
      field_description = c(
        year = "year of the survey", location = "identifier of the location",
        stratum = "name of the stratum",
        visits = "number of visited point locations"
      )
    )

  # analysis metadata
  results@AnalysisMetadata |>
    inner_join(
      read_vc(file.path("species", "speciesgroup"), source_repo) |>
        select("id", speciesgroup = "description"),
      by = c("species_group_id" = "id")
    ) |>
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
      ) |>
        factor(),
      .data$status_fingerprint, status = factor(.data$status)
    ) |>
    left_join(
      results@Parameter |>
        filter(.data$description == "WAIC") |>
        semi_join(
          x = results@ParameterEstimate, by = c("parameter" = "fingerprint")
        ) |>
        select("analysis", waic = "estimate"),
      by = "analysis"
    ) |>
    write_vc(
      file = file.path("inst", "results", "meta"), root = target_repo,
      sorting = "analysis", strict = strict
    )
  file.path("inst", "results", "meta") |>
    update_metadata(
      root = target_repo, name = "meta", title = "Metadata on the analyses",
      field_description = c(
        analysis = "Unique identifier of the analysis",
        speciesgroup = "Name of the species or species group",
        composite = "TRUE indicates a species group, FALSE a species",
        cycle = "TRUE indicates a three year cycle, FALSE yearly cycle",
        linear = "TRUE indicates a linear trend, FALSE a nonlinear trend",
        family = "Statistical distribution used in the analysis",
        status_fingerprint = "Status fingerprint of the analysis",
        status = "Status of the analysis",
        waic = "Wantabe-Akaike Information Criterion"
      )
    )

  results@AnalysisRelation |>
    transmute(
      analysis = factor(.data$analysis), parent = .data$parent_analysis
    ) |>
    write_vc(
      file = file.path("inst", "results", "parent"), root = target_repo,
      sorting = c("analysis", "parent"), strict = strict
    )
  file.path("inst", "results", "parent") |>
    update_metadata(
      file = file.path("inst", "results", "parent"), root = target_repo,
      name = "parent", title = "Parent-child links between analyses",
      description = "Lists only analyses with at least one parent",
      field_description = c(
        analysis = "Identifier of the child analysis.",
        parent = "Identifier of the parent analysis."
      )
    )

  # differences among year or cycle
  results@Contrast |>
    filter(str_detect(.data$description, "^index")) |>
    mutate(
      reference = str_replace(.data$description, "index_(.*)_.*", "\\1") |>
        as.integer(),
      alternative = str_replace(.data$description, "index_.*_(.*)", "\\1") |>
        as.integer()
    ) |>
    inner_join(results@ContrastEstimate, by = c("fingerprint" = "contrast")) |>
    mutate(
      s = (.data$upper_confidence_limit - .data$lower_confidence_limit) /
        (2 * qnorm(0.975)),
      lower_confidence_limit = qnorm(0.05, mean = .data$estimate, sd = .data$s),
      upper_confidence_limit = qnorm(0.95, mean = .data$estimate, sd = .data$s)
    ) |>
    transmute(
      analysis = factor(.data$analysis), .data$reference, .data$alternative,
      estimate = round(.data$estimate, 4),
      lower_confidence_limit = round(.data$lower_confidence_limit, 4),
      upper_confidence_limit = round(.data$upper_confidence_limit, 4)
    ) |>
    write_vc(
      file = file.path("inst", "results", "index"), root = target_repo,
      sorting = c("analysis", "reference", "alternative"), strict = strict
    )
  file.path("inst", "results", "index") |>
    update_metadata(
      root = target_repo, name = "index", title = "Estimated indices",
      field_description = c(
        analysis = "Identifier of the analysis",
        reference = "Year or period used as reference",
        alternative = "Year of period compared with the reference",
        estimate = "Estimate of the different on the log-scale",
        lower_confidence_limit = "5% quantile of the distribution of the index",
        upper_confidence_limit = "95% quantile of the distribution of the index"
      )
    )

  # estimated average count per year or cycle
  results@Contrast |>
    filter(str_detect(.data$description, "^estimate")) |>
    mutate(
      year = str_replace(.data$description, "estimate_(.*)", "\\1") |>
        as.integer()
    ) |>
    inner_join(results@ContrastEstimate, by = c("fingerprint" = "contrast")) |>
    mutate(
      s = (.data$upper_confidence_limit - .data$lower_confidence_limit) /
        (2 * qnorm(0.975)),
      lower_confidence_limit = qnorm(0.05, mean = .data$estimate, sd = .data$s),
      upper_confidence_limit = qnorm(0.95, mean = .data$estimate, sd = .data$s)
    ) |>
    transmute(
      analysis = factor(.data$analysis), .data$year,
      estimate = round(.data$estimate, 4),
      lower_confidence_limit = round(.data$lower_confidence_limit, 4),
      upper_confidence_limit = round(.data$upper_confidence_limit, 4)
    ) |>
    write_vc(
      file = file.path("inst", "results", "estimate"), root = target_repo,
      sorting = c("analysis", "year"), strict = strict
    )
  file.path("inst", "results", "estimate") |>
    update_metadata(
      root = target_repo, name = "estimate",
      title = "Estimate average number of bird per point",
      field_description = c(
        analysis = "Identifier of the analysis",
        year = "Year or period of the estimate",
        estimate = "Estimate on the log-scale",
        lower_confidence_limit =
          "5% quantile of the distribution on the log-scale",
        upper_confidence_limit =
          "95% quantile of the distribution on the log-scale"
      )
    )

  # trend in moving window
  results@Contrast |>
    filter(str_detect(.data$description, "^trend")) |>
    mutate(
      midpoint = str_replace(.data$description, "trend_(.*)_.*", "\\1") |>
        as.numeric(),
      window = str_replace(.data$description, "trend_.*_(.*)", "\\1") |>
        as.integer()
    ) |>
    inner_join(results@ContrastEstimate, by = c("fingerprint" = "contrast")) |>
    mutate(
      s = (.data$upper_confidence_limit - .data$lower_confidence_limit) /
        (2 * qnorm(0.975)),
      lower_confidence_limit = qnorm(0.05, mean = .data$estimate, sd = .data$s),
      upper_confidence_limit = qnorm(0.95, mean = .data$estimate, sd = .data$s)
    ) |>
    transmute(
      analysis = factor(.data$analysis), .data$midpoint, .data$window,
      estimate = round(.data$estimate, 4),
      lower_confidence_limit = round(.data$lower_confidence_limit, 4),
      upper_confidence_limit = round(.data$upper_confidence_limit, 4)
    ) |>
    write_vc(
      file = file.path("inst", "results", "moving_trend"), root = target_repo,
      sorting = c("analysis", "window", "midpoint"), strict = strict
    )
  file.path("inst", "results", "moving_trend") |>
    update_metadata(
      root = target_repo, name = "moving_trend",
      title = "Trend for different moving windows",
      field_description = c(
        analysis = "Identifier of the analysis",
        midpoint = "Year in the middle of the interval",
        window = "Width of the interval as a number of years",
        estimate = "Estimate on the log-scale",
        lower_confidence_limit =
          "5% quantile of the distribution on the log-scale",
        upper_confidence_limit =
          "95% quantile of the distribution on the log-scale"
      )
    )

  # linear trend
  results@Contrast |>
    filter(str_detect(.data$description, "^linear_trend")) |>
    inner_join(results@ContrastEstimate, by = c("fingerprint" = "contrast")) |>
    mutate(
      s = (.data$upper_confidence_limit - .data$lower_confidence_limit) /
        (2 * qnorm(0.975)),
      lower_confidence_limit = qnorm(0.05, mean = .data$estimate, sd = .data$s),
      upper_confidence_limit = qnorm(0.95, mean = .data$estimate, sd = .data$s)
    ) |>
    transmute(
      analysis = factor(.data$analysis), estimate = round(.data$estimate, 4),
      lower_confidence_limit = round(.data$lower_confidence_limit, 4),
      upper_confidence_limit = round(.data$upper_confidence_limit, 4)
    ) |>
    write_vc(
      file = file.path("inst", "results", "linear_trend"), root = target_repo,
      sorting = "analysis", strict = strict
    )
  file.path("inst", "results", "linear_trend") |>
    update_metadata(
      root = target_repo, name = "linear_trend",
      title = "Slope of the linear trend",
      field_description = c(
        analysis = "Identifier of the analysis",
        estimate = "Estimate on the log-scale",
        lower_confidence_limit =
          "5% quantile of the distribution on the log-scale",
        upper_confidence_limit =
          "95% quantile of the distribution on the log-scale"
      )
    )

  # estimate along linear trend
  results@Contrast |>
    filter(str_detect(.data$description, "^linear_estimate")) |>
    mutate(
      year = str_replace(.data$description, "linear_estimate_(.*)", "\\1") |>
        as.integer()
    ) |>
    inner_join(results@ContrastEstimate, by = c("fingerprint" = "contrast")) |>
    mutate(
      s = (.data$upper_confidence_limit - .data$lower_confidence_limit) /
        (2 * qnorm(0.975)),
      lower_confidence_limit = qnorm(0.05, mean = .data$estimate, sd = .data$s),
      upper_confidence_limit = qnorm(0.95, mean = .data$estimate, sd = .data$s)
    ) |>
    transmute(
      analysis = factor(.data$analysis), .data$year,
      estimate = round(.data$estimate, 4),
      lower_confidence_limit = round(.data$lower_confidence_limit, 4),
      upper_confidence_limit = round(.data$upper_confidence_limit, 4)
    ) |>
    write_vc(
      file = file.path("inst", "results", "linear_estimate"),
      root = target_repo, sorting = c("analysis", "year"), strict = strict
    )
  file.path("inst", "results", "linear_estimate") |>
    update_metadata(
      root = target_repo, name = "linear_estimate",
      title = "Estimates along a linear trend",
      field_description = c(
        analysis = "Identifier of the analysis",
        year = "Year of estimate, centered to the start of the analysis",
        estimate = "Estimate on the log-scale",
        lower_confidence_limit =
          "5% quantile of the distribution on the log-scale",
        upper_confidence_limit =
          "95% quantile of the distribution on the log-scale"
      )
    )

  # composite index
  results@AnalysisMetadata |>
    filter(str_detect(.data$model_type, "composite")) |>
    semi_join(
      x = results@ParameterEstimate,
      by = c("analysis" = "file_fingerprint")
    ) |>
    inner_join(
      results@Parameter |>
        filter(str_detect(.data$description, "^index")) |>
        mutate(
          reference = str_replace(.data$description, "index_(.*)_.*", "\\1") |>
            as.integer(),
          alternative = str_replace(
            .data$description, "index_.*_(.*)", "\\1"
          ) |>
            as.integer()
        ),
      by = c("parameter" = "fingerprint")
    ) |>
    mutate(
      s = (.data$upper_confidence_limit - .data$lower_confidence_limit) /
        (2 * qnorm(0.975)),
      lower_confidence_limit = qnorm(0.05, mean = .data$estimate, sd = .data$s),
      upper_confidence_limit = qnorm(0.95, mean = .data$estimate, sd = .data$s)
    ) |>
    transmute(
      analysis = factor(.data$analysis), .data$reference, .data$alternative,
      estimate = round(.data$estimate, 4),
      lower_confidence_limit = round(.data$lower_confidence_limit, 4),
      upper_confidence_limit = round(.data$upper_confidence_limit, 4)
    ) |>
    write_vc(
      file = file.path("inst", "results", "composite_index"), strict = strict,
      root = target_repo, sorting = c("analysis", "reference", "alternative")
    )
  file.path("inst", "results", "composite_index") |>
    update_metadata(
      root = target_repo, name = "composite_index",
      title = "Indices by species group",
      field_description = c(
        analysis = "Identifier of the analysis",
        reference = "Year or period used as reference",
        alternative = "year or period to compare with the reference",
        estimate = "Estimate on the log-scale",
        lower_confidence_limit =
          "5% quantile of the distribution on the log-scale",
        upper_confidence_limit =
          "95% quantile of the distribution on the log-scale"
      )
    )

  # trend in moving window
  results@Parameter |>
    filter(str_detect(.data$description, "^trend")) |>
    mutate(
      midpoint = str_replace(.data$description, "trend_(.*)_.*", "\\1") |>
        as.numeric(),
      window = str_replace(.data$description, "trend_.*_(.*)", "\\1") |>
        as.integer()
    ) |>
    inner_join(
      results@ParameterEstimate, by = c("fingerprint" = "parameter")
    ) |>
    semi_join(
      results@AnalysisMetadata |>
        filter(str_detect(.data$model_type, "composite")),
      by = c("analysis" = "file_fingerprint")
    ) |>
    mutate(
      s = (.data$upper_confidence_limit - .data$lower_confidence_limit) /
        (2 * qnorm(0.975)),
      lower_confidence_limit = qnorm(0.05, mean = .data$estimate, sd = .data$s),
      upper_confidence_limit = qnorm(0.95, mean = .data$estimate, sd = .data$s)
    ) |>
    transmute(
      .data$analysis, .data$midpoint, .data$window,
      estimate = round(.data$estimate, 4),
      lower_confidence_limit = round(.data$lower_confidence_limit, 4),
      upper_confidence_limit = round(.data$upper_confidence_limit, 4)
    ) |>
    write_vc(
      file = file.path("inst", "results", "composite_moving_trend"),
      root = target_repo, sorting = c("analysis", "window", "midpoint"),
      strict = strict
    )
  file.path("inst", "results", "composite_moving_trend") |>
    update_metadata(
      root = target_repo, name = "composite_moving_trend",
      title  = "Moving trend for the composite indices",
      field_description = c(
        analysis = "Identifier of the analysis",
        midpoint = "Year in the middle of the interval",
        window = "Width of the interval as a number of years",
        estimate = "Estimate on the log-scale",
        lower_confidence_limit =
          "5% quantile of the distribution on the log-scale",
        upper_confidence_limit =
          "95% quantile of the distribution on the log-scale"
      )
    )

  # stratum weights
  read_relevant(base = base, project = project, verbose = verbose) |>
    write_vc(
      file = file.path("inst", "results", "stratum_weight"),
      root = target_repo, sorting = c("analysis", "stratum"), strict = strict
    )
  file.path("inst", "results", "stratum_weight") |>
    update_metadata(
      root = target_repo, name = "stratum_weight"
    )
  return(invisible(NULL))
}
