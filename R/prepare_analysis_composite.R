#' Prepare the analysis files for the composite indices
#' @inheritParams prepare_analysis_dataset
#' @inheritParams prepare_analysis_comparison
#' @inheritParams prepare_dataset
#' @inheritParams n2kanalysis::store_model
#' @param type the model type
#' @export
#' @importFrom assertthat assert_that is.string has_name
#' @importFrom n2kanalysis n2k_composite
#' @importFrom dplyr %>% add_rownames select mutate arrange filter slice
#' @importFrom rlang .data
prepare_analysis_composite <- function(
  frequency, type, species_group, location_group, models,
  base, project, overwrite = FALSE
){
  assert_that(is.string(frequency))
  assert_that(is.string(type))
  assert_that(is.string(species_group))
  assert_that(is.string(location_group))
  assert_that(
    inherits(models, "data.frame"),
    has_name(models, "fingerprint"),
    has_name(models, "status_fingerprint"),
    has_name(models, "status"),
    has_name(models, "seed"),
    has_name(models, "scheme"),
    has_name(models, "first_imported_year"),
    has_name(models, "last_imported_year"),
    has_name(models, "analysis_date")
  )
  message(location_group, " ", species_group, " ", frequency, " ", "type")
  flush.console()

  extractor <- function(model){
    if (!require(dplyr)) {
      stop("'dplyr' is required")
    }
    if (!require(INLA)) {
      stop("'INLA' is required")
    }
    parameter <- model$summary.lincomb.derived %>%
      add_rownames("Value") %>%
      select("Value", Estimate = "mean", Variance = "sd") %>%
      mutate(
        Variance = .data$Variance ^ 2,
        Number = suppressWarnings(as.integer(.data$Value))
      ) %>%
      arrange("Number", "Value") %>%
      select(-"Number")
    reference <- parameter %>%
      filter(.data$Value != "Trend") %>%
      slice(1)
    parameter %>%
      mutate(
        Estimate =
          ifelse(
            .data$Value == "Trend",
            .data$Estimate,
            .data$Estimate - reference$Estimate
          )
      )
  }

  n2k_composite(
    result.datasource.id = models$result_datasource[1],
    parent = models$fingerprint,
    parent.status = models %>%
      select(
        ParentAnalysis = "fingerprint",
        ParentStatusFingerprint = "status_fingerprint",
        ParentStatus = "status"
      ),
    seed = models$seed[1],
    scheme.id = models$scheme[1],
    species.group.id = species_group,
    location.group.id = location_group,
    first.imported.year = models$first_imported_year[1],
    last.imported.year = models$last_imported_year[1],
    model.type = paste("composite index:", frequency, type),
    formula = paste("~", frequency),
    extractor = extractor,
    analysis.date = max(models$analysis_date)
  ) %>%
    storage(base = base, project = project, overwrite = overwrite)
}
