#' Prepare the analysis files for the composite indices
#' @inheritParams prepare_analysis_dataset
#' @inheritParams prepare_analysis_comparison
#' @inheritParams prepare_dataset
#' @inheritParams n2kanalysis::store_model
#' @param type the model type
#' @export
#' @importFrom assertthat assert_that is.string has_name
#' @importFrom n2kanalysis n2k_composite
#' @importFrom dplyr %>% select mutate arrange filter slice
#' @importFrom rlang .data
#' @importFrom tibble rownames_to_column
prepare_analysis_composite <- function(
  frequency, type, species_group_id, location_group_id, models, base, project,
  scheme_id = "ABV", seed = 20070315, overwrite = FALSE, verbose = TRUE
){
  assert_that(
    is.string(frequency), is.string(type), is.string(species_group_id),
    is.string(location_group_id)
  )
  assert_that(
    inherits(models, "data.frame"), has_name(models, "fingerprint"),
    has_name(models, "status_fingerprint"), has_name(models, "status"),
    has_name(models, "first_imported_year"),
    has_name(models, "last_imported_year"), has_name(models, "analysis_date")
  )
  display(
    verbose = verbose,
    message = c(
      location_group_id, " ", species_group_id, " ", frequency, " ", type
    )
  )

  extractor <- function(model) {
    stopifnot(require(tibble, quietly = TRUE))
    stopifnot(require(dplyr, quietly = TRUE))
    stopifnot(require(INLA, quietly = TRUE))
    parameter <- model$summary.lincomb.derived %>%
      rownames_to_column("Value") %>%
      select("Value", Estimate = "mean", Variance = "sd") %>%
      mutate(
        Variance = .data$Variance ^ 2,
        Number = suppressWarnings(as.integer(.data$Value))
      ) %>%
      arrange(.data$Number, .data$Value) %>%
      select(-"Number")
  }

  n2k_composite(
    result_datasource_id = models$result_datasource[1],
    parent_status = models %>%
      select(
        parent_analysis = "fingerprint", parent_status = "status",
        parentstatus_fingerprint = "status_fingerprint"
      ),
    seed = seed, scheme_id = scheme_id, species_group_id = species_group_id,
    location_group_id = location_group_id,
    first_imported_year = models$first_imported_year[1],
    last_imported_year = models$last_imported_year[1],
    model_type = paste("composite index:", frequency, type),
    formula = paste("~", frequency), extractor = extractor,
    analysis_date = max(models$analysis_date), status = "waiting"
  ) %>%
    storage(base = base, project = project, overwrite = overwrite)
}
