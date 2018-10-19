#' Prepare comparison among models
#' @param frequency a indicator of the frequency of the analysis
#' @param models a data.frame with the metadata of the models
#' @inheritParams n2kanalysis::store_model
#' @inheritParams prepare_analysis_dataset
#' @export
#' @importFrom assertthat assert_that is.string has_name
#' @importFrom utils flush.console
#' @importFrom n2kanalysis n2k_inla_comparison
#' @importFrom dplyr %>% select
prepare_analysis_comparison <- function(
  frequency, species_group, location_group, models,
  base, project, overwrite = FALSE
){
  assert_that(is.string(frequency))
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
  message(location_group, " ", species_group, " ", frequency)
  flush.console()

  n2k_inla_comparison(
    result.datasource.id = models$result_datasource[1],
    parent.status = models %>%
      select(
        ParentAnalysis = "fingerprint",
        ParentStatusFingerprint = "status_fingerprint",
        ParentStatus = "status"
      ),
    status = "waiting",
    seed = models$seed[1],
    scheme.id = models$scheme[1],
    species.group.id = species_group,
    location.group.id = location_group,
    model.type = paste("inla comparison:", frequency),
    formula = paste("~", frequency),
    first.imported.year = models$first_imported_year[1],
    last.imported.year = models$last_imported_year[1],
    analysis.date = models$analysis_date[1]
  ) %>%
    storage(base = base, project = project, overwrite = overwrite)
}
