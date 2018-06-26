#' Prepare comparison among models
#' @inheritParams prepare_analysis_dataset
#' @inheritParams prepare_dataset
#' @param dataset A data.frame with the parent analyses
#' @export
#' @importFrom n2khelper check_path check_dataframe_variable read_delim_git
#' @importFrom assertthat assert_that is.count
#' @importFrom dplyr %>% rename_ arrange_ filter_
#' @importFrom n2kanalysis n2k_inla_comparison
prepare_analysis_comparison <- function(dataset, raw.connection, analysis.path){
  analysis.path <- check_path(paste0(analysis.path, "/"), type = "directory")
  check_dataframe_variable(
    df = dataset,
    variable = c("LocationGroupID", "SpeciesGroupID", "Covariate", "FileFingerprint", "StatusFingerprint", "Seed"),
    name = "dataset"
  )

  metadata <- read_delim_git("metadata.txt", connection = raw.connection)
  scheme.id <- metadata$Value[metadata$Key == "SchemeID"]
  assert_that(is.count(scheme.id))
  first.year <- metadata$Value[metadata$Key == "FirstImportedYear"]
  assert_that(is.count(first.year))
  last.year <- metadata$Value[metadata$Key == "LastImportedYear"]
  assert_that(is.count(last.year))
  dataset <- dataset %>%
    rename_(
      ParentAnalysis = ~FileFingerprint,
      ParentStatusFingerprint = ~StatusFingerprint,
      ParentStatus = ~ Status
    ) %>%
    arrange_(~ParentAnalysis)
  seed  <- dataset$Seed[1]
  species.group.id <- dataset$SpeciesGroupID[1]
  location.group.id <- dataset$LocationGroupID[1]
  analysis.date <- dataset$AnalysisDate[1]

  dataset.year <- dataset %>%
    filter_(~ grepl("Year", Covariate))
  if (nrow(dataset.year) > 1) {
    analysis <- n2k_inla_comparison(
      parent = dataset.year$ParentAnalysis,
      parent.status = dataset.year %>%
        select_(~ParentAnalysis, ~ParentStatusFingerprint, ~ParentStatus) %>%
        as.data.frame(),
      seed = seed,
      scheme.id = scheme.id,
      species.group.id = species.group.id,
      location.group.id = location.group.id,
      model.type = "inla comparison: Year",
      formula = "~ Year",
      first.imported.year = first.year,
      last.imported.year = last.year,
      analysis.date = analysis.date
    )
    file.fingerprint <- get_file_fingerprint(analysis)
    filename <- paste0(analysis.path, "/", file.fingerprint, ".rda")
    if (!file.exists(filename)) {
      save(analysis, file = filename)
    }
  }
  this.dataset <- dataset %>%
    filter_(~ grepl("Cycle", Covariate))
  if (nrow(this.dataset) > 1) {
    analysis <- n2k_inla_comparison(
      parent = this.dataset$ParentAnalysis,
      parent.status = this.dataset %>%
        select_(~ParentAnalysis, ~ParentStatusFingerprint, ~ParentStatus) %>%
        as.data.frame(),
      seed = seed,
      scheme.id = scheme.id,
      species.group.id = species.group.id,
      location.group.id = location.group.id,
      model.type = "inla comparison: Cycle",
      formula = "~ Cycle",
      first.imported.year = first.year,
      last.imported.year = last.year,
      analysis.date = analysis.date
    )
    file.fingerprint <- get_file_fingerprint(analysis)
    filename <- paste0(analysis.path, "/", file.fingerprint, ".rda")
    if (!file.exists(filename)) {
      save(analysis, file = filename)
    }
  }
}
