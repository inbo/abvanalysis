#' Prepare likelihood ratio tests among models
#' @inheritParams prepare_analysis_dataset
#' @inheritParams prepare_dataset
#' @param dataset A data.frame with the parent analyses
#' @export
#' @importFrom n2khelper check_path check_dataframe_variable read_delim_git check_single_strictly_positive_integer
#' @importFrom n2kanalysis n2k_lrt_glmer
prepare_analysis_lrt <- function(dataset, raw.connection, analysis.path){
  analysis.path <- check_path(paste0(analysis.path, "/"), type = "directory")
  check_dataframe_variable(
    df = dataset, 
    variable = c("LocationGroupID", "SpeciesGroupID", "Covariate", "FileFingerprint", "StatusFingerprint", "Seed"), 
    name = "dataset"
  )
  
  metadata <- read_delim_git("metadata.txt", connection = raw.connection)
  scheme.id <- metadata$Value[metadata$Key == "SchemeID"]
  scheme.id <- check_single_strictly_positive_integer(scheme.id, name = "SchemeID")
  first.year <- metadata$Value[metadata$Key == "FirstImportedYear"]
  first.year <- check_single_strictly_positive_integer(
    first.year, 
    name = "FirstImportedYear"
  )
  last.year <- metadata$Value[metadata$Key == "LastImportedYear"]
  last.year <- check_single_strictly_positive_integer(
    last.year, 
    name = "LastImportedYear"
  )
  dataset$ParentAnalysis <- dataset$FileFingerprint
  dataset$ParentStatusFingerprint <- dataset$StatusFingerprint
  dataset$ParentStatus <- dataset$Status
  
  if ("cYear" %in% dataset$Covariate) {
    analysis <- n2k_lrt_glmer(
      parent = dataset$FileFingerprint[dataset$Covariate == "cYear"],
      parent.0 = dataset$FileFingerprint[dataset$Covariate == "fYear"],
      parent.status = dataset[
        dataset$Covariate %in% c("fYear", "cYear"), 
        c("ParentAnalysis", "ParentStatusFingerprint", "ParentStatus")
      ],
      seed = dataset$Seed[dataset$Covariate == "fYear"],
      scheme.id = scheme.id,
      species.group.id = dataset$SpeciesGroupID[dataset$Covariate == "fYear"],
      location.group.id = dataset$LocationGroupID[dataset$Covariate == "fYear"],
      model.type = "glmer lrt: cYear / fYear",
      formula = "~ cYear",
      first.imported.year = first.year,
      last.imported.year = last.year,
      analysis.date = dataset$AnalysisDate[dataset$Covariate == "fYear"]
    )
    file.fingerprint <- get_file_fingerprint(analysis)
    filename <- paste0(analysis.path, file.fingerprint, ".rda")
    if (!file.exists(filename)) {
      save(analysis, file = filename)
    }
  }
  if ("fCycle" %in% dataset$Covariate) {
    analysis <- n2k_lrt_glmer(
      parent = dataset$FileFingerprint[dataset$Covariate == "fCycle"],
      parent.0 = dataset$FileFingerprint[dataset$Covariate == "fYear"],
      parent.status = dataset[
        dataset$Covariate %in% c("fYear", "cYear"), 
        c("ParentAnalysis", "ParentStatusFingerprint", "ParentStatus")
      ],
      seed = dataset$Seed[dataset$Covariate == "fYear"],
      scheme.id = scheme.id,
      species.group.id = dataset$SpeciesGroupID[dataset$Covariate == "fYear"],
      location.group.id = dataset$LocationGroupID[dataset$Covariate == "fYear"],
      model.type = "glmer lrt: fCycle / fYear",
      formula = "~ fCycle",
      first.imported.year = first.year,
      last.imported.year = last.year,
      analysis.date = dataset$AnalysisDate[dataset$Covariate == "fYear"]
    )
    file.fingerprint <- get_file_fingerprint(analysis)
    filename <- paste0(analysis.path, file.fingerprint, ".rda")
    if (!file.exists(filename)) {
      save(analysis, file = filename)
    }
  }
}
