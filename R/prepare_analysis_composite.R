#' Prepare the analysis files for the composite indices
#' @inheritParams prepare_analysis_dataset
#' @inheritParams prepare_analysis_lrt
#' @inheritParams prepare_dataset
#' @export
#' @importFrom n2kanalysis n2k_composite
prepare_analysis_composite <- function(dataset, raw.connection, analysis.path){
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
  
  
  analysis <- n2k_composite(
    parent.status = dataset[, c("ParentAnalysis", "ParentStatusFingerprint", "ParentStatus")],
    seed = min(dataset$Seed),
    scheme.id = scheme.id,
    species.group.id = dataset$SpeciesGroupID[1],
    location.group.id = dataset$LocationGroupID[1],
    model.type = paste("composite index:", dataset$Covariate[1]),
    formula = paste("~", dataset$Covariate[1]),
    first.imported.year = first.year,
    last.imported.year = last.year,
    analysis.date = max(dataset$AnalysisDate)
  )
  file.fingerprint <- get_file_fingerprint(analysis)
  filename <- paste0(analysis.path, file.fingerprint, ".rda")
  if (!file.exists(filename)) {
    save(analysis, file = filename)
  }
}
