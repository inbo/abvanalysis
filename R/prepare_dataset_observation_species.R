#' Read the counts and save them to git
#' @param this.species a dataframe with the ExternalCode and the SpeciesGroupID
#' @param observation a data.frame with observations
#' @inheritParams n2khelper::odbc_connect
#' @export
#' @importFrom n2khelper check_dataframe_variable odbc_get_id  odbc_get_multi_id connect_result check_id
#' @importFrom RODBC sqlQuery odbcClose
prepare_dataset_species_observation <- function(this.species, observation, develop = TRUE){
  check_dataframe_variable(
    df = this.species,
    variable = c("ExternalCode", "SpeciesGroupID"),
    name = "this.species"
  )
  if(nrow(this.species) != 1){
    stop("this.species must contain exactly one row")
  }
  check_dataframe_variable(
    df = observation,
    variable = c("ObservationID", "SubExternalCode", "SubLocationID"),
    name = "observation"
  )
  
  import.date <- Sys.time()
  observation.species <- read_observation_species(
    species.id = this.species$ExternalCode, develop = develop
  )
  observation.species <- merge(
    observation.species,
    observation
  )
  observation.species$SubExternalCode <- NULL
  observation.species <- select_relevant(observation, observation.species)

  filename <- paste0(this.species$SpeciesGroupID, ".txt")
  pathname <- "abv"
  
  if(is.null(observation.species)){
    observation.species.sha <- NA
    status.id <- odbc_get_id(
      table = "AnalysisStatus", 
      variable = "Description",
      value = "No data",
      develop = develop
    )
  } else {
    observation.species <- observation.species[
      order(observation.species$ObservationID, observation.species$SubLocationID),
      c("ObservationID", "SubLocationID", "Count")
    ]
    observation.species.sha <- write_delim_git(
      x = observation.species, 
      file = filename, 
      path = pathname
    )
    status.id <- odbc_get_id(
      table = "AnalysisStatus", 
      variable = "Description",
      value = "Working",
      develop = develop
    )
  }
  
  import.id <- odbc_get_id(
    table = "ModelType", 
    variable = "Description", 
    value = "import", 
    develop = develop
  )
  model.set <- data.frame(
    ModelTypeID = import.id,
    FirstYear = NA,
    LastYear = NA,
    Duration = NA
  )
  
  channel <- connect_result(develop = develop)
  model.set.id <- odbc_get_multi_id(
    data = model.set,
    id.field = "ID", merge.field = c("ModelTypeID", "FirstYear", "LastYear", "Duration"), 
    table = "ModelSet",
    channel = channel, create = TRUE
  )$ID
  #   version <- paste(
  #     "abvanalyis version", 
  #     sessionInfo()$otherPkgs$abvanalysis$Version
  #   )
  #   version.id <- odbc_get_id(
  #     table = "AnalysisVersion", 
  #     variable = "Description", 
  #     value = version, 
  #     develop = develop
  #   )
  version.id <- 5
  test <- check_id(value = version.id, field = "ID", table = "AnalysisVersion", channel = channel)
  if(!test){
    stop("Unknown version id")
  }
  warning("update code when version description is nchar instead of int")
  
  analysis <- data.frame(
    ModelSetID = model.set.id,
    LocationGroupID = odbc_get_id(
      table = "LocationGroup",
      variable = c("Description", "SchemeID"),
      value = c("Vlaanderen", scheme_id(develop = develop)),
      develop = develop
    ),
    SpeciesGroupID = this.species$SpeciesGroupID,
    AnalysisVersionID = version.id,
    AnalysisDate = import.date,
    StatusID = status.id
  )
  warning("add fingerprint to code when available in database")
  analysis.id <- odbc_get_multi_id(
    data = analysis,
    id.field = "ID", 
    merge.field = c("ModelSetID", "LocationGroupID", "SpeciesGroupID", "AnalysisVersionID"),
    table = "Analysis",
    channel = channel,
    create = TRUE
  )$ID
  
  sql <- paste0("
    SELECT
      ID, FileName, PathName, Fingerprint
    FROM
      Dataset
    WHERE
      PathName = '", pathname, "' AND
      Filename IN ('observation.txt', 'speciesgroup.txt', 'speciesgroupspecies.txt', 'locationgrouplocation.txt') AND
      Obsolete = 0
  ")
  location.ds <- sqlQuery(channel = channel, query = sql)
  
  if(!is.na(observation.species.sha)){
    dataset <- data.frame(
      FileName = filename,
      PathName = pathname,
      Fingerprint = observation.species.sha,
      ImportDate = import.date,
      Obsolete = FALSE
    )
    location.ds <- rbind(
      location.ds,
      odbc_get_multi_id(
        data = dataset,
        id.field = "ID", merge.field = c("FileName", "PathName", "Fingerprint"), 
        table = "Dataset", 
        channel = channel, create = TRUE
      )
    )
  }
  analysis.dataset <- data.frame(
    AnalysisID = analysis.id,
    DatasetID = location.ds$ID
  )
  database.id <- odbc_get_multi_id(
    data = analysis.dataset,
    id.field = "ID", merge.field = c("AnalysisID", "DatasetID"), 
    table = "AnalysisDataset", 
    channel = channel, create = TRUE
  )
  odbcClose(channel)
}
