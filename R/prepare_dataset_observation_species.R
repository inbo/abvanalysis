#' Read the counts and save them to git
#' @param this.species a dataframe with the ExternalCode and the SpeciesGroupID
#' @param observation a data.frame with observations
#' @inheritParams prepare_dataset
#' @export
#' @importFrom n2khelper check_dataframe_variable odbc_get_id  check_single_strictly_positive_integer odbc_get_multi_id
#' @importFrom RODBC sqlQuery
#' @importFrom digest digest
prepare_dataset_species_observation <- function(
  this.species, observation, result.channel, source.channel, raw.connection
){
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
  scheme.id <- read_delim_git("scheme.txt", connection = raw.connection)$SchemeID
  scheme.id <- check_single_strictly_positive_integer(scheme.id, name = "scheme.txt")
  
  import.date <- Sys.time()
  observation.species <- read_observation_species(
    species.id = this.species$ExternalCode,
    source.channel = source.channel
  )
  observation.species <- merge(
    observation.species,
    observation
  )
  observation.species$SubExternalCode <- NULL
  observation.species <- select_relevant(observation, observation.species)

  filename <- paste0(this.species$SpeciesGroupID, ".txt")
  
  if(is.null(observation.species)){
    observation.species.sha <- NA
    status.id <- odbc_get_id(
      table = "AnalysisStatus", 
      variable = "Description",
      value = "No data",
      channel = result.channel
    )
  } else {
    observation.species <- observation.species[
      order(observation.species$ObservationID, observation.species$SubLocationID),
      c("ObservationID", "SubLocationID", "Count")
    ]
    observation.species.sha <- write_delim_git(
      x = observation.species, 
      file = filename, 
      connection = raw.connection
    )
    status.id <- odbc_get_id(
      table = "AnalysisStatus", 
      variable = "Description",
      value = "Working",
      channel = result.channel
    )
  }
  
  import.id <- odbc_get_id(
    table = "ModelType", 
    variable = "Description", 
    value = "import", 
    channel = result.channel
  )
  
  model.set <- data.frame(
    ModelTypeID = import.id,
    FirstYear = NA,
    LastYear = NA,
    Duration = NA
  )
  model.set.id <- odbc_get_multi_id(
    data = model.set,
    id.field = "ID", 
    merge.field = c("ModelTypeID", "FirstYear", "LastYear", "Duration"), 
    table = "ModelSet",
    channel = result.channel, 
    create = TRUE
  )$ID
  
  version <- data.frame(
    Description = paste(
      "abvanalyis version", 
      sessionInfo()$otherPkgs$abvanalysis$Version
    ),
    Obsolete = FALSE
  )
  version.id <- odbc_get_multi_id(
    data = version,
    id.field = "ID", 
    merge.field = "Description", 
    table = "AnalysisVersion",
    channel = result.channel, 
    create = TRUE
  )$ID
  
  sql <- paste0("
    SELECT
      ID, FileName, PathName, Fingerprint
    FROM
      Dataset
    WHERE
      PathName = '", raw.connection@LocalPath, "' AND
      Filename IN ('observation.txt', 'species.txt', 'speciesgroup.txt', 'locationgrouplocation.txt') AND
      Obsolete = 0
  ")
  location.ds <- sqlQuery(channel = result.channel, query = sql, stringsAsFactors = FALSE)
  sha <- sort(c(location.ds$Fingerprint, observation.species.sha))
  
  analysis <- data.frame(
    ModelSetID = model.set.id,
    LocationGroupID = odbc_get_id(
      table = "LocationGroup",
      variable = c("Description", "SchemeID"),
      value = c("Vlaanderen", scheme.id),
      channel = result.channel
    ),
    SpeciesGroupID = this.species$SpeciesGroupID,
    AnalysisVersionID = version.id,
    AnalysisDate = import.date,
    StatusID = status.id,
    Fingerprint = digest(sha, algo = "sha1")
  )
  analysis.id <- odbc_get_multi_id(
    data = analysis,
    id.field = "ID", 
    merge.field = c("ModelSetID", "LocationGroupID", "SpeciesGroupID", "AnalysisVersionID", "Fingerprint"),
    table = "Analysis",
    channel = result.channel,
    create = TRUE
  )$ID
  
  if(!is.na(observation.species.sha)){
    dataset <- data.frame(
      FileName = filename,
      PathName = raw.connection@LocalPath,
      Fingerprint = observation.species.sha,
      ImportDate = import.date,
      Obsolete = FALSE
    )
    location.ds <- rbind(
      location.ds,
      odbc_get_multi_id(
        data = dataset,
        id.field = "ID", 
        merge.field = c("FileName", "PathName", "Fingerprint"), 
        table = "Dataset", 
        channel = result.channel, 
        create = TRUE
      )
    )
  }
  analysis.dataset <- data.frame(
    AnalysisID = analysis.id,
    DatasetID = location.ds$ID
  )
  database.id <- odbc_get_multi_id(
    data = analysis.dataset,
    id.field = "ID", 
    merge.field = c("AnalysisID", "DatasetID"), 
    table = "AnalysisDataset", 
    channel = result.channel, 
    create = TRUE
  )
}
