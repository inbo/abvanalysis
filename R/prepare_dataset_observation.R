#' Read the observations and save them to git and the results database
#' @inheritParams prepare_dataset
#' @export
#' @importFrom n2khelper check_single_strictly_positive_integer odbc_get_multi_id connect_result
#' @importFrom n2kanalysis mark_obsolete_dataset
#' @importFrom RODBC odbcClose
prepare_dataset_observation <- function(
  source.channel, result.channel, raw.connection, attribute.connection
){
  scheme.id <- read_delim_git("scheme.txt", connection = raw.connection)$SchemeID
  scheme.id <- check_single_strictly_positive_integer(scheme.id, name = "scheme.txt")
  
  import.date <- Sys.time()
  observation <- read_observation(
    source.channel = source.channel, 
    result.channel = result.channel
  )
  observation <- calculate_weight(observation = observation, result.channel = result.channel)
  
  # store the locations
  main.location <- unique(observation[, c("ExternalCode", "DatasourceID")])
  main.location$Description <- main.location$ExternalCode
  database.id <- odbc_get_multi_id(
    data = main.location,
    id.field = "ID", 
    merge.field = c("ExternalCode", "DatasourceID"), 
    table = "Location",
    channel = result.channel, 
    create = TRUE
  )
  observation <- merge(
    observation,
    database.id
  )
  colnames(observation) <- gsub("^ID$", "LocationID", colnames(observation))
  
  location.group <- data.frame(
    Description = "Vlaanderen",
    SchemeID = scheme.id
  )
  location.group.id <- odbc_get_multi_id(
    data = location.group,
    id.field = "ID", 
    merge.field = c("SchemeID", "Description"), 
    table = "LocationGroup",
    channel = result.channel, 
    create = TRUE
  )$ID
  location.group.location <- data.frame(
    LocationGroupID = location.group.id,
    LocationID = database.id$ID
  )
  location.group.location <- location.group.location[
    order(location.group.location$LocationGroupID, location.group.location$LocationID),
  ]
  database.id <- odbc_get_multi_id(
    data = location.group.location,
    id.field = "ID", 
    merge.field = c("LocationGroupID", "LocationID"), 
    table = "LocationGroupLocation",
    channel = result.channel, 
    create = TRUE
  )
  
  sub.location <- unique(
    observation[, c("LocationID", "ExternalCode", "SubExternalCode", "DatasourceID")]
  )
  observation$ExternalCode <- NULL
  colnames(sub.location) <- gsub("^LocationID$", "ParentLocationID", colnames(sub.location))
  sub.location$Description <- paste(sub.location$ExternalCode, sub.location$SubExternalCode)
  sub.location$ExternalCode <- NULL
  colnames(sub.location) <- gsub(
    "^SubExternalCode$", 
    "ExternalCode", 
    colnames(sub.location)
  )
  database.id <- odbc_get_multi_id(
    data = sub.location,
    id.field = "ID", 
    merge.field = c("ParentLocationID", "ExternalCode", "DatasourceID"),
    table = "Location",
    channel = result.channel, 
    create = TRUE
  )
  observation <- merge(
    observation,
    database.id,
    by.x = c("DatasourceID", "LocationID", "SubExternalCode"),
    by.y = c("DatasourceID", "ParentLocationID", "ExternalCode")
  )
  colnames(observation) <- gsub("^ID$", "SubLocationID", colnames(observation))

  observation <- observation[
    order(
      observation$Stratum, 
      observation$LocationID, 
      observation$SubLocationID, 
      observation$Year, 
      observation$Period
    ),
    c("DatasourceID", "ObservationID", "Stratum", "LocationID", "SubLocationID", "Year", "Period", "Weight", "SubExternalCode")
  ]
  
  location.group.location.sha <- write_delim_git(
    x = location.group.location, 
    file = "locationgrouplocation.txt", 
    connection = raw.connection
  )
  observation.sha <- write_delim_git(
    x = observation[, c("DatasourceID", "ObservationID", "LocationID", "SubLocationID", "Year", "Period", "Weight")], 
    file = "observation.txt", 
    connection = raw.connection
  )
  
  dataset <- data.frame(
    FileName = c("observation.txt",  "locationgrouplocation.txt"),
    PathName = raw.connection@LocalPath,
    Fingerprint = c(observation.sha, location.group.location.sha),
    ImportDate = import.date,
    Obsolete = FALSE
  )
  database.id <- odbc_get_multi_id(
    data = dataset,
    id.field = "ID", 
    merge.field = c("FileName", "PathName", "Fingerprint"), 
    table = "Dataset", 
    channel = result.channel, 
    create = TRUE
  )
  mark_obsolete_dataset(channel = result.channel)
  
  return(observation)
}