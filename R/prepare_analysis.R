#' Prepare all datasets and a to do list of models
#' @inheritParams prepare_analysis_dataset
#' @inheritParams prepare_dataset
#' @export
#' @importFrom n2khelper check_single_strictly_positive_integer check_path list_files_git git_sha
prepare_analysis <- function(
  raw.connection, scheme.id, analysis.path = ".", min.observation = 100
){
  min.observation <- check_single_strictly_positive_integer(min.observation)
  path <- check_path(paste0(analysis.path, "/"), type = "directory", error = FALSE)
  if(is.logical(path)){
    dir.create(path = analysis.path, recursive = TRUE)
    path <- check_path(paste0(analysis.path, "/"), type = "directory")
  }
  analysis.path <- path
  
  observation <- read_delim_git(file = "observation.txt", connection = raw.connection)
  if(class(observation) != "data.frame"){
    stop("observation.txt not available")
  }
  check_dataframe_variable(
    df = observation,
    variable = c("ObservationID", "DatasourceID", "LocationID", "SubLocationID", "Year", "Period", "Weight"),
    name = "observation.txt"
  )
  
  location.group.location <- read_delim_git(
    file = "locationgrouplocation.txt", 
    connection = raw.connection
  )
  if(class(location.group.location) != "data.frame"){
    stop("locationgrouplocation.txt not available")
  }
  check_dataframe_variable(
    df = location.group.location,
    variable = c("LocationID", "LocationGroupID"),
    name = "locationgrouplocation.txt"
  )
  
  observation <- merge(observation, location.group.location)
  rm(location.group.location)
  
  rawdata.files <- list_files_git(connection = raw.connection, pattern = "^[0-9]*\\.txt$")
  if(length(rawdata.files) == 0){
    warning("Nothing to do")
    return(NULL)
  }
  analysis <- do.call(
    rbind,
    lapply(
      rawdata.files, 
      prepare_analysis_dataset, 
      min.observation = min.observation, 
      observation = observation,
      raw.connection = raw.connection,
      analysis.path = analysis.path,
      scheme.id = scheme.id
    )
  )
  sha.rawdata <- git_sha(
    file = c(rawdata.files, "observation.txt", "locationgrouplocation.txt"), 
    connection = raw.connection
  )
  dataset <- merge(
    analysis[, c("FileName", "PathName", "Fingerprint")], 
    sha.rawdata,
    by.x = c("FileName", "PathName"),
    by.y = c("File", "Path")
  )

  to.do.extra <- analysis[
    !is.na(analysis$Covariate), 
    c("SchemeID", "SpeciesGroupID", "LocationGroupID", "ModelType", "Covariate", "AnalysisDate", "NObs", "NLocation", "Fingerprint")
  ]
  to.do.extra$Status <- "new"
  to.do.file <- check_path(paste0(analysis.path, "todo.rda"), type = "file", error = FALSE)
  if(is.logical(to.do.file)){
    to.do <- to.do.extra
  } else {
    load(to.do.file)
    to.do <- rbind(to.do, to.do.extra)
  }
  save(to.do, file = paste0(analysis.path, "todo.rda"))

  return(
    list(
      Analysis = analysis,
      AnalysisDataset = dataset
    )
  )
}
