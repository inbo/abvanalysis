#' Prepare all datasets and a to do list of models
#' @inheritParams prepare_analysis_dataset
#' @export
#' @importFrom n2khelper check_single_strictly_positive_integer check_single_character list_files_git git_sha
prepare_analysis <- function(min.observation = 100, analysis.path = "analysis"){
  min.observation <- check_single_strictly_positive_integer(min.observation)
  analysis.path <- check_single_character(analysis.path, name = "analysis.path")
  analysis.path <- normalizePath(
    paste0(analysis.path, "/"), 
    winslash = "/", 
    mustWork = FALSE
  )
  
  success <- file.remove(list.files(analysis.path, pattern = "\\.rda$", full.names = TRUE))
  if(length(success) > 0 && !all(success)){
    remaining <- list.files(analysis.path, pattern = "\\.rda$")
    stop(
      "Unable to remove ", length(remaining), " existing rda files:\n\n", 
      paste(head(remaining, 10), collapse = "\n"), 
      "\n..."
    )
  }
  
  rawdata.path <- "abv"
  observation <- read_delim_git(file = "observation.txt", path = rawdata.path)
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
    path = rawdata.path
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
  
  rawdata.files <- list_files_git(path = rawdata.path)
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
      rawdata.path = rawdata.path,
      analysis.path = analysis.path
    )
  )
  sha.rawdata <- git_sha(
    file = rawdata.files, 
    path = rawdata.path
  )[, c("path", "name", "sha")]
  dataset <- merge(
    analysis[, c("name", "path", "Fingerprint")], 
    sha.rawdata
  )[, c("Fingerprint", "sha")]
  analysis$name <- NULL
  analysis$path <- NULL
  
  sha.design <- git_sha(
    file = c("observation.txt", "locationgrouplocation.txt"), 
    path = rawdata.path
  )[, "sha", drop = FALSE]
  dataset <- rbind(
    dataset,
    merge(
      analysis[, "Fingerprint", drop = FALSE], 
      sha.design
    )
  )
  to.do <- analysis[, c("Fingerprint", "NObs", "NLocation")]
  save(to.do, file = paste0(analysis.path, "todo.rda"))

  return(
    list(
      Analysis = analysis,
      AnalysisDataset = dataset
    )
  )
}
