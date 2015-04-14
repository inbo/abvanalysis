#' Prepare all datasets
#' Prepare all datasets and store the version into a git repository
#' @export
#' @importFrom n2khelper write_delim_git auto_commit
#' @importFrom plyr d_ply
#' @param verbose Display a progress bar when TRUE (default)
#' @inheritParams n2khelper::auto_commit
#' @inheritParams n2khelper::odbc_connect
#' @importFrom n2khelper remove_files_git
prepare_dataset <- function(username, password, verbose = TRUE, develop = TRUE){
  
  path <- "abv"
  pattern <- "txt$"
  success <- remove_files_git(path = path, pattern = pattern)
  if(length(success) > 0 && !all(success)){
    stop("Error cleaning existing files in the git repository. Path: '", path, "', pattern: '", pattern, "'")
  }
  
  observation <- prepare_dataset_observation(develop = develop)
  species <- prepare_dataset_species(develop = develop)
  
  if(verbose){
    progress <- "time"
  } else {
    progress <- "none"
  }
  junk <- d_ply(
    .data = species, 
    .variables = "SpeciesGroupID", 
    .progress = progress, 
    .fun = prepare_dataset_species_observation,
    observation = observation,
    develop = develop
  )
  
  auto_commit(
    package = environmentName(parent.env(environment())),
    username = username,
    password = password
  )
}
