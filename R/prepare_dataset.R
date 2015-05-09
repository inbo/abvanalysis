#' Prepare all datasets
#' Prepare all datasets and store the version into a git repository
#' @param result.channel An open RODBC connection to the results database
#' @param source.channel An open ODBC connection to the source database
#' @param attribute.connection a git-connection object to the attribute data
#' @param raw.connection a git-connection object to write the output to
#' @param scheme.id The id of the scheme
#' @param verbose Display a progress bar when TRUE (default)
#' @export
#' @importFrom n2khelper check_single_logical write_delim_git auto_commit
#' @importFrom plyr d_ply
#' @importFrom n2khelper git_connect remove_files_git write_delim_git
prepare_dataset <- function(
  result.channel, 
  source.channel, 
  attribute.connection, 
  raw.connection, 
  scheme.id,
  verbose = TRUE
){
  scheme.id <- check_single_strictly_positive_integer(scheme.id, name = "scheme.id")
  verbose <- check_single_logical(verbose, name = "verbose")
  
  remove_files_git(connection = raw.connection, pattern = "txt$")
  
  scheme.sha <- write_delim_git(
    x = data.frame(SchemeID = scheme.id), 
    file = "scheme.txt", 
    connection = raw.connection
  )
  
  if(verbose){
    message("Importing observations")
  }
  observation <- prepare_dataset_observation(
    source.channel = source.channel, 
    result.channel = result.channel,
    raw.connection = raw.connection,
    attribute.connection = attribute.connection
  )
  if(verbose){
    message("Importing species")
  }
  species <- prepare_dataset_species(
    source.channel = source.channel, 
    result.channel = result.channel,
    raw.connection = raw.connection
  )
  
  if(verbose){
    progress <- "time"
    message("Importing observations per species")
  } else {
    progress <- "none"
  }
#   this.species <- subset(species, SpeciesGroupID == sample(species$SpeciesGroupID, 1))
  junk <- d_ply(
    .data = species, 
    .variables = "SpeciesGroupID", 
    .progress = progress, 
    .fun = prepare_dataset_species_observation,
    observation = observation,
    result.channel = result.channel,
    source.channel = source.channel,
    raw.connection = raw.connection
  )
  
  auto_commit(
    package = environmentName(parent.env(environment())),
    connection = raw.connection
  )
}
