#' Prepare all datasets
#' Prepare all datasets and store the version into a git repository
#' @export
#' @importFrom n2khelper write_delim_git auto_commit
#' @importFrom plyr d_ply
#' @param verbose Display a progress bar when TRUE (default)
#' @inheritParams n2khelper::auto_commit
prepare_dataset <- function(username, password, verbose = TRUE){
  observation <- read_observation()
  observation <- calculate_weight(observation = observation)
  observation <- observation[
    order(observation$ObservationID, observation$SubLocationName),
    c("ObservationID", "LocationID", "SubLocationName", "Year", "Period", "Weight")
  ]
  write_delim_git(observation, file = "observation.txt", path = "abv")
  
  species.list <- read_specieslist()
  write_delim_git(species.list$Species, file = "species.txt", path = "abv")
  write_delim_git(species.list$Speciesgroup, file = "speciesgroup.txt", path = "abv")
  
  if(verbose){
    progress <- "time"
  } else {
    progress <- "none"
  }
  junk <- d_ply(species.list$Species, "SpeciesID", .progress = progress, function(x){
    observation.species <- read_observation_species(species.id = x$SpeciesID[1])
    observation.species <- select_relevant(
      observation = observation, 
      observation.species = observation.species
    )
    if(!is.null(observation.species)){
      observation.species <- observation.species[
        order(observation.species$ObservationID, observation.species$SubLocationName), 
        c("ObservationID", "SubLocationName", "Count")
      ]
      write_delim_git(observation.species, file = paste0(x$SpeciesID[1], ".txt"), path = "abv")
    }
  })
  
  auto_commit(
    package = environmentName(parent.env(environment())),
    username = username,
    password = password
  )
}
