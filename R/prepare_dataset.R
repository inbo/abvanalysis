#' Prepare all datasets
#' Prepare all datasets and store the version into a git repository
#' @export
#' @importFrom n2khelper write_delim_git
prepare_dataset <- function(){
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
}
