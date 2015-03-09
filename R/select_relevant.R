#' Select the relevant observation of a species
#' 
#' Relevant locations have at least two observations in different years. Relevant periods have average numbers of at least 5% of the most important period.
#' @param observation.species The output from \code{\link{read_observation_species}}
#' @inheritParams calculate_weight
#' @importFrom n2kanalysis select_factor_count_strictly_positive select_factor_threshold
#' @importFrom n2khelper check_dataframe_variable
#' @export
select_relevant <- function(observation, observation.species){
  check_dataframe_variable(
    df = observation, 
    variable = c("ObservationID", "LocationID", "SubLocationName", "Year", "Period"),
    name = "observation"
  )
  check_dataframe_variable(
    df = observation.species, 
    variable = c("ObservationID", "SubLocationName", "Count"),
    name = "observation.species"
  )

  observation.species <- merge(
    observation.species,
    observation[, c("ObservationID", "LocationID", "SubLocationName", "Year", "Period")]
  )
  observation.species <- select_factor_count_strictly_positive(
    observation = observation.species,
    variable = "Period",
    threshold = 0.15,
    relative = TRUE
  )
  if(nrow(observation.species) == 0){
    return(NULL)
  }
  observation.species <- select_factor_count_strictly_positive(
    observation = observation.species,
    variable = c("LocationID", "Year"),
    threshold = 2
  )
  if(nrow(observation.species) == 0){
    return(NULL)
  }
  return(observation.species)
}
