#' Calculate the survey weights for the observations
#' 
#' Adds the survey weights and the stratum to the observations
#' @param observation the dataframe with observations. Should be the output of \code{\link{read_observation}}
#' @export
#' @importFrom n2khelper check_dataframe_variable read_delim_git
calculate_weight <- function(observation){
  check_dataframe_variable(df = observation, variable = c("LocationID", "Year"), name = "observation")
  
  # add the stratum information
  stratum <- read_delim_git("habitat.txt", path = "abv/attribuut")
  check_dataframe_variable(df = stratum, variable = c("TAG", "Stratum"), name = "habitat.txt")
  
  stratum$Stratum <- factor(stratum$Stratum)
  observation <- merge(observation, stratum[, c("TAG", "Stratum")], by.x = "LocationID", by.y = "TAG")
  
  # stratum size in the population
  population <- as.data.frame(table(Stratum = stratum$Stratum), responseName = "N")
  
  # sampling effort per year and stratum
  sampling.effort.year <- unique(observation[, c("Year", "Stratum", "LocationID")])
  sampling.effort.year <- as.data.frame(
    table(
      Year = sampling.effort.year$Year, 
      Stratum = sampling.effort.year$Stratum
    ),
    responseName = "Effort"
  )

  observation$Cycle <- (observation$Year - min(observation$Year)) %/% 3
  cycle.year <- unique(observation[, c("Cycle", "Year")])
  sampling.effort.year <- merge(sampling.effort.year, cycle.year)
  
  # sampling effort per stratum
  sampling.effort.cycle <- aggregate(
    sampling.effort.year[, "Effort", drop = FALSE],
    sampling.effort.year[, c("Cycle", "Stratum")],
    sum
  )

  # check for incomplete cycles
  year.per.cycle <- table(cycle.year$Cycle)
  uncomplete.cycle <- as.integer(names(which(year.per.cycle < 3)))
  # correct effort for incomplete cycles
  if(length(uncomplete.cycle) > 0){
    max.sampling.effort <- aggregate(
      sampling.effort.cycle[, "Effort", drop = FALSE],
      sampling.effort.cycle[, "Stratum", drop = FALSE],
      max
    )
    sampling.to.do <- merge(
      sampling.effort.cycle[sampling.effort.cycle$Cycle %in% uncomplete.cycle, ],
      max.sampling.effort,
      by = "Stratum"
    )
    sampling.to.do$Effort <- sampling.to.do$Effort.y - sampling.to.do$Effort.x
    sampling.to.do$Effort.x <- NULL
    sampling.to.do$Effort.y <- NULL
    sampling.to.do$Year <- "unknown"
    sampling.effort.year <- rbind(sampling.effort.year, sampling.to.do)
    
    max.sampling.effort$Cycle <- uncomplete.cycle
    sampling.effort.cycle <- rbind(
      sampling.effort.cycle[!sampling.effort.cycle$Cycle %in% uncomplete.cycle, ],
      max.sampling.effort
    )
  }
  colnames(sampling.effort.cycle)[3] <- "EffortCycle"
 
  sampling.effort.year <- merge(
    sampling.effort.year,
    population
  )
  sampling.effort.year <- merge(
    sampling.effort.year,
    sampling.effort.cycle
  )
  
  sampling.effort.year$Weight <- 
    sampling.effort.year$Effort * sampling.effort.year$N / sampling.effort.year$EffortCycle ^ 2
  
  total.cycle <- aggregate(
    data.frame(
      TotalWeight = sampling.effort.year$Weight * sampling.effort.year$Effort,
      TotalEffort = sampling.effort.year$Effort
    ),
    sampling.effort.year[, "Cycle", drop = FALSE],
    sum
  )
  sampling.effort.year <- merge(sampling.effort.year, total.cycle)
  sampling.effort.year$Weight <- sampling.effort.year$Weight * sampling.effort.year$TotalEffort / sampling.effort.year$TotalWeight
  sampling.effort.year <- sampling.effort.year[sampling.effort.year$Year != "unknown", ]
  sampling.effort.year$Year <- factor(sampling.effort.year$Year)
  sampling.effort.year$Year <- as.integer(levels(sampling.effort.year$Year))[sampling.effort.year$Year]
  observation$Cycle <- NULL
  observation <- merge(
    observation,
    sampling.effort.year[, c("Year", "Stratum", "Weight")]
  )
  return(observation)
}
