#' Calculate the survey weights for the observations
#'
#' Adds the survey weights and the stratum to the observations
#' @param observation the dataframe with observations. Should be the output of
#'    \code{\link{read_observation}}
#' @inheritParams connect_source
#' @export
#' @importFrom n2khelper check_dataframe_variable git_connect read_delim_git
calculate_weight <- function(observation, result.channel){
  check_dataframe_variable(
    df = observation,
    variable = c("ExternalCode", "Year"),
    name = "observation"
  )

  # add the stratum information
  stratum <- read_delim_git(
    file = "habitat.txt",
    connection = git_connect(
      data.source.name = "Attributes ABV",
      channel = result.channel
    )
  )
  check_dataframe_variable(
    df = stratum,
    variable = c("ExternalCode", "Stratum"),
    name = "habitat.txt"
  )

  stratum$Stratum <- factor(stratum$Stratum)
  observation <- merge(
    observation,
    stratum[, c("ExternalCode", "Stratum")],
  )

  # stratum size in the population
  population <- as.data.frame(
    table(Stratum = stratum$Stratum),
    responseName = "N"
  )

  # sampling effort per year and stratum
  effort.year <- unique(
    observation[, c("Year", "Stratum", "ExternalCode")]
  )
  effort.year <- as.data.frame(
    table(
      Year = effort.year$Year,
      Stratum = effort.year$Stratum
    ),
    responseName = "Effort"
  )

  observation$Cycle <- (observation$Year - min(observation$Year)) %/% 3
  cycle.year <- unique(observation[, c("Cycle", "Year")])
  effort.year <- merge(effort.year, cycle.year)

  # sampling effort per stratum
  effort.cycle <- aggregate(
    effort.year[, "Effort", drop = FALSE],
    effort.year[, c("Cycle", "Stratum")],
    sum
  )

  # check for incomplete cycles
  year.cycle <- table(cycle.year$Cycle)
  uncomplete.cycle <- as.integer(names(which(year.cycle < 3)))
  # correct effort for incomplete cycles
  if (length(uncomplete.cycle) > 0) {
    max.effort <- aggregate(
      effort.cycle[, "Effort", drop = FALSE],
      effort.cycle[, "Stratum", drop = FALSE],
      max
    )
    sampling.todo <- merge(
      effort.cycle[
        effort.cycle$Cycle %in% uncomplete.cycle,
      ],
      max.effort,
      by = "Stratum"
    )
    sampling.todo$Effort <- sampling.todo$Effort.y - sampling.todo$Effort.x
    sampling.todo$Effort.x <- NULL
    sampling.todo$Effort.y <- NULL
    sampling.todo$Year <- "unknown"
    effort.year <- rbind(effort.year, sampling.todo)

    max.effort$Cycle <- uncomplete.cycle
    effort.cycle <- rbind(
      effort.cycle[
        !effort.cycle$Cycle %in% uncomplete.cycle,
      ],
      max.effort
    )
  }
  colnames(effort.cycle) <- gsub(
    "^Effort$", "EffortCycle", colnames(effort.cycle)
  )

  effort.year <- merge(
    effort.year,
    population
  )
  effort.year <- merge(
    effort.year,
    effort.cycle
  )

  effort.year$Weight <-
    effort.year$Effort * effort.year$N /
    effort.year$EffortCycle ^ 2

  total.cycle <- aggregate(
    data.frame(
      TotalWeight = effort.year$Weight * effort.year$Effort,
      TotalEffort = effort.year$Effort
    ),
    effort.year[, "Cycle", drop = FALSE],
    sum
  )
  effort.year <- merge(effort.year, total.cycle)
  effort.year$Weight <- effort.year$Weight *
    effort.year$TotalEffort / effort.year$TotalWeight
  effort.year <- effort.year[
    effort.year$Year != "unknown",
  ]
  effort.year$Year <- factor(effort.year$Year)
  effort.year$Year <- as.integer(levels(effort.year$Year))[
    effort.year$Year
  ]
  observation$Cycle <- NULL
  observation <- merge(
    observation,
    effort.year[, c("Year", "Stratum", "Weight")]
  )
  return(observation)
}
