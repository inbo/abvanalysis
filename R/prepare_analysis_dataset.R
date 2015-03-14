#' Create the analysis dataset based on the available raw data
#' 
#' The analysis dataset is saved to a rda file with the SHA-1 as name.
#' @return A data.frame with the species id number of rows in the analysis dataset, number of precenses in the analysis datset and SHA-1 of the analysis dataset or NULL if not enough data.
#' @importFrom n2khelper check_single_strictly_positive_integer
#' @importFrom digest digest
#' @export
#' @param min.observation The minimum number of positive observations (Count > 0)
#' @inheritParams read_observation_species
prepare_analysis_dataset <- function(species.id, min.observation = 100){
  species.id <- check_single_strictly_positive_integer(species.id)
  
  species.observation <- read_delim_git(file = paste0(species.id, ".txt"), path = "abv")
  if(class(species.observation) != "data.frame"){
    stop("no data file available for species.id ", species.id)
  }
  
  if(sum(species.observation$Count > 0) < min.observation){
    warning("Less than ", min.observation, " observations. No analysis file generated.")
    return(NULL)
  }
  
  observation <- read_delim_git(file = "observation.txt", path = "abv")
  if(class(observation) != "data.frame"){
    stop("observation.txt not available")
  }
  
  analysis <- merge(species.observation, observation)
  rm(species.observation)
  rm(observation)

  analysis$fLocationID <- factor(analysis$LocationID)
  analysis$fSubLocationID <- interaction(analysis$fLocationID, analysis$SubLocationName, drop = TRUE)
  analysis$fYear <- factor(analysis$Year)
  analysis$cYear <- analysis$Year - max(analysis$Year)
  cycle.label <- seq(min(analysis$Year), max(analysis$Year), by = 3)
  cycle.label <- paste(cycle.label, cycle.label + 2, sep = " - ")
  analysis$fCycle <- factor(
    (analysis$Year - min(analysis$Year)) %/% 3, 
    labels = cycle.label
  )
  analysis$Year <- NULL
  analysis$fPeriod <- factor(analysis$Period)
  analysis$fRowID <- factor(seq_along(analysis$Location))
  
  model.type <- "fit_glmer_poisson"
  data <- analysis[, c("Count", "Weight", "fRowID", "ObservationID", "fLocationID", "fSubLocationID", "fPeriod", "fYear")]
  model.set <- "0 + fYear + fPeriod + (1|fLocationID) + (1|fSubLocationID) + (1|fRowID)"
  weight <- "Weight"
  sha <- digest(
    list(species.id, model.type, model.set, weight, data), 
    algo = "sha1"
  )
  save(species.id, model.type, model.set, weight, data, file = paste0(sha, ".rda"))

  data <- analysis[, c("Count", "Weight", "fRowID", "ObservationID", "fLocationID", "fSubLocationID", "fPeriod", "cYear")]
  model.set <- "cYear + fPeriod + (1|fLocationID) + (1|fSubLocationID) + (1|fRowID)"
  sha <- digest(
    list(species.id, model.type, model.set, weight, data), 
    algo = "sha1"
  )
  save(species.id, model.type, model.set, weight, data, file = paste0(sha, ".rda"))

  data <- analysis[, c("Count", "Weight", "fRowID", "ObservationID", "fLocationID", "fSubLocationID", "fPeriod", "fCycle")]
  model.set <- "0 + fCycle + fPeriod + (1|fLocationID) + (1|fSubLocationID) + (1|fRowID)"
  sha <- digest(
    list(species.id, model.type, model.set, weight, data), 
    algo = "sha1"
  )
  save(species.id, model.type, model.set, weight, data, file = paste0(sha, ".rda"))
  
  return(analysis)
}
