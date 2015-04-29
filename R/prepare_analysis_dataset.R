#' Create the analysis dataset based on the available raw data
#' 
#' The analysis dataset is saved to a rda file with the SHA-1 as name.
#' @return A data.frame with the species id number of rows in the analysis dataset, number of precenses in the analysis datset and SHA-1 of the analysis dataset or NULL if not enough data.
#' @importFrom n2khelper check_path check_dataframe_variable
#' @importFrom plyr ddply
#' @importFrom digest digest
#' @export
#' @param rawdata.file The file with the counts per visit
#' @param observation the dataframe with the visits and location group
#' @param analysis.path the path to store the rda files for the analysis
#' @param min.observation The minimum number of positive observations (Count > 0)
#' @inheritParams prepare_dataset
prepare_analysis_dataset <- function(
  rawdata.file, 
  observation, 
  raw.connection, 
  analysis.path, 
  min.observation,
  scheme.id
){
  min.observation <- check_single_strictly_positive_integer(min.observation)
  scheme.id <- check_single_strictly_positive_integer(scheme.id)
  analysis.path <- check_path(paste0(analysis.path, "/"), type = "directory")
  check_dataframe_variable(
    df = observation,
    variable = c("ObservationID", "DatasourceID", "LocationID", "SubLocationID", "Year", "Period", "Weight", "LocationGroupID"),
    name = "observation"
  )
    
  species.observation <- read_delim_git(file = rawdata.file, connection = raw.connection)
  if(class(species.observation) != "data.frame"){
    stop("data file '", rawdata.file, "'")
  }
  check_dataframe_variable(
    df = species.observation,
    variable = c("ObservationID", "SubLocationID", "Count"),
    name = rawdata.file
  )
  species.group.id <- as.integer(gsub("\\.txt$", "", rawdata.file))
  
  rawdata <- merge(species.observation, observation)
  weight <- "Weight"
  
  analysis <- ddply(
    .data = rawdata, 
    .variables = "LocationGroupID",
    .fun = function(dataset){
      dataset$fLocation <- factor(dataset$LocationID)
      n.location <- length(levels(dataset$fLocation))
      location.group.id <- dataset$LocationGroupID[1]
      
      if(sum(dataset$Count > 0) < min.observation){
        return(
          data.frame(
            LocationGroupID = location.group.id,
            ModelType = "weighted glmer poisson: 0 + fYear + Period + Location + SubLocation + RowID",
            Covariate = NA,
            Fingerprint = digest(dataset, algo = "sha1"),
            AnalysisDate = Sys.time(),
            NObs = nrow(dataset),
            NLocation = n.location
          )
        )
      }
      
      dataset$fYear <- factor(dataset$Year)
      if(length(levels(dataset$fYear)) == 1){
        trend <- "1"
      } else {
        dataset$cYear <- dataset$Year - max(dataset$Year)
        trend <- c("0 + fYear", "1 + cYear")
        trend.variable <- c("fYear", "cYear")
        cycle.label <- seq(min(dataset$Year), max(dataset$Year), by = 3)
        if(length(cycle.label) > 1){
          cycle.label <- paste(cycle.label, cycle.label + 2, sep = " - ")
          dataset$fCycle <- factor(
            (dataset$Year - min(dataset$Year)) %/% 3, 
            labels = cycle.label
          )
          trend <- c(trend, "0 + fCycle")
          trend.variable <- c(trend.variable, "fCycle")
        }
      }
      dataset$fRow <- factor(seq_len(nrow(dataset)))
      design <- "(1|fRow)"
      design.variable <- "fRow"
      if(n.location > 0){
        design <- c(design, "(1|fLocation)")
        design.variable <- c(design.variable, "fLocation")
      }
      dataset$fSubLocation <- factor(dataset$SubLocationID)
      if(length(levels(dataset$fSubLocation)) > 0){
        design <- c(design, "(1|fSubLocation)")
        design.variable <- c(design.variable, "fSubLocation")
      }
      dataset$fPeriod <- factor(dataset$Period)
      if(length(levels(dataset$fPeriod)) > 0){
        design <- c(design, "fPeriod")
        design.variable <- c(design.variable, "fPeriod")
      }
      design <- paste(design, collapse = " + ")
      covariates <- paste(trend, design, sep = " + ")
      
      do.call(rbind, lapply(seq_along(covariates), function(i){
        data <- dataset[, c("ObservationID", "DatasourceID", "Count", "Weight", trend.variable[i], design.variable)]
        modeltype <- paste(
          "weighted glmer poisson:", 
          trend.variable[i], "+ Period + Location + SubLocation + RowID"
        )
        covariate <- covariates[i]
        data.fingerprint <- digest(data, algo = "sha1")
        file.fingerprint <- digest(
          list(
            species.group.id, location.group.id, data, covariate, modeltype, data.fingerprint
          ),
          algo = "sha1"
        )
        filename <- paste0(analysis.path, file.fingerprint, ".rda")
        save(
          species.group.id, location.group.id, data, covariate, modeltype, data.fingerprint,
          file = filename
        )
        data.frame(
          LocationGroupID = location.group.id,
          ModelType = modeltype,
          Covariate = covariate,
          Fingerprint = file.fingerprint,
          AnalysisDate = Sys.time(),
          NObs = nrow(data),
          NLocation = n.location
        )
      }))
    }
  )
  analysis$SchemeID <- scheme.id
  analysis$SpeciesGroupID <- species.group.id
  analysis$FileName <- rawdata.file
  analysis$PathName <- raw.connection@LocalPath
  
  return(analysis)
}
