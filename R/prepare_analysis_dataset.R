#' Create the analysis dataset based on the available raw data
#' 
#' The analysis dataset is saved to a rda file with the SHA-1 as name.
#' @return A data.frame with the species id number of rows in the analysis dataset, number of precenses in the analysis datset and SHA-1 of the analysis dataset or NULL if not enough data.
#' @importFrom n2khelper check_path check_dataframe_variable git_recent
#' @importFrom plyr ddply
#' @importClassesFrom n2kanalysis n2kGlmerPoisson
#' @importFrom n2kanalysis n2k_glmer_poisson
#' @importMethodsFrom n2kanalysis get_file_fingerprint
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
  min.observation
){
  min.observation <- check_single_strictly_positive_integer(
    min.observation, 
    name = "min.observation"
  )
  scheme.id <- read_delim_git("scheme.txt", connection = raw.connection)$SchemeID
  scheme.id <- check_single_strictly_positive_integer(scheme.id, name = "scheme.id")
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
  analysis.date <- git_recent(file = rawdata.file, connection = raw.connection)$Date
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
      location.group.id <- dataset$LocationGroupID[1]
      
      if(sum(dataset$Count > 0) < min.observation){
        analysis <- n2k_glmer_poisson(
          scheme.id = scheme.id,
          species.group.id = species.group.id,
          location.group.id = location.group.id,
          analysis.date = analysis.date,
          model.type = "weighted glmer poisson: fYear + Period + Location + SubLocation + RowID",
          covariate = "1",
          data = dataset,
          weight = weight,
          status = "insufficient data"
        )
        filename <- paste0(analysis.path, get_file_fingerprint(analysis), ".rda")
        if(!file.exists(filename)){
          save(analysis, file = filename)
        }
        return(NULL)
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
      dataset$fLocation <- factor(dataset$LocationID)
      if(length(levels(dataset$fLocation)) > 0){
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
      
      junk <- sapply(seq_along(covariates), function(i){
        data <- dataset[, c("ObservationID", "DatasourceID", "Count", "Weight", trend.variable[i], design.variable)]
        model.type <- paste(
          "weighted glmer poisson:", 
          trend.variable[i], "+ Period + Location + SubLocation + RowID"
        )
        covariate <- covariates[i]
        analysis <- n2k_glmer_poisson(
          scheme.id = scheme.id,
          species.group.id = species.group.id,
          location.group.id = location.group.id,
          analysis.date = analysis.date,
          model.type = model.type,
          covariate = covariate,
          data = data,
          weight = weight
        )
        filename <- paste0(analysis.path, get_file_fingerprint(analysis), ".rda")
        if(!file.exists(filename)){
          save(analysis, file = filename)
        }
      })
      return(NULL)
    }
  )
  
  return(invisible(NULL))
}
