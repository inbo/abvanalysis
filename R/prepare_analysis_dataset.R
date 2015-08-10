#' Create the analysis dataset based on the available raw data
#' 
#' The analysis dataset is saved to a rda file with the SHA-1 as name.
#' @return A data.frame with the species id number of rows in the analysis dataset, number of precenses in the analysis datset and SHA-1 of the analysis dataset or NULL if not enough data.
#' @importFrom n2khelper check_path check_dataframe_variable git_recent
#' @importFrom plyr ddply
#' @importClassesFrom n2kanalysis n2kGlmerPoisson
#' @importFrom n2kanalysis n2k_glmer_poisson
#' @importMethodsFrom n2kanalysis get_file_fingerprint get_status_fingerprint get_seed
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
  metadata <- read_delim_git("metadata.txt", connection = raw.connection)
  scheme.id <- metadata$Value[metadata$Key == "SchemeID"]
  scheme.id <- check_single_strictly_positive_integer(scheme.id, name = "SchemeID")
  first.year <- metadata$Value[metadata$Key == "FirstImportedYear"]
  first.year <- check_single_strictly_positive_integer(
    first.year, 
    name = "FirstImportedYear"
  )
  last.year <- metadata$Value[metadata$Key == "LastImportedYear"]
  last.year <- check_single_strictly_positive_integer(
    last.year, 
    name = "LastImportedYear"
  )
  
  parent <- read_delim_git(file = "parent.txt", connection = raw.connection)
  check_dataframe_variable(
    df = parent,
    variable = c("SpeciesGroupID", "Fingerprint"),
    name = "parent.txt"
  )
  
  analysis.path <- check_path(paste0(analysis.path, "/"), type = "directory")
  check_dataframe_variable(
    df = observation,
    variable = c("ObservationID", "DatasourceID", "LocationID", "SubLocationID", "Year", "Period", "Weight", "LocationGroupID"),
    name = "observation"
  )
    
  species.observation <- read_delim_git(file = rawdata.file, connection = raw.connection)
  check_dataframe_variable(
    df = species.observation,
    variable = c("ObservationID", "Count"),
    name = rawdata.file
  )
  analysis.date <- git_recent(file = rawdata.file, connection = raw.connection)$Date
  species.group.id <- as.integer(gsub("\\.txt$", "", rawdata.file))
  species.id <- read_delim_git("species.txt", connection = raw.connection)
  seed <- species.id$Seed[species.id$SpeciesGroupID == species.group.id]
  parent <- parent$Fingerprint[parent$SpeciesGroupID == species.group.id]
  
  rawdata <- merge(species.observation, observation)
  
  message(species.group.id, " ", appendLF = FALSE)
  utils::flush.console()
  analysis <- ddply(
    .data = rawdata, 
    .variables = "LocationGroupID",
    .fun = function(dataset){
      location.group.id <- dataset$LocationGroupID[1]
      
      if (sum(dataset$Count > 0) < min.observation) {
        analysis <- n2k_glmer_poisson(
          scheme.id = scheme.id,
          species.group.id = species.group.id,
          location.group.id = location.group.id,
          analysis.date = analysis.date,
          model.type = "weighted glmer poisson: fYear + Period + Location + SubLocation + Observation",
          formula = "Count ~ 1",
          first.imported.year = first.year,
          last.imported.year = last.year,
          data = dataset,
          status = "insufficient data",
          parent = parent,
          seed = seed
        )
        filename <- paste0(analysis.path, get_file_fingerprint(analysis), ".rda")
        if (!file.exists(filename)) {
          save(analysis, file = filename)
        }
        return(NULL)
      }
      
      dataset$fYear <- factor(dataset$Year)
      if (length(levels(dataset$fYear)) == 1) {
        trend <- "0 + fYear"
        trend.variable <- "fYear"
      } else {
        dataset$cYear <- dataset$Year - max(dataset$Year)
        trend <- c("0 + fYear", "1 + cYear")
        trend.variable <- c("fYear", "cYear")
        cycle.label <- seq(min(dataset$Year), max(dataset$Year), by = 3)
        if (length(cycle.label) > 1) {
          cycle.label <- paste(cycle.label, cycle.label + 2, sep = " - ")
          dataset$fCycle <- factor(
            (dataset$Year - min(dataset$Year)) %/% 3, 
            labels = cycle.label
          )
          trend <- c(trend, "0 + fCycle")
          trend.variable <- c(trend.variable, "fCycle")
        }
      }
      
      dataset$fObservation <- factor(dataset$ObservationID)
      design <- "(1|fObservation)"
      design.variable <- "fObservation"
      
      dataset$fLocation <- factor(dataset$LocationID)
      if (length(levels(dataset$fLocation)) > 1) {
        design <- c(design, "(1|fLocation)")
        design.variable <- c(design.variable, "fLocation")
      }
      
      dataset$fSubLocation <- factor(dataset$SubLocationID)
      if (length(levels(dataset$fSubLocation)) > 1) {
        design <- c(design, "(1|fSubLocation)")
        design.variable <- c(design.variable, "fSubLocation")
      }
      
      dataset$fPeriod <- factor(dataset$Period)
      if (length(levels(dataset$fPeriod)) > 1) {
        design <- c(design, "fPeriod")
        design.variable <- c(design.variable, "fPeriod")
      }
      
      design <- paste(design, collapse = " + ")
      covariates <- paste(trend, design, sep = " + ")
      
      fingerprint <- do.call(rbind, lapply(seq_along(covariates), function(i){
        data <- dataset[, c("ObservationID", "DatasourceID", "Count", "Weight", trend.variable[i], design.variable)]
        model.type <- paste(
          "weighted glmer poisson:", 
          trend.variable[i], "+ Period + Location + SubLocation + Observation"
        )
        formula <- paste("Count ~", covariates[i])
        analysis <- n2k_glmer_poisson(
          scheme.id = scheme.id,
          species.group.id = species.group.id,
          location.group.id = location.group.id,
          analysis.date = analysis.date,
          seed = seed,
          model.type = model.type,
          formula = formula,
          first.imported.year = first.year,
          last.imported.year = last.year,
          data = data,
          parent = parent
        )
        file.fingerprint <- get_file_fingerprint(analysis)
        filename <- paste0(analysis.path, file.fingerprint, ".rda")
        if (!file.exists(filename)) {
          save(analysis, file = filename)
        }
        data.frame(
          ModelType = model.type,
          Covariate = trend.variable[i],
          FileFingerprint = file.fingerprint, 
          StatusFingerprint = get_status_fingerprint(analysis),
          Seed = get_seed(analysis),
          stringsAsFactors = FALSE
        )
      }))
      return(
        cbind(
          LocationGroupID = location.group.id,
          AnalysisDate = analysis.date,
          fingerprint
        )
      )
    }
  )
  if (nrow(analysis) > 0) {
    analysis$SpeciesGroupID <- species.group.id
  }
  return(analysis)
}
