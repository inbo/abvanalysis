#' Prepare all datasets and a to do list of models
#' @inheritParams prepare_analysis_dataset
#' @inheritParams prepare_dataset
#' @export
#' @importFrom n2khelper check_path
#' @importFrom n2kanalysis status
#' @importFrom assertthat assert_that is.count
#' @importFrom dplyr %>% bind_rows do_ select_ group_by_ inner_join
#' @importFrom git2rdata read_vc
prepare_analysis <- function(
  raw.connection, analysis.path = ".", min.observation = 100, min.stratum = 3
){
  assert_that(is.count(min.observation))
  assert_that(is.count(min.stratum))
  path <- check_path(
    paste0(analysis.path, "/"),
    type = "directory",
    error = FALSE
  )
  if (is.logical(path)) {
    dir.create(path = analysis.path, recursive = TRUE)
    path <- check_path(paste0(analysis.path, "/"), type = "directory")
  }
  analysis.path <- path

  observation <- read_delim_git(
    file = "observation.txt",
    connection = raw.connection
  )
  check_dataframe_variable(
    df = observation,
    variable = c(
      "ObservationID", "DatasourceID", "LocationID", "SubLocationID", "Year",
      "Period", "Stratum"
    ),
    name = "observation.txt"
  )

  locationgrouplocation <- read_delim_git(
    file = "locationgrouplocation.txt",
    connection = raw.connection
  )
  check_dataframe_variable(
    df = locationgrouplocation,
    variable = c("LocationID", "LocationGroupID"),
    name = "locationgrouplocation.txt"
  )

  observation <- merge(observation, locationgrouplocation)
  rm(locationgrouplocation)

  message("Prepare analysis per species")
  utils::flush.console()
  rawdata.files <- list.files(
    raw.connection@Repository$repository,
    pattern = "^[0-9]*\\.txt$"
  )
  if (length(rawdata.files) == 0) {
    warning("Nothing to do")
    return(invisible(NULL))
  }
  analysis <- lapply(
    rawdata.files,
    prepare_analysis_dataset,
    min.observation = min.observation,
    min.stratum = min.stratum,
    observation = observation,
    raw.connection = raw.connection,
    analysis.path = analysis.path
  ) %>%
    bind_rows()
  if (nrow(analysis) == 0) {
    return(invisible(NULL))
  }
  analysis <- status(analysis.path) %>%
    select_(~FileFingerprint, ~Status) %>%
    inner_join(analysis, by = "FileFingerprint")

  message("\nPrepare model comparison")
  utils::flush.console()
  analysis %>%
    group_by_(~LocationGroupID, ~SpeciesGroupID) %>%
    do_(
      Output = ~prepare_analysis_comparison(
        .,
        raw.connection = raw.connection,
        analysis.path = analysis.path
      )
    )

  message("\nPrepare Composite indices")
  utils::flush.console()
  species.id <- read_delim_git(
    file = "species.txt",
    connection = raw.connection
  )
  check_dataframe_variable(
    df = species.id,
    variable = c("SpeciesGroupID", "SpeciesID"),
    name = "species.txt"
  )
  speciesgroupid <- read_delim_git(
    file = "speciesgroup.txt",
    connection = raw.connection
  )
  check_dataframe_variable(
    df = speciesgroupid,
    variable = c("SpeciesGroupID", "SpeciesID"),
    name = "species.txt"
  )
  analysis %>%
    inner_join(
      species.id %>%
        select_(~SpeciesGroupID, ~SpeciesID),
      by = "SpeciesGroupID"
    ) %>%
    select_(~-SpeciesGroupID) %>%
    inner_join(
      speciesgroupid,
      by = "SpeciesID"
    ) %>%
    group_by_(~LocationGroupID, ~SpeciesGroupID, ~Covariate) %>%
    do_(
      Output = ~prepare_analysis_composite(
        .,
        raw.connection = raw.connection,
        analysis.path = analysis.path
      )
    )

  return(invisible(NULL))
}
