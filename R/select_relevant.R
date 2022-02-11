#' Select the relevant observation of a species
#'
#' Relevant locations have at least two observations in different cycle.
#' Relevant periods have average numbers of at least 15% of the most important
#' period.
#'
#' @param observation The dataframe with the visits and location group.
#' @param species The fingerprint of the species.
#' @param min_stratum The required minimum number of relevant locations per
#' stratum.
#' Defaults to `3`.
#' @param min_cycle Minimum number of cycles in which a species must be found
#' at a square.
#' Square with sightings from fewer cycles will be ignored.
#' Defaults to `2`.
#' @param proportion Minimum proportion of the least important period.
#' Defaults to `0.15`.
#' @inheritParams prepare_dataset
#' @export
#' @importFrom assertthat assert_that has_name is.count is.number is.string
#' @importFrom dplyr %>% count distinct filter left_join mutate rename select
#' semi_join
#' @importFrom git2rdata is_git2rdata read_vc
#' @importFrom rlang .data
#' @importFrom stats glm coef poisson relevel
#' @importFrom tidyr replace_na
select_relevant <- function(
  observation, species, repo, min_observation = 100, min_stratum = 3,
  min_cycle = 2, proportion = 0.15
){
  assert_that(
    inherits(observation, "data.frame"), has_name(observation, "sample_id"),
    has_name(observation, "stratum"), has_name(observation, "square"),
    has_name(observation, "point"), has_name(observation, "year"),
    has_name(observation, "period")
  )
  assert_that(is.string(species))
  assert_that(
    is.count(min_observation), is.count(min_stratum), is.count(min_cycle)
  )
  assert_that(
    is.number(proportion), proportion > 0, proportion <= 1
  )

  file <- file.path("observation", species)
  if (!is_git2rdata(file, root = repo, message = "none")) {
    return(NULL)
  }

  read_vc(file, root = repo) %>%
    select(-.data$datafield_id) %>%
    left_join(x = observation, by = "sample_id") %>%
    mutate(
      count = replace_na(.data$count, 0),
      cycle = 1 + (.data$year - 2007) %/% 3,
      period = factor(.data$period)
    ) -> rawdata
  # require at least min_observation non-zero observations
  if (sum(rawdata$count > 0) < min_observation) {
    return(NULL)
  }
  # require at least one observation per point
  rawdata %>%
    filter(.data$count > 0) %>%
    distinct(.data$point) %>%
    semi_join(x = rawdata, by = "point") -> rawdata

  # require observation during at least min_cycle different cycles for each
  # square
  rawdata %>%
    filter(.data$count > 0) %>%
    distinct(.data$square, .data$cycle) %>%
    count(.data$square) %>%
    filter(.data$n >= min_cycle) %>%
    semi_join(x = rawdata, by = "square") -> rawdata
  if (sum(rawdata$count > 0) < min_observation) {
    return(NULL)
  }
  # require at least min_stratum locations per stratum
  rawdata %>%
    filter(.data$count > 0) %>%
    distinct(.data$stratum, .data$square) %>%
    count(.data$stratum) %>%
    filter(.data$n >= min_stratum) %>%
    semi_join(x = rawdata, by = "stratum") -> rawdata
  if (sum(rawdata$count > 0) < min_observation) {
    return(NULL)
  }
  # filter periods with at least log_threshold average counts
  model <- glm(count ~ 0 + period, data = rawdata, family = poisson)
  log_threshold <- max(coef(model)) + log(proportion)
  selection <- levels(rawdata$period)[coef(model) >= log_threshold]
  rawdata %>%
    filter(.data$period %in% selection) %>%
    mutate(period = factor(.data$period)) -> rawdata
  if (sum(rawdata$count > 0) < min_observation) {
    return(NULL)
  }
  model <- glm(count ~ 0 + period, data = rawdata, family = poisson)
  rawdata %>%
    mutate(
      period = relevel(.data$period, which.max(coef(model)))
    ) -> rawdata
  # require observation during at least min_cycle different cycles for each
  # location
  rawdata %>%
    filter(.data$count > 0) %>%
    distinct(.data$square, .data$cycle) %>%
    count(.data$square) %>%
    filter(.data$n >= min_cycle) %>%
    semi_join(x = rawdata, by = "square") -> rawdata
  if (sum(rawdata$count > 0) < min_observation) {
    return(NULL)
  }
  # require at least min_stratum locations per stratum
  rawdata %>%
    filter(.data$count > 0) %>%
    distinct(.data$stratum, .data$square) %>%
    count(.data$stratum) %>%
    filter(.data$n >= min_stratum) %>%
    semi_join(x = rawdata, by = "stratum") %>%
    rename(observation_id = .data$sample_id) -> rawdata
  if (sum(rawdata$count > 0) < min_observation) {
    return(NULL)
  }
  return(rawdata)
}
