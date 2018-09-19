#' Select the relevant observation of a species
#'
#' Relevant locations have at least two observations in different years. Relevant periods have average numbers of at least 15% of the most important period.
#' @param observation the dataframe with the visits and location group
#' @param species the fingerprint of the species
#' @param datafield a named vector with the datafield id for "sample" and "observation"
#' @param min.observation the required minimum number of occassion where the present was observed, defaults to 100
#' @param min.stratum the required minimum number of relevant locations per stratum, defaults to 3
#' @param min.cycle the required minimum number of cycles in which the species was present at a location. defaults to 2
#' @param proportion minimum proportion of the least important period, defaults to 0.15
#' @inheritParams prepare_dataset
#' @export
#' @importFrom assertthat assert_that is.count is.number
#' @importFrom dplyr %>% left_join mutate select filter distinct semi_join count
#' @importFrom git2rdata read_vc
#' @importFrom rlang .data
#' @importFrom stats glm coef poisson
select_relevant <- function(
  observation,
  species,
  datafield,
  repo,
  min.observation = 100,
  min.stratum = 3,
  min.cycle = 2,
  proportion = 0.15
){
  assert_that(
    inherits(observation, "data.frame"),
    has_name(observation, "sample_id"),
    has_name(observation, "stratum"),
    has_name(observation, "location"),
    has_name(observation, "sublocation"),
    has_name(observation, "year"),
    has_name(observation, "period")
  )
  assert_that(is.string(species))
  assert_that(
    is.character(datafield),
    has_name(datafield, "sample"),
    has_name(datafield, "observation")
  )
  assert_that(is.count(min.observation))
  assert_that(is.count(min.stratum))
  assert_that(is.count(min.cycle))
  assert_that(
    is.number(proportion),
    proportion > 0,
    proportion <= 1
  )

  file.path("observation", species) %>%
    read_vc(root = repo) %>%
    left_join(x = observation, by = "sample_id") %>%
    mutate(
      DataFieldID = datafield[
        ifelse(is.na(.data$count), "sample", "observation")
      ],
      ObservationID = ifelse(
        is.na(.data$count),
        .data$sample_id,
        .data$observation_id
      ),
      count = pmax(.data$count, 0, na.rm = TRUE),
      cycle = 1 + (.data$year - 2007) %/% 3,
      period = factor(.data$period)
    ) %>%
    select(-"sample_id", -"observation_id") -> rawdata
  if (sum(rawdata$count > 0) < min.observation) {
    return(NULL)
  }
  rawdata %>%
    filter(.data$count > 0) %>%
    distinct(.data$location, .data$cycle) %>%
    count(.data$location) %>%
    filter(.data$n >= min.cycle) %>%
    semi_join(x = rawdata, by = "location") -> rawdata
  if (sum(rawdata$count > 0) < min.observation) {
    return(NULL)
  }
  rawdata %>%
    filter(.data$count > 0) %>%
    distinct(.data$stratum, .data$location) %>%
    count(.data$stratum) %>%
    filter(.data$n >= min.stratum) %>%
    semi_join(x = rawdata, by = "stratum") -> rawdata
  if (sum(rawdata$count > 0) < min.observation) {
    return(NULL)
  }
  model <- glm(count ~ 0 + period, data = rawdata, family = poisson)
  log_threshold <- max(coef(model)) + log(proportion)
  selection <- levels(rawdata$period)[coef(model) >= log_threshold]
  rawdata %>%
    filter(.data$period %in% selection) -> rawdata
  if (sum(rawdata$count > 0) < min.observation) {
    return(NULL)
  }
  rawdata %>%
    filter(.data$count > 0) %>%
    distinct(.data$location, .data$cycle) %>%
    count(.data$location) %>%
    filter(.data$n >= min.cycle) %>%
    semi_join(x = rawdata, by = "location") -> rawdata
  if (sum(rawdata$count > 0) < min.observation) {
    return(NULL)
  }
  rawdata %>%
    filter(.data$count > 0) %>%
    distinct(.data$stratum, .data$location) %>%
    count(.data$stratum) %>%
    filter(.data$n >= min.stratum) %>%
    semi_join(x = rawdata, by = "stratum") -> rawdata
  if (sum(rawdata$count > 0) < min.observation) {
    return(NULL)
  }
  return(rawdata)
}
