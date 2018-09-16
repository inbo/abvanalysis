#' Calculate the survey weights for the observations
#'
#' Adds the survey weights and the stratum to the observations
#' @param observation the dataframe with observations. Should be the output of
#'    \code{\link{read_observation}}
#' @inheritParams connect_source
#' @inheritParams prepare_dataset
#' @export
#' @importFrom n2khelper check_dataframe_variable git_connect
#' @importFrom git2rdata read_vc
#' @importFrom dplyr %>% group_by_ mutate_ n select_ semi_join inner_join
calculate_weight <- function(observation, attribute.connection, result.channel){
  check_dataframe_variable(
    df = observation,
    variable = c("ExternalCode", "Year"),
    name = "observation"
  )

  # add the stratum information
  stratum <- read_delim_git(
    file = "habitat.txt",
    connection = attribute.connection
  )
  check_dataframe_variable(
    df = stratum,
    variable = c("ExternalCode", "Stratum"),
    name = "habitat.txt"
  )

  observation <- stratum %>%
    group_by_(~Stratum) %>%
    mutate_(
      Weight = ~n()
    ) %>%
    select_(~Stratum, ~ExternalCode, ~Weight) %>%
    semi_join(observation, by = "ExternalCode") %>%
    group_by_(~Stratum) %>%
    mutate_(Weight = ~ Weight / n()) %>%
    inner_join(observation, by = "ExternalCode")

  return(observation)
}
