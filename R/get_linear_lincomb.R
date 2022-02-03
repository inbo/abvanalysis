#' Calculate the matrix for linear combinations of strata
#' @inheritParams get_nonlinear_lincomb
#' @param formula the formula for the trend component
#' @export
#' @importFrom assertthat assert_that has_name is.string
#' @importFrom dplyr %>% across bind_cols bind_rows distinct group_by inner_join
#' mutate rename select summarise
#' @importFrom rlang !! :=
#' @importFrom stats model.matrix
get_linear_lincomb <- function(
  dataset, time_var, stratum_var = "stratum", stratum_weights, formula
){
  assert_that(is.string(time_var), is.string(stratum_var))
  assert_that(
    inherits(dataset, "data.frame"), has_name(dataset, time_var),
    has_name(dataset, stratum_var)
  )
  assert_that(
    inherits(stratum_weights, "data.frame"),
    has_name(stratum_weights, "weight"), has_name(stratum_weights, stratum_var)
  )

  dataset %>%
    select(!!stratum_var, !!time_var) %>%
    distinct() %>%
    inner_join(stratum_weights, by = stratum_var) -> all_weight
  model.matrix(object = formula, all_weight) -> mm
  (as.data.frame(mm) * all_weight$weight) %>%
    mutate(ID = sprintf("%02i", all_weight[[time_var]])) %>%
    group_by(.data$ID) %>%
    summarise(across(.fns = sum)) -> mm
  stratum_weights %>%
    mutate(!!time_var := 1) -> trend_weight
  mm_trend <- model.matrix(object = formula, trend_weight)
  mm_trend <- as.data.frame(mm_trend) * trend_weight$weight
  mm_trend[, -grep(":", colnames(mm_trend))] <- 0
  if (has_name(mm_trend, "(Intercept)")) {
    mm_trend$`(Intercept)` <- 0
  }
  mm_trend %>%
    summarise(across(.fns = sum)) %>%
    mutate(ID = "Trend") %>%
    bind_rows(mm) -> weights
  w_names <- weights$ID
  weights %>%
    select(-"ID") %>%
    as.list() -> weights
  names(weights[[1]]) <- w_names
  weights
}
