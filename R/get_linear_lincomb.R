#' Calculate the matrix for linear combinations of strata
#' @inheritParams get_nonlinear_lincomb
#' @param formula the formula for the trend component
#' @export
#' @importFrom assertthat assert_that is.string has_name
#' @importFrom dplyr %>% select distinct inner_join bind_cols rename mutate bind_rows group_by summarise_all funs
#' @importFrom rlang !! :=
#' @importFrom stats model.matrix
get_linear_lincomb <- function(
  dataset,
  time.var,
  stratum.var = "stratum",
  stratum_weights,
  formula
){
  assert_that(is.string(time.var))
  assert_that(is.string(stratum.var))
  assert_that(
    inherits(dataset, "data.frame"),
    has_name(dataset, time.var),
    has_name(dataset, stratum.var)
  )
  assert_that(
    inherits(stratum_weights, "data.frame"),
    has_name(stratum_weights, "weight"),
    has_name(stratum_weights, stratum.var)
  )

  dataset %>%
    select(stratum.var, time.var) %>%
    distinct() %>%
    inner_join(stratum_weights, by = stratum.var) -> all.weight
  model.matrix(object = formula, all.weight) -> mm
  (as.data.frame(mm) * all.weight$weight) %>%
    bind_cols(
      all.weight[, time.var, drop = FALSE]
    ) %>%
    rename(ID = time.var) %>%
    mutate(ID = as.character(.data$ID)) %>%
    group_by(.data$ID) %>%
    summarise_all(.funs = funs(sum)) -> mm
  stratum_weights %>%
    mutate(!!time.var := 1) -> trend.weight
  mm.trend <- model.matrix(object = formula, trend.weight)
  mm.trend <- as.data.frame(mm.trend) * trend.weight$weight
  mm.trend[, -grep(":", colnames(mm.trend))] <- 0
  mm.trend %>%
    summarise_all(.funs = funs(sum)) %>%
    mutate(ID = "Trend") %>%
    bind_rows(mm) -> weights
  weights %>%
    select(-"ID") %>%
    as.list() %>%
    lapply(
      function(x){
        names(x) <- weights$ID
        x
      }
    )
}
