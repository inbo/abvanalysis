#' Calculate the matrix for linear combinations of strata
#' @inheritParams get_nonlinear_lincomb
#' @export
#' @importFrom n2khelper check_dataframe_variable
#' @importFrom dplyr %>% group_by_ summarise_ summarise_each inner_join mutate_ select_ bind_cols funs
#' @importFrom stats model.matrix
get_linear_lincomb <- function(
  dataset,
  time.var,
  stratum.var = "fStratum",
  formula
){
  check_dataframe_variable(
    df = dataset,
    variable = c(time.var, stratum.var, "Weight", "fPeriod")
  )

  available.weight <- dataset %>%
    group_by_(stratum.var) %>%
    summarise_(Weight = ~mean(Weight)) %>%
    mutate_(
      Weight = ~Weight / sum(Weight),
      fPeriod = ~sort(unique(dataset$fPeriod))[1]
    )
  trend.weight <- available.weight %>%
    merge(
      matrix(1, dimnames = list(1, time.var))
    )
  available.weight <- available.weight %>%
    merge(
      unique(dataset[, time.var, drop = FALSE])
    )
  mm <- available.weight %>%
    model.matrix(object = formula) %>%
    as.data.frame() %>%
    '*'(available.weight$Weight) %>%
    bind_cols(
      available.weight %>%
        select_(ID = time.var)
    ) %>%
    group_by_(~ID) %>%
    summarise_each(funs = funs(sum))
  mm.trend <- trend.weight %>%
    model.matrix(object = formula) %>%
    as.data.frame() %>%
    '*'(trend.weight$Weight)
  mm.trend[, -grep(":cYear", colnames(mm.trend))] <- 0
  mm.trend <- mm.trend %>%
    summarise_each(funs = funs(sum)) %>%
    mutate_(ID = ~"Trend")
  weights <- rbind(mm, mm.trend)
  weights[, -1] %>%
    as.list() %>%
    lapply(
      function(x){
        names(x) <- weights$ID
        x
      }
    )
}
