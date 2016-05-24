#' Calculate the matrix for linear combinations of strata
#' @param dataset the raw dataset
#' @param time.var the name of the time variable
#' @param stratum.var the name of the stratum variable
#' @param formula the formula for the model.matrix
#' @export
#' @importFrom n2khelper check_dataframe_variable
#' @importFrom dplyr %>% group_by_ summarise_ inner_join mutate_ select_ bind_cols summarise_each
#' @importFrom stats model.matrix
get_nonlinear_lincomb <- function(
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
    group_by_(time.var, stratum.var, ~Weight) %>%
    summarise_() %>%
    group_by_(time.var, stratum.var) %>%
    summarise_(Weight = ~mean(Weight))
  available.weight <- available.weight %>%
    group_by_(time.var) %>%
    summarise_(TotalWeight = ~sum(Weight)) %>%
    inner_join(available.weight, by = time.var) %>%
    mutate_(
      Weight = ~Weight / TotalWeight,
      fPeriod = ~sort(unique(dataset$fPeriod))[1]
    ) %>%
    select_(~-TotalWeight)
  weights <-
    available.weight %>%
      model.matrix(object = formula) %>%
    '*'(available.weight$Weight) %>%
    as.data.frame() %>%
    bind_cols(
      available.weight %>%
        select_(ID = time.var)
    ) %>%
    group_by_(~ID) %>%
    summarise_each(funs = funs(sum))
  rownames(weights) <- weights$ID
  as.matrix(weights[, -1])
}
