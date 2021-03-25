#' Calculate the matrix for linear combinations of strata
#' @param dataset the raw dataset
#' @param stratum_weights a dataframe with stratum weights
#' @param time.var the name of the time variable
#' @param stratum.var the name of the stratum variable
#' @param label.var an optional variable used to label the time variable, `time.var` will be used when missing
#' @export
#' @importFrom assertthat assert_that is.string has_name
#' @importFrom dplyr %>%
get_nonlinear_lincomb <- function(
  dataset,
  stratum_weights,
  time.var,
  label.var,
  stratum.var = "stratum"
){
  assert_that(is.string(time.var))
  assert_that(is.string(stratum.var))
  if (missing(label.var)) {
    label.var <- time.var
  } else {
    assert_that(is.string(label.var))
  }
  assert_that(
    inherits(dataset, "data.frame"),
    has_name(dataset, time.var),
    has_name(dataset, label.var)
  )
  assert_that(
    inherits(stratum_weights, "data.frame"),
    has_name(stratum_weights, "weight"),
    has_name(stratum_weights, stratum.var)
  )

  min(dataset[[time.var]]):max(dataset[[time.var]]) %>%
    length() %>%
    diag() %>%
    outer(stratum_weights$weight) %>%
    apply(3, as.data.frame) %>%
    unlist(recursive = FALSE) %>%
    do.call(what = cbind) -> lc
  rownames(lc) <- min(dataset[[label.var]]):max(dataset[[label.var]])
  colnames(lc) <- outer(
    min(dataset[[time.var]]):max(dataset[[time.var]]) %>%
    sprintf(fmt = "%2$s%1$s:", time.var),
    stratum_weights[[stratum.var]] %>%
      sprintf(fmt = "stratum%s"),
    FUN = "paste0"
  ) %>%
  as.vector()
  return(lc)
}
