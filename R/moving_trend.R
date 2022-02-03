#' Create weight matrix for linear trend over a moving window
#' @param n_total Length of the time series.
#' Defaults to `20`.
#' @param n_window Size of the moving window.
#' Defaults to `10`.
#' @param start Optional offset for the time series.
#' Defaults to `0`.
#' @export
#' @importFrom assertthat assert_that is.count is.number
moving_trend <- function(n_total = 20, n_window = 10, start = 0) {
  assert_that(
    is.count(n_total), is.count(n_window), n_window <= n_total, is.number(start)
  )
  trend_coef <- seq_len(n_window) - (n_window + 1) / 2
  trend_coef <- trend_coef / sum(trend_coef ^ 2)
  lc <- vapply(
    seq_len(n_total - n_window + 1),
    function(i) {
      c(rep(0, i - 1), trend_coef, rep(0, n_total - n_window - i + 1))
    },
    numeric(n_total)
  )
  colnames(lc) <- sprintf(
    "trend_%0.1f_%i",
    seq_len(ncol(lc)) + start - 1 + (n_window - 1) / 2, n_window
  )
  t(lc)
}
