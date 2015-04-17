#' Connect to the source data
#' @inheritParams n2khelper::odbc_connect
#' @importFrom n2khelper odbc_connect
#' @export
connect_source <- function(develop = TRUE){
  odbc_connect(data.source.name = "Source data ABV", develop = develop)
}
