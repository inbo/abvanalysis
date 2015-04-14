#' Connect to the rawdata
#' @inheritParams n2khelper::odbc_connect
#' @importFrom n2khelper odbc_connect
#' @export
connect_rawdata <- function(develop = TRUE){
  odbc_connect(data.source.name = "Raw data ABV", develop = develop)
}
