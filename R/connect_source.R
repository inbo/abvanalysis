#' Connect to the source data
#' @param result.channel An open RODBC connection to the results database
#' @importFrom n2khelper odbc_connect
#' @export
connect_source <- function(result.channel){
  odbc_connect(data.source.name = "Source data ABV", channel = result.channel)
}
