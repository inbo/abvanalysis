#' Get the id of the data source
#' @inheritParams connect_source
#' @importFrom n2khelper odbc_get_id
#' @export
datasource_id <- function(result.channel){
  odbc_get_id(
    table = "Datasource",
    variable = "Description",
    value = "Source data ABV",
    channel = result.channel
  )
}

#' Get the id of the data source of the results
#' @inheritParams connect_source
#' @importFrom n2khelper odbc_get_id
#' @export
result_datasource_id <- function(result.channel){
  odbc_get_id(
    table = "Datasource",
    variable = "Description",
    value = "Results",
    channel = result.channel
  )
}
