#' Get the id of the scheme
#' @inheritParams n2khelper::odbc_connect
#' @importFrom n2khelper odbc_get_id
#' @export
scheme_id <- function(develop = TRUE){
  odbc_get_id(
    table = "Scheme",
    variable = "Description",
    value = "Algemene broedvogels",
    develop = develop
  )
}
