#' Get the id of the scheme
#' @inheritParams connect_source
#' @importFrom n2khelper odbc_get_id
#' @export
scheme_id <- function(result.channel){
  odbc_get_id(
    table = "scheme",
    variable = "description",
    value = "Algemene broedvogels",
    channel = result.channel
  )
}
