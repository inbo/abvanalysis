#' Get the id of the scheme
#' @inheritParams connect_source
#' @importFrom DBI dbGetQuery
#' @export
scheme_id <- function(result.channel){
  dbGetQuery(
    result.channel$con,
    "SELECT fingerprint FROM scheme WHERE description = 'Algemene broedvogels'"
  ) %>%
    unlist() %>%
    unname()
}
