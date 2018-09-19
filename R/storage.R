#' store the n2kModel and retrieve its fingerprints
#' @return a data.frame with the fingerprint, status and status_fingerprint as stored
#' @param model a n2kModel object
#' @inheritParams n2kanalysis::store_model
#' @export
#' @importFrom n2kanalysis store_model read_model get_file_fingerprint status get_status_fingerprint
#' @importFrom dplyr %>%
storage <- function(model, base, project, overwrite = FALSE) {
  store_model(model, base = base, project = project, overwrite = overwrite) %>%
    basename() %>%
    read_model(base = base, project = project) -> x
  data.frame(
    fingerprint = get_file_fingerprint(x),
    status = status(x),
    status_fingerprint = get_status_fingerprint(x),
    stringsAsFactors = FALSE
  )
}
