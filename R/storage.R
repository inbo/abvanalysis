#' store the n2kModel and retrieve its fingerprints
#' @return a data.frame with the fingerprint, status and status_fingerprint as stored
#' @param model a n2kModel object
#' @inheritParams n2kanalysis::store_model
#' @export
#' @importFrom n2kanalysis store_model read_model get_file_fingerprint status get_status_fingerprint
storage <- function(model, base, project, overwrite = FALSE) {
  x <- store_model(model, base = base, project = project, overwrite = overwrite)
  x <- read_model(basename(x), base = base, project = project)
  data.frame(
    fingerprint = get_file_fingerprint(x),
    status = status(x),
    status_fingerprint = get_status_fingerprint(x),
    stringsAsFactors = FALSE
  )
}
