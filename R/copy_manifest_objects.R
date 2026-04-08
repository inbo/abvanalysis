#' Copy all object of the most recent manifest
#' @export
#' @importFrom dplyr filter
#' @importFrom n2kanalysis connect_inbo_s3 read_manifest read_model
#'   store_manifest store_model
#' @importFrom rlang .data
copy_manifest_objects <- function(
  base,
  project,
  target_folder = ".",
  overwrite = FALSE
) {
  manifest <- read_manifest(base = base, project = project)
  store_manifest(manifest, base = target_folder, project = project)
  manifest |>
    slot("Manifest") |>
    filter(is.na(.data$parent)) -> todo
  file.path(target_folder, project) |>
    list.files(recursive = TRUE) -> local_objects
  if (overwrite) {
    grepv("(converged|insufficient_data)", local_objects) -> local_objects
  }
  basename(local_objects) |>
    gsub(pattern = "\\.rds$", replacement = "") -> done
  for (hash in todo$fingerprint[!todo$fingerprint %in% done]) {
    message(hash)
    x <- try(read_model(hash, base = base, project = project))
    while (inherits(x, "try-error")) {
      browser()
      connect_inbo_s3()
      x <- try(read_model(hash, base = base, project = project))
    }
    store_model(
      x,
      base = target_folder,
      project = project,
      overwrite = overwrite
    )
    rm(x)
    gc()
  }
}
