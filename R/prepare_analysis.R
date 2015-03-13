#' Prepare all datasets and a to do list of models
#' @inheritParams prepare_analysis_dataset
#' @export
#' @importFrom n2khelper list_files_git
prepare_analysis <- function(min.observation = 100){
  rawdata.files <- list_files_git(path = "abv")
  species <- as.integer(gsub("\\.txt$", "", rawdata.files))
  sha <- sapply(species, prepare_analysis_dataset, min.observation = min.observation)
  sha <- do.call(rbind, sha)
  to.do <- sha
  save(to.do, file = "todo.rda")
  return(to.do)
}
