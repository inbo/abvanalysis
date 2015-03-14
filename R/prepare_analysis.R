#' Prepare all datasets and a to do list of models
#' @inheritParams prepare_analysis_dataset
#' @export
#' @importFrom n2khelper list_files_git check_single_strictly_positive_integer
prepare_analysis <- function(min.observation = 100){
  min.observation <- check_single_strictly_positive_integer(min.observation)
  
  rawdata.files <- list_files_git(path = "abv")
  species <- as.integer(gsub("\\.txt$", "", rawdata.files))
  analysis <- lapply(species, prepare_analysis_dataset, min.observation = min.observation)
  return(species[sapply(analysis, is.null)])
}
