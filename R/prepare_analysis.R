#' Prepare all datasets and a to do list of models
#' @inheritParams prepare_analysis_dataset
#' @export
#' @importFrom n2khelper list_files_git check_single_strictly_positive_integer
prepare_analysis <- function(min.observation = 100){
  min.observation <- check_single_strictly_positive_integer(min.observation)
  
  rawdata.files <- list_files_git(path = "abv")
  species <- as.integer(gsub("\\.txt$", "", rawdata.files))
  sha <- sapply(species, prepare_analysis_dataset, min.observation = min.observation)
  sha <- do.call(rbind, sha)
  model.set <- data.frame(
    ModelSet = c(
      "0 + fYear + Period + (1|LocationID) + (1|SubLocationID) + (1|RowID)",
      "0 + fCycle + Period + (1|LocationID) + (1|SubLocationID) + (1|RowID)",
      "cYear + Period + (1|LocationID) + (1|SubLocationID) + (1|RowID)"
    )
  )
  to.do <- merge(sha, model.set)
  to.do$Covariate <- to.do$ModelSet
  save(to.do, file = "todo.rda")
  return(to.do)
}
