#' Prepare all datasets
#'
#' Prepare all datasets and store the version into a git repository
#' @param origin `DBI` connection to the source database
#' @param end_date the latest date to import.
#' Default to now.
#' @param verbose display the progress
#' @param repo a [git2rdata::repository()] object
#' @param push push the changes to the repository.
#' Defaults to `FALSE`.
#' @param min_observation The minimum number of observations for taking a
#' species into account.
#' Defaults to `100`.
#' @param ... arguments passed to [git2rdata::commit()] and [git2rdata::push()].
#' @inheritParams git2rdata::write_vc
#' @export
#' @importFrom assertthat is.string is.flag noNA
#' @importFrom git2rdata commit push
#' @importFrom n2kanalysis display
#' @importFrom utils flush.console
#' @importFrom rlang .data
prepare_dataset <- function(
  origin, repo, end_date = Sys.time(), verbose = TRUE, push = FALSE,
  strict = TRUE, min_observation = 100, ...
) {
  assert_that(
    inherits(end_date, "POSIXct"), length(end_date) == 1, noNA(end_date)
  )
  assert_that(is.flag(verbose), noNA(verbose))
  assert_that(is.flag(push), noNA(push))

  display(verbose, "Importing locations")
  prepare_dataset_location(
    origin = origin, repo = repo, end_date = end_date, strict = strict
  )

  display(verbose, "Importing species")
  prepare_dataset_species(
    origin = origin, repo = repo, end_date = end_date, strict = strict
  )

  display(verbose, "Importing observations")
  prepare_dataset_observation(
    origin = origin, repo = repo, end_date = end_date, strict = strict
  )

  if (length(git2rdata::status(repo)$staged) == 0) {
    return(invisible(NULL))
  }
  commit(
    message = "Automated commit from abvanalysis", repo = repo,
    session = TRUE, ...
  )
  if (!push) {
    return(invisible(NULL))
  }
  push(object = repo, ...)

  return(invisible(NULL))
}
