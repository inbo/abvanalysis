#' Prepare all datasets
#'
#' Prepare all datasets and store the version into a git repository
#' @param result An open ODBC connection to the results database
#' @param origin `dbplyr::src_dbi` connection to the source database
#' @param scheme.id the fingerprint of the scheme
#' @param end.date the latest date to import. Default to now.
#' @param verbose display the progress
#' @param repo a \code{\link[git2rdata]{repository}} object
#' @param push push the changes to the repository. Defaults to `FALSE`.
#' @param ... arguments passed to \code{\link[git2rdata]{commit}} and
#' \code{\link[git2rdata]{push}}.
#' @export
#' @importFrom assertthat is.string is.flag noNA
#' @importFrom git2rdata rm_data write_vc commit recent_commit push
#' @importFrom utils flush.console
#' @importFrom tibble rownames_to_column
#' @importFrom purrr map pmap map_chr map_dfr
#' @importFrom tidyr unnest nest
#' @importFrom n2kanalysis n2k_import
#' @importFrom n2kupdate store_n2kImport store_datafield
#' @importFrom methods slot
#' @importFrom dplyr %>% mutate transmute n select filter group_by inner_join
#' @importFrom rlang .data
prepare_dataset <- function(
  result,
  origin,
  repo,
  scheme.id,
  end.date = Sys.time(),
  verbose = TRUE,
  push = FALSE,
  ...
){
  assert_that(is.string(scheme.id))
  assert_that(inherits(end.date, "POSIXct"))
  assert_that(length(end.date) == 1)
  assert_that(is.flag(verbose))
  assert_that(noNA(verbose))
  assert_that(is.flag(push), noNA(push))

  if (verbose) {
    message("Importing locations")
    flush.console()
  }
  location_hash <- prepare_dataset_location(
    origin = origin, result = result, repo = repo, scheme.id = scheme.id,
    end.date = end.date
  )
  if (verbose) {
    message("Importing species")
    flush.console()
  }
  stored <- prepare_dataset_species(
    origin = origin, result = result, repo = repo,
    end.date = end.date, scheme.id = scheme.id
  )
  if (verbose) {
    message("Importing observations")
    flush.console()
  }
  observation <- prepare_dataset_observation(
    origin = origin, result = result, repo = repo,
    end.date = end.date, species = stored$species
  )

  commit(
    message = "Automated commit from abvanalysis",
    repo = repo,
    session = TRUE,
    ...
  )

  if (verbose) {
    message("Storing metadata")
    flush.console()
  }
  dbGetQuery(
    result$con,
    "SELECT fingerprint FROM datasource WHERE description = 'Raw data ABV'"
  ) -> datasource
  data.frame(
    fingerprint = c(observation$hashes, stored$hash, location_hash),
    stringsAsFactors = FALSE
  ) %>%
    rownames_to_column("filename") %>%
    mutate(
      commit = map(.data$filename, recent_commit, root = repo, data = FALSE)
    ) %>%
    unnest() %>%
    transmute(
      .data$fingerprint,
      .data$filename,
      import_date = as.POSIXct(.data$when),
      datasource = datasource$fingerprint,
      hash = gsub(".*/([[:xdigit:]]{40}).*", "\\1", .data$filename)
    ) -> dataset
  read_vc("species/species_group_species", root = repo) %>%
    group_by(.data$species_group) %>%
    mutate(
      species = .data$species[1],
      n = n()
    ) %>%
    filter(.data$n == 1) %>%
    ungroup() %>%
    select(-"n") %>%
    inner_join(dataset, by = c("species" = "hash")) -> sgs
  dataset %>%
    filter(!grepl(".*/([[:xdigit:]]{40}).*", .data$filename)) %>%
    select(-"hash") -> dataset
  dbGetQuery(
    result$con,
    "SELECT fingerprint FROM datasource WHERE description = 'n2kresult_develop'"
  ) -> result_datasource
  read_vc("location/location_group", repo) -> location_group
  sgs %>%
    select(
      "species_group", "fingerprint", "filename", "import_date", "datasource"
    ) %>%
    group_by(.data$species_group) %>%
    nest() %>%
    mutate(
      data = map(.data$data, bind_rows, dataset),
      analysis_date = map(.data$data, function(x){max(x[["import_date"]])}),
      import = pmap(
        list(
          sg = .data$species_group,
          ad = .data$analysis_date,
          ds = .data$data
        ),
        function(sg, ad, ds) {
          n2k_import(
            scheme.id = scheme.id,
            result.datasource.id = result_datasource$fingerprint,
            status = "converged",
            species.group.id = sg,
            location.group.id = location_group$fingerprint,
            model.type = "Import source data",
            formula = "~ 1",
            first.imported.year = observation$period[1],
            last.imported.year = observation$period[2],
            analysis.date = ad,
            dataset = ds,
            seed = 20070301
          )
        }
      ),
      hash = map_chr(.data$import, store_n2kImport, conn = result$con)
    ) -> imports
  dbGetQuery(
    result$con,
    "SELECT fingerprint FROM datasource WHERE description = 'Source data ABV'"
  ) -> datasource
  df <- store_datafield(
    datafield = data.frame(
      local_id = c("sample", "observation", "autocomplete"),
      datasource = c(
        rep(datasource$fingerprint, 2), result_datasource$fingerprint
      ),
      table_name = c("fieldwork_sample", "fieldwork_observation", "none"),
      primary_key = "id",
      datafield_type = "integer",
      stringsAsFactors = FALSE
    ),
    conn = result$con
  )
  rm_data(root = repo, path = "metadata", stage = TRUE)
  map_dfr(imports$import, slot, "AnalysisMetadata") %>%
    select(
      scheme = "SchemeID",
      result_datasource = "ResultDatasourceID",
      analysis = "FileFingerprint",
      species_group = "SpeciesGroupID",
      location_group = "LocationGroupID",
      first_imported_year = "FirstImportedYear",
      last_imported_year = "LastImportedYear",
      analysis_date = "AnalysisDate",
      seed = "Seed",
      file_fingerprint = "FileFingerprint"
  ) %>%
    mutate(
      sample_df = df$fingerprint[df$local_id == "sample"],
      observation_df = df$fingerprint[df$local_id == "observation"],
      autocomplete_df = df$fingerprint[df$local_id == "autocomplete"]
    ) %>%
    write_vc(
      file = "metadata/metadata",
      root = repo,
      sorting = "file_fingerprint",
      stage = TRUE
    )
  rm_data(root = repo, path = "metadata", type = "yml", stage = TRUE)
  commit(
    message = "Automated commit from abvanalysis",
    repo = repo,
    session = TRUE,
    ...
  )
  if (push) {
    push(object = repo, ...)
  }
}
