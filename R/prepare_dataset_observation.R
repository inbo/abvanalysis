#' Read the observations and save them to git and the results database
#' @inheritParams prepare_dataset
#' @param species a data.frame with the NBN key and fingerprint for each species
#' @export
#' @importFrom git2rdata rm_data write_vc prune_meta
#' @importFrom DBI dbQuoteString dbGetQuery
#' @importFrom dplyr %>% inner_join rename semi_join select group_by filter mutate pull
#' @importFrom tidyr nest
#' @importFrom purrr map2 map_int
#' @importFrom rlang .data
prepare_dataset_observation <- function(
  origin, result, repo, end.date, species
){
  rm_data(root = repo, path = "observation", stage = TRUE)
  sprintf("
    SELECT
      fs.id AS sample_id, fv.location_id, fs.location_id AS sub_location_id,
      EXTRACT(YEAR FROM fv.start_date) AS year,
      CASE
        WHEN fv.start_date <
          CAST(EXTRACT(YEAR FROM fv.start_date) || '-04-16' AS TIMESTAMP) THEN 1
        WHEN fv.start_date <
          CAST(EXTRACT(YEAR FROM fv.start_date) || '-06-01' AS TIMESTAMP) THEN 2
        ELSE 3
      END AS period
    FROM projects_project AS pp
    INNER JOIN fieldwork_visit AS fv ON pp.id = fv.project_id
    INNER JOIN fieldwork_sample AS fs ON fv.id = fs.visit_id
    WHERE
      pp.name = 'Algemene Broedvogelmonitoring (ABV)' AND
      fv.validation_status != -1 AND
      fv.start_date <= %s AND
      fs.not_counted = FALSE AND
      CAST(EXTRACT(YEAR FROM fv.start_date) || '-3-1' AS TIMESTAMP) <=
        fv.start_date AND
      fv.start_date <=
          CAST(EXTRACT(YEAR FROM fv.start_date) || '-7-16' AS TIMESTAMP)",
    dbQuoteString(origin$con, as.character(end.date))
  ) %>%
    dbGetQuery(conn = origin$con) -> import_observations
  dbGetQuery(
    conn = result$con, "
    SELECT
      l.fingerprint AS location,
      CAST(l.external_code AS INTEGER) AS external_code
    FROM datafield AS df
    INNER JOIN datasource AS ds ON df.datasource = ds.id
    INNER JOIN location AS l on l.datafield = df.id
    WHERE
      ds.description = 'Source data ABV' AND
      df.table_name = 'locations_location'"
  ) -> import_location
  import_observations %>%
    inner_join(
      import_location,
      by = c("location_id" = "external_code")
    ) %>%
    inner_join(
      import_location %>%
        rename(sublocation = "location"),
      by = c("sub_location_id" = "external_code")
    ) %>%
    select("sample_id", "location", "sublocation", "year", "period") ->
    observations
  hash <- write_vc(
    observations,
    file = "observation/visit",
    root = repo,
    sorting = "sample_id",
    stage = TRUE
  )
  sprintf("
    SELECT
      fo.id AS observation_id,
      fo.sample_id,
      s.reference_inbo AS nbn_key,
      fo.number_min AS count
    FROM projects_project AS pp
    INNER JOIN fieldwork_visit AS fv ON pp.id = fv.project_id
    INNER JOIN fieldwork_sample AS fs ON fv.id = fs.visit_id
    INNER JOIN fieldwork_observation AS fo ON fs.id = fo.sample_id
    INNER JOIN species_species AS s on fo.species_id = s.id
    WHERE
      pp.name = 'Algemene Broedvogelmonitoring (ABV)' AND
      fv.validation_status != -1 AND
      fv.start_date <= %s AND
      fs.not_counted = FALSE AND
      CAST(EXTRACT(YEAR FROM fv.start_date) || '-3-1' AS TIMESTAMP) <=
        fv.start_date AND
      fv.start_date <=
        CAST(EXTRACT(YEAR FROM fv.start_date) || '-7-16' AS TIMESTAMP)",
    dbQuoteString(origin$con, as.character(end.date))
  ) %>%
    dbGetQuery(conn = origin$con) %>%
    semi_join(observations, by = "sample_id") %>%
    group_by(.data$nbn_key) %>%
    nest() %>%
    mutate(n = map_int(.data$data, nrow)) %>%
    filter(.data$n >= 100) %>%
    inner_join(species, by = "nbn_key") %>%
    mutate(
      fingerprint = paste0("observation/", .data$fingerprint),
      hash = map2(
        .data$data,
        .data$fingerprint,
        write_vc,
        root = repo,
        sorting = c("sample_id", "observation_id"),
        stage = TRUE
      )
    ) %>%
    pull("hash") %>%
    unlist() %>%
    c(hash) -> hashes

  prune_meta(root = repo, path = "observation", stage = TRUE)

  return(list(hashes = hashes, period = range(observations$year)))
}
