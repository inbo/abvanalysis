#' Read the observations and save them to git and the results database
#' @inheritParams prepare_dataset
#' @param species a data.frame with the NBN key and fingerprint for each species
#' @export
#' @importFrom git2rdata rm_data write_vc prune_meta
#' @importFrom DBI dbQuoteString dbGetQuery
#' @importFrom dplyr %>% inner_join rename semi_join select group_by filter
#' mutate pull
#' @importFrom tidyr nest
#' @importFrom purrr map2 map_int
#' @importFrom rlang .data
prepare_dataset_observation <- function(
  origin, repo, end_date, min_observation = 100, strict = TRUE
) {
  rm_data(root = repo, path = "observation", stage = TRUE)

  # visits
  sprintf("
    SELECT
      fs.id, fv.location_id AS square_id,
      fs.location_id AS point_id,
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
    dbQuoteString(origin, as.character(end_date))
  ) %>%
    dbGetQuery(conn = origin) %>%
    mutate(
      datafield_id = get_field_id(
        repo = repo, table_name = "fieldwork_sample", field_name = "id"
      )
    ) -> observations
  write_vc(
    observations, file = "observation/visit", root = repo, stage = TRUE,
    sorting = c("year", "period", "point_id", "id"),
    strict = strict
  )

  # counts per species
  sprintf("
    SELECT
      fo.species_id, fo.sample_id, SUM(fo.number_min) AS count
    FROM projects_project AS pp
    INNER JOIN fieldwork_visit AS fv ON pp.id = fv.project_id
    INNER JOIN fieldwork_sample AS fs ON fv.id = fs.visit_id
    INNER JOIN fieldwork_observation AS fo ON fs.id = fo.sample_id
    WHERE
      pp.name = 'Algemene Broedvogelmonitoring (ABV)' AND
      fv.validation_status != -1 AND
      fv.start_date <= %s AND
      fs.not_counted = FALSE AND
      CAST(EXTRACT(YEAR FROM fv.start_date) || '-3-1' AS TIMESTAMP) <=
        fv.start_date AND
      fv.start_date <=
        CAST(EXTRACT(YEAR FROM fv.start_date) || '-7-16' AS TIMESTAMP)
    GROUP BY fo.species_id, fo.sample_id",
    dbQuoteString(origin, as.character(end_date))) %>%
    dbGetQuery(conn = origin) -> counts
  counts %>%
    semi_join(observations, by = c("sample_id" = "id")) %>%
    mutate(
      count = as.integer(count),
      datafield_id = get_field_id(
        repo = repo, table_name = "fieldwork_sample", field_name = "id"
      )
    ) %>%
    group_by(.data$species_id) %>%
    nest() %>%
    mutate(n = map_int(.data$data, nrow)) %>%
    filter(.data$n >= min_observation) %>%
    mutate(
      filename = file.path("observation", sprintf("%05i", .data$species_id)),
      hash = map2(
        .data$data, .data$filename, write_vc, root = repo, stage = TRUE,
        sorting = "sample_id", strict = strict
      )
    )

  prune_meta(root = repo, path = "observation", stage = TRUE)

  return(invisible(NULL))
}
