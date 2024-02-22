#' Read the observations and save them to git and the results database
#' @inheritParams prepare_dataset
#' @export
#' @importFrom git2rdata rm_data write_vc prune_meta
#' @importFrom DBI dbGetQuery dbQuoteIdentifier dbQuoteString Id
#' @importFrom dplyr %>% inner_join rename semi_join select group_by filter
#' mutate pull
#' @importFrom tidyr nest
#' @importFrom purrr map2 map_int
#' @importFrom rlang .data
prepare_dataset_observation <- function(
  origin, repo, end_date, min_observation = 100, strict = TRUE, db_scheme = ""
) {
  rm_data(root = repo, path = "observation", stage = TRUE)

  # visits
  sprintf("
    SELECT
      fs.id, fs.location_id AS point_id,
      %s AS year,
      %s AS period
    FROM %s AS pp
    INNER JOIN %s AS fv ON pp.id = fv.project_id
    INNER JOIN %s AS fs ON fv.id = fs.visit_id
    WHERE
      pp.name = 'Algemene Broedvogelmonitoring (ABV)' AND
      fv.validation_status != -1 AND
      fv.start_date <= %s AND
      fs.not_counted = %s AND
      %s",
          ifelse(
            inherits(origin, "Microsoft SQL Server"),
            "YEAR(fv.start_date)",
            "EXTRACT(YEAR FROM fv.start_date)"
          ),
          ifelse(
            inherits(origin, "Microsoft SQL Server"),
            "CASE
  WHEN
    fv.start_date < CAST(
      CAST(YEAR(fv.start_date) AS VARCHAR) + '-04-16' AS DATETIME
    )
    THEN 1
  WHEN
    fv.start_date < CAST(
      CAST(YEAR(fv.start_date) AS VARCHAR) + '-06-01' AS DATETIME
    )
    THEN 2
  ELSE 3
END",
            "CASE
  WHEN fv.start_date <
    CAST(EXTRACT(YEAR FROM fv.start_date) || '-04-16' AS TIMESTAMP) THEN 1
  WHEN fv.start_date <
    CAST(EXTRACT(YEAR FROM fv.start_date) || '-06-01' AS TIMESTAMP) THEN 2
  ELSE 3
END"
          ),
          dbQuoteIdentifier(
            origin, Id(scheme = db_scheme, table = "projects_project")
          ),
          dbQuoteIdentifier(
            origin, Id(scheme = db_scheme, table = "fieldwork_visit")
          ),
          dbQuoteIdentifier(
            origin, Id(scheme = db_scheme, table = "fieldwork_sample")
          ),
          dbQuoteString(origin, as.character(end_date)),
          ifelse(inherits(origin, "Microsoft SQL Server"), "0", "FALSE"),
          ifelse(
            inherits(origin, "Microsoft SQL Server"),
            "CAST(
      CAST(YEAR(fv.start_date) AS VARCHAR) + '-3-1' AS DATETIME
    ) <= fv.start_date AND
    fv.start_date <= CAST(
      CAST(YEAR(fv.start_date) AS VARCHAR) + '-7-16' AS DATETIME
    )",
            "CAST(EXTRACT(YEAR FROM fv.start_date) || '-3-1' AS TIMESTAMP) <=
        fv.start_date AND
      fv.start_date <=
          CAST(EXTRACT(YEAR FROM fv.start_date) || '-7-16' AS TIMESTAMP)"
          )
  ) |>
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
    FROM %s AS pp
    INNER JOIN %s AS fv ON pp.id = fv.project_id
    INNER JOIN %s AS fs ON fv.id = fs.visit_id
    INNER JOIN %s AS fo ON fs.id = fo.sample_id
    WHERE
      pp.name = 'Algemene Broedvogelmonitoring (ABV)' AND
      fv.validation_status != -1 AND
      fv.start_date <= %s AND
      fs.not_counted = %s AND
      %s
    GROUP BY fo.species_id, fo.sample_id",
    dbQuoteIdentifier(
      origin, Id(scheme = db_scheme, table = "projects_project")
    ),
    dbQuoteIdentifier(
      origin, Id(scheme = db_scheme, table = "fieldwork_visit")
    ),
    dbQuoteIdentifier(
      origin, Id(scheme = db_scheme, table = "fieldwork_sample")
    ),
    dbQuoteIdentifier(
      origin, Id(scheme = db_scheme, table = "fieldwork_observation")
    ),
    dbQuoteString(origin, as.character(end_date)),
    ifelse(inherits(origin, "Microsoft SQL Server"), "0", "FALSE"),
    ifelse(
      inherits(origin, "Microsoft SQL Server"),
      "CAST(
      CAST(YEAR(fv.start_date) AS VARCHAR) + '-3-1' AS DATETIME
    ) <= fv.start_date AND
    fv.start_date <= CAST(
      CAST(YEAR(fv.start_date) AS VARCHAR) + '-7-16' AS DATETIME
    )",
      "CAST(EXTRACT(YEAR FROM fv.start_date) || '-3-1' AS TIMESTAMP) <=
        fv.start_date AND
      fv.start_date <=
          CAST(EXTRACT(YEAR FROM fv.start_date) || '-7-16' AS TIMESTAMP)"
    )
  ) %>%
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
