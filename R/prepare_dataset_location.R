#' import and store location metadata
#' @inheritParams prepare_dataset
#' @inheritParams git2rdata::write_vc
#' @importFrom DBI dbQuoteString dbGetQuery
#' @importFrom dplyr %>% arrange count filter full_join inner_join mutate
#' row_number transmute
#' @importFrom rlang .data
#' @importFrom git2rdata prune_meta read_vc rm_data write_vc
#' @export
prepare_dataset_location <- function(origin, repo, end_date, strict = TRUE) {
  rm_data(root = repo, path = "location", stage = TRUE)

  # import sampling framework
  sampling_frame <- read_vc("sampling_frame", repo)
  sampling_frame %>%
    count(description = .data$Stratum) %>%
    filter(!is.na(.data$description)) %>%
    arrange(.data$description) %>%
    mutate(description = as.character(.data$description)) -> new_strata
  strata <- try(
    read_vc(file.path("location", "stratum"), root = repo), silent = TRUE
  )
  if (inherits(strata, "try-error")) {
    new_strata %>%
      mutate(id = row_number()) -> strata
  } else {
    strata %>%
      full_join(new_strata, by = "description") %>%
      arrange(.data$id) %>%
      transmute(
        .data$description,
        n = ifelse(is.na(.data$id), .data$n.y, .data$n.x),
        id = ifelse(
          is.na(.data$id),
          max(.data$id, na.rm = TRUE) + cumsum(is.na(.data$id)),
          .data$id
        )
      ) -> strata
  }
  write_vc(
    strata, file = file.path("location", "stratum"), root = repo,
    sorting = "description", stage = TRUE, strict = strict
  )
  strata %>%
    select(stratum = .data$description, stratum_id = .data$id) %>%
    inner_join(
      sampling_frame %>%
        transmute(
          description = sprintf("ABV_%s", .data$ExternalCode),
          stratum = .data$Stratum
        ),
      by = c("stratum")
    ) %>%
    select(-.data$stratum) -> strata

  # import UTM squares
  sprintf("
    WITH cte AS (
      SELECT fv.location_id
      FROM projects_project AS pp
      INNER JOIN fieldwork_visit AS fv ON pp.id = fv.project_id
      INNER JOIN fieldwork_sample AS fs ON fv.id = fs.visit_id
      WHERE
        pp.name = 'Algemene Broedvogelmonitoring (ABV)' AND
        fv.validation_status != -1 AND
        fv.start_date <= %s AND
        fs.not_counted = FALSE
      GROUP BY fv.location_id
    )

    SELECT
      l.id, l.name AS description
    FROM cte
    INNER JOIN locations_location AS l ON cte.location_id = l.id",
    dbQuoteString(origin, as.character(end_date))
  ) %>%
    dbGetQuery(conn = origin) %>%
    inner_join(strata, by = "description") %>%
    mutate(
      datafield_id = get_field_id(
        repo = repo, table_name = "locations_location", field_name = "id"
      )
    ) %>%
    write_vc(
      file = file.path("location", "square"), root = repo,
      sorting = "description", stage = TRUE, strict = strict
    )

  # import points
  sprintf("
    WITH cte AS (
      SELECT fs.location_id AS id, fv.location_id AS square_id
      FROM projects_project AS pp
      INNER JOIN fieldwork_visit AS fv ON pp.id = fv.project_id
      INNER JOIN fieldwork_sample AS fs ON fv.id = fs.visit_id
      WHERE
        pp.name = 'Algemene Broedvogelmonitoring (ABV)' AND
        fv.validation_status != -1 AND
        fv.start_date <= %s AND
        fs.not_counted = FALSE
      GROUP BY fv.location_id, fs.location_id
    )

    SELECT
      l.id, cte.square_id, l.name AS description
    FROM cte
    INNER JOIN locations_location AS l ON cte.id = l.id",
    dbQuoteString(origin, as.character(end_date))
  ) %>%
    dbGetQuery(conn = origin) %>%
    mutate(
      datafield_id = get_field_id(
        repo = repo, table_name = "locations_location", field_name = "id"
      )
    ) %>%
    write_vc(
      file = file.path("location", "point"), root = repo, stage = TRUE,
      sorting = "description", strict = strict
    )

  prune_meta(root = repo, path = "location", stage = TRUE)

  return(invisible(NULL))
}
