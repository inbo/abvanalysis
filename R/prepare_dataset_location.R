#' import and store location metadata
#' @inheritParams prepare_dataset
#' @importFrom DBI dbQuoteString dbGetQuery
#' @importFrom dplyr %>% mutate transmute distinct filter row_number inner_join bind_rows select count
#' @importFrom rlang .data
#' @importFrom n2kupdate store_location_group_location
#' @importFrom git2rdata write_vc rm_data
#' @export
prepare_dataset_location <- function(origin, result, repo, scheme.id, end.date) {
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
      cte.location_id AS local_id,
      CAST(cte.location_id AS CHARACTER VARYING) AS external_code,
      l.name AS description
    FROM cte
    INNER JOIN locations_location AS l ON cte.location_id = l.id",
    dbQuoteString(origin$con, as.character(end.date))
  ) %>%
    dbGetQuery(conn = origin$con) %>%
    mutate(datafield_local_id = "Source data ABV") -> import_locations
  # import points
  sprintf("
    WITH cte AS (
      SELECT fs.location_id AS local_id, fv.location_id AS parent_local_id
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
      cte.local_id,
      cte.parent_local_id,
      CAST(cte.local_id AS CHARACTER VARYING) AS external_code,
      l.name AS description
    FROM cte
    INNER JOIN locations_location AS l ON cte.local_id = l.id",
    dbQuoteString(origin$con, as.character(end.date))
  ) %>%
    dbGetQuery(conn = origin$con) %>%
    mutate(datafield_local_id = "Source data ABV") -> import_sub_locations
  # import sampling framework
  read_vc("sampling_frame", repo) %>%
    transmute(
      local_id = .data$ExternalCode,
      external_code = .data$local_id,
      description = as.character(.data$Stratum)
    ) %>%
    mutate(datafield_local_id = "Raw data ABV") -> import_habitat

  # data wrangling
  import_habitat %>%
    count(.data$description, .data$datafield_local_id) %>%
    filter(!is.na(.data$description)) %>%
    mutate(
      local_id = row_number(),
      external_code = .data$description,
      .data$datafield_local_id
    ) -> strata
  import_locations %>%
    mutate(hok = gsub("ABV_", "", description)) %>%
    inner_join(
      import_habitat %>%
        select("local_id", stratum = "description"),
      by = c("hok" = "local_id")
    ) %>%
    inner_join(
      strata %>%
        select("description", parent_local_id = "local_id"),
      by = c("stratum" = "description")
    ) %>%
    select(
      "local_id", "parent_local_id", "external_code", "description",
      "datafield_local_id"
    ) %>%
    bind_rows(
      strata %>%
        select(-"n"),
      import_sub_locations
    ) -> locations
  data.frame(
      local_id = 1,
      description = "Vlaanderen",
      scheme = scheme.id,
      stringsAsFactors = FALSE
    ) -> location_group
  strata %>%
    transmute(
      location_group_local_id = 1,
      location_local_id = .data$local_id
    ) -> location_group_location
  dbGetQuery(result$con, "
    SELECT fingerprint AS datasource, description AS local_id
    FROM datasource
    WHERE description IN ('Source data ABV', 'Raw data ABV')
  ") %>%
    mutate(
      table_name = ifelse(
        grepl("Source", .data$local_id),
        "locations_location",
        "sampling_frame"
      ),
      primary_key = ifelse(
        grepl("Source", .data$local_id),
        "id",
        "ExternalCode"
      ),
      datafield_type = ifelse(
        grepl("Source", .data$local_id),
        "integer",
        "character"
      )
    ) -> datafield
  store_location_group_location(
    location = locations,
    location_group_location = location_group_location,
    location_group = location_group,
    datafield = datafield,
    conn = result$con
  )

  rm_data(repo, "location", stage = TRUE)
  sprintf(
    "SELECT lg.fingerprint
    FROM location_group AS lg
    INNER JOIN scheme AS s ON lg.scheme = s.id
    WHERE s.fingerprint = %s",
    dbQuoteString(result$con, scheme.id)
  ) %>%
    dbGetQuery(conn = result$con) %>%
    write_vc(
      file = "location/location_group",
      root = repo,
      sorting = "fingerprint",
      stage = TRUE
    ) -> location_group_hash
  sprintf(
    "SELECT lg.fingerprint AS location_group, l.fingerprint AS location
    FROM location_group AS lg
    INNER JOIN scheme AS s ON lg.scheme = s.id
    INNER JOIN location_group_location AS lgl ON lg.id = lgl.location_group
    INNER JOIN location AS l ON lgl.location = l.id
    WHERE s.fingerprint = %s AND lgl.destroy IS NULL",
    dbQuoteString(result$con, scheme.id)
  ) %>%
    dbGetQuery(conn = result$con) %>%
    write_vc(
      file = "location/location_group_location",
      root = repo,
      sorting = c("location_group", "location"),
      stage = TRUE
    ) -> location_group_location_hash
  sprintf("
    WITH RECURSIVE cl AS (
      SELECT
        l.id,
        l.parent_location,
        fingerprint AS location,
        CAST('' AS CHARACTER VARYING) parent
      FROM location AS l
      WHERE EXISTS (
        SELECT lgl.location
        FROM location_group AS lg
        INNER JOIN location_group_location AS lgl ON lg.id = lgl.location_group
        INNER JOIN scheme AS s ON lg.scheme = s.id
        WHERE s.fingerprint = %s AND lgl.destroy IS NULL AND l.id = lgl.location
      )
    UNION ALL
      SELECT
        l.id,
        l.parent_location,
        l.fingerprint AS location,
        cl.location AS parent
      FROM location AS l INNER JOIN cl ON l.parent_location = cl.id
    )

    SELECT cl.location, cl.parent FROM cl",
    dbQuoteString(result$con, scheme.id)
  ) %>%
    dbGetQuery(conn = result$con) %>%
    write_vc(
      file = "location/location",
      root = repo,
      sorting = c("location", "parent"),
      stage = TRUE
    ) -> location_hash
  sprintf(
    "SELECT l.fingerprint, l.description
    FROM scheme AS s
    INNER JOIN location_group AS lg ON lg.scheme = s.id
    INNER JOIN location_group_location AS lgl ON lg.id = lgl.location_group
    INNER JOIN location AS l ON lgl.location = l.id
    WHERE
      s.fingerprint = %s AND
      lgl.destroy IS NULL",
    dbQuoteString(result$con, scheme.id)
  ) %>%
    dbGetQuery(conn = result$con) %>%
    inner_join(strata, by = "description") %>%
    select("fingerprint", "n") %>%
    write_vc(
      file = "location/strata",
      root = repo,
      sorting = "fingerprint",
      stage = TRUE
    ) -> strata_hash
  rm_data(repo, "location", type = "yml", stage = TRUE)

  c(location_hash, location_group_location_hash, location_group_hash,
    strata_hash)
}
