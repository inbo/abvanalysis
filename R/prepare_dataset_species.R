#' Read the species groups and save them to git and the results database
#' @inheritParams prepare_dataset
#' @importFrom DBI dbQuoteString dbGetQuery
#' @importFrom dplyr %>% transmute select mutate bind_rows
#' @importFrom rlang .data
#' @importFrom n2kupdate store_species_group_species
#' @importFrom git2rdata rm_data write_vc prune_meta
#' @export
prepare_dataset_species <- function(origin, result, repo, scheme.id, end.date){
  sprintf("
    WITH cte AS (
      SELECT fo.species_id
      FROM projects_project AS pp
      INNER JOIN fieldwork_visit AS fv ON pp.id = fv.project_id
      INNER JOIN fieldwork_sample AS fs ON fv.id = fs.visit_id
      INNER JOIN fieldwork_observation AS fo ON fs.id = fo.sample_id
      WHERE
        pp.name = 'Algemene Broedvogelmonitoring (ABV)' AND
        fv.validation_status != -1 AND
        fv.start_date <= %s AND
        fs.not_counted = FALSE
      GROUP BY species_id
    )

    SELECT
      'S' || s.id AS local_id,
      s.id AS external_code,
      s.scientific_name,
      s.name AS nl,
      s.reference_inbo AS nbn_key
    FROM cte
    INNER JOIN species_species AS s ON cte.species_id = s.id
    WHERE s.reference_inbo IS NOT NULL",
    dbQuoteString(origin$con, as.character(end.date))
  ) %>%
    dbGetQuery(conn = origin$con) -> source_species
  source_species %>%
    transmute(
      source_species_local_id = .data$local_id,
      species_local_id = .data$local_id
    ) -> source_species_species
  source_species %>%
    select(-"external_code") -> species
  source_species %>%
    select("local_id", "external_code", description = "nl") %>%
    mutate(datafield_local_id = 1) -> source_species
  language <- data.frame(
    code = "nl",
    description = "Nederlands",
    stringsAsFactors = FALSE
  )
  dbGetQuery(result$con, "
    SELECT fingerprint AS datasource
    FROM datasource
    WHERE description = 'Source data ABV'
  ") -> datasource
  data.frame(
    local_id = 1,
    datasource = datasource$datasource,
    table_name = "species_species",
    primary_key = "id",
    datafield_type = "integer",
    stringsAsFactors = FALSE
  ) -> datafield
  dbGetQuery(
    origin$con, "
    SELECT
      'G' || id AS local_id,
      REGEXP_REPLACE(name, '(.*) \\(ABV\\)', '\\1') AS description
    FROM species_group
    WHERE REGEXP_REPLACE(name, '.* \\((.*)\\)$', '\\1') = 'ABV'
  ") %>%
    bind_rows(
      species %>%
      select("local_id", description = "nl")
    ) %>%
    mutate(scheme = scheme.id) -> species_group
  dbGetQuery(
    origin$con, "
    SELECT
      'G' || sg.id AS species_group_local_id,
      'S' || sgs.species_id AS species_local_id
    FROM species_group AS sg
    INNER JOIN species_speciesgrouprelation AS sgs ON sg.id = sgs.group_id
    WHERE REGEXP_REPLACE(sg.name, '.* \\((.*)\\)$', '\\1') = 'ABV'
  ") %>%
    bind_rows(
      species %>%
        transmute(
          species_group_local_id = .data$local_id,
          species_local_id = .data$local_id
        )
    ) -> species_group_species
  stored <- store_species_group_species(
    species = species,
    language = language,
    source_species = source_species,
    source_species_species = source_species_species,
    datafield = datafield,
    species_group = species_group,
    species_group_species = species_group_species,
    conn = result$con
  )

  rm_data(repo, "species", stage = TRUE)
  sprintf("
    SELECT sg.fingerprint
    FROM species_group AS sg
    INNER JOIN scheme AS s ON sg.scheme = s.id
    WHERE s.fingerprint = %s",
    dbQuoteString(result$con, scheme.id)
  ) %>%
    dbGetQuery(conn = result$con) %>%
    write_vc(
      file = "species/species_group",
      root = repo,
      sorting = "fingerprint",
      stage = TRUE
    ) -> sg_hash
  sprintf("
    SELECT sg.fingerprint AS species_group, sp.fingerprint AS species
    FROM species_group AS sg
    INNER JOIN scheme AS s ON sg.scheme = s.id
    INNER JOIN species_group_species AS sgs on sg.id = sgs.species_group
    INNER JOIN species AS sp ON sgs.species = sp.id
    WHERE
      s.fingerprint = %s AND
      sgs.destroy IS NULL",
    dbQuoteString(result$con, scheme.id)
  ) %>%
    dbGetQuery(conn = result$con) %>%
    write_vc(
      file = "species/species_group_species",
      root = repo,
      sorting = c("species_group", "species"),
      stage = TRUE
    ) -> sgs_hash
 prune_meta(repo, "species", stage = TRUE)

  list(
    species = stored$species %>%
      select("nbn_key", "fingerprint"),
    hash = c(sg_hash, sgs_hash)
  )
}
