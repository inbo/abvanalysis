#' Read the species groups and save them to git and the results database
#' @inheritParams prepare_dataset
#' @inheritParams git2rdata::write_vc
#' @importFrom DBI dbGetQuery dbQuoteString
#' @importFrom digest sha1
#' @importFrom dplyr %>% transmute select mutate bind_rows
#' @importFrom purrr map_chr
#' @importFrom rlang .data
#' @importFrom git2rdata prune_meta rm_data write_vc
#' @export
prepare_dataset_species <- function(origin, repo, end_date, strict = FALSE) {
  rm_data(root = repo, path = "location", stage = TRUE)

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
      s.id, s.scientific_name, s.name AS nl, s.euring_code AS euring
    FROM cte
    INNER JOIN species_species AS s ON cte.species_id = s.id
    WHERE s.reference_inbo IS NOT NULL",
    dbQuoteString(origin, as.character(end_date))
  ) %>%
    dbGetQuery(conn = origin) %>%
    mutate(
      datafield_id = get_field_id(
        repo = repo, table_name = "species_species", field_name = "id"
      )
    ) -> species
  write_vc(
    species, file = file.path("species", "species"), root = repo,
    sorting = "euring", stage = TRUE
  )
  dbGetQuery(
    origin, "
    SELECT
      id AS external_id,
      REGEXP_REPLACE(name, '(.*) \\(ABV\\)', '\\1') AS description
    FROM species_group
    WHERE REGEXP_REPLACE(name, '.* \\((.*)\\)$', '\\1') = 'ABV'
  ") %>%
    mutate(
      datafield_id = get_field_id(
        repo = repo, table_name = "species_group", field_name = "id"
      ),
      id = map_chr(.data$description, sha1)
    ) -> speciesgroup
  species %>%
    transmute(
      external_id = .data$id,
      id = map_chr(.data$nl, sha1),
      description = .data$nl,
      .data$datafield_id
    ) -> speciesgroup2
  dbGetQuery(
    origin, "
    SELECT
      sg.id AS external_id,
      sgs.species_id AS species_id
    FROM species_group AS sg
    INNER JOIN species_speciesgrouprelation AS sgs ON sg.id = sgs.group_id
    WHERE REGEXP_REPLACE(sg.name, '.* \\((.*)\\)$', '\\1') = 'ABV'
  ") %>%
    inner_join(
      speciesgroup %>%
        select(speciesgroup_id = .data$id, .data$external_id),
      by = "external_id"
    ) %>%
    inner_join(
      speciesgroup2 %>%
        select(species_id = .data$external_id, parent_id = .data$id),
      by = "species_id"
    ) %>%
    transmute(.data$speciesgroup_id, .data$parent_id, species = FALSE) %>%
    bind_rows(
      species %>%
        select(external_id = .data$id, parent = .data$id) %>%
        inner_join(
          speciesgroup2 %>%
            select(speciesgroup_id = .data$id, .data$external_id),
          by = "external_id"
        ) %>%
        transmute(
          .data$speciesgroup_id, parent_id = as.character(.data$parent),
          species = TRUE
        )
    ) %>%
    mutate(
      speciesgroup_id = factor(.data$speciesgroup_id),
      parent_id = factor(.data$parent_id)
    ) %>%
    write_vc(
      file = file.path("species", "speciesgroup_species"), root = repo,
      sorting = c("speciesgroup_id", "parent_id"), stage = TRUE, strict = strict
    )
  bind_rows(speciesgroup, speciesgroup2) %>%
    write_vc(
      file = file.path("species", "speciesgroup"), root = repo, sorting = "id",
      stage = TRUE, strict = strict
    )

  prune_meta(root = repo, path = "species", stage = TRUE)

  return(invisible(NULL))
}
