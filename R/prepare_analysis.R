#' Prepare all datasets and a to do list of models
#' @inheritParams prepare_dataset
#' @inheritParams select_relevant
#' @inheritParams n2kanalysis::store_model
#' @export
#' @importFrom git2rdata read_vc
#' @importFrom dplyr %>% semi_join filter inner_join rename add_count ungroup anti_join
#' @importFrom tidyr complete
#' @importFrom n2kanalysis n2k_manifest store_manifest
prepare_analysis <- function(
  repo, base, project, overwrite = FALSE,
  min.observation = 100, min.stratum = 3, min.cycle = 2, proportion = 0.15
){
  message("Read data from repository")
  flush.console()

  raw_metadata <- read_vc(file = "metadata/metadata", root = repo)
  if (nrow(raw_metadata) == 0) {
    warning("Nothing to do")
    return(invisible(NULL))
  }
  assert_that(
    has_name(raw_metadata, "file_fingerprint"),
    msg = "no 'file_fingerprint' in data 'metadata/metadata'"
  )
  assert_that(
    has_name(raw_metadata, "scheme"),
    msg = "no 'scheme' in data 'metadata/metadata'"
  )
  assert_that(
    has_name(raw_metadata, "result_datasource"),
    msg = "no 'result_datasource' in data 'metadata/metadata'"
  )
  assert_that(
    has_name(raw_metadata, "species_group"),
    msg = "no 'species_group' in data 'metadata/metadata'"
  )
  assert_that(
    has_name(raw_metadata, "location_group"),
    msg = "no 'location_group in data 'metadata/metadata'"
  )
  assert_that(
    has_name(raw_metadata, "first_imported_year"),
    msg = "no 'first_imported_year' in data 'metadata/metadata'"
  )
  assert_that(
    has_name(raw_metadata, "last_imported_year"),
    msg = "no 'last_imported_year' in data 'metadata/metadata'"
  )
  assert_that(
    has_name(raw_metadata, "seed"),
    msg = "no 'seed' in data 'metadata/metadata'"
  )
  assert_that(
    has_name(raw_metadata, "analysis_date"),
    msg = "no 'analysis_date' in data 'metadata/metadata'"
  )

  raw_l <- read_vc(file = "location/location", root = repo)
  assert_that(
    has_name(raw_l, "location"),
    msg = "no 'location' in data 'location/location'"
  )
  assert_that(
    has_name(raw_l, "parent"),
    msg = "no 'parent' in data 'location/location'"
  )

  locationgrouplocation <- read_vc(
    file = "location/location_group_location", root = repo
  )
  assert_that(
    has_name(locationgrouplocation, "location_group"),
    msg = "no 'location_group' in data 'location/location_group_location'"
  )
  assert_that(
    has_name(locationgrouplocation, "location"),
    msg = "no 'location' in data 'location/location_group_location'"
  )

  strata <- read_vc(file = "location/strata", root = repo)
  assert_that(
    has_name(strata, "fingerprint"),
    msg = "no 'fingerprint' in data 'location/strata'"
  )
  assert_that(
    has_name(strata, "n"),
    msg = "no 'n' in data 'location/strata'"
  )

  raw_visit <- read_vc(file = "observation/visit", root = repo)
  assert_that(
    has_name(raw_visit, "sample_id"),
    msg = "no 'sample_id' in data 'observation/visit'"
  )
  assert_that(
    has_name(raw_visit, "location"),
    msg = "no 'location' in data 'observation/visit'"
  )
  assert_that(
    has_name(raw_visit, "sublocation"),
    msg = "no 'sublocation' in data 'observation/visit'"
  )
  assert_that(
    has_name(raw_visit, "year"),
    msg = "no 'year' in data 'observation/visit'"
  )
  assert_that(
    has_name(raw_visit, "period"),
    msg = "no 'period' in data 'observation/visit'"
  )

  raw_sgs <- read_vc(file = "species/species_group_species", root = repo)
  assert_that(
    has_name(raw_sgs, "species_group"),
    msg = "no 'species_group' in data 'species/species_group_species'"
  )
  assert_that(
    has_name(raw_sgs, "species"),
    msg = "no 'species' in data 'species/species_group_species'"
  )

  raw_distribution <- read_vc(file = "metadata/distribution", root = repo)
  assert_that(
    has_name(raw_distribution, "species_group"),
    msg = "no 'species_group' in data 'metadata/distribution'"
  )
  assert_that(
    has_name(raw_distribution, "family"),
    msg = "no 'family' in data 'metadata/distribution'"
  )


  message("Data wrangling")
  flush.console()

  raw_l %>%
    semi_join(
      raw_l %>%
        filter(.data$parent == ""),
      by = c("parent" = "location")
    ) %>%
    group_by(.data$parent) %>%
    add_count() %>%
    ungroup() %>%
    rename(stratum = "parent", n_sample = "n") %>%
    inner_join(
      strata %>%
        rename(n_stratum = "n"),
      by = c("stratum" = "fingerprint")
    ) %>%
    inner_join(
      raw_l %>%
        rename(sublocation = "location"),
      by = c("location" = "parent")
    ) %>%
    inner_join(raw_visit, by = c("location", "sublocation")) -> observation

  message("Prepare analysis per species")
  flush.console()
  raw_metadata %>%
    inner_join(raw_sgs, by = "species_group") -> base_analysis
  raw_distribution %>%
    mutate(family = as.character(.data$family)) %>%
    complete(
      species_group = base_analysis$species_group,
      fill = list(family = "poisson")
    ) %>%
    inner_join(x = base_analysis, by = "species_group") -> base_analysis
  base_analysis %>%
    group_by(.data$species_group, .data$location_group) %>%
    arrange(.data$location_group, .data$species_group) %>%
    nest(.key = "metadata") -> base_nested
  base_nested$stored <- lapply(
    seq_along(base_nested$species_group),
    function(i) {
      prepare_analysis_dataset(
        species_group = base_nested$species_group[i],
        location_group = base_nested$location_group[i],
        metadata = base_nested$metadata[[i]],
        observation = observation,
        repo = repo,
        base = base,
        project = project,
        overwrite = overwrite,
        min.observation = min.observation,
        min.stratum = min.stratum,
        min.cycle = min.cycle,
        proportion = proportion
      )
    }
  )
  base_nested %>%
    select(-"metadata") %>%
    unnest() %>%
    inner_join(
      base_analysis %>%
        select(
          "species_group", "location_group", "scheme", "result_datasource",
          "seed", "first_imported_year", "last_imported_year", "analysis_date"
        ),
      by = c("species_group", "location_group")
    ) -> base_analysis

  message("Prepare model comparison")
  flush.console()
  base_analysis %>%
    nest(
      "type", "fingerprint", "status", "status_fingerprint", "scheme", "seed",
      "result_datasource", "first_imported_year", "last_imported_year",
      "analysis_date"
    ) %>%
    mutate(
      stored = pmap(
        list(
          frequency = .data$frequency,
          species_group = .data$species_group,
          location_group = .data$location_group,
          models = .data$data
        ),
        prepare_analysis_comparison,
        base = base,
        project = project,
        overwrite = overwrite
      )
    ) -> comparison

  message("\nPrepare Composite indices")
  flush.console()
  raw_sgs %>%
    anti_join(raw_metadata, by = "species_group") %>%
    inner_join(
      raw_sgs %>%
        rename(parent = "species_group"),
      by = "species"
    ) %>%
    select(-"species") -> composite_sg
  composite_sg %>%
    inner_join(base_analysis, by = c("parent" = "species_group")) %>%
    nest(
      "fingerprint", "status", "status_fingerprint", "scheme", "seed",
      "result_datasource", "first_imported_year", "last_imported_year",
      "analysis_date", "parent"
    ) %>%
    mutate(
      stored = pmap(
        list(
          frequency = .data$frequency,
          type = .data$type,
          species_group = .data$species_group,
          location_group = .data$location_group,
          models = .data$data
        ),
        prepare_analysis_composite,
        base = base,
        project = project,
        overwrite = overwrite
      )
    ) -> composite

  message("Prepare manifests")
  flush.console()
  comparison %>%
    transmute(
      .data$species_group,
      .data$location_group,
      .data$frequency,
      Fingerprint = map_chr(.data$stored, "fingerprint"),
      Parent = map(.data$data, select, Parent = "fingerprint")
    ) %>%
    unnest() -> meta_comparison
  meta_comparison %>%
    select(-"Fingerprint", Fingerprint = "Parent") %>%
    bind_rows(meta_comparison) %>%
    nest(.data$Fingerprint, .data$Parent) %>%
    mutate(
      manifest = map(.data$data, n2k_manifest),
      stored = map(
        .data$manifest, store_manifest, base = base, project = project
      )
    )
  composite %>%
    transmute(
      .data$location_group,
      .data$species_group,
      .data$frequency,
      .data$type,
      Fingerprint = map_chr(.data$stored, "fingerprint"),
      map(.data$data, select, Parent = "fingerprint")
    ) %>%
    unnest() -> meta_composite
  meta_composite %>%
    select(-"Fingerprint", Fingerprint = "Parent") %>%
    bind_rows(meta_composite) %>%
    nest(.data$Fingerprint, .data$Parent) %>%
    mutate(
      manifest = map(.data$data, n2k_manifest),
      stored = map(
        .data$manifest, store_manifest, base = base, project = project
      )
    )

  return(invisible(NULL))
}
