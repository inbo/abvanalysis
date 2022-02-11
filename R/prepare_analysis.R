#' Prepare all datasets and a to do list of models
#' @inheritParams prepare_dataset
#' @inheritParams select_relevant
#' @inheritParams n2kanalysis::store_model
#' @param scheme_id a string with the scheme id.
#' @param seed The seed for the analysis.
#' Defaults to `20070315`.
#' @param docker The docker image to use during model fit.
#' @param dependencies A vector of R packages as understood by the `repo`
#' argument of [remotes::install_github()].
#' @param volume The argument passed to the '-v' of docker.
#' Only used when `base` is a local file system.
#' @export
#' @importFrom digest sha1
#' @importFrom dplyr %>% anti_join arrange bind_rows distinct filter inner_join
#' left_join mutate pull select transmute
#' @importFrom git2rdata verify_vc
#' @importFrom methods slot
#' @importFrom n2kanalysis display get_file_fingerprint n2k_manifest
#' store_manifest_yaml
#' @importFrom purrr map map_chr pmap_dfr
#' @importFrom rlang .data
#' @importFrom tidyr complete nest replace_na unnest
prepare_analysis <- function(
  repo, base, project, overwrite = FALSE, seed = 20070315, verbose = TRUE,
  min_observation = 100, min_stratum = 3, min_cycle = 2, proportion = 0.15,
  scheme_id = "ABV", volume,
  docker = "inbobmk/rn2k:0.7", dependencies = c(
    "inbo/n2khelper@v0.4.3", "inbo/n2kanalysis@v0.2.9",
    "inbo/n2kupdate@v0.1.1"
  )
) {
  display(verbose, "Read data from repository")

  raw_visit <- verify_vc(
    file = file.path("observation", "visit"), root = repo,
    variables = c("id", "year", "period", "point_id", "datafield_id")
  )
  raw_point <- verify_vc(
    file = file.path("location", "point"), root = repo,
    variables = c("id", "description", "square_id")
  )
  raw_square <- verify_vc(
    file = file.path("location", "square"), root = repo,
    variables = c("id", "description", "stratum_id")
  )
  raw_stratum <- verify_vc(
    file = file.path("location", "stratum"), root = repo,
    variables = c("id", "description", "n")
  )
  raw_speciesgroup <- verify_vc(
    file = file.path("species", "speciesgroup"), root = repo,
    variables = c("id", "description")
  )
  raw_speciesgroup_species <- verify_vc(
    file = file.path("species", "speciesgroup_species"), root = repo,
    variables = c("speciesgroup_id", "parent_id", "species")
  )
  raw_distribution <- verify_vc(
    file = file.path("distribution", "distribution"), root = repo,
    variables = c("species_id", "family")
  )

  display(verbose, "Data wrangling")

  raw_square %>%
    transmute(
      square_id = .data$id, .data$stratum_id, square = factor(.data$description)
    ) %>%
    inner_join(
      raw_stratum %>%
        transmute(
          stratum_id = .data$id, stratum = factor(.data$description),
          .data$n
        ),
      by = "stratum_id"
    ) %>%
    inner_join(
      raw_point %>%
        transmute(
          point_id = .data$id, point = factor(.data$description),
          .data$square_id
        ),
      by = "square_id"
    ) %>%
    inner_join(raw_visit, by = "point_id") %>%
    select(
      -.data$square_id, -.data$stratum_id, -.data$point_id, sample_id = .data$id
    ) -> observation
  raw_speciesgroup_species %>%
    filter(.data$species) %>%
    left_join(raw_distribution, by = c("parent_id" = "species_id")) %>%
    inner_join(raw_speciesgroup, by = c("speciesgroup_id" = "id")) %>%
    transmute(
      species_group_id = .data$speciesgroup_id,
      species = sprintf("%05i", .data$external_id),
      family = as.character(.data$family) %>%
        replace_na("poisson"),
    ) %>%
    arrange(.data$species_group_id) -> speciesgroup

  display(verbose, "Prepare analysis per species")

  pmap_dfr(
    as.list(speciesgroup), prepare_analysis_dataset, scheme_id = scheme_id,
    location_group_id = "Vlaanderen", seed = seed, observation = observation,
    repo = repo, base = base, project = project, overwrite = overwrite,
    min_observation = min_observation, min_stratum = min_stratum,
    min_cycle = min_cycle, proportion = proportion, verbose = verbose
  ) -> base_analysis

  display(verbose, "Prepare model composite")

  base_analysis %>%
    inner_join(raw_speciesgroup_species, by = "parent_id") %>%
    select(
      -.data$parent_id, -.data$species, species_group_id = .data$speciesgroup_id
    ) %>%
    mutate(species_group_id = as.character(.data$species_group_id)) %>%
    nest(
      models = c(
        .data$fingerprint, .data$status_fingerprint, .data$status,
        .data$result_datasource, .data$first_imported_year,
        .data$last_imported_year, .data$analysis_date
      )
    ) %>%
    arrange(.data$species_group_id, .data$frequency, .data$type) -> composite
  pmap_dfr(
    as.list(composite), prepare_analysis_composite, scheme_id = scheme_id,
    location_group_id = "Vlaanderen", seed = seed, verbose = verbose,
    base = base, project = project, overwrite = overwrite
  ) %>%
    bind_cols(composite) -> composite

  display(verbose, "Prepare manifests")

  composite %>%
    transmute(
      location_group = "Vlaanderen", species_group = .data$species_group_id,
      .data$frequency, .data$type, .data$fingerprint,
      data = map(.data$models, select, parent = "fingerprint")
    ) %>%
    unnest(cols = "data") -> meta_composite
  meta_composite %>%
    select(-.data$fingerprint, fingerprint = "parent") %>%
    bind_rows(meta_composite) %>%
    nest(data = c("fingerprint", "parent")) %>%
    mutate(manifest = map(.data$data, n2k_manifest)) -> manifest_composite

  base_analysis %>%
    anti_join(meta_composite, by = c("fingerprint" = "parent")) %>%
    transmute(.data$parent_id, .data$fingerprint, parent = NA_character_) %>%
    nest(data = c("fingerprint", "parent")) %>%
    pull(.data$data) %>%
    map(n2k_manifest) %>%
    c(manifest_composite$manifest) %>%
    map(
      store_manifest_yaml, base = base, project = project, docker = docker,
      dependencies = dependencies
    ) -> stored

  manifest_composite %>%
    transmute(manifest = map(.data$manifest, slot, "Manifest")) %>%
    unnest(cols = .data$manifest) %>%
    mutate(parent = !is.na(.data$parent)) %>%
    distinct(.data$fingerprint, .data$parent) %>%
    arrange(.data$parent, .data$fingerprint) %>%
    pull(.data$fingerprint) -> models

  manifests <- map_chr(manifest_composite$manifest, get_file_fingerprint)

  args <- ", dependencies = FALSE, upgrade = \\\"never\\\", keep_source = FALSE"
  docker_hash <- sha1(manifests)
  sprintf(
    "Rscript -e 'remotes::install_github(\\\"%s\\\"%s)'", dependencies, args
  ) -> deps
  sprintf(
    "#!/bin/bash
echo \"FROM %s
RUN %s\" > Dockerfile
docker build --pull --tag rn2k:%s .
rm Dockerfile",
    docker, paste(deps, collapse = " \\\n&&  "), docker_hash
  ) -> init

  if (inherits(base, "s3_bucket")) {
    sprintf(
      "echo \"model %i of %i\"
docker run %s --name=%s rn2k:%s ./fit_model_aws.sh -b %s -p %s -m %s
date
docker stop --time 14400 %s
date",
      seq_along(models), length(models), "--rm -d --env-file ./env.list",
      models, docker_hash, attr(base, "Name"), project, models, models
    ) -> model_scripts

    sprintf(
      "echo \"manifest %i of %i\"
docker run %s --name=%s rn2k:%s ./fit_model_aws.sh -b %s -p %s -m %s
date
docker stop --time 14400 %s
date",
      seq_along(manifests), length(manifests), "--rm -d --env-file ./env.list",
      manifests, docker_hash, attr(base, "Name"), project,
      paste0(manifests, ".manifest"), manifests
    ) -> manifest_scripts
  } else {
    base <- normalizePath(base, winslash = "/")
    if (missing(volume)) {
      volume <- paste(base, base, "rw", sep = ":")
    }
    sprintf(
      "echo \"model %i of %i\"
docker run %s --name=%s -v %s rn2k:%s ./fit_model_file.sh -b %s -p %s -m %s
date
docker stop --time 14400 %s
date",
      seq_along(models), length(models), "--rm -d", models, volume, docker_hash,
      base, project, models, models
    ) -> model_scripts

    sprintf(
      "echo \"manifest %i of %i\"
docker run %s --name=%s -v %s rn2k:%s ./fit_model_file.sh -b %s -p %s -m %s
date
docker stop --time 14400 %s
date",
      seq_along(manifests), length(manifests), "--rm -d", manifests, volume,
      docker_hash, base, project, paste0(manifests, ".manifest"), manifests
    ) -> manifest_scripts
  }

  return(list(init = init, models = model_scripts, manifest = manifest_scripts))
}
