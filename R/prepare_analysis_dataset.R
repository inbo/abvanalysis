#' Create the analysis dataset based on the available raw data
#'
#' This analysis fits an unweighed model but adds the stratum effect. The
#'    indices per year of the different strata are combined with a linear
#'    combination into a single index per year.
#' @return A data.frame with the species id number of rows in the analysis
#'    dataset, number of presences in the analysis dataset and SHA-1 of the
#'    analysis dataset or NULL if not enough data.
#' @inheritParams prepare_dataset
#' @inheritParams prepare_analysis
#' @inheritParams select_relevant
#' @inheritParams n2kanalysis::store_model
#' @param species_group_id the species_group ID
#' @param location_group_id the location_group ID
#' @param family The statistical distribution to use for this species.
#' @export
#' @importFrom dplyr %>% bind_rows count distinct slice_max
#' @importFrom git2rdata read_vc recent_commit
#' @importFrom n2kanalysis display n2k_inla
#' @importFrom rlang .data
#' @importFrom stats as.formula
#' @importFrom tidyr replace_na
prepare_analysis_dataset <- function(
  scheme_id, species_group_id, species, location_group_id, seed = 20070315,
  observation, repo, base, project, overwrite = FALSE, min_observation = 100,
  min_stratum = 3, min_cycle = 2, proportion = 0.15, verbose = TRUE, family
){
  display(verbose, c(location_group_id, " ", species_group_id))

  control <- list(control.fixed = list(prec = list(default = 0.2)))

  # define metadata

  first_imported_year <- min(observation$year)
  last_imported_year <- max(observation$year)
  bind_rows(
    recent_commit(
      file = file.path("observation", "visit"), root = repo, data = TRUE
    ),
    recent_commit(
      file = file.path("observation", species), root = repo, data = TRUE
    )
  ) %>%
    slice_max(.data$when, n = 1) -> rc

  # select relevant data

  dataset <- select_relevant(
    observation = observation, species = species, repo = repo,
    min_observation = min_observation, min_stratum = min_stratum,
    min_cycle = min_cycle, proportion = proportion
  )

  # empty dataset implies insufficient data
  if (is.null(dataset)) {
    display(verbose, "insufficient data")
    n2k_inla(
      result_datasource_id = rc$commit, scheme_id = scheme_id,
      species_group_id = species_group_id, family = family,
      location_group_id = location_group_id, analysis_date = rc$when,
      model_type = paste0(
        "inla ", family, ": Year:Stratum + Period + Location + SubLocation"
      ),
      formula = "count ~ 1", first_imported_year = first_imported_year,
      last_imported_year = last_imported_year,
      data = data.frame(
        count = integer(0), observation_id = integer(0),
        datafield_id = character(0)
      ),
      status = "insufficient_data", seed = seed
    ) %>%
      storage(base = base, project = project, overwrite = overwrite)
    return(data.frame())
  }

  # calculate stratum weights
  dataset %>%
    complete(
      year = min(.data$year):max(.data$year),
      stratum = as.character(unique(.data$stratum))
    ) %>%
    mutate(
      cycle = 1 + (.data$year - 2007) %/% 3,
      cyear = .data$year - min(.data$year) + 1
    ) -> dataset
  dataset %>%
    filter(!is.na(.data$square)) %>%
    distinct(.data$stratum, .data$square, n = .data$n) %>%
    count(.data$stratum, .data$n, name = "r") %>%
    inner_join(
      observation %>%
        distinct(.data$stratum, .data$square) %>%
        count(.data$stratum, name = "t"),
      by = "stratum"
    ) %>%
    transmute(
      .data$stratum,
      weight = .data$n * .data$r / .data$t,
      weight = .data$weight / sum(.data$weight)
    ) %>%
    inner_join(dataset, by = "stratum") %>%
    mutate(
      stratum = factor(.data$stratum),
      observation_id = ifelse(
        is.na(.data$observation_id), -row_number(), .data$observation_id
      ),
      datafield_id = replace_na(.data$datafield_id, -1)
    ) %>%
    arrange(
      .data$cyear, .data$stratum, .data$square, .data$point,
      .data$observation_id
    ) -> dataset
  stratum_weights <- distinct(dataset, .data$stratum, .data$weight)

  base_effects <- c(
    "period"[length(levels(dataset$period)) > 1],
    "f(square, model = \"iid\",
      hyper = list(theta = list(prior = \"pc.prec\", param = c(1, 0.01))))",
    "f(point, model = \"iid\",
      hyper = list(theta = list(prior = \"pc.prec\", param = c(1, 0.01))))"[
        length(levels(dataset$point)) > 2 * length(levels(dataset$square))
      ]
  )

  # analysis on data by year
  display(verbose, "linear year, ", linefeed = FALSE)

  ifelse(
    length(levels(dataset$stratum)) > 1, "0 + stratum + cyear:stratum", "cyear"
  ) %>%
    c(base_effects) %>%
    paste(collapse = " + ") %>%
    sprintf(fmt = "count ~ %s") -> formula_ly
  expand_grid(
    cyear = min(dataset$cyear):max(dataset$cyear),
    stratum = unique(dataset$stratum)
  ) %>%
    get_linear_lincomb(
      stratum_weights = stratum_weights, time_var = "cyear",
      stratum_var = "stratum",
      formula = as.formula(
        ifelse(
          length(levels(dataset$stratum)) == 1, "~1 + cyear",
          "~0 + stratum + cyear:stratum"
        )
      )
    ) -> lin_comb
  n2k_inla(
    result_datasource_id = rc$commit, scheme_id = scheme_id,
    species_group_id = species_group_id, location_group_id = location_group_id,
    analysis_date = rc$when, family = family, lin_comb = lin_comb,
    model_type = sprintf(
      "inla %s: year:stratum + period + square + point", family
    ),
    formula = formula_ly, control = control,
    first_imported_year = first_imported_year,
    last_imported_year = last_imported_year, data = dataset, status = "new",
    seed = seed
  ) %>%
    storage(base = base, project = project, overwrite = overwrite) ->
    year_lin_fingerprint

  display(verbose, "non-linear year, ", linefeed = FALSE)

  expand_grid(
    cyear = min(dataset$cyear):max(dataset$cyear),
    stratum = unique(dataset$stratum)
  ) %>%
    mutate(label = .data$cyear + first_imported_year - 1) %>%
    get_nonlinear_lincomb(
      stratum_weights = stratum_weights, time_var = "cyear",
      label_var = "label", stratum_var = "stratum"
    ) -> lin_comb
  c(base_effects,
  "f(cyear, model = \"rw1\", replicate = as.integer(stratum),
    hyper = list(theta = list(prior = \"pc.prec\", param = c(0.5, 0.01))))"
  ) %>%
    paste(collapse = " + ") %>%
    sprintf(fmt = "count ~ %s") -> formula_nly
  n2k_inla(
    result_datasource_id = rc$commit, scheme_id = scheme_id,
    species_group_id = species_group_id, location_group_id = location_group_id,
    analysis_date = rc$when, family = family,
    model_type = sprintf(
      "inla %s: RW1(year|stratum) + period + square + point", family
    ),
    formula = formula_nly, lin_comb = lin_comb, control = control,
    first_imported_year = first_imported_year,
    last_imported_year = last_imported_year, data = dataset, status = "new",
    seed = seed, replicate_name = list(cyear = levels(dataset$stratum))
  ) %>%
    storage(base = base, project = project, overwrite = overwrite) ->
    year_nl_fingerprint

  # analysis on data by cycle
  display(verbose, "linear cycle, ", linefeed = FALSE)

  ifelse(
    length(levels(dataset$stratum)) > 1, "0 + stratum + cycle:stratum", "cycle"
  ) %>%
    c(base_effects) %>%
    paste(collapse = " + ") %>%
    sprintf(fmt = "count ~ %s") -> formula_ly
  expand_grid(
    cycle = min(dataset$cycle):max(dataset$cycle),
    stratum = unique(dataset$stratum)
  ) %>%
    get_linear_lincomb(
      stratum_weights = stratum_weights,
      time_var = "cycle",
      stratum_var = "stratum",
      formula = as.formula(
        ifelse(
          length(levels(dataset$stratum)) == 1, "~1 + cycle",
          "~0 + stratum + cycle:stratum"
        )
      )
    ) -> lin_comb
  n2k_inla(
    result_datasource_id = rc$commit, scheme_id = scheme_id,
    species_group_id = species_group_id, location_group_id = location_group_id,
    analysis_date = rc$when, family = family, lin_comb = lin_comb,
    model_type = sprintf(
      "inla %s: cycle:stratum + period + square + point", family
    ),
    formula = formula_ly, control = control,
    first_imported_year = first_imported_year,
    last_imported_year = last_imported_year, data = dataset, status = "new",
    seed = seed
  ) %>%
    storage(base = base, project = project, overwrite = overwrite) ->
    cycle_lin_fingerprint

  display(verbose, "non-linear cycle")

  expand_grid(
    cycle = min(dataset$cycle):max(dataset$cycle),
    stratum = unique(dataset$stratum)
  ) %>%
    mutate(label = (.data$cycle - 1) * 3 + first_imported_year) %>%
    get_nonlinear_lincomb(
      stratum_weights = stratum_weights, time_var = "cycle",
      label_var = "label", stratum_var = "stratum"
    ) -> lin_comb
  c(base_effects,
    "f(cycle, model = \"rw1\", replicate = as.integer(stratum),
    hyper = list(theta = list(prior = \"pc.prec\", param = c(0.5, 0.01))))"
  ) %>%
    paste(collapse = " + ") %>%
    sprintf(fmt = "count ~ %s") -> formula_nly
  n2k_inla(
    result_datasource_id = rc$commit, scheme_id = scheme_id,
    species_group_id = species_group_id, location_group_id = location_group_id,
    analysis_date = rc$when, family = family,
    model_type = sprintf(
      "inla %s: RW1(cycle|stratum) + period + square + point", family
    ),
    formula = formula_nly, lin_comb = lin_comb, control = control,
    first_imported_year = first_imported_year,
    last_imported_year = last_imported_year, data = dataset, status = "new",
    seed = seed, replicate_name = list(cyear = levels(dataset$stratum))
  ) %>%
    storage(base = base, project = project, overwrite = overwrite) ->
    cycle_nl_fingerprint

  expand_grid(
    frequency = c("year", "cycle"),
    type = c("linear", "non linear")
  ) %>%
    mutate(
      parent_id = species_group_id, first_imported_year = first_imported_year,
      last_imported_year = last_imported_year, analysis_date = rc$when,
      result_datasource = rc$commit
    ) %>%
    bind_cols(
      bind_rows(
        year_lin_fingerprint, year_nl_fingerprint, cycle_lin_fingerprint,
        cycle_nl_fingerprint
      )
    )
}
