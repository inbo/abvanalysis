#' Create the analysis dataset based on the available raw data
#'
#' This analysis fits an unweighted model but adds the stratum effect. The
#'    indices per year of the different strata are combined with a linear
#'    combination into a single index per year.
#' @return A data.frame with the species id number of rows in the analysis
#'    dataset, number of precenses in the analysis datset and SHA-1 of the
#'    analysis dataset or NULL if not enough data.
#' @inheritParams prepare_dataset
#' @inheritParams select_relevant
#' @inheritParams n2kanalysis::store_model
#' @param species_group the species_group ID
#' @param location_group the location_group ID
#' @param metadata a dataframe with the metadata of a single import
#' @export
#' @importFrom utils flush.console
#' @importFrom dplyr %>% mutate_at mutate distinct count inner_join transmute arrange bind_rows bind_cols
#' @importFrom tidyr complete full_seq
#' @importFrom git2rdata read_vc
#' @importFrom n2kanalysis n2k_inla
#' @importFrom rlang .data
#' @importFrom stats setNames
prepare_analysis_dataset <- function(
  species_group,
  location_group,
  metadata,
  observation,
  repo,
  base,
  project,
  overwrite = FALSE,
  min.observation = 100,
  min.stratum = 3,
  min.cycle = 2,
  proportion = 0.15
){
  message(location_group, " ", species_group)
  flush.console()

  dataset <- select_relevant(
    observation = observation,
    species = metadata$species,
    datafield = c(
      sample = metadata$sample_df, observation = metadata$observation_df
    ),
    repo = repo,
    min.observation = min.observation,
    min.stratum = min.stratum,
    min.cycle = min.cycle
  )

  if (is.null(dataset)) {
    message("insufficient data")
    n2k_inla(
      result.datasource.id = metadata$result_datasource,
      scheme.id = metadata$scheme,
      species.group.id = species_group,
      location.group.id = location_group,
      analysis.date = metadata$analysis_date,
      family = metadata$family,
      model.type = paste0(
        "inla ", metadata$family,
        ": Year:Stratum + Period + Location + SubLocation"
      ),
      formula = "count ~ 1",
      first.imported.year = metadata$first_imported_year,
      last.imported.year = metadata$last_imported_year,
      data = data.frame(
        count = integer(0),
        ObservationID = integer(0),
        DataFieldID = character(0)
      ),
      status = "insufficient_data",
      parent = metadata$file_fingerprint,
      seed = metadata$seed
    ) %>%
      storage(base = base, project = project, overwrite = overwrite) ->
      fingerprint
    return(data.frame())
  }

  dataset %>%
    complete(
      stratum = .data$stratum,
      year = full_seq(.data$year, 1),
      fill = list(DataFieldID = metadata$autocomplete_df)
    ) %>%
    mutate(
      cycle = 1 + (.data$year - 2007) %/% 3,
      cyear = .data$year - min(.data$year) + 1,
      ObservationID = ifelse(
        is.na(.data$count),
        cumsum(is.na(.data$count)),
        .data$ObservationID
      )
    ) %>%
    mutate_at(
      .vars = c("stratum", "location", "sublocation", "period", "DataFieldID"),
      .funs = factor
    ) -> dataset

  design.var <- c("ObservationID", "DataFieldID", "count")
  if (length(levels(dataset$period)) > 1) {
    design.var <- c(design.var, "period")
    design <- "period"
  } else {
    design <- ""
  }
  design.var <- c(design.var, "location")
  design <- paste(
    design,
    "f(
      location,
      model = 'iid',
      hyper = list(theta = list(prior = 'pc.prec', param = c(1, 0.01)))
    )",
    sep = "+"
  )
  if (length(levels(dataset$sublocation)) > length(levels(dataset$location))) {
    design.var <- c(design.var, "sublocation")
    design <- paste(
      design,
      "f(
        sublocation,
        model = 'iid',
        hyper = list(theta = list(prior = 'pc.prec', param = c(1, 0.01)))
      )",
      sep = "+"
    )
  }
  multi_stratum <- length(levels(dataset$stratum)) > 1
  if (!multi_stratum) {
    message("single linear year", appendLF = FALSE)
    # linear trend along year
    cbind(
      `(Intercept)` = 1,
      cyear = c(min(dataset$cyear):max(dataset$cyear), 1)
    ) -> lc.trend
    rownames(lc.trend) <- c(min(dataset$year):max(dataset$year), "Trend")
    n2k_inla(
      result.datasource.id = metadata$result_datasource,
      scheme.id = metadata$scheme,
      species.group.id = species_group,
      location.group.id = location_group,
      analysis.date = metadata$analysis_date,
      family = metadata$family,
      model.type = paste0(
        "inla ", metadata$family,
        ": Year:Stratum + Period + Location + SubLocation"
      ),
      formula = paste("count ~ 1 + cyear +", design),
      lin.comb = lc.trend,
      first.imported.year = metadata$first_imported_year,
      last.imported.year = metadata$last_imported_year,
      data = dataset %>%
        select(design.var, "cyear") %>%
        arrange(.data$DataFieldID, .data$ObservationID),
      status = "new",
      parent = metadata$file_fingerprint,
      seed = metadata$seed
    ) %>%
      storage(base = base, project = project, overwrite = overwrite) ->
      year_trend_fingerprint

    # linear trend along cycle
    message(" cycle", appendLF = FALSE)
    cbind(
      `(Intercept)` = 1,
      cycle = c(min(dataset$cycle):max(dataset$cycle), 1)
    ) -> lc.trend
    cycle <- seq_len(max(dataset$cycle)) * 3 + 2004
    rownames(lc.trend) <- c(paste(cycle, cycle + 2, sep = "-"), "Trend")
    n2k_inla(
      result.datasource.id = metadata$result_datasource,
      scheme.id = metadata$scheme,
      species.group.id = species_group,
      location.group.id = location_group,
      analysis.date = metadata$analysis_date,
      family = metadata$family,
      model.type = paste0(
        "inla ", metadata$family,
        ": Cycle:Stratum + Period + Location + SubLocation"
      ),
      formula = paste("count ~ 1 + cycle +", design),
      lin.comb = lc.trend,
      first.imported.year = metadata$first_imported_year,
      last.imported.year = metadata$last_imported_year,
      data = dataset %>%
        select(design.var, "cycle") %>%
        arrange(.data$DataFieldID, .data$ObservationID),
      status = "new",
      parent = metadata$file_fingerprint,
      seed = metadata$seed
    ) %>%
      storage(base = base, project = project, overwrite = overwrite) ->
      cycle_trend_fingerprint

    # non linear trend along year
    message(" non-linear year", appendLF = FALSE)
    n_year <- diff(range(dataset$cyear)) + 1
    lc.index <- list(cyear = diag(n_year), `(Intercept)` = rep(1, n_year))
    rownames(lc.index[[1]]) <- min(dataset$year):max(dataset$year)
    n2k_inla(
      result.datasource.id = metadata$result_datasource,
      scheme.id = metadata$scheme,
      species.group.id = species_group,
      location.group.id = location_group,
      analysis.date = metadata$analysis_date,
      family = metadata$family,
      model.type = paste0(
        "inla ", metadata$family,
        ": RW1(Year|Stratum) + Period + Location + SubLocation"
      ),
      formula = paste("count ~ 1 + f(
        cyear,
        model = 'rw1',
        hyper = list(theta = list(prior = 'pc.prec', param = c(0.5, 0.01)))
      ) +", design),
      lin.comb = lc.index,
      first.imported.year = metadata$first_imported_year,
      last.imported.year = metadata$last_imported_year,
      data = dataset %>%
        select(design.var, "cyear") %>%
        arrange(.data$DataFieldID, .data$ObservationID),
      status = "new",
      parent = metadata$file_fingerprint,
      seed = metadata$seed
    ) %>%
      storage(base = base, project = project, overwrite = overwrite) ->
      year_nl_fingerprint

    # non linear trend along cycle
    message(" cycle")
    cycle <- seq_len(max(dataset$cycle)) * 3 + 2004
    lc.index <- list(
      cycle = diag(length(cycle)),
      `(Intercept)` = rep(1, length(cycle))
    )
    rownames(lc.index[[1]]) <- paste(cycle, cycle + 2, sep = "-")
    n2k_inla(
      result.datasource.id = metadata$result_datasource,
      scheme.id = metadata$scheme,
      species.group.id = species_group,
      location.group.id = location_group,
      analysis.date = metadata$analysis_date,
      family = metadata$family,
      model.type = paste0(
        "inla ", metadata$family,
        ": RW1(Cycle|Stratum) + Period + Location + SubLocation"
      ),
      formula = paste("count ~ 1 + f(
        cycle,
        model = 'rw1',
        hyper = list(theta = list(prior = 'pc.prec', param = c(0.5, 0.01)))
      ) +", design),
      lin.comb = lc.index,
      first.imported.year = metadata$first_imported_year,
      last.imported.year = metadata$last_imported_year,
      data = dataset %>%
        select(design.var, "cycle") %>%
        arrange(.data$DataFieldID, .data$ObservationID),
      status = "new",
      parent = metadata$file_fingerprint,
      seed = metadata$seed
    ) %>%
      storage(base = base, project = project, overwrite = overwrite) ->
      cycle_nl_fingerprint
  } else {
    design.var <- c(design.var, "stratum")
    dataset %>%
      distinct(.data$stratum, .data$location) %>%
      count(.data$stratum) %>%
      inner_join(
        dataset %>%
          distinct(.data$stratum, .data$n_sample, .data$n_stratum),
        by = "stratum"
      ) %>%
      transmute(
        .data$stratum,
        weight = .data$n_stratum * .data$n / .data$n_sample,
        weight = .data$weight / sum(.data$weight)
      ) -> stratum_weights

    # linear trend along years
    message("multi linear year", appendLF = FALSE)
    expand.grid(
      cyear = min(dataset$cyear):max(dataset$cyear),
      stratum = unique(dataset$stratum),
      stringsAsFactors = FALSE
    ) %>%
    get_linear_lincomb(
      stratum_weights = stratum_weights,
      time.var = "cyear",
      stratum.var = "stratum",
      formula = ~ 0 + stratum + cyear:stratum
    ) -> lc.trend
    names(lc.trend[[1]]) <- c("Trend", min(dataset$year):max(dataset$year))
    n2k_inla(
      result.datasource.id = metadata$result_datasource,
      scheme.id = metadata$scheme,
      species.group.id = species_group,
      location.group.id = location_group,
      analysis.date = metadata$analysis_date,
      family = metadata$family,
      model.type = paste0(
        "inla ", metadata$family,
        ": Year:Stratum + Period + Location + SubLocation"
      ),
      formula = paste("count ~ 0 + stratum + cyear:stratum +", design),
      lin.comb = lc.trend,
      first.imported.year = metadata$first_imported_year,
      last.imported.year = metadata$last_imported_year,
      data = dataset %>%
        select(design.var, "cyear") %>%
        arrange(.data$DataFieldID, .data$ObservationID),
      status = "new",
      parent = metadata$file_fingerprint,
      seed = metadata$seed
    ) %>%
      storage(base = base, project = project, overwrite = overwrite) ->
      year_trend_fingerprint

    # linear trend along cycle
    message(" cycle", appendLF = FALSE)
    expand.grid(
      cycle = min(dataset$cycle):max(dataset$cycle),
      stratum = unique(dataset$stratum),
      stringsAsFactors = FALSE
    ) %>%
      get_linear_lincomb(
        stratum_weights = stratum_weights,
        time.var = "cycle",
        stratum.var = "stratum",
        formula = ~ 0 + stratum + cycle:stratum
      ) -> lc.trend
    cycle <- seq_len(max(dataset$cycle)) * 3 + 2004
    names(lc.trend[[1]]) <- c("Trend", paste(cycle, cycle + 2, sep = "-"))
    n2k_inla(
      result.datasource.id = metadata$result_datasource,
      scheme.id = metadata$scheme,
      species.group.id = species_group,
      location.group.id = location_group,
      analysis.date = metadata$analysis_date,
      family = metadata$family,
      model.type = paste0(
        "inla ", metadata$family,
        ": Cycle:Stratum + Period + Location + SubLocation"
      ),
      formula = paste("count ~ 0 + stratum + cycle:stratum +", design),
      lin.comb = lc.trend,
      first.imported.year = metadata$first_imported_year,
      last.imported.year = metadata$last_imported_year,
      data = dataset %>%
        select(design.var, "cycle") %>%
        arrange(.data$DataFieldID, .data$ObservationID),
      status = "new",
      parent = metadata$file_fingerprint,
      seed = metadata$seed
    ) %>%
      storage(base = base, project = project, overwrite = overwrite) ->
      cycle_trend_fingerprint

    # non linear trend along year
    message(" non-linear year", appendLF = FALSE)
    n_year <- diff(range(dataset$cyear)) + 1
    outer(diag(n_year), stratum_weights$weight) %>%
      apply(3, as.data.frame) %>%
      unlist(recursive = FALSE) %>%
      do.call(what = cbind) %>%
      list() %>%
      setNames("cyear") -> lc.year
    rep(stratum_weights$weight, each = n_year) %>%
      matrix(nrow = n_year) %>%
      as.data.frame() -> lc.stratum
    colnames(lc.stratum) <- paste0("stratum", stratum_weights$stratum)
    lc.index <- c(lc.year, as.list(lc.stratum))
    rownames(lc.index[[1]]) <- min(dataset$year):max(dataset$year)
    n2k_inla(
      result.datasource.id = metadata$result_datasource,
      scheme.id = metadata$scheme,
      species.group.id = species_group,
      location.group.id = location_group,
      analysis.date = metadata$analysis_date,
      family = metadata$family,
      model.type = paste0(
        "inla ", metadata$family,
        ": RW1(Year|Stratum) + Period + Location + SubLocation"
      ),
      formula = paste("count ~ 0 + stratum + f(
        cyear,
        model = 'rw1',
        replicate = as.integer(stratum),
        hyper = list(theta = list(prior = 'pc.prec', param = c(0.5, 0.01)))
      ) +", design),
      lin.comb = lc.index,
      first.imported.year = metadata$first_imported_year,
      last.imported.year = metadata$last_imported_year,
      data = dataset %>%
        select(design.var, "cyear") %>%
        arrange(.data$DataFieldID, .data$ObservationID),
      status = "new",
      parent = metadata$file_fingerprint,
      seed = metadata$seed
    ) %>%
      storage(base = base, project = project, overwrite = overwrite) ->
      year_nl_fingerprint

    # non linear trend along cycle
    message(" cycle")
    cycle <- seq_len(max(dataset$cycle)) * 3 + 2004
    outer(diag(length(cycle)), stratum_weights$weight) %>%
      apply(3, as.data.frame) %>%
      unlist(recursive = FALSE) %>%
      do.call(what = cbind) %>%
      list() %>%
      setNames("cycle") -> lc.cycle
    rep(stratum_weights$weight, each = length(cycle)) %>%
      matrix(nrow = length(cycle)) %>%
      as.data.frame() -> lc.stratum
    colnames(lc.stratum) <- paste0("stratum", stratum_weights$stratum)
    lc.index <- c(lc.cycle, as.list(lc.stratum))
    rownames(lc.index[[1]]) <- paste(cycle, cycle + 2, sep = "-")
    n2k_inla(
      result.datasource.id = metadata$result_datasource,
      scheme.id = metadata$scheme,
      species.group.id = species_group,
      location.group.id = location_group,
      analysis.date = metadata$analysis_date,
      family = metadata$family,
      model.type = paste0(
        "inla ", metadata$family,
        ": RW1(Cycle|Stratum) + Period + Location + SubLocation"
      ),
      formula = paste("count ~ 0 + stratum + f(
        cycle,
        model = 'rw1',
        replicate = as.integer(stratum),
        hyper = list(theta = list(prior = 'pc.prec', param = c(0.5, 0.01)))
      ) +", design),
      lin.comb = lc.index,
      first.imported.year = metadata$first_imported_year,
      last.imported.year = metadata$last_imported_year,
      data = dataset %>%
        select(design.var, "cycle") %>%
        arrange(.data$DataFieldID, .data$ObservationID),
      status = "new",
      parent = metadata$file_fingerprint,
      seed = metadata$seed
    ) %>%
      storage(base = base, project = project, overwrite = overwrite) ->
      cycle_nl_fingerprint
  }
  expand.grid(
    frequency = c("year", "cycle"),
    type = c("linear", "non linear"),
    stringsAsFactors = FALSE
  ) %>%
    bind_cols(
      bind_rows(
        year_trend_fingerprint,
        cycle_trend_fingerprint,
        year_nl_fingerprint,
        cycle_nl_fingerprint
      )
    )
}
