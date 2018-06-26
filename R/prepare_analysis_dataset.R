#' Create the analysis dataset based on the available raw data
#'
#' This analysis fits an unweighted model but adds the stratum effect. The
#'    indices per year of the different strata are combined with a linear
#'    combination into a single index per year.
#' @return A data.frame with the species id number of rows in the analysis
#'    dataset, number of precenses in the analysis datset and SHA-1 of the
#'    analysis dataset or NULL if not enough data.
#' @importFrom n2khelper check_path check_dataframe_variable git_recent
#' @importFrom assertthat assert_that is.count
#' @importFrom plyr ddply
#' @importClassesFrom n2kanalysis n2kInlaNbinomial
#' @importFrom n2kanalysis n2k_inla_nbinomial
#' @importMethodsFrom n2kanalysis get_file_fingerprint get_status_fingerprint get_seed
#' @importFrom dplyr %>% mutate_ group_by_ filter_ inner_join select_ distinct ungroup inner_join bind_rows
#' @export
#' @param rawdata.file The file with the counts per visit
#' @param observation the dataframe with the visits and location group
#' @param analysis.path the path to store the rda files for the analysis
#' @param min.observation The minimum number of positive observations
#'    (Count > 0)
#' @param min.stratum minimum of locations per stratum. Strata with less locations will be dropped from the analysis
#' @inheritParams prepare_dataset
prepare_analysis_dataset <- function(
  rawdata.file,
  observation,
  raw.connection,
  analysis.path,
  min.observation,
  min.stratum
){
  assert_that(is.count(min.observation))
  metadata <- read_delim_git("metadata.txt", connection = raw.connection)
  scheme.id <- metadata$Value[metadata$Key == "SchemeID"]
  assert_that(is.count(scheme.id))
  first.year <- metadata$Value[metadata$Key == "FirstImportedYear"]
  assert_that(is.count(first.year))
  last.year <- metadata$Value[metadata$Key == "LastImportedYear"]
  assert_that(is.count(last.year))

  parent <- read_delim_git(file = "parent.txt", connection = raw.connection)
  check_dataframe_variable(
    df = parent,
    variable = c("SpeciesGroupID", "Fingerprint"),
    name = "parent.txt"
  )

  analysis.path <- check_path(paste0(analysis.path, "/"), type = "directory")
  check_dataframe_variable(
    df = observation,
    variable = c(
      "ObservationID", "DatasourceID", "LocationID", "SubLocationID", "Year",
      "Period", "Stratum", "Weight", "LocationGroupID"
    ),
    name = "observation"
  )

  species.observation <- read_delim_git(
    file = rawdata.file,
    connection = raw.connection
  )
  check_dataframe_variable(
    df = species.observation,
    variable = c("ObservationID", "Count"),
    name = rawdata.file
  )
  analysis.date <- git_recent(
    file = rawdata.file,
    connection = raw.connection
  )$Date
  speciesgroup.id <- as.integer(gsub("\\.txt$", "", rawdata.file))
  species.id <- read_delim_git("species.txt", connection = raw.connection)
  seed <- species.id$Seed[species.id$SpeciesGroupID == speciesgroup.id]
  parent <- parent$Fingerprint[parent$SpeciesGroupID == speciesgroup.id]

  rawdata <- observation %>%
    group_by_(~Stratum) %>%
    mutate_(Nobserved = ~n_distinct(LocationID)) %>%
    inner_join(species.observation, by = "ObservationID") %>%
    mutate_(
      Nrelevant = ~n_distinct(LocationID)
    ) %>%
    ungroup()

  message(speciesgroup.id, " ", appendLF = FALSE)
  utils::flush.console()
  analysis <- ddply(
    .data = rawdata,
    .variables = "LocationGroupID",
    .fun = function(dataset){
      locationgroup.id <- dataset$LocationGroupID[1]

      dataset <- dataset %>%
        filter_(~Nrelevant >= 3) %>%
        select_(~Stratum, ~Weight, ~Nobserved, ~Nrelevant) %>%
        distinct() %>%
        mutate_(
          Weight = ~Weight * Nrelevant / Nobserved,
          Weight = ~Weight / sum(Weight)
        ) %>%
        select_(~Stratum, ~Weight) %>%
        inner_join(
          dataset %>%
            select_(~-Weight, ~-Nobserved, ~-Nrelevant),
          by = "Stratum"
        )

      if (sum(dataset$Count > 0) < min.observation) {
        analysis <- n2k_inla_nbinomial(
          scheme.id = scheme.id,
          species.group.id = speciesgroup.id,
          location.group.id = locationgroup.id,
          analysis.date = analysis.date,
          model.type =
            "inla nbinomial: Year:Stratum + Period + Location + SubLocation",
          formula = "Count ~ 1",
          first.imported.year = first.year,
          last.imported.year = last.year,
          data = dataset,
          status = "insufficient data",
          parent = parent,
          seed = seed
        )
        filename <- sprintf(
          "%s/%s.rda",
          analysis.path,
          get_file_fingerprint(analysis)
        )
        if (!file.exists(filename)) {
          save(analysis, file = filename)
        }
        return(NULL)
      }

      dataset <- dataset %>%
        mutate_(
          fPeriod = ~factor(Period),
          fLocation = ~factor(LocationID),
          fSubLocation = ~factor(SubLocationID),
          fStratum = ~factor(Stratum),
          cYear = ~Year - max(Year),
          Cycle = ~(Year - 2007) %/% 3
        )
      multi.stratum <- length(levels(dataset$fStratum)) > 1
      design.variable <- character(0)
      design <- character(0)

      if (min(dataset$cYear) == 0) {
        stop("Dataset with a single year not yet handled")
      }
      weights <- dataset %>%
        ungroup() %>%
        select_(~fStratum, ~Weight) %>%
        distinct() %>%
        arrange_(~fStratum)
      lc.index <- min(dataset$cYear):max(dataset$cYear) %>%
        length() %>%
        diag() %>%
        outer(weights$Weight) %>%
        apply(3, as.data.frame) %>%
        unlist(recursive = FALSE) %>%
        do.call(what = cbind)
      rownames(lc.index) <- min(dataset$Year):max(dataset$Year)
      colnames(lc.index) <- outer(
          min(dataset$cYear):max(dataset$cYear) %>%
          sprintf(fmt = "cYear%s:"),
          weights$fStratum %>%
            sprintf(fmt = "fStratum%s"),
          FUN = "paste0"
        ) %>%
        as.vector()
      lc.index <- list(cYear = lc.index)

      if (multi.stratum) {
        lc.trend <- get_linear_lincomb(
          dataset = dataset,
          time.var = "cYear",
          stratum.var = "fStratum",
          formula = ~ 0 + fStratum + cYear:fStratum
        )
        names(lc.trend[[1]]) <- c(min(dataset$Year):max(dataset$Year), "Trend")
      } else {
        lc.trend <- cbind(
          "(Intercept)" = 1,
          cYear = min(dataset$cYear):max(dataset$cYear)
        )
        rownames(lc.trend) <- c(min(dataset$Year):max(dataset$Year))
        lc.trend <- rbind(lc.trend, Trend = c(0, 1))
      }
      if (multi.stratum) {
        trend <- c(
          "0 + fStratum +
          f(
            cYear,
            model = \"ar1\",
            replicate = as.integer(fStratum),
            hyper = list(
              theta1 = list(param = c(.1, 1e-5)),
              theta2 = list(param = c(0, .5))
            )
          )",
          "0 + fStratum +
          f(
            cYear,
            model = \"rw1\",
            replicate = as.integer(fStratum),
            hyper = list(theta = list(param = c(.1, 1e-5)))
          )",
          "0 + fStratum + cYear:fStratum"
        )
        trend.variable <- rep(list(list("cYear", "fStratum")), 3)
      } else {
        trend <- c(
          "f(
            cYear,
            model = \"ar1\",
            hyper = list(
              theta1 = list(param = c(.1, 1e-5)),
              theta2 = list(param = c(0, .5))
            )
          )",
          "f(
            cYear,
            model = \"rw1\",
            hyper = list(theta = list(param = c(.1, 1e-5)))
          )",
          "cYear"
        )
        trend.variable <- rep(list("cYear"), 3)
      }
      trend.name <- c("AR1(Year|Stratum)", "RW1(Year|Stratum)", "Year:Stratum")
      if (max(dataset$Cycle) == 0) {
        lc <- list(lc.index, lc.index, lc.trend)
        if (multi.stratum) {
          replicate.name <- list(
            list(cYear = levels(dataset$fStratum)),
            list(cYear = levels(dataset$fStratum)),
            list()
          )
        } else {
          replicate.name <- rep(list(list()), 3)
        }
      } else {
        cycle.label <- sort(unique(dataset$Cycle)) * 3 + 2007
        cycle.label <- sprintf("%i - %i", cycle.label, cycle.label + 2)
        dataset$fCycle <- factor(
          dataset$Cycle,
          labels = cycle.label
        )

        lc.cycle <- min(dataset$Cycle):max(dataset$Cycle) %>%
          length() %>%
          diag() %>%
          outer(weights$Weight) %>%
          apply(3, as.data.frame) %>%
          unlist(recursive = FALSE) %>%
          do.call(what = cbind)
        rownames(lc.cycle) <- cycle.label
        colnames(lc.cycle) <- outer(
            min(dataset$Cycle):max(dataset$Cycle) %>%
            sprintf(fmt = "Cycle%s:"),
            weights$fStratum %>%
              sprintf(fmt = "fStratum%s"),
            FUN = "paste0"
          ) %>%
          as.vector()
        lc.cycle <- list(Cycle = lc.cycle)
        if (multi.stratum) {
          lc.cycletrend <- get_linear_lincomb(
            dataset = dataset,
            time.var = "Cycle",
            stratum.var = "fStratum",
            formula = ~ 0 + fStratum + Cycle:fStratum
          )
          names(lc.cycletrend[[1]]) <- c(cycle.label, "Trend")
          replicate.name <- list(
            list(cYear = levels(dataset$fStratum)),
            list(cYear = levels(dataset$fStratum)),
            list(),
            list(Cycle = levels(dataset$fStratum)),
            list(Cycle = levels(dataset$fStratum)),
            list()
          )
        } else {
          lc.cycletrend <- cbind(
            "(Intercept)" = 1,
            Cycle = min(dataset$Cycle):max(dataset$Cycle)
          )
          rownames(lc.cycletrend) <- cycle.label
          lc.cycletrend <- rbind(lc.cycletrend, Trend = c(0, 3))
          replicate.name <- rep(list(list()), 6)
        }
        lc <- list(lc.index, lc.index, lc.trend, lc.cycle, lc.cycle, lc.cycletrend)
        if (multi.stratum) {
          trend <- c(
            trend,
            "0 + fStratum +
            f(
              Cycle,
              model = \"ar1\",
              replicate = as.integer(fStratum),
              hyper = list(
                theta1 = list(param = c(.1, 1e-5)),
                theta2 = list(param = c(0, .5))
              )
            )",
            "0 + fStratum +
            f(
              Cycle,
              model = \"rw1\",
              replicate = as.integer(fStratum),
              hyper = list(theta = list(param = c(.1, 1e-5)))
            )",
            "0 + fStratum + Cycle:fStratum"
          )
          trend.variable <- c(
            trend.variable,
            rep(list(list("fStratum", "Cycle", "fCycle")), 2),
            list(list("fStratum", "Cycle"))
          )
        } else {
          trend <- c(
            trend,
            "f(
              Cycle,
              model = \"ar1\",
              hyper = list(
                theta1 = list(param = c(.1, 1e-5)),
                theta2 = list(param = c(0, .5))
              )
            )",
            "f(
              Cycle,
              model = \"rw1\",
              hyper = list(theta = list(param = c(.1, 1e-5)))
            )",
            "Cycle"
          )
          trend.variable <- c(
            trend.variable,
            rep(list(list("Cycle", "fCycle")), 2),
            list(list("Cycle"))
          )
        }
        trend.name <- c(
          trend.name,
          c("AR1(Cycle|Stratum)", "RW1(Cycle|Stratum)", "Cycle:Stratum")
        )
      }

      if (length(levels(dataset$fLocation)) > 1) {
        design <- c(
          design,
          "f(
            fLocation,
            model = \"iid\",
            hyper = list(theta = list(param = c(.1, 1e-5)))
          )"
        )
        design.variable <- c(design.variable, "fLocation")
      }
      if (length(levels(dataset$fSubLocation)) > 1) {
        design <- c(
          design,
          "f(
            fSubLocation,
            model = \"iid\",
            hyper = list(theta = list(param = c(.1, 1e-5)))
          )"
        )
        design.variable <- c(design.variable, "fSubLocation")
      }
      if (length(levels(dataset$fPeriod)) > 1) {
        design <- c(design, "fPeriod")
        design.variable <- c(design.variable, "fPeriod")
      }

      design <- paste(design, collapse = " + ")
      covariates <- paste(trend, design, sep = " + ")

      fingerprint <- lapply(
        seq_along(covariates),
        function(i){
          data <- dataset %>%
            ungroup() %>%
            select_(
              ~ObservationID, ~DatasourceID, ~Count, ~Weight,
              .dots = c(trend.variable[[i]], design.variable)
            )
          model.type <- paste(
            "inla nbinomial:",
            trend.name[i], "+ Period + Location + SubLocation"
          )
          formula <- paste("Count ~", covariates[i])
          analysis <- n2k_inla_nbinomial(
            scheme.id = scheme.id,
            species.group.id = speciesgroup.id,
            location.group.id = locationgroup.id,
            analysis.date = analysis.date,
            seed = seed,
            model.type = model.type,
            formula = formula,
            first.imported.year = first.year,
            last.imported.year = last.year,
            data = data,
            parent = parent,
            replicate.name = replicate.name[[i]],
            lin.comb = lc[[i]]
          )
          file.fingerprint <- get_file_fingerprint(analysis)
          filename <- sprintf("%s/%s.rda", analysis.path, file.fingerprint)
          if (!file.exists(filename)) {
            save(analysis, file = filename)
          }
          data.frame(
            ModelType = model.type,
            Covariate = trend.name[i],
            FileFingerprint = file.fingerprint,
            StatusFingerprint = get_status_fingerprint(analysis),
            Seed = get_seed(analysis),
            stringsAsFactors = FALSE
          )
        }
      ) %>%
        bind_rows()
      return(
        cbind(
          LocationGroupID = locationgroup.id,
          AnalysisDate = analysis.date,
          fingerprint
        )
      )
    }
  )
  if (nrow(analysis) > 0) {
    analysis$SpeciesGroupID <- speciesgroup.id
  }
  return(analysis)
}
