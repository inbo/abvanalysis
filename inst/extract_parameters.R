# docker run --rm -it --env-file ./env.list -v ~/n2kdocker:/root/n2k inbobmk/rn2k:0.6 R
remotes::install_github(
  "inbo/n2khelper@v0.4.3",
  dependencies = FALSE,
  upgrade = FALSE
)
remotes::install_github(
  "inbo/n2kanalysis@v0.2.9",
  dependencies = FALSE,
  upgrade = FALSE
)

library(purrr)
library(tibble)
library(dplyr)
library(n2kanalysis)
library(INLA)

base <- "~/n2kanalysis/analysis"
project <- "abv"

extract_inla <- function(x) {
  x@AnalysisMetadata %>%
    transmute(
      analysis = FileFingerprint,
      status = StatusFingerprint,
      species = SpeciesGroupID,
      modeltype = ModelType,
      duration = Duration,
      last_year = LastAnalysedYear,
      waic = x@Model$waic$waic
    ) -> meta
  x@Model$summary.fixed %>%
    rownames_to_column("parameter") %>%
    transmute(analysis = get_file_fingerprint(x), parameter, mean, sd) -> fixef
  x@Model$summary.lincomb.derived %>%
    rownames_to_column("parameter") %>%
    transmute(analysis = get_file_fingerprint(x), parameter, mean, sd) -> lc
  x@Model$summary.random[
    names(x@Model$summary.random) %in% c("cycle", "cyear")
  ] -> ranef
  if (length(ranef) > 0) {
    fixef %>%
      filter(grepl("stratum", parameter)) %>%
      pull(parameter) -> strata
    if (length(strata) > 0) {
      ranef[[1]] %>%
        rownames_to_column("parameter") %>%
        transmute(
          analysis = get_file_fingerprint(x),
          stratum = rep(strata, each = n() / length(strata)),
          parameter, mean, sd
        ) -> ranef
    } else {
      ranef[[1]] %>%
        rownames_to_column("parameter") %>%
        transmute(
          analysis = get_file_fingerprint(x),
          stratum = "één stratum",
          parameter, mean, sd
        ) -> ranef
    }
  }
  if (!grepl("RW1", meta$modeltype)) {
    design <- list()
  } else {
    if (!grepl("RW1\\(Year", meta$modeltype)) {
      design <- list()
    } else {
      if (length(x@LinearCombination) <= 2) {
        strata <- tibble(
          analysis = get_file_fingerprint(x),
          stratum = "één stratum",
          gewicht = 1
        )
        x@Data %>%
          filter(!is.na(count)) %>%
          distinct(location) %>%
          mutate(stratum = "één stratum") %>%
          count(stratum, name = "hokken") %>%
          inner_join(
            x@Data %>%
              filter(!is.na(count)) %>%
              mutate(stratum = "één stratum") %>%
              count(stratum, name = "bezoeken"),
            by = "stratum"
          ) %>%
          mutate(
            stratum = as.character(stratum),
            analysis = get_file_fingerprint(x)
          ) -> inspanning
        x@Data %>%
          filter(!is.na(count)) %>%
          mutate(stratum = "één stratum") %>%
          distinct(cyear, stratum, location) %>%
          count(cyear, stratum, name = "hokken") %>%
          mutate(
            stratum = as.character(stratum),
            analysis = get_file_fingerprint(x)
          ) -> hokken_jaar
        x@Data %>%
          filter(!is.na(count)) %>%
          mutate(stratum = "één stratum") %>%
          count(cyear, location, stratum) %>%
          group_by(cyear, stratum) %>%
          summarise(
            min = min(n),
            mean = mean(n),
            max = max(n)
          ) %>%
          mutate(
            stratum = as.character(stratum),
            analysis = get_file_fingerprint(x)
          ) -> inspanning_per_jaar
      } else {
        x@LinearCombination[names(x@LinearCombination) != "cyear"] %>%
          do.call(what = "rbind") %>%
          `[`(, 1, drop = FALSE) %>%
          data.frame() %>%
          rename(gewicht = ".") %>%
          rownames_to_column("stratum") %>%
          mutate(
            analysis = get_file_fingerprint(x)
          ) -> strata
        x@Data %>%
          filter(!is.na(count)) %>%
          distinct(stratum, location) %>%
          count(stratum, name = "hokken") %>%
          inner_join(
            x@Data %>%
              filter(!is.na(count)) %>%
              count(stratum, name = "bezoeken"),
            by = "stratum"
          ) %>%
          mutate(
            stratum = as.character(stratum),
            analysis = get_file_fingerprint(x)
          ) -> inspanning
        x@Data %>%
          filter(!is.na(count)) %>%
          distinct(cyear, stratum, location) %>%
          count(cyear, stratum, name = "hokken") %>%
          mutate(
            stratum = as.character(stratum),
            analysis = get_file_fingerprint(x)
          ) -> hokken_jaar
        x@Data %>%
          filter(!is.na(count)) %>%
          count(cyear, location, stratum) %>%
          group_by(cyear, stratum) %>%
          summarise(
            min = min(n),
            mean = mean(n),
            max = max(n)
          ) %>%
          mutate(
            stratum = as.character(stratum),
            analysis = get_file_fingerprint(x)
          ) -> inspanning_per_jaar
      }
      x@Data %>%
        filter(!is.na(count)) %>%
        distinct(cyear, location) %>%
        count(location, name = "jaren") %>%
        count(jaren, name = "hokken") %>%
        mutate(analysis = get_file_fingerprint(x)) -> herbezoeken
      x@Data %>%
        filter(!is.na(count)) %>%
        distinct(location, cyear) %>%
        arrange(cyear) %>%
        group_by(location) %>%
        slice(1) %>%
        ungroup() %>%
        count(cyear) %>%
        full_join(
          x@Data %>%
            filter(!is.na(count)) %>%
            distinct(location, cyear) %>%
            count(cyear, name = "onderzocht"),
          by = "cyear"
        ) %>%
        transmute(
          cyear, onderzocht,
          cumulatief = cumsum(pmax(n, 0, na.rm = TRUE)),
          nieuw = diff(c(0, cumulatief)),
          analysis = get_file_fingerprint(x)
        ) -> zoekinspanning
      x@Data %>%
        filter(!is.na(count)) %>%
        distinct(location, cyear) %>%
        arrange(cyear) %>%
        group_by(location) %>%
        transmute(
          cyear,
          delta = c(NA, diff(cyear))
        ) %>%
        filter(!is.na(delta)) %>%
        group_by(cyear) %>%
        summarise(
          min = min(delta),
          mean = mean(delta),
          max = max(delta)
        ) %>%
        mutate(analysis = get_file_fingerprint(x)) -> delta
      design <- list(
        inspanning = inspanning, herbezoeken = herbezoeken,
        hokken_jaar = hokken_jaar, inspanning_per_jaar = inspanning_per_jaar,
        zoekinspanning = zoekinspanning, delta = delta, strata = strata
      )
    }
    design$voorspelling <- extract_prediction(x)
  }
  list(meta = meta, fixef = fixef, ranef = ranef, lc = lc, design = design)
}

extract_prediction <- function(x) {
  lc <- x@LinearCombination
  if (has_name(lc, "cycle")) {
    colnames(lc$cycle) <- sprintf(
      "cycle:%i",
      seq_len(nrow(x@Model$summary.random$cycle))
    )
  } else {
    colnames(lc$cyear) <- sprintf(
      "cyear:%i",
      seq_len(nrow(x@Model$summary.random$cyear))
    )
  }
  do.call(cbind, lc) %>%
    as.matrix() %>%
    `[`(-grep("change:", rownames(lc[[1]])), ) -> lc

  post_sample <- inla.posterior.sample(1e3, x@Model)
  map_dfc(post_sample, "latent") %>%
    as.data.frame() %>%
    `rownames<-`(rownames(post_sample[[1]]$latent)) %>%
    `[`(colnames(lc), ) %>%
    as.matrix() -> z
  mu <- exp(lc %*% z)
  map(post_sample, "hyperpar") %>%
    do.call(what = "rbind") -> hyperpar
  y <- switch(
    x@Family,
    poisson = mu,
    nbinomial = mu,
    zeroinflatedpoisson1 = mu *
    (1 - hyperpar[, "zero-probability parameter for zero-inflated poisson_1"]),
    zeroinflatednbinomial1 = mu *
  (1 - hyperpar[, "zero-probability parameter for zero-inflated nbinomial_1"]),
    stop(x@Family)
  )
  expand.grid(
    naar = seq_len(nrow(y)),
    referentie = seq_len(nrow(y))
  ) %>%
    filter(naar > referentie) -> wijziging
  extra <- y[wijziging$naar, ] / y[wijziging$referentie, ]
  rownames(extra) <- sprintf(
    "ratio: %s/%s",
    rownames(y)[wijziging$naar],
    rownames(y)[wijziging$referentie]
  )
  y <- rbind(y, extra)
  apply(y, 1,  quantile, probs = c(0.05, 0.2, 0.35, 0.5, 0.65, 0.8, 0.95)) %>%
    t() %>%
    as.data.frame() %>%
    `colnames<-`(
      c("lcl90", "lcl60", "lcl30", "median", "ucl30", "ucl60", "ucl90")
    ) %>%
    rownames_to_column("parameter") %>%
    mutate(
      log_mean = rowMeans(log(y)),
      log_sd = apply(log(y), 1, sd),
      analysis = get_file_fingerprint(x)
    )
}

extract_composite <- function(x) {
  x@AnalysisMetadata %>%
    transmute(
      analysis = FileFingerprint,
      status = StatusFingerprint,
      species = SpeciesGroupID,
      modeltype = ModelType,
      duration = Duration,
      last_year = LastAnalysedYear
    ) -> meta
  x@AnalysisRelation %>%
    select(analysis = Analysis, parent = ParentAnalysis) -> relation
  x@Index %>%
    transmute(
      analysis = get_file_fingerprint(x),
      parameter = Value,
      log_mean = Estimate,
      log_sd = (UpperConfidenceLimit - LowerConfidenceLimit) /
        diff(qnorm(c(0.025, 0.975)))
    ) -> index
  return(list(meta = meta, relation = relation, index = index))
}

file.path(base, project) %>%
  list.files(recursive = TRUE, full.names = TRUE) -> files
files[grep("converged", files)] %>%
  basename() %>%
  gsub(pattern = ".rds", replacement = "") -> hashes
file.path(base, "result", project) %>%
  dir.create(recursive = TRUE, showWarnings = FALSE)
file.path(base, "result", project) %>%
  list.files() %>%
  gsub(pattern = ".rds", replacement = "") -> done
for (hash in hashes[!hashes %in% done]) {
  message(hash)
  x <- read_model(x = hash, base = base, project = project)
  output <- switch(
    class(x),
    n2kInla = extract_inla(x),
    n2kInlaComparison = list(),
    n2kComposite = extract_composite(x),
    stop(class(x))
  )
  file.path(base, "result", project, hash) %>%
    paste0(".rds") %>%
    saveRDS(object = output)
  rm(x, output)
  gc()
}

file.path(base, "result", project) %>%
  list.files(full.names = TRUE)  %>%
  map(readRDS) -> results
map(results, "design") -> design
design <- design[!map_lgl(design, is.null)]
list(
  meta = map_df(results, "meta"),
  fixef = map_df(results, "fixef"),
  ranef = map_df(results, "ranef"),
  lc = map_df(results, "lc"),
  inspanning = map_df(design, "inspanning"),
  herbezoeken = map_df(design, "herbezoeken"),
  hokken_jaar = map_df(design, "hokken_jaar"),
  inspanning_per_jaar = map_df(design, "inspanning_per_jaar"),
  zoekinspanning = map_df(design, "zoekinspanning"),
  strata = map_df(design, "strata"),
  delta = map_df(design, "delta"),
  voorspelling = map_df(design, "voorspelling"),
  relation = map_df(results, "relation"),
  index = map_df(results, "index")
) %>%
  saveRDS("inst/results.rds")

conn <- n2khelper::connect_result(
  username = Sys.getenv("N2KRESULT_USERNAME"),
  password = Sys.getenv("N2KRESULT_PASSWORD")
)
tbl(conn, "scheme") %>%
  filter(description == "Algemene broedvogels") %>%
  semi_join(
    x = tbl(conn, "species_group"),
    by = c("scheme" = "id")
  ) %>%
  select(fingerprint, species_group = description) %>%
  collect() %>%
  mutate(
    hash = map2_chr(fingerprint, species_group, ~digest::sha1(c(.x, .y)))
  ) %>%
  saveRDS("inst/species.rds")
