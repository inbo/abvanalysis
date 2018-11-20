devtools::install_github(
  "inbo/n2khelper@v0.4.2",
  dependencies = FALSE,
  upgrade_dependencies = FALSE
)
devtools::install_github(
  "inbo/n2kanalysis@inla_poisson",
  dependencies = FALSE,
  upgrade_dependencies = FALSE
)
devtools::install_github(
  "inbo/inlatools@extras",
  dependencies = TRUE,
  upgrade_dependencies = FALSE
)
library(dplyr)
library(tibble)
library(purrr)
library(inlatools)
library(n2kanalysis)
bucket <- "n2kmonitoring"
project <- "abv"
local <- "/root/n2k/abv_result"
dir.create(local)
# file.remove(list.files(local, recursive = TRUE, full.names = TRUE))

extract_composite <- function(z) {
  z@Index %>%
    transmute(
      parameter = Value,
      mean = Estimate,
      median = NA_real_,
      lcl = LowerConfidenceLimit,
      ucl = UpperConfidenceLimit,
      stratum = "Vlaanderen",
      species_group = z@AnalysisMetadata$SpeciesGroupID,
      analysis = z@AnalysisMetadata$FileFingerprint,
      status = z@AnalysisMetadata$StatusFingerprint,
      model_type = z@AnalysisMetadata$ModelType,
      waic = NA_real_
    )
}

extract_lc <- function(z) {
  z@Model$summary.lincomb.derived %>%
    select(
      mean, median = "0.5quant", lcl = "0.025quant", ucl = "0.975quant"
    ) %>%
    rownames_to_column("parameter") %>%
    mutate(stratum = "Vlaanderen") -> index
  if (grepl("stratum", z@AnalysisMetadata$Formula)) {
    return(index)
  }
  if (grepl("RW1", z@AnalysisMetadata$ModelType)) {
    return(index)
  }
  z@Model$summary.fixed %>%
    select(
      mean, median = "0.5quant", lcl = "0.025quant", ucl = "0.975quant"
    ) %>%
    rownames_to_column("parameter") %>%
    filter(parameter == "cyear") %>%
    mutate(
      parameter = "Trend",
      stratum = "Vlaanderen"
    ) %>%
    bind_rows(
      index %>%
        filter(parameter != "Trend")
    )
}

extract_stratum <- function(z) {
  if (grepl("stratum", z@AnalysisMetadata$Formula)) {
    if (grepl("cycle", z@AnalysisMetadata$Formula)) {
      if (grepl("cycle:stratum", z@AnalysisMetadata$Formula)) {
        z@Model$summary.fixede %>%
          select(
            mean,
            median = "0.5quant",
            lcl = "0.025quant",
            ucl = "0.975quant"
          ) %>%
          rownames_to_column("parameter") %>%
          filter(grepl(":cycle", parameter)) %>%
          mutate(
            stratum = gsub(
              "stratum([[:xdigit:]]{40}):cycle", "\\1",
              parameter
            ),
            parameter = "Trend"
          )
      } else {
        z@Model$summary.random$cycle %>%
          select(
            parameter = ID,
            mean,
            median = "0.5quant",
            lcl = "0.025quant",
            ucl = "0.975quant"
          ) %>%
          mutate(
            parameter = as.character(parameter),
            stratum = rep(levels(z@Data$stratum), each = max(z@Data$cycle))
          )
      }
    } else {
      if (grepl("cyear:stratum", z@AnalysisMetadata$Formula)) {
        z@Model$summary.fixed %>%
          select(
            mean,
            median = "0.5quant",
            lcl = "0.025quant",
            ucl = "0.975quant"
          ) %>%
          rownames_to_column("parameter") %>%
          filter(grepl(":cyear", parameter)) %>%
          mutate(
            stratum = gsub(
              "stratum([[:xdigit:]]{40}):cyear", "\\1",
              parameter
            ),
            parameter = "Trend"
          )
      } else {
        z@Model$summary.random$cyear %>%
          select(
            parameter = ID,
            mean,
            median = "0.5quant",
            lcl = "0.025quant",
            ucl = "0.975quant"
          ) %>%
          mutate(
            parameter = as.character(parameter),
            stratum = rep(levels(z@Data$stratum), each = max(z@Data$cyear))
          )
      }
    }
  } else {
    if (grepl("cycle", z@AnalysisMetadata$Formula)) {
      if (grepl("\\+.*cycle.*\\+", z@AnalysisMetadata$Formula)) {
        z@Model$summary.fixed %>%
          select(
            mean,
            median = "0.5quant",
            lcl = "0.025quant",
            ucl = "0.975quant"
          ) %>%
          rownames_to_column("parameter") %>%
          filter(grepl("cycle", parameter)) %>%
          mutate(
            stratum = "een stratum",
            parameter = "Trend"
          )
      } else {
        stop("single stratum cycle nl")
      }
    } else {
      if (grepl("\\+.*cyear.*\\+", z@AnalysisMetadata$Formula)) {
        z@Model$summary.fixed %>%
          select(
            mean,
            median = "0.5quant",
            lcl = "0.025quant",
            ucl = "0.975quant"
          ) %>%
          rownames_to_column("parameter") %>%
          filter(grepl("cyear", parameter)) %>%
          mutate(
            stratum = "een stratum",
            parameter = "Trend"
          )
      } else {
        stop("single stratum cyear nl")
      }
    }
  }
}

extract_design <- function(z) {
  z@Data %>%
    select_at(
      na.omit(c(
        ifelse(has_name(z@Data, "stratum"), "stratum", NA),
        ifelse(has_name(z@Data, "cyear"), "cyear", "cycle"),
        "location"
      ))
    ) %>%
    group_by_all() %>%
    summarise(n_obs = n()) %>%
    summarise(
      n_location = n(),
      mean_obs = mean(n_obs),
      min_obs = min(n_obs),
      max_obs = max(n_obs)
    ) -> time_obs
  z@Data %>%
    select_at(
      na.omit(c(
        ifelse(has_name(z@Data, "stratum"), "stratum", NA),
        "location"
      ))
    ) %>%
    group_by_all() %>%
    summarise(n_obs = n()) %>%
    summarise(
      n_location = n(),
      n_visit = sum(n_obs)
    ) -> total
  z@Data %>%
    select_at(
      c(
        "location",
        ifelse(has_name(z@Data, "cyear"), "cyear", "cycle")
      )
    ) %>%
    distinct() %>%
    count(location) %>%
    rename(visits = n) %>%
    count(visits) -> revisit
  z@Data %>%
    select_at(
      c(
        "location",
        time = ifelse(has_name(z@Data, "cyear"), "cyear", "cycle")
      )
    ) %>%
    distinct() %>%
    arrange(location, time) %>%
    group_by(location) %>%
    mutate(
      delta = lag(time)
    ) %>%
    group_by(time) %>%
    summarise(
      n = n(),
      new = sum(is.na(delta)),
      min_delta = min(delta, na.rm = TRUE),
      max_delta = max(delta, na.rm = TRUE),
      mean_delta = mean(delta, na.rm = TRUE)
    ) -> time_revisit
  list(total = total, time_obs = time_obs, revisit = revisit, time_revisit)
}

available <- aws.s3::get_bucket(bucket = bucket, prefix = project, max = Inf)
keys <- map_chr(available, "Key")
hashes <- gsub(".*([[:xdigit:]]{40}).*", "\\1", keys)
manifests <- grep(paste0(project, "/manifest/[[:xdigit:]]{40}.manifest"), keys)
manifest <- map_dfr(
  hashes[manifests],
  function(m_hash) {
    message("manifest: ", m_hash)
    manifest <- read_manifest(m_hash, base = available, project = project)
    if (sum(is.na(manifest@Manifest$Parent)) == 2) {
      manifest@Manifest %>%
        filter(is.na(Parent)) %>%
        transmute(hash = Fingerprint, manifest = manifest@Fingerprint)
    } else {
      manifest@Manifest %>%
        filter(!is.na(Parent)) %>%
        distinct(Fingerprint) %>%
        transmute(hash = Fingerprint, manifest = manifest@Fingerprint)
    }
  }
)
data.frame(
  hash = hashes,
  size = map_dbl(available, "Size"),
  status = dirname(keys) %>%
    basename() %>%
    factor(),
  stringsAsFactors = FALSE
) %>%
  filter(status == "converged") %>%
  inner_join(manifest, by = "hash") %>%
  arrange(size) %>%
  pull(hash) %>%
  sort() -> todo

walk(
  todo,
  function(x) {
    message(x)
    target <- paste0(local, "/", x, ".rds")
    if (file.exists(target)) {
      return(NULL)
    }
    z <- try(read_model(x, base = local, project = "abv"), silent = TRUE)
    if (inherits(z, "try-error")) {
      z <- read_model(x, base = available, project = "abv")
      try(store_model(z, base = local, project = "abv"))
    }
    if (inherits(z, "n2kComposite")) {
      ec <- try(extract_composite(z))
      if (inherits(ec, "try-error")) {
        fit_model(x, base = available, project = "abv", status = "converged")
        z <- read_model(x, base = available, project = "abv")
        try(store_model(z, base = local, project = "abv"))
        ec <- extract_composite(z)
      }
      saveRDS(list(result = ec), file = target)
    } else if (inherits(z, "n2kInla")) {
      bind_rows(
        extract_lc(z),
        extract_stratum(z)
      ) %>%
        mutate(
          species_group = z@AnalysisMetadata$SpeciesGroupID,
          analysis = z@AnalysisMetadata$FileFingerprint,
          status = z@AnalysisMetadata$StatusFingerprint,
          model_type = z@AnalysisMetadata$ModelType,
          waic = z@Model$waic$waic
        ) -> result
      dc <- fast_distribution_check(get_model(z))
      disp <- dispersion_check(get_model(z), plot = FALSE)
      ds <- extract_design(z)
      saveRDS(
        list(
          result = result, distribution = dc, dispersion = disp, design = ds
        ),
        file = target
      )
    }
    delete_model(x, base = local, project = "abv")
    rm(z)
    gc()
  }
)

map(
  hashes[manifests],
  function(m_hash) {
    message("manifest: ", m_hash)
    manifest <- read_manifest(m_hash, base = available, project = project)
    if (sum(is.na(manifest@Manifest$Parent)) == 2) {
      manifest@Manifest %>%
        filter(is.na(Parent)) %>%
        pull(Fingerprint) -> fp
      lk <- keys[hashes %in% fp]
    } else {
      manifest@Manifest %>%
        filter(!is.na(Parent)) %>%
        distinct(Fingerprint) %>%
        pull(Fingerprint) -> fp
      lk <- keys[hashes %in% fp]
    }
    lk[grep("error", lk)]
  }
) %>%
  unlist() %>%
  aws.s3::s3saveRDS("abv/results/errors.rds", bucket = available)

result_hash <- list.files(
  local,
  pattern = "[[:xdigit:]]{40}.rds",
  full.names = TRUE
)
results <- map(result_hash, readRDS)
names(results) <- gsub(".*([[:xdigit:]]{40}).*", "\\1", result_hash)
aws.s3::s3saveRDS(results, "abv/results/results.rds", bucket = "n2kmonitoring")
