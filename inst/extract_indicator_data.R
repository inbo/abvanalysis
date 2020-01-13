library(tidyverse)
library(git2rdata)
root <- repository(indicatoren_repo)
results <- readRDS("inst/results.rds")
species <- readRDS("inst/species.rds")
results$meta %>%
  filter(str_detect(modeltype, "composite index: (year|cycle) non linear")) %>%
  inner_join(species, by = c("species" = "fingerprint")) %>%
  filter(species_group %in% c("Bos", "Generalist", "Landbouw")) %>%
  transmute(
    analysis,
    status,
    indicator = factor(species_group),
    frequentie = factor(
      str_detect(modeltype, "cycle"),
      levels = c(FALSE, TRUE),
      labels = c("jaarlijks", "driejaarlijks")
    )
  ) %>%
  inner_join(results$index, by = "analysis") %>%
  filter(str_detect(parameter, "change")) %>%
  extract(
    parameter,
    c("naar", "referentie"),
    "([0-9]{4}) - ([0-9]{4})",
    convert = TRUE
  ) %>%
  mutate(
    log_lcl90 = qnorm(0.05, log_mean, log_sd),
    log_lcl60 = qnorm(0.20, log_mean, log_sd),
    log_lcl30 = qnorm(0.35, log_mean, log_sd),
    log_ucl30 = qnorm(0.65, log_mean, log_sd),
    log_ucl60 = qnorm(0.80, log_mean, log_sd),
    log_ucl90 = qnorm(0.95, log_mean, log_sd)
  ) %>%
  mutate_at(vars(starts_with("log")), c(exp = exp)) %>%
  select(-log_sd_exp) %>%
  mutate_at(vars(starts_with("log")), ~round(., 3)) %>%
  mutate_at(vars(ends_with("exp")), ~ round(. * 100, 1)) %>%
  rename_at(
    vars(ends_with("exp")),
    str_replace,
    pattern = "log_(.*)_exp",
    replacement = "\\1"
  ) %>%
  arrange(indicator, frequentie, naar, referentie) -> basis
basis %>%
  select(-analysis, -status) %>%
  write_vc(
    file = "source/abv/abv_indicatoren",
    sorting = c("indicator", "naar", "referentie"),
    root = root,
    optimize = FALSE,
    stage = TRUE
  )
basis %>%
  distinct(indicator, frequentie, analysis, status) %>%
  write_vc(
    file = "source/abv/abv_data_hash",
    sorting = c("indicator", "frequentie"),
    root = root,
    optimize = FALSE,
    stage = TRUE
  )
results$relation %>%
  semi_join(basis, by = "analysis") %>%
  inner_join(
    results$meta %>%
      select(parent = analysis, species),
    by = "parent"
  ) %>%
  inner_join(species, by = c("species" = "fingerprint")) %>%
  select(analysis, species = species_group) %>%
  write_vc(
    file = "source/abv/abv_soorten",
    sorting = c("species", "analysis"),
    root = root,
    optimize = FALSE,
    stage = TRUE
  )
