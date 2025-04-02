library(abvanalysis)
library(git2rdata)
library(keyring)
library(knitr)
library(quarto)
library(tidyverse)
conflicted::conflicts_prefer(dplyr::filter, dplyr::pull)
n_head <- Inf
results_folder <- system.file("results", package = "abvanalysis")
source_folder <- system.file("interactive", package = "abvanalysis")
target_folder <- key_get("vbp_folder", keyring = "abv")
raw_folder <- key_get("raw_folder", keyring = "abv")
dir.create(target_folder, showWarnings = FALSE, recursive = TRUE)

# species
read_vc("meta", results_folder) |>
  filter(.data$status == "converged", !.data$composite) |>
  distinct(.data$speciesgroup) |>
  arrange(.data$speciesgroup) |>
  head(n_head) |>
  mutate(
    label = str_replace_all(.data$speciesgroup, " ", "-") |>
      str_remove_all("'") |>
      tolower(),
    output_file = sprintf(fmt = "%s.qmd", .data$label)
  ) -> species
for (i in seq_len(nrow(species))) {
  file.path(target_folder, species$output_file[i]) |>
    dirname() |>
    dir.create(showWarnings = FALSE)
  file.path(source_folder, "vbp_species.qmd") |>
    knit_expand(
      species = species$speciesgroup[i], doi = "10.21436/inbor.119461371",
      url = "https://www.vlaanderen.be/inbo/rapporten/abv-trends-2007-2024",
      label = species$label[i]
    ) |>
    writeLines(file.path(target_folder, species$output_file[i]))
}

file.path(target_folder, species$output_file) |>
  quarto_render(use_freezer = TRUE, cache = TRUE, as_job = FALSE)

read_vc("species/species", root = raw_folder) |>
  select("id", "nl", "scientific_name") |>
  inner_join(species, by = c("nl" = "speciesgroup")) |>
  select(-"label") |>
  write_csv(file.path(target_folder, "species.csv"))

old_wd <- getwd()
on.exit(setwd(old_wd), add = TRUE)
setwd(target_folder)
list.files(pattern = "\\.(csv|html)") |>
  zip(zipfile = "vbp.zip", flags = "-r9XqT")
