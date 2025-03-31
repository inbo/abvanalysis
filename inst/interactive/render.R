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
target_folder <- key_get("quarto_folder", keyring = "abv")
dir.create(target_folder, showWarnings = FALSE, recursive = TRUE)

# methodology
file.path(target_folder, "methodologie") |>
  dir.create(showWarnings = FALSE, recursive = TRUE)
list.files(source_folder, pattern = "\\.(bib|md)", full.names = TRUE) |>
  file.copy(to = target_folder, overwrite = TRUE)
file.path(source_folder, "methodologie") |>
  list.files(pattern = "\\.qmd", full.names = TRUE) |>
  file.copy(to = file.path(target_folder, "methodologie"), overwrite = TRUE)

# species
file.path(target_folder, "soort") |>
  dir.create(showWarnings = FALSE, recursive = TRUE)
list.files(source_folder, pattern = "trends.qmd", full.names = TRUE) |>
  file.copy(
    to = file.path(target_folder, "soort", "index.qmd"), overwrite = TRUE
  )
read_vc("meta", results_folder) |>
  filter(.data$status == "converged", !.data$composite) |>
  distinct(.data$speciesgroup) |>
  arrange(.data$speciesgroup) |>
  head(n_head) |>
  mutate(
    label = str_replace_all(.data$speciesgroup, " ", "-") |>
      str_remove_all("'") |>
      tolower(),
    output_file = sprintf(fmt = "soort/%s.qmd", .data$label)
  ) -> species
for (i in seq_len(nrow(species))) {
  file.path(target_folder, species$output_file[i]) |>
    dirname() |>
    dir.create(showWarnings = FALSE)
  file.path(source_folder, "species.qmd") |>
    knit_expand(species = species$species[i], label = species$label[i]) |>
    writeLines(file.path(target_folder, species$output_file[i]))
}

# composite index
read_vc("meta", results_folder) |>
  filter(.data$status == "converged", .data$composite) |>
  distinct(.data$speciesgroup) |>
  arrange(.data$speciesgroup) |>
  head(n_head) |>
  mutate(
    label = str_replace_all(.data$speciesgroup, " ", "-") |>
      str_remove_all("'") |>
      tolower(),
    output_file = sprintf(fmt = "index/%s.qmd", .data$label)
  ) -> composite
for (i in seq_len(nrow(composite))) {
  file.path(target_folder, composite$output_file[i]) |>
    dirname() |>
    dir.create(showWarnings = FALSE)
  file.path(source_folder, "composite.qmd") |>
    knit_expand(species = composite$species[i], label = composite$label[i]) |>
    writeLines(file.path(target_folder, composite$output_file[i]))
}

# toc
file.path(source_folder, "_quarto.yml") |>
  readLines() |>
  c(
    "    - section: \"Indicatoren\"", "      contents:",
    sprintf(
      "      - text: \"%s\"\n        file: %s", composite$speciesgroup,
      composite$output_file
    ),
    "    - section: \"Soorten\"",  "      file: soort/index.qmd",
    "      contents:",
    "      - text: \"Overzicht\"\n        file: soort/index.qmd",
    sprintf(
      "      - text: \"%s\"\n        file: %s", species$speciesgroup,
      species$output_file
    )
  ) |>
  writeLines(file.path(target_folder, "_quarto.yml"))

if (
  !file_test("-f", file.path(target_folder, "cover.png")) &&
  file_test("-f", system.file("website/cover.pdf", package = "abvanalysis"))
) {
  requireNamespace("pdftools")
  pdftools::pdf_convert(
    system.file("website/cover.pdf", package = "abvanalysis"), format = "png",
    pages = 1, dpi = 770 * 25.4 / 210,
    filenames = file.path(target_folder, "cover.png")
  )
}

old_wd <- getwd()
on.exit(setwd(old_wd), add = TRUE)
setwd(target_folder)
system2(
  "quarto", c("install extension inbo/flandersqmd-website@draft", "--no-prompt")
)
quarto_render(".", use_freezer = TRUE, cache = TRUE, as_job = FALSE)
