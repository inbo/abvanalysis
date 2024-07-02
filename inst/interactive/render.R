library(git2rdata)
library(knitr)
library(quarto)
library(tidyverse)
conflicted::conflicts_prefer(dplyr::filter, dplyr::pull)
n_head <- Inf
results_folder <- "../../../abv/results"
target_folder <- "../../../abv/quarto"
file.path(target_folder, "_extensions") |>
  dir.create(showWarnings = FALSE, recursive = TRUE)

# styling
list.files("_extensions", full.names = TRUE) |>
  file.copy(to = file.path(target_folder, "_extensions"), overwrite = TRUE)
system.file("css_styles", package = "INBOmd") |>
  file.copy(to = target_folder, recursive = TRUE, overwrite = TRUE)
file.path(target_folder, "css_styles") |>
  file.copy(from = "custom.css", overwrite = TRUE)

# methodology
file.path(target_folder, "methodologie") |>
  dir.create(showWarnings = FALSE, recursive = TRUE)
list.files(pattern = "\\.(bib|md)", full.names = TRUE) |>
  file.copy(to = target_folder, overwrite = TRUE)
list.files("methodologie", pattern = "\\.qmd", full.names = TRUE) |>
  file.copy(to = file.path(target_folder, "methodologie"), overwrite = TRUE)

# species
file.path(target_folder, "soort") |>
  dir.create(showWarnings = FALSE, recursive = TRUE)
list.files(pattern = "trends.qmd", full.names = TRUE) |>
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
  knit_expand(
    "species.qmd", species = species$species[i], label = species$label[i]
  ) |>
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
  knit_expand(
    "composite.qmd", species = composite$species[i], label = composite$label[i]
  ) |>
    writeLines(file.path(target_folder, composite$output_file[i]))
}

# toc
readLines("_quarto.yml") |>
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
  file_test("-f", "../website/cover.pdf")
) {
  requireNamespace("pdftools")
  pdftools::pdf_convert(
    "../website/cover.pdf", format = "png", pages = 1, dpi = 770 * 25.4 / 210,
    filenames = file.path(target_folder, "cover.png")
  )
}

quarto_render(target_folder, use_freezer = TRUE, cache = TRUE, as_job = FALSE)

# render
# fs::dir_delete(fs::path(target_folder, "_freeze"))
# quarto_render(target_folder, use_freezer = FALSE, cache = FALSE, as_job = FALSE)
