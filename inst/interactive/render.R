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
target_pdf <- paste0(target_folder, "_pdf")

dir.create(target_folder, showWarnings = FALSE, recursive = TRUE)
dir.create(target_pdf, showWarnings = FALSE, recursive = TRUE)

# logo
system.file("interactive/natuurpunt.png", package = "abvanalysis") |>
  file.copy(to = target_folder, overwrite = TRUE)
system.file("interactive/natuurpunt.png", package = "abvanalysis") |>
  file.copy(to = target_pdf, overwrite = TRUE)

# methodology
## html
file.path(target_folder, "methodologie") |>
  dir.create(showWarnings = FALSE, recursive = TRUE)
list.files(source_folder, pattern = "\\.md", full.names = TRUE) |>
  file.copy(to = target_folder, overwrite = TRUE)

cite_package <- function(x) {
  citation(ifelse(x == "R", "base", x)) |>
    toBibtex() |>
    str_replace("Manual\\{,", paste0("Manual\\{", x, ","))
}
c("R", "git2rdata", "abvanalysis", "n2kanalysis") |>
  map(cite_package) |>
  c(
    file.path(source_folder, "references.bib") |>
      readLines()
  ) |>
  do.call(what = c) -> refs
file.path(target_folder, "references.bib") |>
  writeLines(text = refs)
file.path(source_folder, "methodologie") |>
  list.files(pattern = "\\.qmd", full.names = TRUE) |>
  file.copy(to = file.path(target_folder, "methodologie"), overwrite = TRUE)
## pdf
file.path(target_pdf, "methodologie") |>
  dir.create(showWarnings = FALSE, recursive = TRUE)
file.path(target_pdf, "references.bib") |>
  writeLines(text = refs)
file.path(source_folder, "abstract.md") |>
  readLines() -> abstract
c(head(abstract, 2), "# Abstract {-}", tail(abstract, -5)) |>
  writeLines(file.path(target_pdf, "abstract.md"))
c("# Bibliografie {-}", "", "::: {#refs}", ":::") |>
  writeLines(file.path(target_pdf, "references.md"))

c(
  "{{< colophon >}}",
  "# Samenvatting {-}",
  "",
  file.path(source_folder, "samenvatting.md") |>
    readLines() |>
    tail(-5)
) |>
  writeLines(file.path(target_pdf, "index.md"))
file.path(source_folder, "methodologie") |>
  list.files(pattern = "\\.qmd", full.names = TRUE) |>
  file.copy(to = file.path(target_pdf, "methodologie"), overwrite = TRUE)

# species
file.path(target_folder, "soort") |>
  dir.create(showWarnings = FALSE, recursive = TRUE)
list.files(source_folder, pattern = "trends.qmd", full.names = TRUE) |>
  file.copy(
    to = file.path(target_folder, "soort", "trends.qmd"),
    overwrite = TRUE
  )
file.path(source_folder, "trends_pdf.qmd") |>
  file.copy(to = file.path(target_pdf, "soort", "trends.qmd"), overwrite = TRUE)
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
  file.path(target_pdf, species$output_file[i]) |>
    dirname() |>
    dir.create(showWarnings = FALSE)
  file.path(source_folder, "species_pdf.qmd") |>
    knit_expand(species = species$species[i], label = species$label[i]) |>
    writeLines(file.path(target_pdf, species$output_file[i]))
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
  file.path(target_pdf, composite$output_file[i]) |>
    dirname() |>
    dir.create(showWarnings = FALSE)
  file.path(source_folder, "composite_pdf.qmd") |>
    knit_expand(species = composite$species[i], label = composite$label[i]) |>
    writeLines(file.path(target_pdf, composite$output_file[i]))
}

# toc
file.path(source_folder, "_quarto.yml") |>
  readLines() -> base_yaml
c(
  base_yaml,
  "    - section: \"Indicatoren\"",
  "      contents:",
  sprintf(
    "      - text: \"%s\"\n        file: %s",
    composite$speciesgroup,
    composite$output_file
  ),
  "    - section: \"Soorten\"",
  "      contents:",
  "      - text: \"Overzicht\"\n        file: soort/trends.qmd",
  sprintf(
    "      - text: \"%s\"\n        file: %s",
    species$speciesgroup,
    species$output_file
  )
) |>
  writeLines(file.path(target_folder, "_quarto.yml"))
tail(base_yaml, -4) |>
  head(grep("website:", base_yaml) - 5) |>
  gsub(pattern = "website-html", replacement = "book-pdf") |>
  grepv(pattern = "cover: cover.png", invert = TRUE) |>
  c(
    "book:",
    "  chapters:",
    "    - index.md",
    "    - abstract.md",
    "    - part: Methodologie",
    "      chapters:",
    "        - methodologie/inzameling.qmd",
    "        - methodologie/verwerking.qmd",
    "        - methodologie/voorstelling.qmd",
    "        - methodologie/reproduceerbaarheid.qmd",
    "    - part: Indicatoren",
    "      chapters:",
    sprintf("        - %s", composite$output_file),
    "    - part: Soorten",
    "      chapters:",
    "        - soort/trends.qmd",
    sprintf("        - %s", species$output_file),
    "    - references.md",
    "",
    "project:",
    "  output-dir: ../website",
    "  type: book",
    "  post-render: _extensions/inbo/flandersqmd-book/filters/post_render.R"
  ) |>
  writeLines(file.path(target_pdf, "_quarto.yml"))

old_wd <- getwd()
on.exit(setwd(old_wd), add = TRUE)
setwd(target_pdf)
quarto_add_extension("inbo/flandersqmd-book", no_prompt = TRUE)
quarto::quarto_render(output_format = "flandersqmd-book-pdf")

setwd(target_folder)
if (
  !file_test("-f", file.path(target_folder, "cover.png")) &&
    file_test(
      "-f",
      system.file("interactive/cover.pdf", package = "abvanalysis")
    )
) {
  requireNamespace("pdftools")
  system.file("interactive/cover.pdf", package = "abvanalysis") |>
    file.copy("cover.pdf")
  pdftools::pdf_convert(
    "cover.pdf",
    format = "png",
    pages = 1,
    dpi = 770 * 25.4 / 210,
    filenames = "cover.png"
  )
  file.remove("cover.pdf")
}

quarto_add_extension("inbo/flandersqmd-website", no_prompt = TRUE)
quarto::quarto_render(output_format = "flandersqmd-website-html")
