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
  "inbo/n2kupdate",
  dependencies = FALSE,
  upgrade_dependencies = FALSE
)
devtools::install_github(
  "inbo/git2rdata",
  dependencies = FALSE,
  upgrade_dependencies = FALSE
)
devtools::install_github(
  "inbo/abvanalysis@meetnetten",
  dependencies = FALSE,
  upgrade_dependencies = FALSE
)
abvanalysis::prepare_analysis(
  repo = git2rdata::repository("~/n2k/abv"),
  base = aws.s3::get_bucket("n2kmonitoring"),
  project = "abv"
)

base <- aws.s3::get_bucket("n2kmonitoring", prefix = "abv/manifest")
keys <- purrr::map_chr(base, "Key")
lmod <- purrr::map_chr(base, "LastModified")
keys <- keys[order(lmod)]

writeLines(
  paste(
    sprintf(
      "docker run --rm -it --env-file ./env.list inbobmk/rn2k:latest ./analysis.sh -g inbo/n2khelper@v0.4.2 -g inbo/n2kanalysis@inla_poisson -b n2kmonitoring -p abv -m %s",
      keys
    ),
    collapse = " \n"
  ),
  "~/n2k/abv.sh"
)
system("chmod 771 abv.sh")
