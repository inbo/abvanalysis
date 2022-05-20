library(keyring)
library(abvanalysis)
repo <- git2rdata::repository(file.path("~", "n2k", "abv"))
origin <- RPostgreSQL::dbConnect(
  RPostgreSQL::PostgreSQL(), host = "127.0.0.1", user = "readonly",
  dbname = "meetnetten", port = "65432",
  password = keyring::key_get("meetnetten", username = "readonly")
)
base <- file.path("~", "n2kanalysis")
project <- "abv"

prepare_dataset(
  origin = origin, repo = repo, end_date = Sys.time(), verbose = TRUE,
  push = TRUE, strict = TRUE
)

script <- prepare_analysis(
  repo = repo, base = base, project = project,
  dependencies = c(
    "inbo/n2khelper@checklist", "inbo/n2kanalysis@inla_arguments"
  )
)

writeLines(
  c(script$init, script$model), file.path(base, project, "abv_model.sh")
)
writeLines(
  c(script$init, script$manifest), file.path(base, project, "abv.sh")
)

retrieve_results(
  base = base, project = project, source_repo = repo,
  target_repo = git2rdata::repository(), strict = TRUE
)
