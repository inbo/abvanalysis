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
dir.create(file.path(base, project), recursive = TRUE, showWarnings = FALSE)

prepare_dataset(
  origin = origin, repo = repo, end_date = Sys.time(), verbose = TRUE,
  push = FALSE, strict = TRUE
)

script <- prepare_analysis(
  repo = repo, base = base, project = project, docker = "inbobmk/rn2k:0.8",
  dependencies = c("inbo/n2khelper", "inbo/n2kanalysis")
)

writeLines(
  c(script$init, script$model), file.path(base, project, "abv_model.sh")
)
writeLines(
  c(script$init, script$manifest), file.path(base, project, "abv.sh")
)

retrieve_results(
  base = base, project = project, source_repo = repo,
  target_repo = git2rdata::repository(), strict = FALSE
)
