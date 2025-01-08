library(abvanalysis)
repo <- git2rdata::repository(file.path("~", "n2k", "abv"))
origin <- odbc::dbConnect(
  odbc::odbc(), driver = "ODBC Driver 18 for SQL Server",
  server = "inbo-sql08-prd.inbo.be", Encrypt = "no",
  database = "S0008_00_Meetnetten", port = 1433, uid = "bmkreader",
  pwd = keyring::key_get("meetnetten", username = "bmkreader")
) # sql server

prepare_dataset(
  origin = origin, repo = repo, end_date = Sys.time(), verbose = TRUE,
  push = FALSE, strict = FALSE, db_scheme = "staging_Meetnetten"
)

base <- file.path("~", "n2kanalysis")
project <- "abv"
dir.create(file.path(base, project), recursive = TRUE, showWarnings = FALSE)
script <- prepare_analysis(
  repo = repo, base = base, project = project, docker = "inbobmk/rn2k:0.9",
  dependencies = c("inbo/n2khelper@v0.5.0", "inbo/n2kanalysis@v0.3.2")
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
