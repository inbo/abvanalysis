library(abvanalysis)
repo <- git2rdata::repository(file.path("~", "n2k", "abv"))
origin <- odbc::dbConnect(
  odbc::odbc(), driver = "ODBC Driver 18 for SQL Server",
  server = "inbo-sql08-prd.inbo.be", Encrypt = "no",
  database = "S0008_00_Meetnetten", port = 1433, uid = "bmkreader",
  pwd = keyring::key_get("meetnetten", username = "bmkreader")
) # sql server
project <- "abv"
base <- aws.s3::get_bucket(Sys.getenv("N2KBUCKET"), prefix = project, max = 1)

prepare_dataset(
  origin = origin, repo = repo, end_date = Sys.time(), verbose = TRUE,
  push = FALSE, strict = FALSE, db_scheme = "staging_Meetnetten"
)

prepare_analysis(
  repo = repo, base = base, project = project, docker = "inbobmk/rn2k:dev-0.10",
  dependencies = c("inbo/n2khelper@v0.5.0", "inbo/n2kanalysis@v0.4.0")
)

read_manifest(base = base, project = project) |>
  fit_model(base = base, project = project)

retrieve_results(
  base = base, project = project, source_repo = repo,
  target_repo = git2rdata::repository(), strict = FALSE
)
