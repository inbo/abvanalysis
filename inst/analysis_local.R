# import data
library(abvanalysis)
repo <- git2rdata::repository(file.path("~", "n2k", "abv"))
origin <- odbc::dbConnect(
  odbc::odbc(), driver = "ODBC Driver 18 for SQL Server",
  server = "inbo-sql08-prd.inbo.be", Encrypt = "no",
  database = "S0008_00_Meetnetten", port = 1433, uid = "bmkreader",
  pwd = keyring::key_get("meetnetten", username = "bmkreader")
) # sql server
project <- "abv"
prepare_dataset(
  origin = origin, repo = repo, end_date = Sys.time(), verbose = TRUE,
  push = FALSE, strict = FALSE, db_scheme = "staging_Meetnetten"
)

# prepare analysis
library(abvanalysis)
library(n2kanalysis)
repo <- git2rdata::repository(file.path("~", "n2k", "abv"))
project <- "abv"
connect_inbo_s3()
base <- aws.s3::get_bucket(Sys.getenv("N2KBUCKET"), prefix = project, max = 1)
prepare_analysis(
  repo = repo,
  base = base,
  project = project,
  docker = "inbobmk/rn2k:dev-0.10",
  dependencies = c("inbo/n2khelper@v0.5.0", "inbo/n2kanalysis@0.4.1")
) -> bash_script
sprintf(
  "aws s3 cp s3://%s/%s ~/abv.sh\nchmod +x ~/abv.sh",
  Sys.getenv("N2KBUCKET"),
  bash_script
) |>
  cat()

# fit models
library(n2kanalysis)
connect_inbo_s3()
base <- aws.s3::get_bucket(Sys.getenv("N2KBUCKET"), prefix = project, max = 1)
read_manifest(base = base, project = project) |>
  fit_model(base = base, project = project)

# get results
library(abvanalysis)
library(n2kanalysis)
connect_inbo_s3()
project <- "abv"
base <- aws.s3::get_bucket(Sys.getenv("N2KBUCKET"), prefix = project, max = 1)
repo <- git2rdata::repository(file.path("~", "n2k", "abv"))
retrieve_results(
  base = base, project = project, source_repo = repo,
  target_repo = git2rdata::repository(), strict = FALSE
)

