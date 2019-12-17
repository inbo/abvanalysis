library(abvanalysis)
repo <- git2rdata::repository("~/n2k/abv")
result <- n2khelper::connect_result(
  username = Sys.getenv(
    "N2KRESULT_USERNAME",
    keyringr::decrypt_gk_pw("db n2kresult n2k_analysis user")
  ),
  password = Sys.getenv(
    "N2KRESULT_PASSWORD",
    keyringr::decrypt_gk_pw("db n2kresult n2k_analysis pwd")
  )
)
dbplyr::src_dbi(
  RPostgreSQL::dbConnect(
    RPostgreSQL::PostgreSQL(),
    host = "localhost",
    port = Sys.getenv("meetnetten_port"),
    dbname = "meetnetten",
    user = Sys.getenv("meetnetten_user"),
    password = Sys.getenv("meetnetten_password")
  )
) -> origin
prepare_dataset(
  result = result, origin = origin, repo = repo,
  scheme.id = scheme_id(result.channel = result)
)

library(abvanalysis)
repo <- git2rdata::repository("~/n2k/abv")
base <- aws.s3::get_bucket("n2kmonitoring", max = 1)
script <- prepare_analysis(repo = repo, base = base, project = "abv")
writeLines(c(script$init, script$model), "~/n2kanalysis/abv_model.sh")
writeLines(c(script$init, script$manifest), "~/n2kanalysis/abv.sh")
