protocol <- "ssh"
library(abvanalysis)
if (protocol == "ssh") {
  raw.connection <- n2khelper::git_connection(
    repo.path = "~/n2k/ssh/rawdata", #nolint
    local.path = "abv",
    commit.user = "abvanalysis",
    commit.email = "bmk@inbo.be"
  )
} else {
  raw.connection <- n2khelper::git_connection(
    repo.path = "~/n2k/https/rawdata", #nolint
    local.path = "abv",
    username = username,
    password = password,
    commit.user = "abvanalysis",
    commit.email = "bmk@inbo.be"
  )
}
prepare_analysis(
  raw.connection = raw.connection,
  analysis.path = "~/analysis/abv" #nolint
)

library(n2kanalysis)
fit_every_model(
  path = "~/analysis/abv", #nolint
  status = c("new", "waiting"),
  n.cluster = 1
)
fit_every_model(
  path = "~/analysis/abv", #nolint
  status = c("new", "waiting"),
  n.cluster = 1
)
current.status <- status("~/analysis/abv") #nolint
table(current.status$Status)

library(abvanalysis)
library(n2kanalysis)
result.channel <- n2khelper::connect_result(
  username = Sys.getenv("N2KRESULT_USERNAME"),
  password = Sys.getenv("N2KRESULT_PASSWORD")
)
datasource.id <- abvanalysis::result_datasource_id(
  result.channel = result.channel
)
result <- get_result(
  x = "~/analysis/abv", #nolint
  datasource.id = datasource.id,
  keep.fingerprint = FALSE,
  n.cluster = parallel::detectCores() - 1
)
save(result, file = "~/analysis/output/abv.rda") #nolint
