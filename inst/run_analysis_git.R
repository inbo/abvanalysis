library(abvanalysis)
raw.connection <- n2khelper::git_connection(
  repo.path = "~/n2k/ssh/rawdata", #nolint
  local.path = "abv",
  key = "~/.ssh/id_rsa_n2kreadonly" #nolint
)
prepare_analysis(
  raw.connection = raw.connection,
  analysis.path = "~/analysis/abv" #nolint
)

library(optimx)
library(n2kanalysis)
fit_every_model(
  path = "~/analysis/abv", #nolint
  status = c("new", "waiting"),
  n.cluster = parallel::detectCores() - 1
)
fit_every_model(
  path = "~/analysis/abv", #nolint
  status = c("new", "waiting"),
  n.cluster = 1
)
status("~/analysis/abv") #nolint

library(n2kanalysis)
result.channel <- n2khelper::connect_result()
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
