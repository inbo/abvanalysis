library(abvanalysis)
raw.connection <- n2khelper::git_connection(
  repo.path = "~/n2k/ssh/rawdata",
  local.path = "abv",
  key = "~/.ssh/id_rsa_n2kreadonly"
)
prepare_analysis(
  raw.connection = raw.connection,
  analysis.path = "~/analysis/abv"
)

library(optimx)
library(n2kanalysis)
fit_every_model(
  path = "~/analysis/abv", 
  status = c("new", "waiting"), 
  n.cluster = parallel::detectCores() - 1
)
fit_every_model(
  path = "~/analysis/abv", 
  status = c("new", "waiting"), 
  n.cluster = 1
)
status("~/analysis/abv")

library(n2kanalysis)
result <- get_result(
  x = "~/analysis/abv", 
  datasource.id = result_datasource_id(result.channel = result.channel),
  keep.fingerprint = FALSE, 
  n.cluster = parallel::detectCores() - 1
)
save(result, file = "~/analysis/output/abv.rda")
