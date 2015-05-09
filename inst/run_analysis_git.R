library(abvanalysis)
raw.connection <- n2khelper::git_connection(
  repo.path = "~/rawdata",
  local.path = "abv",
  key = "~/.ssh/id_rsa_n2kreadonly"
)
prepare_analysis(
  raw.connection = raw.connection,
  analysis.path = "~/analysis"
)
