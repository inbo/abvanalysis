# this script imports the raw data from the ODBC data source and creates the
# analysis files

# setting below assumes ssh authentication
# set correct username and password for https authentication
username <- character(0)
password <- character(0)

library(abvanalysis)

result.channel <- n2khelper::connect_result(
  username = Sys.getenv("N2KRESULT_USERNAME"),
  password = Sys.getenv("N2KRESULT_PASSWORD")
)
raw.connection <- connect_raw(
  result.channel = result.channel,
  username = username,
  password = password
)
scheme.id <- scheme_id(result.channel)
prepare_dataset(
  result.channel = result.channel,
  source.channel = connect_source(result.channel),
  attribute.connection = connect_attribute(
    result.channel = result.channel,
    username = username,
    password = password
  ),
  raw.connection = raw.connection,
  scheme.id = scheme.id
)
