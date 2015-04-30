library(abvanalysis)
result.channel <- n2khelper::connect_result()
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
tmp <- prepare_analysis(
  raw.connection = raw.connection,
  analysis.path = "~/analysis",
  scheme.id = scheme.id
)
