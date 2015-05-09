library(abvanalysis)
result.channel <- n2khelper::connect_result()
raw.connection <- connect_raw(result.channel = result.channel)
scheme.id <- scheme_id(result.channel)
prepare_dataset(
  result.channel = result.channel, 
  source.channel = connect_source(result.channel), 
  attribute.connection = connect_attribute(result.channel = result.channel), 
  raw.connection = raw.connection, 
  scheme.id = scheme.id
)
prepare_analysis(
  raw.connection = raw.connection,
  analysis.path = "~/analysis"
)
