#' Return the set of all visited locations
#' @export
#' @inheritParams prepare_dataset
#' @importFrom RODBC sqlQuery
#' @importFrom lubridate floor_date year
#' @importFrom n2khelper cut_date
read_observation <- function(source.channel, result.channel){
  check_dbtable_variable(
    table = "tblWaarneming", 
    variable = c("WRNG_ID", "WRNG_DTE_BGN", "WRNG_UTM1_CDE"), 
    channel = source.channel
  )
  check_dbtable_variable(
    table = "tblWaarnemingPunt", 
    variable = c("WRPT_PTN", "WRPT_WRNG_ID"), 
    channel = source.channel
  )
  
  sql <- "
    SELECT 
      WRNG_ID AS ObservationID, 
      WRNG_DTE_BGN AS Timestamp,
      WRNG_UTM1_CDE AS ExternalCode, 
      WRPT_PTN AS SubExternalCode
    FROM 
        tblWaarneming 
      INNER JOIN 
        tblWaarnemingPunt 
      ON 
        tblWaarneming.WRNG_ID = tblWaarnemingPunt.WRPT_WRNG_ID
    WHERE 
      WRPT_BZT = 'True' AND 
      WRNG_UCWT_CDE = 'ABV' AND 
      NOT (WRNG_WGST_CDE = 'NV')
  "
  observation <- sqlQuery(channel = source.channel, query = sql, stringsAsFactors = FALSE)
  
  observation$Date <- floor_date(observation$Timestamp, unit = "day")
  observation$Period <- cut_date(
    x = observation$Date, 
    dm = c("1-3", "16-4", "1-6", "16-7"), 
    include.last = FALSE
  )
  levels(observation$Period) <- 1:3
  observation$Year <- year(observation$Date)
  observation <- observation[!is.na(observation$Period), ]
  observation$DatasourceID <- datasource_id(result.channel = result.channel)
  observation <- observation[
    order(observation$ObservationID, observation$SubExternalCode),
    c("DatasourceID", "ObservationID", "ExternalCode", "SubExternalCode", "Year", "Period")
  ]
  return(observation)  
}
