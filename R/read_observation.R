#' Return the set of all visited locations
#' @export
#' @inheritParams n2khelper::odbc_connect
#' @importFrom RODBC sqlQuery odbcClose
#' @importFrom lubridate floor_date year
#' @importFrom n2khelper cut_date
read_observation <- function(develop = TRUE){
  channel <- connect_rawdata(develop = develop)
  sql <- "
    SELECT 
      WRNG_ID AS ObservationID, 
      WRNG_DTE_BGN AS Timestamp,
      WRNG_UTM1_CDE AS LocationID, 
      WRPT_PTN AS SubLocationName
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
  observation <- sqlQuery(channel = channel, query = sql, stringsAsFactors = FALSE)
  odbcClose(channel)
  observation$Date <- floor_date(observation$Timestamp, unit = "day")
  observation$Period <- cut_date(
    x = observation$Date, dm = c("1-3", "16-4", "1-6", "16-7"), include.last = FALSE
  )
  levels(observation$Period) <- 1:3
  observation$Year <- year(observation$Date)
  observation <- observation[!is.na(observation$Period), ]
  observation <- observation[
    order(observation$ObservationID, observation$SubLocationName),
    c("ObservationID", "LocationID", "SubLocationName", "Year", "Period")
  ]
  return(observation)  
}
