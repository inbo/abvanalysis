#' Return the set of all visited locations
#' @export
#' @inheritParams prepare_dataset
#' @importFrom RODBC sqlQuery
#' @importFrom lubridate floor_date year
#' @importFrom n2khelper cut_date
read_observation <- function(source.channel, result.channel){
  latest.year <- as.integer(format(Sys.time(), "%Y")) - 1
  if (Sys.time() < as.POSIXct(format(Sys.time(), "%Y-03-01"))) {
    latest.year <- latest.year - 1
  }

  sql <- paste0("
    SELECT
      WRPT_ID AS ObservationID,
      WRNG_DTE_BGN AS Timestamp,
      WRNG_UTM1_CDE AS ExternalCode,
      WRPT_PTN AS SubExternalCode,
      WRNG_USR_CRE AS Username
    FROM
        tblWaarneming
      INNER JOIN
        tblWaarnemingPunt
      ON
        tblWaarneming.WRNG_ID = tblWaarnemingPunt.WRPT_WRNG_ID
    WHERE
      WRNG_WRNG_ID IS NULL AND
      WRNG_DTE_BGN < '", latest.year, "-07-16' AND
      WRPT_BZT = 1 AND
      WRNG_UCWT_CDE IN ('ABV', 'LSABV', 'IJK') AND
      WRNG_WGST_CDE <> 'NV'
    ORDER BY
      WRNG_UTM1_CDE, WRNG_DTE_BGN
  ")
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
