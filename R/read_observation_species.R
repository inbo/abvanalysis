#' Read the observations of a given species
#' @export
#' @importFrom RODBC sqlQuery odbcClose
#' @param species.id The external id of the species
#' @inheritParams n2khelper::odbc_connect
read_observation_species <- function(species.id, develop = TRUE){
  channel <- connect_rawdata(develop = develop)
  sql <- paste0("
    SELECT
      visit.ObservationID AS ObservationID,
      visit.SubLocationName AS SubLocationName,
      observed.Count AS Count
    FROM
        (
          SELECT 
            WRNG_ID AS ObservationID, 
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
        ) AS visit
      LEFT JOIN
        (
          SELECT
            WRME_WRNG_ID AS ObservationID, 
            Left([WRME_PNT],3) AS SubLocationName, 
            Sum(WRME_ANT) AS Count
          FROM
            tblWaarnemingMeting
          WHERE 
            NOT Left([WRME_PNT],3) = '0' AND
            WRME_SPEC_CDE = ", species.id, "
          GROUP BY
            WRME_WRNG_ID,
            Left([WRME_PNT],3)
        ) AS observed
      ON
        visit.ObservationID = observed.ObservationID AND
        visit.SubLocationName = observed.SubLocationName
  ")
  observed.count <- sqlQuery(channel = channel, query = sql, stringsAsFactors = FALSE)
  odbcClose(channel)
  observed.count$Count[is.na(observed.count$Count)] <- 0
  return(observed.count)
}
