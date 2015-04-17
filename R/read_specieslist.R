#' Read the list of species
#' @return A list with two components: \code{Species} holds the names of all species, \code{Speciesgroup} lists the species per user defined group.
#' @export
#' @inheritParams n2khelper::odbc_connect
#' @importFrom RODBC sqlQuery odbcClose
#' @examples
#' read_specieslist()
read_specieslist <- function(develop = TRUE){
  channel <- connect_source(develop = develop)
  sql <- "
    SELECT
      SPEC_CDE AS ExternalCode,
      SPEC_NAM_WET As ScientificName,
      SPEC_NAM_NED AS DutchName,
      SPEC_NAM_ENG AS EnglishName,
      SPEC_NAM_FRA AS FrenchName
    FROM
      tblSoort
    ORDER BY
      SPEC_CDE
  "
  species <- sqlQuery(channel = channel, query = sql, stringsAsFactors = FALSE)
  sql <- "
    SELECT
      Beschrijving AS Description,
      SoortCode AS ExternalCode
    FROM
        (
          SoortIndicatorType
        INNER JOIN
          IndicatorType
        ON
          SoortIndicatorType.IndicatorTypeCode = IndicatorType.Code
        )
      INNER JOIN
        tblSoort
      ON
        SoortIndicatorType.SoortCode = tblSoort.SPEC_CDE
    ORDER BY
      IndicatorTypeCode,
      SoortCode
  "
  speciesgroup <- sqlQuery(channel = channel, query = sql, stringsAsFactors = FALSE)
  odbcClose(channel)
  speciesgroup$SchemeID <- scheme_id(develop = develop)
  return(
    list(
      Species = species,
      Speciesgroup = speciesgroup
    )
  )
}
