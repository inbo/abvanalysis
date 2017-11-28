#' Read the list of species
#' @return A list with two components: \code{Species} holds the names of all species, \code{Speciesgroup} lists the species per user defined group.
#' @export
#' @inheritParams prepare_dataset
#' @importFrom RODBC sqlQuery
#' @examples
#' \dontrun{
#'  result.channel <- n2khelper::connect_result()
#'  source.channel <- connect_source(result.channel = result.channel)
#'  read_specieslist(
#'    source.channel = source.channel,
#'     result.channel = result.channel
#'  )
#' }
read_specieslist <- function(source.channel, result.channel){
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
  species <- sqlQuery(channel = source.channel, query = sql, stringsAsFactors = FALSE)
  species$TableName <- "tblSoort"
  species$PrimaryKey <- "SPEC_CDE"
  species$DatasourceID <- datasource_id(result.channel = result.channel)

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
  speciesgroup <- sqlQuery(channel = source.channel, query = sql, stringsAsFactors = FALSE)

  return(
    list(
      Species = species,
      Speciesgroup = speciesgroup
    )
  )
}
