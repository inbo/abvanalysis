#' Read the list of species
#' @return A list with two components: \code{Species} holds the names of all species, \code{Speciesgroup} lists the species per user defined group.
#' @export
#' @importFrom RODBC sqlQuery odbcClose
#' @examples
#' read_specieslist()
read_specieslist <- function(){
  channel <- connect_abv()
  sql <- "
    SELECT
      SPEC_CDE AS SpeciesID,
      SPEC_NAM_WET As Species,
      SPEC_NAM_NED AS SpeciesNL,
      SPEC_NAM_ENG AS SpeciesEN,
      SPEC_NAM_FRA AS SpeciesFR
    FROM
      tblSoort
    ORDER BY
      SPEC_CDE
  "
  species <- sqlQuery(channel = channel, query = sql, stringsAsFactors = FALSE)
  sql <- "
    SELECT
      Beschrijving AS SpeciesGroup,
      SoortCode AS SpeciesID
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
  return(
    list(
      Species = species,
      Speciesgroup = speciesgroup
    )
  )
}
