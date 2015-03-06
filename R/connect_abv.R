#' Opens an ODBC connection to the 'Broedvogel' database
#' @export
#' @importFrom RODBC odbcDriverConnect
connect_abv <- function(){
  odbcDriverConnect("Driver=SQL Server;Server=INBOSQL03\\PRD;Database=Broedvogel;Trusted_Connection=Yes;")
}
