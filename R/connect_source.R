#' Connect to the source data
#' @param result.channel An open RODBC connection to the results database
#' @importFrom n2khelper odbc_connect
#' @export
connect_source <- function(result.channel){
  odbc_connect(data.source.name = "Source data ABV", channel = result.channel)
}


#' Make a git connection to the attribute
#' @export
#' @inheritParams connect_source
#' @inheritParams n2khelper::git_connection
#' @importFrom n2khelper git_connect
connect_attribute <- function(result.channel, username, password){
  git_connect(
    data.source.name = "Attributes ABV", 
    channel = result.channel,
    username = username,
    password = password
  )
}

#' Make a git connection to the raw data
#' @export
#' @inheritParams connect_source
#' @inheritParams n2khelper::git_connection
#' @importFrom n2khelper git_connect
connect_raw <- function(result.channel, username, password){
  git_connect(
    data.source.name = "Raw data ABV", 
    channel = result.channel,
    username = username,
    password = password
  )
}
