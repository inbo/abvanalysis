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
#' @param username The username for a https connection. Leave default for a ssh connection.
#' @param password The password for the connection.
#' @importFrom n2khelper git_connect
connect_attribute <- function(result.channel, username = character(0), password = character(0)){
  git_connect(
    data.source.name = "Attributes ABV", 
    channel = result.channel,
    type = ifelse(length(username) == 0, "ssh", "https"),
    username = username,
    password = password
  )
}

#' Make a git connection to the raw data
#' @export
#' @inheritParams connect_source
#' @inheritParams connect_attribute
#' @importFrom n2khelper git_connect
connect_raw <- function(result.channel, username = character(0), password = character(0)){
  git_connect(
    data.source.name = "Raw data ABV", 
    channel = result.channel,
    type = ifelse(length(username) == 0, "ssh", "https"),
    username = username,
    password = password
  )
}
