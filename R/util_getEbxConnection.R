#' @title Get the EBX5 connection details; internal
#'
#' @description Loads the connection from the environment (ebx5_env)
#' Requires that the EBX5 connection was configured using \code{\link[faoebx5]{SetupEBXConnection}}.
#'
#' @seealso \code{\link[faoebx5]{SetupEBXConnection}}
#'
#' @importFrom faoebx5 GetEBXConnection
#'
#' @export
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
getEbxConnection <- function() {
  connection <- get("ebx5.connection", envir = ebx5_env)
  if(is.null(connection) || all(is.na(connection))) {
    connection <- GetEBXConnection()
    if (is.null(connection)) {
      stop('Cannot load connection; run faoebx5::SetupEBXConnection()')
    }

    assign("ebx5.connection", connection, envir = ebx5_env)
  }
  return (connection)
}
