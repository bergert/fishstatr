#' @title Get EBX Groups
#'
#' @description This function returns the index of groups
#' defined in the folder 'Metadata', table name 'EBXGroup'.
#' Requires that the EBX5 connection was configured using \code{\link{SetupEBXConnection}}.
#'
#' @param connection is optional
#'
#' @seealso \code{\link{ReadEBXGroup}} \code{\link{GetEBXCodeLists}}
#'
#' @return Returns an object of the class \code{\link[data.table]{data.table}}
#'
#' @import data.table
#' @importFrom faoebx5 GetEBXConnection EBXRead
#'
#' @examples
#' \dontrun{
#' library(faoebx5)
#' library(fishstatr)
#' GetEBXGroups()
#' }
#' @export
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
GetEBXGroups <- function(connection = NA) {

  #-- EBX5: connection ----
  if (missing(connection)) {
    connection <- getEbxConnection()
  }

  #-- check if the ebx5.gr_data is loaded already
  ebx5.gr_data <- get("ebx5.gr_data", envir = ebx5_env)
  if(is.null(ebx5.gr_data) || is.na(ebx5.gr_data)) {
    ebx5.gr_data <- EBXRead(
      branch = connection$meta_branch,
      instance = connection$meta_instance,
      folder='Metadata',
      table='EBXGroup',
      connection=connection)

    if (is.null(ebx5.gr_data)) {
      stop('Cannot load [ebx5.gr_data]')
    }
    assign("ebx5.gr_data", ebx5.gr_data, envir = ebx5_env)
  }

  #-- read metadata::EBXCodelist ----
  return (ebx5.gr_data)
}
