#' @title Get the index of EBX5 attributes
#'
#' @description This function returns the list of EBX5 attributes
#' defined in the folder 'Metadata', table name 'EBXAttribute'.
#' Requires that the EBX5 connection was configured using \code{\link{SetupEBXConnection}}.
#'
#' @param connection is optional
#'
#' @seealso \code{\link{ReadEBXCodeList}} \code{\link{GetEBXGroups}}
#'
#' @return Returns an object of the class \code{\link[data.table]{data.table}}
#'
#' @import data.table
#' @importFrom faoebx5 GetEBXConnection EBXRead
#'
#' @examples
#'
#' \dontrun{
#' library(faoebx5)
#' library(fishstatr)
#' GetEBXCodeLists()
#' }
#'
#' @export
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
GetEBXAttributes <- function(connection = NA) {

  #-- EBX5: connection ----
  if (missing(connection)) {
    connection <- getEbxConnection()
  }

  #-- check if the ebx5.attrib_data is loaded already
  ebx5.attrib_data <- get("ebx5.attrib_data", envir = ebx5_env)
  if(is.null(ebx5.attrib_data) || all(is.na(ebx5.attrib_data))) {
    ebx5.attrib_data <- EBXRead(
      branch = connection$meta_branch,
      instance = connection$meta_instance,
      folder='Metadata',
      table='EBXAttribute',
      connection=connection)

    if (is.null(ebx5.attrib_data)) {
      stop('Cannot load [ebx5.attrib_data]')
    }
    assign("ebx5.attrib_data", ebx5.attrib_data, envir = ebx5_env)
  }

  return (ebx5.attrib_data)
}

