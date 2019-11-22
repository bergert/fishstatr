#' @title Get en EBX5 codelist using the ID
#'
#' @description This function returns the EBX5 codelist for a given EBX5 codelist ID;
#' codelists are normally referred by Acronymn, but fishstat uses the ID
#' This function requires that EBX5 metadata was loded already
#'
#' @param ebxCodeListID EBX5 codelist ID
#'
#'
#' @return Returns an object of the class \code{\link[data.table]{data.table}}
#'
#' @importFrom faoebx5 ReadEBXCodeList
#' @import data.table
#'
#' @examples
#'
#' \dontrun{
#' country_codelist_ID <- 200 # the ID for CL_FI_COUNTRY_ITEM
#' cl <- GetCodelistByID(country_codelist_ID)
#'
#' # same as
#' cl <- ReadEBXCodeList('CL_FI_COUNTRY_ITEM')
#' }
#'
#' @export
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
GetCodelistByID <- function(ebxCodeListID) {

  if (!exists('ebx5.cl_data')) {
    stop('EBX5 data strcuture is not loaded')
  }

  if (nrow(ebx5.cl_data[ebx5.cl_data$Identifier == ebxCodeListID,]) == 0) {
    stop('ebxCodeListID <',ebxCodeListID,'> is not defined in EBXCodelist metadata')
  }

  ebxCodelistName <- ebx5.cl_data[ebx5.cl_data$Identifier == ebxCodeListID,'Acronym']
  return(ReadEBXCodeList(ebxCodelistName))
}
