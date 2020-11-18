#' @title Get en EBX5 codelist using the ID; internal
#'
#' @description This function returns the EBX5 codelist for a given EBX5 codelist ID;
#' codelists are normally referred by Acronymn, but fishstat uses the ID
#' This function requires that EBX5 metadata was loded already
#'
#' @param ebxCodeList_ID EBX5 codelist ID
#'
#' @return Returns an object of the class \code{\link[data.table]{data.table}}
#'
#' @import data.table
#'
#' @examples
#'
#' \dontrun{
#' library(faoebx5)
#' library(fishstatr)
#' ReadMetadata()
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
GetCodelistByID <- function(ebxCodeList_ID) {

  ebx5.cl_data <- getEbxConnection()
  if (nrow(ebx5.cl_data[ebx5.cl_data$Identifier == ebxCodeList_ID,]) == 0) {
    stop('ebxCodeList_ID <',ebxCodeList_ID,'> is not defined in EBXCodelist metadata')
  }

  ebxCodelistName <- ebx5.cl_data[ebx5.cl_data$Identifier == ebxCodeList_ID,'Acronym']
  return(ReadEBXCodeList(ebxCodelistName))
}
