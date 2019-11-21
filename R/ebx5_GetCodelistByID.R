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
#' GetCodelistByID(200)
#' }
#'
#' @export
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
GetCodelistByID <- function(ebxCodeListID) {

  if (!exists('ebx5.cl_data')) {
    stop('EBX5 data strcuture is not loaded')
  }

  df <- ebx5.cl_data[ebx5.cl_data$Identifier == ebxCodeListID,'Acronym']
  df <- df %>% droplevels()

  ebxCodelistName <- as.character(df$Acronym)
  return(ReadEBXCodeList(ebxCodelistName))
}
