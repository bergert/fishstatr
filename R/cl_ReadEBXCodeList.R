#' @title Read EBX5 Code List
#'
#' @description This function reads a code list from EBX5 to R.
#' Requires that the EBX5 connection was configured using \code{\link{SetupEBXConnection}}.
#'
#' @param sdmx_codelist_name code list name, in SDMX style.
#' Available code lists are shown by function \code{\link{GetEBXCodeLists}} in the field "Acronym".
#' The actual codelist location in EBX5 (branch, instance, code-list-name) are resolved
#' using the metadata structure.
#'
#' @seealso \code{\link{GetEBXCodeLists}}.
#'
#' @return Return an object of the class \code{\link[data.table]{data.table}}.
#'
#' @importFrom faoebx5 GetEBXConnection EBXRead
#' @import data.table
#'
#' @examples
#' \dontrun{
#' ReadEBXCodeList(sdmx_codelist_name = "CL_FI_COMMODITY_ISSCFC")
#' }
#'
#' @export
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
#'
ReadEBXCodeList <- function(sdmx_codelist_name) {

  request_read(.sdmx_name=sdmx_codelist_name, .isCodeList = TRUE)
}
