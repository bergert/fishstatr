#' @title Remove rows in EBX5 Code List
#'
#' @description This function deletes data rows from a code list stored in EBX5.
#' Requires that the EBX5 connection was configured using \code{\link{SetupEBXConnection}}.
#'
#' @param data a \code{\link[base]{data.frame}} that will be removed.
#' data must specify the unique primary key(s); which identify the rows to be removed.
#' @param sdmx_codelist_name code list name, in SDMX style. Please, see
#' available code lists are shown by function \code{\link{GetEBXCodeLists}} in the field "Acronym".
#' The actual codelist location in EBX5 (branch, instance, code-list-name) are resolved
#' using the metadata structure.
#'
#' @inheritParams InsertEBXCodeList
#'
#' @return boolean
#'
#' @importFrom faoebx5 GetEBXConnection
#'
#' @details For a delete operation, data must specify all keys of the code-list to uniqely idetify the rows.
#' Using the SOAP-API the column name is not the label visible in EBX, but the field name.
#'
#' @examples
#' \dontrun{
#' cl_remove <- data.frame(
#' Identifier = c(999, 888))
#'
#' UpdateEBXCodeList(data     = cl_remove,
#'                   sdmx_code_list_name  = 'EBXCodelist')
#' }
#'
#' @export
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
RemoveEBXCodeList <- function(data, sdmx_codelist_name) {

  # action to EBX5
  return (request_update(
    .data       = data,
    .sdmx_name  = sdmx_codelist_name,
    .verb       = 'delete',
    .isCodeList = TRUE))
}
