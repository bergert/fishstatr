#' @title Update rows in EBX5 Code List
#'
#' @description This function updates data rows of a code list stored in EBX5.
#' Requires that the EBX5 connection was configured using \code{\link{SetupEBXConnection}}.
#'
#' @param data a \code{\link[base]{data.frame}} containing all columns to be be updated
#' @param sdmx_codelist_name code list name, in SDMX style.
#' Available code lists are shown by function \code{\link{GetEBXCodeLists}} in the field "Acronym".
#' The actual codelist location in EBX5 (branch, instance, code-list-name) are resolved
#' using the metadata structure.
#'
#' @return boolean
#'
#' @details Note that the udpated rows must have the same columns name os the table that will be updated.
#'
#' @examples
#' \dontrun{
#' cl_new <- data.frame(
#' Identifier = c(999, 888),
#' Acronym = 'TEST_ACRONYM',
#' Folder = 'TESTFOLDER',
#' Name = 'TEST_NAME',
#' Branch = 'Fishery',
#' Instance = 'Fishery')
#'
#' UpdateEBXCodeList(data     = cl_new,
#'                   sdmx_codelist_name  = 'EBXCodelist')
#' }
#'
#' @export
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
UpdateEBXCodeList <- function(data, sdmx_codelist_name) {

  # action to EBX5
  return (request_update(
    .data       = data,
    .sdmx_name  = sdmx_codelist_name,
    .verb       = 'update',
    .isCodeList = TRUE))
}
