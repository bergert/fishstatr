#' @title Insert rows in EBX5 Code List
#'
#' @description This function will insert
#' data rows into a code list stored in EBX5 through R.
#'
#' @param data a \code{\link[base]{data.frame}} containing all columns to be be inserted
#'
#' @inheritParams ReadEBXCodeList
#'
#' @return boolean
#'
#' @importFrom faoebx5 GetEBXConnection EBXInsert
#'
#' @details The data columns provided must follow the code-list which is being updated.
#' All keys of the code-list must be given, as well as any fields which are mandatory.
#' Using the SOAP-API the column name is not the label visible in EBX, but the field name.
#' Requires that the EBX5 connection was configured using \code{\link{SetupEBXConnection}}.
#'
#' @examples
#'
#' \dontrun{
#' cl_new <- data.frame(Identifier = c(999, 888), Name_En=c('Test1','Test2'))
#' InsertEBXCodeList(data = cl_new, sdmx_codelist_name = 'CL_FI_COUNTRY_ITEM')
#' }
#'
#' @export
#'
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
InsertEBXCodeList <- function(data, sdmx_codelist_name) {

  # action to EBX5
  return (request_update(
    .data       = data,
    .sdmx_name  = sdmx_codelist_name,
    .verb       = 'update',
    .isCodeList = TRUE))
}
