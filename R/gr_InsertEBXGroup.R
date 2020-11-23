#' @title Insert rows in EBX5 Group
#'
#' @description This function inserts data rows into a group stored in EBX5.
#' Requires that the EBX5 connection was configured using \code{\link{SetupEBXConnection}}.
#'
#' @inheritParams ReadEBXGroup
#' @param data a \code{\link[base]{data.frame}} defining the rows to insert.
#' All primary keys (usually the Identifier) and all mndatory attributes must be present.
#'
#' @seealso \code{\link{GetEBXGroups}}
#'
#' @details Note that the new rows must have the same columns name os the table that will be appended.
#'
#' @return boolean
#'
#' @importFrom faoebx5 GetEBXConnection
#'
#' @export
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
InsertEBXGroup <- function(data, sdmx_group_name) {

  # action to EBX5
  return (request_update(
    .data       = data,
    .sdmx_name  = sdmx_group_name,
    .verb       = 'insert',
    .isCodeList = FALSE))
}
