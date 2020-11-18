#' @title Remove rows in EBX5 Group
#'
#' @description This function removes data rows from a group stored in EBX5.
#' Requires that the EBX5 connection was configured using \code{\link{SetupEBXConnection}}.
#'
#' @inheritParams InsertEBXGroup
#' @param data a \code{\link[base]{data.frame}} that will be removed.
#' data must specify the unique primary key(s); which identify the rows to be removed.
#'
#' @return boolean
#'
#' @importFrom faoebx5 GetEBXConnection
#'
#' @export
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
RemoveEBXGroup <- function(data, sdmx_group_name) {

  # action to EBX5
  return (request_update(
    .data       = data,
    .sdmx_name  = sdmx_group_name,
    .verb       = 'delete',
    .isCodeList = FALSE))

}
