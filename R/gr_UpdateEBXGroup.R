#' @title Update rows in EBX5 Group
#'
#' @description This function updates data rows into a group stored in EBX5.
#'
#' @inheritParams InsertEBXGroup
#' @param data a \code{\link[base]{data.frame}} containing all columns to be be updated
#' All primary keys (usually the Identifier) and all mndatory attributes must be present.
#'
#' @return boolean
#'
#' @importFrom faoebx5 GetEBXConnection
#'
#' @details Note that the udpated rows must have the same columns name os the table that will be updated.
#'
#' @examples
#' \dontrun{
#' UpdateEBXGroup(data = gr_update,
#' gr_name = 'EBXGroup',
#' folder = 'Metadata')
#' }
#'
#' @export
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
UpdateEBXGroup <- function(data, sdmx_group_name) {

  # action to EBX5
  return (request_update(
    .data       = data,
    .sdmx_name  = sdmx_group_name,
    .verb       = 'update',
    .isCodeList = FALSE))
}
