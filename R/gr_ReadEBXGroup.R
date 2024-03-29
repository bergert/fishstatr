#' @title Read EBX5 Group
#'
#' @description This function reads group data from EBX5 to R.
#'
#' @param sdmx_group_name group name, in SDMX style.
#' Available code lists are shown by function \code{\link{GetEBXGroups}} in the field "Acronym".
#' The actual group location in EBX5 (branch, instance, code-list-name) are resolved
#' using the Metadata structure.
#'
#' @seealso \code{\link{GetEBXGroups}}.
#'
#' @return Return an object of the class \code{\link[data.table]{data.table}}.
#'
#' @details The first column called 'Group' defines the parent codes. The second
#' column called 'Member' defines the child identifier. Additional colunmns may
#' be used to define the grouping like start/end year.
#' The correct names and order must be used when defining the EBX5 data model.
#'
#' @import data.table
#' @importFrom faoebx5 GetEBXConnection EBXRead
#'
#' @examples
#' \dontrun{
#'ReadEBXGroup(sdmx_group_name = 'HCL_FI_COMMODITY_FAOL1_FAOL2')
#' }
#'
#' @export
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
ReadEBXGroup <- function(sdmx_group_name) {

  result <- request_read(.sdmx_name=sdmx_group_name, .isCodeList = FALSE)
  colnames(result) <- c('Group', 'Member')
  return (result)
}
