#' @title Merges hierachical group levels
#'
#' @param level1_group the higher level group
#' @param level2_group the higher level group
#'
#' @seealso \code{\link{ReadEBXGroup}}.
#'
#' @description Converts the list of Level1 child Identifiers,
#' into Level2 child Identifiers
#'
#' @details A group is a data table, column1=parent, column2=list(children)
#'
#' @return grouping in the format data.table(parent=level1, child=list(level2))
#'
#' @importFrom dplyr group_by group_map
#' @importFrom data.table data.table
#' @export
#'
#' @examples
#' \dontrun{
#' }
#'
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
MergeRalation <- function(level1_group, level2_group) {


}


