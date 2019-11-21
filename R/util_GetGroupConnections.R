#' @title Get the connections between two hierarchies
#'
#' @param ebx_parent_group parent, using the EBX5 codelist Identifier
#' @param ebx_child_group child, using the EBX5 codelist Identifier
#'
#' @seealso \code{\link{ReadEBXGroup}}.
#'
#' @description Using the EBX5 metadata, calculates the grouping from parent to child group,
#' based on codelist Identifier; as defined in 'EBXGroup.to' and 'EBXGroup.from'.
#'
#' @details The solution may invaolve several steps (a list of group ID's), and may involve
#' multiple solutions, which have to be merged for the final result:
#' MAJOR(306)->ODER(307)->FAMILY(302)->SPECIES(301)
#' MAJOR(306)->ODER(307)->SPECIES(301)
#' data.table(1=list(306,307,302,301), 2=list(306,307,301))
#' In the simplest case, if a matching group exists, list(ebx_parent_group, ebx_child_group) is returned
#'
#' @return grouping in the format int=parent list=(child1,child2, child3...)
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
GetGroupConnections <- function(ebx_parent_group, ebx_child_group) {



}


