#' @title read a comple group hierarchy
#'
#' @param sdmx_level1
#' @param sdmx_level2
#'
#' @seealso \code{\link{ReadEBXGroup}}.
#'
#' @description This function will resolve all possible groupings, accross multiple levels,
#'  combining these levels into the final result. In case there are multiple group paths
#'  between the levels requested, all paths are resolved and combined.
#'
#'
#' @details The group names are using the sdmx name, resolved via metadata
#'
#' @return grouping in the format int=parent list=(child1,child2,child3...)
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
GetEBXHierarchy <- function(sdmx_level1, sdmx_level2) {


}


