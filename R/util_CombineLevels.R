#' @title Convert an grouping from EBX5 to a list
#'
#' @param grouping a grouping in the format (int=parent, int=child), as returned by EBX5
#'
#' @seealso \code{\link{ReadEBXGroup}}.
#'
#' @description This function will merge the child ID's belonging to a single parent ID
#'
#' @details Required, to use EBX5 groups in applications, and also to resolve relationships of
#'  multi-level hierarchies.
#'
#' @return grouping in the format int=parent list=(child1,child2, child3...)
#'
#' @importFrom dplyr group_by group_map
#' @importFrom data.table data.table
#' @export
#'
#' @examples
#' \dontrun{
#' df <- data.frame(group=c(100,100, 200, 200), children=c(10,11,21,22))
#' str(GroupAsList(df))
#'  $ grouping[, 1]: num  100 200
#'  $ children     :List of 2
#'    ..$ : num  10 11
#'    ..$ : num  21 22
#'
#' df <- ReadEBXGroup('HCL_FI_COUNTRY_CONTINENT_GEOREGION')
#' GroupAsList(df)
#' }
#'
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
GroupAsList <- function(grouping) {

  #-- first: group using the parent column
  df1 <- grouping %>% group_by(grouping[,1])

  #-- second: convert all children to a list
  df2 <- df1 %>% group_map(~convert2List(.x[2]))

  return (df2 %>% ungroup)
}

convert2List <- function(values) {
  return(tibble::tibble(children = as.list(values)))
}
