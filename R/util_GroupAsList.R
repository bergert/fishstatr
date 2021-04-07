utils::globalVariables(c("."))
#' @title Convert an grouping from EBX5 to a list; internal
#'
#' @param grouping a grouping in the format (int=parent, int=child), as returned by EBX5
#'
#' @seealso \code{\link{ReadEBXGroup}}.
#'
#' @description This function will merge the child ID's belonging to a single parent ID
#'
#' @details Using \code{\link{dplyr}} to reformat a grouping in a way to use groups in applications.
#' The funcion was re-written in order not to use tibble::tibble, or dplyr::group_map as the shiny server server
#' does not have compatible version installed.
#'
#' @return grouping in the format int=parent list=(child1,child2, child3...)
#'
#' @importFrom data.table data.table
#'
#' @export
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
GroupAsList <- function(grouping) {

  dt1 <- data.table(grouping[,1:2])
  colnames(dt1) <- c('group','member')

  # third attempt: solved using data.table
  dt2 <- dt1[, list(list(member)), by=group]
  setattr(dt2, ".internal.selfref", NULL)
  colnames(dt2) <- c('group','children')
  return(as.data.frame(dt2))
}

# second attempt: cannot update dplyr on shiny server
#GroupAsList_original <- function(grouping) {
#
#  #-- first: group using the parent column
#  df1 <- grouping %>% group_by(grouping[,1])
#
#  #-- second: summarise the groups to a list
#  df2 <- df1 %>% summarise_all(funs(summary = list(.)))
#
#  #-- remove the original group column
#  colnames(df2) <- c('grouping','group','children')
#  df2$group <- NULL
#
#  return(as.data.frame(df2 %>% ungroup()))
#}

# first attempt: cannot update dplyr on shiny server
#GroupAsList_original <- function(grouping) {
#
#  #-- first: group using the parent column
#  df1 <- grouping %>% group_by(grouping[,1])
#
#  #-- second: convert all children to a list
#  df2 <- df1 %>% group_map(~convert2List(.x[2]))
#
#  return (df2 %>% ungroup())
#}
#
#convert2List <- function(values) {
#  return(tibble::tibble(children = as.list(values)))
#}
