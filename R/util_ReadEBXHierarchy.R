#' @title Read a comple group hierarchy
#'
#' @param sdmx_level1 ID upper level of a hierarchy, as returned by GetDimensionGroups$EBXCodelist
#' @param sdmx_level2 lower level of a hierarchy, as returned by GetDimensions$EBXCodelist
#'
#' @seealso \code{\link{GetDimensionGroups}} and \code{\link{GetDatasetDimensions}}.
#'
#' @description This function will resolve all possible groupings, accross multiple levels,
#'  combining these levels into the final result. In case there are multiple group paths
#'  between the levels requested, all paths are resolved and combined.
#'
#'
#' @details The group names are using the EBX5-ID, returned by
#'
#' @return grouping in the format int=parent list=(child1,child2,child3...)
#'
#' @importFrom dplyr group_by group_map
#' @importFrom data.table data.table
#' @importFrom faoebx5 ReadEBXGroup
#' @export
#'
#' @examples
#' \dontrun{
#' ReadEBXHierarchy(306,301)
#' }
#'
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
ReadEBXHierarchy <- function(sdmx_level1_ID, sdmx_level2_ID) {

  # resolve grouping solutions
  allSolutions <- GetGroupConnections(sdmx_level1_ID, sdmx_level2_ID)
  if (length(allSolutions) == 0) {
    stop('the grouping ',sdmx_level1_ID,'->',sdmx_level2_ID,' is not defined in EBX5 metadata')
  }

  # read the first grouping
  result <- getGrouping(allSolutions[[1]])

  if (length(allSolutions) > 1) {
    # in case there is more than one solution, combine!
    for (x in 2:length(allSolutions)) {
      # get the grouping for this solution
      grouping <- getGrouping(allSolutions[[x]])

      # merge with the final result
      result <- mergeRelation(result, grouping)
    }
  }

  return(result)
}

# resolve a single grouping
# a solution is a list of EBX-codelist-IDs
# A hierarchy solution can span over several levels example: c(306,307,301)
getGrouping <- function(solution) {

  # read the inital grouping
  parentID <- solution[1]
  childID <- solution[2]
  sdmxGroupName <- ebx5.gr_data[ebx5.gr_data$from==parentID & ebx5.gr_data$to==childID, 'Acronym']
  print(paste0('  parentID=',parentID,', childID=',childID,', sdmxGroupName=',sdmxGroupName))
  result <- ReadEBXGroup(sdmxGroupName)

  if (length(solution) > 2) {
    # if there are more levels, combine them all!
    for(step in 3:length(solution)) {
      # resolving solution for this step
      parentID <- solution[step-1]
      childID <- solution[step]
      sdmxGroupName <- ebx5.gr_data[ebx5.gr_data$from==parentID & ebx5.gr_data$to==childID, 'Acronym']
      print(paste0('  parentID=',parentID,', childID=',childID,', sdmxGroupName=',sdmxGroupName))
      step <- ReadEBXGroup(sdmxGroupName)

      # combine it with the result
      result <- combineLevels(result, step)
    }
  }
  return(result)
}

combineLevels <- function(level1, level2) {

  # prepare column names for merge
  colnames(level1) <- c('L1.group', 'L1.member')
  colnames(level2) <- c('L2.group', 'L2.member')

  # inner join (level1, level2)
  result <-merge (x=level1,y=level2, by.x='L1.member', by.y='L2.group')

  # drop the join column
  result['L1.member'] <- NULL

  return(result)
}

mergeRelation  <- function(merge1, merge2) {

  colnames(merge1) <- c('group', 'member')
  colnames(merge2) <- c('group', 'member')

  return(rbind(merge1, merge2))
}

