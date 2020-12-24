#' @title Read a complete group hierarchy; internal
#'
#' @param dim_level1_ID ID upper level of a hierarchy, as returned by GetDimensionGroups$EBXCodelist
#' @param dim_level2_ID lower level of a hierarchy, as returned by GetDimensions$EBXCodelist
#'
#' @seealso \code{\link{GetDimensionGroups}} and \code{\link{GetDatasetDimensions}}.
#'
#' @description Is used by \code{\link{ReadDatasetCodelists}}.
#'
#' @details This function calls \code{\link{GetGroupConnectionIDs}} to find possible
#' grouping solutions accross multiple levels. For each of these levels if reads the
#' \code{\link{ReadEBXGroup}} grouping, combining all levels to a final result.
#' In case there are multiple group solutions reported by \code{\link{GetGroupConnectionIDs}},
#' each solution is resolved and the groupings for each solution are combined into a final
#' result.
#'
#' @return grouping as a \code{\link[data.table]{data.table}} with two columns(parent,child)
#'
#' @importFrom data.table data.table
#' @export
#'
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
ReadEBXHierarchy <- function(dim_level1_ID, dim_level2_ID) {

  # resolve grouping solutions
  allSolutions <- GetGroupConnectionIDs(dim_level1_ID, dim_level2_ID)
  if (length(allSolutions) == 0) {
    stop('the grouping ',dim_level1_ID,'->',dim_level2_ID,' is not defined in EBX5 metadata')
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

##############################################################################################
# resolve a single grouping
# a solution is a list of EBX-codelist-IDs
# A hierarchy solution can span over several levels example: c(306,307,301)
##############################################################################################
getGrouping <- function(solution) {

  # read the inital grouping
  parentID <- solution[1]
  childID <- solution[2]
  ebx5.gr_data <- GetEBXGroups()
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

##############################################################################################
# combine grouping, to remove a intermediate level
# level1(L1.group,L1.member),level2(L2.group,L2.member) -> (L1.group,L2.member)
##############################################################################################
combineLevels <- function(level1, level2) {

  # prepare column names for merge
  colnames(level1) <- c('L1.group', 'L1.member')
  colnames(level2) <- c('L2.group', 'L2.member')

  # inner join (level1, level2)
  result <-merge (x=level1[1:2],y=level2[1:2], by.x='L1.member', by.y='L2.group')

  # drop the join column
  result['L1.member'] <- NULL

  colnames(result) <- c('group', 'member')

  return(result)
}

##############################################################################################
# merge relation = rbdind(merge1, merge2)
##############################################################################################
mergeRelation  <- function(merge1, merge2) {

  colnames(merge1) <- c('group', 'member')
  colnames(merge2) <- c('group', 'member')

  return(rbind(merge1[1:2], merge2[1:2]))
}
