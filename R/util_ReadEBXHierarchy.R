#' @title Read a complete group hierarchy
#'
#' @param sdmx_level1 ID upper level of a hierarchy, as returned by GetDimensionGroups$EBXCodelist
#' @param sdmx_level2 lower level of a hierarchy, as returned by GetDimensions$EBXCodelist
#'
#' @seealso \code{\link{GetDimensionGroups}} and \code{\link{GetDatasetDimensions}}.
#'
#' @description Is used by \code{\link{ReadDatasetCodelists}}.
#'
#' @details This function calls \code{\link{GetGroupConnections}} to find possible
#' grouping solutions accross multiple levels. For each of these levels if reads the
#' \code{\link[faoebx5]{ReadEBXGroup}} grouping, combining all levels to a final result.
#' In case there are multiple group solutions reported by \code{\link{GetGroupConnections}},
#' each solution is combined and the groupings for each solution are merged into the final
#' result.
#'
#' @return grouping as a \code{\link[data.table]{data.table}} with two columns(parent,child)
#'
#' @importFrom dplyr group_by group_map
#' @importFrom data.table data.table
#' @importFrom faoebx5 ReadEBXGroup
#' @export
#'
#' @examples
#' \dontrun{
#' library(faoebx5)
#' library(fishstatr)
#' ReadMetadata()
#' result <- ReadEBXHierarchy(306,301)
#' [1] "  parentID=306, childID=307, sdmxGroupName=HCL_FI_SPECIES_MAJOR_ORDER"
#' [1] "  parentID=307, childID=302, sdmxGroupName=HCL_FI_SPECIES_ORDER_FAMILY"
#' [1] "  parentID=302, childID=301, sdmxGroupName=HCL_FI_SPECIES_FAMILY_ITEM"
#' [1] "  parentID=306, childID=307, sdmxGroupName=HCL_FI_SPECIES_MAJOR_ORDER"
#' [1] "  parentID=307, childID=301, sdmxGroupName=HCL_FI_SPECIES_ORDER_ITEM"
#' nrow(result)
#' [1] 12751
#' }
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

##############################################################################################
# resolve a single grouping
# a solution is a list of EBX-codelist-IDs
# A hierarchy solution can span over several levels example: c(306,307,301)
##############################################################################################
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

##############################################################################################
# combine grouping, to remove a intermediate level
# level1(L1.group,L1.member),level2(L2.group,L2.member) -> (L1.group,L2.member)
##############################################################################################
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

##############################################################################################
# merge relation = rbdind(merge1, merge2)
##############################################################################################
mergeRelation  <- function(merge1, merge2) {

  colnames(merge1) <- c('group', 'member')
  colnames(merge2) <- c('group', 'member')

  return(rbind(merge1, merge2))
}
