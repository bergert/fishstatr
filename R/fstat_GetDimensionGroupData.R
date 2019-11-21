#' @title Get FishStat Dimension Data
#'
#' @description This function returns the groups for a given dimension (Concept) inclduing the child codes
#'
#' @param metadata FishStat metadata; obtained using ReadMetadata()
#' @param dimensionConceptID dimension Identifier (a concept)
#'
#'
#' @return Returns an object of the class \code{\link[data.table]{data.table}}
#'
#' @importFrom rlang is_empty
#' @import data.table
#'
#' @examples
#'
#' \dontrun{
#' GetConcepts()
#' }
#'
#' @export
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
GetDimensionGroupData <- function(metadata, dimensionConceptID = 1, ebxConceptID=200) {

  result <- GetDimensionGroups(metadata,1)
  return(result)
}


getGrouping <- fuction (toID=200, groupingID=201, solution=list()) {
  result <- list()
  grp <- ebx5.gr_data[ebx5.gr_data$from==groupingID,'to']
  grp <- as.character(grp$to)
  for (aGroup in grp) {
     if (aGroup == toID) {
       return(solution)
     }
     else {
       newSolution <- list(solution, c(groupingID))
       getGrouping(toID, aGroup, newSolution)
     }
  }

}
