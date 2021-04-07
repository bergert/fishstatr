#' @title Get FishStat Dimension Groups
#'
#' @description This function returns the groups for a given dimension (Concept).
#' On a dataset with dimension country, the dimension groups (or hierachies) are used for
#' aggregation (Continent, Region, ...).
#'
#' @param metadata FishStat metadata; obtained using \code{\link{ReadMetadata}}
#' @param dimensionConceptID dimension Identifier (a concept) obtained using \code{\link{GetDatasetDimensions}}
#'
#' @seealso \code{\link{ReadEBXHierarchy}}.
#'
#' @return Returns an object of the class \code{\link[data.table]{data.table}}
#'
#' @import data.table
#' @importFrom dplyr %>%
#'
#' @export
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
GetDimensionGroups <- function(metadata, dimensionConceptID) {

  if (!is.list(metadata) || length(names(metadata))!=11 || !is.data.frame(metadata$Relation)) {
    stop('metadata is not valid for FishStat')
  }
  if (missing(dimensionConceptID)){
    stop('dimensionConceptID is required')
  }
  if (nrow(metadata$Relation[metadata$Relation$ConceptChild == dimensionConceptID,]) == 0) {
    stop('dimensionConceptID=<',dimensionConceptID,'> is not defined in FishStat.Relation')
  }

  # get the ID's of the relations
  groupIDs <- metadata$Relation[metadata$Relation$ConceptChild == dimensionConceptID,'ConceptParent']

  # return the matching concepts
  result <- metadata$Concept[metadata$Concept$Identifier %in% groupIDs,c('Identifier','Acronym','Sort','EBXCodelist','Name_En')] %>% droplevels()
  result$Identifier <- as.character(result$Identifier)
  result$Acronym <- as.character(result$Acronym)
  result$Sort <- as.character(result$Sort)
  result$EBXCodelist <- as.character(result$EBXCodelist)
  result$Name_En <- as.character(result$Name_En)
  attr(result, ".internal.selfref") <- NULL
  return(result)
}
