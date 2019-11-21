#' @title Get FishStat Dimension
#'
#' @description This function returns the groups for a given dimension (Concept)
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
GetDimensionGroups <- function(metadata, dimensionConceptID = 1) {

  if (!is.list(metadata) || length(names(metadata))!=11 || !is.data.table(metadata$Relation)) {
    stop('metadata is not valid for FishStat')
  }

  # get the ID's of the relations
  groupIDs <- metadata$Relation[metadata$Relation$ConceptChild == dimensionConceptID,'ConceptParent']

  # return the matching concepts
  result <- metadata$Concept[metadata$Concept$Identifier %in% groupIDs$ConceptParent,c('Identifier','Acronym','Sort','EBXCodelist','Name_En')] %>% droplevels()
  result$Identifier <- as.character(result$Identifier)
  result$Acronym <- as.character(result$Acronym)
  result$Sort <- as.character(result$Sort)
  result$EBXCodelist <- as.character(result$EBXCodelist)
  result$Name_En <- as.character(result$Name_En)
  attr(result, ".internal.selfref") <- NULL
  return(result)
}
