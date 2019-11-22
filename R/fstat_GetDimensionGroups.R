#' @title Get FishStat Dimension Groups
#'
#' @description This function returns the groups for a given dimension (Concept).
#' On a dataset with dimension country, the dimension groups (or hierachies) are used for
#' aggregation (Continent, Region, ...).
#'
#' @param metadata FishStat metadata; obtained using \code{\link{ReadMetadata}}
#' @param dimensionConceptID dimension Identifier (a concept) obtained using \code{\link{GetDimensions}}
#'
#' @seealso \code{\link{GetEBXHierarchy}}.
#'
#' @return Returns an object of the class \code{\link[data.table]{data.table}}
#'
#' @importFrom rlang is_empty
#' @import data.table
#'
#' @examples
#'
#' \dontrun{
#' library(faoebx5)
#' library(fishstatr)
#' metadata <- ReadMetadata()
#' GetDimensionGroups(metadata, dimensionConceptID = 1)
#'    Identifier             Acronym Sort EBXCodelist                                Name_En
#' 3           3           CONTINENT  102         201                              Continent
#' 4           4          GEO_REGION  103         208                    Geographical region
#' 5           5          ECON_CLASS  105         202                         Economic class
#' 6           6          ECON_GROUP  106         203                         Economic group
#' }
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
    stop('dimensionConceptID=<',dimensionConceptID,'> is not defined in FishStat.Realation')
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
