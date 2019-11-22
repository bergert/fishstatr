#' @title Get FishStat Dimension Data
#'
#' @description This function returns the groups for a given dimension (Concept) inclduing the child codes
#'
#' @param metadata FishStat metadata; obtained using \code{\link{ReadMetadata}}
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
#' }
#'
#' @export
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
GetDimensionGroupData <- function(metadata, dimensionConceptID, ebxConceptID) {

  result <- GetDimensionGroups(metadata,1)
  return(result)
}



