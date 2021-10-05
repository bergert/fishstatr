#' @title Get FishStat Dataset Dimensions
#'
#' @description This function returns the Dimensions for a given dataset.
#' A Dimension is based on a Concept(Country). The field of this concept used for the
#' dimension is called Attribute(UN_Code).
#' The EBXCodelist-ID is what hierarchies for this dimension are based on.
#'
#' @param metadata FishStat metadata; obtained using \code{\link{ReadMetadata}}
#' @param datasetID dataset Identifier obtained using \code{\link{GetDatasets}}
#'
#' @seealso \code{\link{ReadEBXHierarchy}}.
#'
#' @return Returns an object of the class \code{\link[data.table]{data.table}}
#'
#' @importFrom data.table data.table
#'
#' @export
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
GetDatasetDimensions <- function(metadata, datasetID) {

  if (!is.list(metadata) || length(names(metadata))!=13) {
    stop('metadata is not valid for FishStat')
  }
  if (!is.data.frame(metadata$Dimension) || !is.data.frame(metadata$Concept)) {
    stop('metadata is not complete for FishStat')
  }

  # read dimensions from EBX5
  df <- metadata$Dimension

  # get our dataset
  filter <- df[df$DatasetKey==datasetID,c('Identifier','AttributeKey','SWS_Dimension')]
  attr(filter, ".internal.selfref") <- NULL
  names(filter)[names(filter) == "Identifier"] <- "DimensionID"


  # resolve FishStat attribute
  fsjAttributes = metadata$Attribute[,c('Identifier','FishStat_Concept','EBX_Attribute')]
  result <- merge.data.frame(x=filter, y=fsjAttributes, by.x = 'AttributeKey', by.y='Identifier')
  names(result)[names(result) == "AttributeKey"] <- "AttributeID"
  names(result)[names(result) == "FishStat_Concept"] <- "ConceptID"

  # add concept name
  result <- merge.data.frame(x=result, y=metadata$Concept[,c('Identifier','Name_En','Acronym')], by.x = 'ConceptID', by.y='Identifier')

  # get the EBX attribute name
  result <- merge.data.frame(x=result, y=GetEBXAttributes()[, c('Identifier','EBX_Name','EBX_Codelist')], by.x = 'AttributeID', by.y='Identifier')

  result$EBX_Attribute <- NULL
  names(result)[names(result) == "EBX_Codelist"] <- "EBXCodelist"
  names(result)[names(result) == "EBX_Name"] <- "EBXName"

  return(result)
}
