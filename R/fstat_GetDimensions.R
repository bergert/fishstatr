#' @title Get FishStat Dimensions
#'
#' @description This function returns the AttributeKeys for a given dataset.
#' AttributeKey is a EBX5 composite key, in the form of paste0(ConceptID,'|',AttributeID)
#'
#' @param metadata FishStat metadata; obtained using ReadMetadata()
#' @param datasetID dataset Identifier.
#'
#'
#' @return Returns an object of the class \code{\link[data.table]{data.table}}
#'
#' @import data.table
#'
#' @examples
#'
#' \dontrun{
#' GetDimension(datasetID=2)
#' }
#'
#' @export
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
GetDimensions <- function(metadata, datasetID = 1) {

  if (!is.list(metadata) || length(names(metadata))!=11 || !is.data.table(metadata$Dimension) || !is.data.table(metadata$Concept)) {
    stop('metadata is not valid for FishStat')
  }

  # read dimensions from EBX5
  df <- metadata$Dimension

  # get our dataset
  filter <- df[df$DatasetKey==datasetID,c('Identifier','AttributeKey')]
  attr(filter, ".internal.selfref") <- NULL

  # split the AttributeKey
  result <- data.table(do.call('rbind', strsplit(as.character(filter$AttributeKey),'|',fixed=TRUE)), filter$Identifier)
  names(result) <- c("AttributeID","ConceptID","DimensionID")
  attr(result, ".internal.selfref") <- NULL

  # resolve the concept name
  name_en <- subset(metadata$Concept, metadata$Concept$Identifier %in% result$ConceptID, c('Identifier','Name_En', 'EBXCodelist'))
  name_en <- name_en %>% droplevels()
  names(name_en)[names(name_en) == "Identifier"] <- "ConceptID"
  name_en$ConceptID <- as.character(name_en$ConceptID)
  name_en$Name_En <- as.character(name_en$Name_En)
  attr(name_en, ".internal.selfref") <- NULL

  # resolve the attribute name
  attribute <- subset(metadata$Attribute, metadata$Attribute$Identifier %in% result$AttributeID, c('Identifier','EBXName'))
  attribute <- attribute %>% droplevels()
  names(attribute)[names(attribute) == "Identifier"] <- "AttributeID"
  attribute$AttributeID <- as.character(attribute$AttributeID)
  attribute$EBXName <- as.character(attribute$EBXName)
  attr(attribute, ".internal.selfref") <- NULL

  result <- merge(result, name_en, by='ConceptID')
  attr(result, ".internal.selfref") <- NULL
  attr(result, "sorted") <- NULL
  result <- merge(result, attribute, by='AttributeID')
  attr(result, ".internal.selfref") <- NULL
  attr(result, "sorted") <- NULL
  return(result[order(result$DimensionID)])
}
