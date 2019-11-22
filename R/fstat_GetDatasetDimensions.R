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
#' @seealso \code{\link{GetEBXHierarchy}}.
#'
#' @return Returns an object of the class \code{\link[data.table]{data.table}}
#'
#' @importFrom data.table data.table
#' @importFrom magrittr "%>%"
#'
#' @examples
#'
#' \dontrun{
#' library(faoebx5)
#' library(fishstatr)
#' metadata <- ReadMetadata()
#' GetDatasetDimensions(metadata, datasetID = 1)
#'    AttributeID ConceptID DimensionID                Name_En EBXCodelist    EBXName
#'1:           5         1          11                Country         200    UN_Code
#'2:         101         2          12          ASFIS species         301 Alpha_Code
#'3:          41         8          13 FAO major fishing area         403       Code
#'4:         151        30          14            Environment         502       Code
#' }
#'
#' @export
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
GetDatasetDimensions <- function(metadata, datasetID) {

  if (!is.list(metadata) || length(names(metadata))!=11) {
    stop('metadata is not valid for FishStat')
  }
  if (!is.data.frame(metadata$Dimension) || !is.data.frame(metadata$Concept)) {
    stop('metadata is not complete for FishStat')
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
  name_en <- subset(metadata$Concept, metadata$Concept$Identifier %in% result$ConceptID, c('Identifier','Name_En', 'EBXCodelist','Acronym'))
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
