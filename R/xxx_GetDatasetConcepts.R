#' @title Get FishStat Dataset Concepts
#'
#' @description This function returns the concept IDs for a given dataset
#'
#' @param metadata FishStat metadata; obtained using \code{\link{ReadMetadata}}
#' @param datasetID dataset Identifier obtained using \code{\link{GetDatasets}}
#'
#' @return Returns an object of the class \code{\link[data.table]{data.table}}
#' of a single column listing the FishStat ConceptIDs.
#'
#' @import data.table
#'
#' @examples
#'
#' \dontrun{
#' metadata <- ReadMetadata()
#' GetDatasetConcepts(metadata, datasetID = 1)
#' }
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
GetDatasetConcepts <- function(metadata, datasetID) {

  if (!is.list(metadata) || length(names(metadata))!=11 || !is.data.frame(metadata$Dataset2Concept)) {
    stop('metadata is not valid for FishStat')
  }

  if (length(datasetID)==0 | !is.numeric(datasetID)) {
    stop('A numeric datasetID is required')
  }

  df <- metadata$Dataset2Concept

  return(df[df$Dataset==datasetID,'Concept'])
}
