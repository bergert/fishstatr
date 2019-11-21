#' @title Get FishStat Concepts
#'
#' @description This function returns the concept IDs for a given dataset
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
#' GetConcepts()
#' }
#'
#' @export
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
GetConcepts <- function(metadata, datasetID = 1) {

  if (!is.list(metadata) || length(names(metadata))!=11 || !is.data.table(metadata$Dataset2Concept)) {
    stop('metadata is not valid for FishStat')
  }

  df <- metadata$Dataset2Concept

  return(df[df$Dataset==datasetID,'Concept'])
}
