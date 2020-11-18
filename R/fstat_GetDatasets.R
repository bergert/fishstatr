#' @title Get FishStat Datasets
#'
#' @description This is a convenience function to make it easier to get started usign data
#' from EBX5. It provides a dataset name, and the Dataset-Identifier required by other fucntions.
#'
#' @param metadata FishStat metadata; obtained using \code{\link{ReadMetadata}}
#'
#'
#' @return Returns an object of the class \code{\link[data.table]{data.table}}
#'
#' @import data.table
#'
#' @export
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
GetDatasets <- function(metadata) {

  if (!is.list(metadata) || length(names(metadata))!=11 || !is.data.frame(metadata$Dataset)) {
    stop('metadata is not valid for FishStat')
  }

  return(subset(metadata$Dataset, select=c('Identifier','Acronym','Name_En')))
}
