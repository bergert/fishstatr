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
#' @examples
#'
#' \dontrun{
#' library(faoebx5)
#' library(fishstatr)
#' metadata <- ReadMetadata()
#' GetDatasets(metadata)
#'    Identifier           Acronym                                               Name_En
#'1            1       AQUACULTURE                         Global aquaculture production
#'2            2           CAPTURE                             Global capture production
#'3            3 GLOBAL_PRODUCTION                Global production by production source
#'4            4             TRADE            Fisheries commodities production and trade
#'5            5     CECAF_CAPTURE   CECAF (Eastern Central Atlantic) capture production
#'6            6      GFCM_CAPTURE GFCM (Mediterranean and Black Sea) capture production
#'7            7    RECOFI_CAPTURE                             RECOFI capture production
#'8            8     SEATL_CAPTURE                 Southeast Atlantic capture production
#'9            9      FOOD_BALANCE      Food balance sheets of fish and fishery products
#'10          10        POPULATION                                      World population
#' }
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
