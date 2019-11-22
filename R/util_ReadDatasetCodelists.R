#' @title Read a complete group hierarchy, for use with applications
#'
#' @param metadata FishStat metadata; obtained using \code{\link{ReadMetadata}}
#' @param datasetID dataset Identifier obtained using \code{\link{GetDatasets}}
#'
#' @seealso \code{\link{GetDimensionGroups}} and \code{\link{ReadEBXHierarchy}}.
#'
#' @description This function will read all codelists and hierarchies
#'
#' @details It first calls \code{\link{GetDatasetDimensions}} to obtain the dimensions for
#' the dataset. For each of the dimensions, calls \code{\link{GetDimensionGroups}} to
#' obtain the hierarchies for this dimension. Finally calls \code{\link{ReadEBXHierarchy}}
#' to obtain the grouping from the bottom hierarchy (as specified in GetDatasetDimensions) to the
#' top (as specified in GetDimensionGroups).
#' It saves all objects into an RData file with the name of the dataset.
#'
#' @importFrom dplyr group_by group_map
#' @importFrom data.table data.table
#' @importFrom faoebx5 ReadEBXGroup
#' @export
#'
#' @examples
#' \dontrun{
#'  library(faoebx5)
#'  library(fishstatr)
#'  #use row1 for this example
#'  datasetID <- GetDatasets(metadata)[1,'Identifier']
#'  datasetName <- GetDatasets(metadata)[1,'Acronym']
#'
#'  ReadDatasetCodelists(metadata, datasetID)
#'  [1] "=== saved 29 to AQUACULTURE.RData, size=349984"
#'
#'  load(file = paste0(datasetName,'.RData'))
#'  > ls()
#' [1] "COUNTRY.Codelist"                   "COUNTRY.Groups"                     "COUNTRY.Groups.CONTINENT"
#' [4] "COUNTRY.Groups.GEO_REGION"          "COUNTRY.Groups.ECON_CLASS"          "COUNTRY.Groups.ECON_GROUP"
#' [7] "COUNTRY.Groups.COMMISSION"          "COUNTRY.Groups.ECO_REGION"          "COUNTRY.Groups.OTHER_COUNTRY_GROUP"
#' [10] "SPECIES.Codelist"                   "SPECIES.Groups"                     "SPECIES.Groups.YEARBOOK_GROUP"
#' [13] "SPECIES.Groups.ISSCAAP_DIVISION"    "SPECIES.Groups.ISSCAAP_GROUP"       "SPECIES.Groups.MAIN_GROUP"
#' [16] "SPECIES.Groups.ORDER"               "SPECIES.Groups.FAMILY"              "SPECIES.Groups.SPECIES_GROUP"
#' [19] "SPECIES.Groups.CPC_DIVISION"        "SPECIES.Groups.CPC_GROUP"           "SPECIES.Groups.CPC"
#' [22] "AREA.Codelist"                      "AREA.Groups"                        "AREA.Groups.INLAND_MARINE"
#' [25] "AREA.Groups.OCEAN"                  "AREA.Groups.SUB_OCEAN"              "AREA.Groups.FA_REGION"
#' [28] "ENVIRONMENT.Codelist"               "ENVIRONMENT.Groups"
#' }
#'
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
ReadDatasetCodelists <- function(metadata, datasetID) {

  if (!is.list(metadata) || length(names(metadata))!=11) {
    stop('metadata is not valid for FishStat')
  }

  objectlist <- c('Dimensions')
  datasetName <- metadata$Dataset[metadata$Dataset$Identifier==datasetID,'Acronym']
  Dimensions <- GetDatasetDimensions(metadata, datasetID)
  for (dimRow in 1:nrow(Dimensions)) {
    dimName <- Dimensions[dimRow]$Acronym

    # create the Country.Codelist
    assign(paste0(dimName,'.Codelist'), GetCodelistByID(dimensions[dimRow]$EBXCodelist))
    objectlist <- c(objectlist, paste0(dimName,'.Codelist'))
    print(paste0('=',dimName,'.Codelist ID=',Dimensions[dimRow]$EBXCodelist))

    # create Country.Groups
    if (nrow(metadata$Relation[metadata$Relation$ConceptChild == Dimensions[dimRow]$ConceptID,]) == 0) {
      # create empty
      assign(paste0(dimName,'.Groups'), data.table())
      objectlist <- c(objectlist, paste0(dimName,'.Groups'))
    }
    else {
      assign(paste0(dimName,'.Groups'), GetDimensionGroups(metadata, Dimensions[dimRow]$ConceptID))
      objectlist <- c(objectlist, paste0(dimName,'.Groups'))
      print(paste0('=',dimName,'.Groups ID=',Dimensions[dimRow]$ConceptID))

      # create Country.Hierarchies
      hierarchies <- get(paste0(dimName,'.Groups'))
      for (hierRow in 1:nrow(hierarchies)) {
        hierarchy <- ReadEBXHierarchy(hierarchies[hierRow,'EBXCodelist'], Dimensions[dimRow]$EBXCodelist)
        hierName <- paste0(dimName,'.Groups.',hierarchies[hierRow,'Acronym'])
        print(paste0('=',hierName,' ID=',hierarchies[hierRow,'EBXCodelist'],'->',Dimensions[dimRow]$EBXCodelist))
        if (nrow(hierarchy) == 0) {
          stop(paste('hierarchy ',hierName,' ID=', hierarchies[hierRow,'EBXCodelist'],'->',
                     Dimensions[dimRow]$EBXCodelist),' does not have any data')
        }
        assign(hierName, hierarchy)
        objectlist <- c(objectlist,hierName)
      }
    }
  }
  print(paste0('=== saved ',length(objectlist),' to ',datasetName,'.RData, size=', object.size(get(objectlist))))
  save(list=objectlist, file = paste0(datasetName,'.RData'))
}

