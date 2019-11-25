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
#'  library(data.table)
#'  #use row1 for this example
#'  datasetID <- GetDatasets(metadata)[1,'Identifier']
#'  datasetName <- GetDatasets(metadata)[1,'Acronym']
#'
#'  ReadDatasetCodelists(metadata, datasetID)
#'  [1] "=== saved 72 to AQUACULTURE.RData, size=23293352"
#'
#'  load(file = paste0(datasetName,'.RData'))
#'  > ls()
#'  [1] "Dimensions"                           "COUNTRY.Codelist"                     "COUNTRY.Groups"
#'  [4] "COUNTRY.CONTINENT.Codelist"           "COUNTRY.CONTINENT.Groups"             "COUNTRY.GEO_REGION.Codelist"
#'  [7] "COUNTRY.GEO_REGION.Groups"            "COUNTRY.ECON_CLASS.Codelist"          "COUNTRY.ECON_CLASS.Groups"
#'  [10] "COUNTRY.ECON_GROUP.Codelist"          "COUNTRY.ECON_GROUP.Groups"            "COUNTRY.COMMISSION.Codelist"
#'  [13] "COUNTRY.COMMISSION.Groups"            "COUNTRY.ECO_REGION.Codelist"          "COUNTRY.ECO_REGION.Groups"
#'  [16] "COUNTRY.OTHER_COUNTRY_GROUP.Codelist" "COUNTRY.OTHER_COUNTRY_GROUP.Groups"   "SPECIES.Codelist"
#'  [19] "SPECIES.Groups"                       "SPECIES.YEARBOOK_GROUP.Codelist"      "SPECIES.YEARBOOK_GROUP.Groups"
#'  [22] "SPECIES.ISSCAAP_DIVISION.Codelist"    "SPECIES.ISSCAAP_DIVISION.Groups"      "SPECIES.ISSCAAP_GROUP.Codelist"
#'  [25] "SPECIES.ISSCAAP_GROUP.Groups"         "SPECIES.MAIN_GROUP.Codelist"          "SPECIES.MAIN_GROUP.Groups"
#'  [28] "SPECIES.ORDER.Codelist"               "SPECIES.ORDER.Groups"                 "SPECIES.FAMILY.Codelist"
#'  [31] "SPECIES.FAMILY.Groups"                "SPECIES.SPECIES_GROUP.Codelist"       "SPECIES.SPECIES_GROUP.Groups"
#'  [34] "SPECIES.CPC_DIVISION.Codelist"        "SPECIES.CPC_DIVISION.Groups"          "SPECIES.CPC_GROUP.Codelist"
#'  [37] "SPECIES.CPC_GROUP.Groups"             "SPECIES.CPC.Codelist"                 "SPECIES.CPC.Groups"
#'  [40] "AREA.Codelist"                        "AREA.Groups"                          "AREA.INLAND_MARINE.Codelist"
#'  [43] "AREA.INLAND_MARINE.Groups"            "AREA.OCEAN.Codelist"                  "AREA.OCEAN.Groups"
#'  [46] "AREA.SUB_OCEAN.Codelist"              "AREA.SUB_OCEAN.Groups"                "AREA.FA_REGION.Codelist"
#'  [49] "AREA.FA_REGION.Groups"                "ENVIRONMENT.Codelist"
#'
#'  subset(get("COUNTRY.Codelist"),Identifier==29|Identifier==45|Identifier==24,select=c('Identifier','Name_En','UN_Code','ISO3_Code'))
#'    Identifier                  Name_En UN_Code ISO3_Code
#'  24         24 British Indian Ocean Ter     086       IOT
#'  29         29                  Burundi     108       BDI
#'  45         45                  Comoros     174       COM
#'  get("COUNTRY.CONTINENT.Codelist")[2,]
#'    Identifier UN_Code    Name_En     Name_Fr   Name_Es
#'  2        359     002     Africa     Afrique    África
#'  head(get("COUNTRY.CONTINENT.Groups"))
#'    L1.group L2.member   NA NA.1
#'  1      359        29 1900 9999
#'  2      359        45 1900 9999
#'  3      359        24 1900 9999
#'  4      359        72 1900 9999
#'  5      359       114 1900 9999
#'  6      359        62 1900 9999
#'  get('COUNTRY.CONTINENT.Groups.2')[[2,1]]
#'  [1] "359"
#'  head(get('COUNTRY.CONTINENT.Groups.2')[[2,2]])
#'  [1] "29"  "45"  "24"  "72"  "114" "62"
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
    assign(paste0(dimName,'.Codelist'), GetCodelistByID(Dimensions[dimRow]$EBXCodelist))
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


      # create Country.Groups
      groups <- get(paste0(dimName,'.Groups'))
      for (groupRow in 1:nrow(groups)) {
        # create Country.Continent.Codelist
        groupCLname <- paste0(dimName,'.',groups[groupRow,'Acronym'],'.Codelist')
        objectlist <- c(objectlist, groupCLname)
        groupCL <- GetCodelistByID(groups[groupRow,'EBXCodelist'])
        assign(groupCLname, groupCL)

        # create Country.Continent.Groups
        hierarchy <- ReadEBXHierarchy(groups[groupRow,'EBXCodelist'], Dimensions[dimRow]$EBXCodelist)
        hierName <- paste0(dimName,'.',groups[groupRow,'Acronym'],'.Groups')
        print(paste0('=',hierName,' ID=',groups[groupRow,'EBXCodelist'],'->',Dimensions[dimRow]$EBXCodelist))
        if (nrow(hierarchy) == 0) {
          stop(paste('hierarchy ',hierName,' ID=', groups[groupRow,'EBXCodelist'],'->',
                     Dimensions[dimRow]$EBXCodelist),' does not have any data')
        }
        assign(hierName, hierarchy)
        hierarchy <- hierarchy[,c(1,2)]
        colnames(hierarchy) <- c('group', 'member')
        assign(paste0(hierName,'.2'), GroupAsList(hierarchy))

        objectlist <- c(objectlist,hierName)
        objectlist <- c(objectlist,paste0(hierName,'.2'))
      }
    }
  }
  print(paste0('=== saved ',length(objectlist),' to ',datasetName,'.RData, size=', sum(sapply(objectlist,function(x){object.size(get(x))})) ))
  save(list=objectlist, file = paste0(datasetName,'.RData'))
}

