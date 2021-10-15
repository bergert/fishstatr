#' @title Read a complete group hierarchy, for use with applications
#'
#' @param metadata FishStat metadata; obtained using \code{\link{ReadMetadata}}
#' @param timeseries_ID timeseries identifier obtained using \code{\link{GetTimeseries}}
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
#' @importFrom data.table data.table
#' @importFrom utils object.size
#' @importFrom faoebx5 EBXRead
#'
#' @export
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
ReadDatasetCodelists <- function(metadata, timeseries_ID) {

  if (!is.list(metadata) || length(names(metadata))!=13) {
    stop('metadata is not valid for FishStat')
  }

  # filter out query panel
  timeseries <- metadata$Timeseries[metadata$Timeseries$isFishStatJ == 'true' & metadata$Timeseries$Query_Panel == 'true',]
  timeseries$isFishStatJ <- NULL
  timeseries$Query_Panel <- NULL

  if (nrow(timeseries[timeseries$Identifier == timeseries_ID,]) == 0) {
    stop('timeseries_ID=<',timeseries_ID,'> is not defined in FishStat.Timeseries')
  }

  # Timeseries
  Timeseries <- timeseries[timeseries$Identifier == timeseries_ID,]
  dataset_ID <- as.numeric(Timeseries$DatasetKey)
  objectlist <- c('Timeseries')

  # Dimensions
  datasetName <- metadata$Dataset[metadata$Dataset$Identifier==dataset_ID,'Acronym']
  Dimensions <- GetDatasetDimensions(metadata, dataset_ID)
  objectlist <- c(objectlist, 'Dimensions')

  # Unit
  if (nrow(metadata$Measure[metadata$Measure$TimeseriesKey == Timeseries$Identifier & metadata$Measure$Acronym == 'UNIT',]) == 0) {
    stop('timeseries_ID=<',timeseries_ID,'> has no Measure.UNIT defined')
  }
  unitValue <- metadata$Measure$Value[metadata$Measure$TimeseriesKey == Timeseries$Identifier & metadata$Measure$Acronym == 'UNIT']
  Unit <- LookupUnit(metadata, unitValue)
  objectlist <- c(objectlist, 'Unit')

  # Period
  Period <- metadata$Period[metadata$Period$TimeseriesKey == timeseries_ID,]
  objectlist <- c(objectlist, 'Period')

  # Concepts
  conceptIDs <- metadata$Dataset2Concept[metadata$Dataset2Concept$Group==1,'Member']
  Concepts <- metadata$Concept[metadata$Concept$Identifier %in% conceptIDs,]
  EbxConcept<-ReadEBXCodeList('EBX5_METADATA_CODELIST')
  EbxConcept <- EbxConcept[EbxConcept$Identifier %in% Concepts$EBXCodelist,c('Identifier','Acronym')]
  colnames(EbxConcept) <- c('EBXCodelist', 'EBXCodelist_Name')
  Concepts <- merge(Concepts, EbxConcept, by="EBXCodelist")
  objectlist <- c(objectlist, 'Concepts')

  # Attributes
  Attributes <- metadata$Attribute[metadata$Attribute$FishStat_Concept %in% conceptIDs,]
  EbxAttribute <- ReadEBXCodeList('EBX5_METADATA_ATTRIBUTE')
  EbxAttribute <- EbxAttribute[EbxAttribute$Identifier %in% Attributes$EBX_Attribute, c('Identifier','EBX_Name','Type','Size','Scale')]
  names(EbxAttribute)[names(EbxAttribute) == "Identifier"] <- "EBX_Attribute"
  Attributes <- merge(Attributes, EbxAttribute, by="EBX_Attribute")
  objectlist <- c(objectlist, 'Attributes')

  for (dimRow in 1:nrow(Dimensions)) {
    dimName <- Dimensions[dimRow,]$Acronym

    assign(paste0(dimName,'.Codelist'), GetCodelistWithName(metadata, Dimensions[dimRow,]$ConceptID))
    objectlist <- c(objectlist, paste0(dimName,'.Codelist'))
    print(paste0('=',dimName,'.Codelist ID=',Dimensions[dimRow,]$EBXCodelist))

    # create Country.Groups
    if (nrow(metadata$Relation[metadata$Relation$ConceptChild == Dimensions[dimRow,]$ConceptID,]) == 0) {
      # create empty
      assign(paste0(dimName,'.Groups'), data.table())
      objectlist <- c(objectlist, paste0(dimName,'.Groups'))
    }
    else {
      assign(paste0(dimName,'.Groups'), GetDimensionGroups(metadata, Dimensions[dimRow,]$ConceptID))
      objectlist <- c(objectlist, paste0(dimName,'.Groups'))
      print(paste0('=',dimName,'.Groups ID=',Dimensions[dimRow,]$ConceptID))

      # create Country.Groups
      groups <- get(paste0(dimName,'.Groups'))
      for (groupRow in 1:nrow(groups)) {
        # create Country.Continent.Codelist
        groupCLname <- paste0(dimName,'.',groups[groupRow,'Acronym'],'.Codelist')
        objectlist <- c(objectlist, groupCLname)

        groupCL <- GetCodelistWithName(metadata, groups[groupRow,'Identifier'])
        #groupCL <- GetCodelistByID(groups[groupRow,'EBXCodelist'])
        assign(groupCLname, groupCL)

        # create Country.Continent.Groups
        hierarchy <- ReadEBXHierarchy(groups[groupRow,'EBXCodelist'], Dimensions[dimRow,]$EBXCodelist)
        if (nrow(hierarchy) == 0) {
          print(paste('from=',groups[groupRow,'EBXCodelist'],", to=",Dimensions[dimRow,]$EBXCodelist))
          print(groups)
          print(Dimensions)
          stop(paste('hierarchy ',hierName,' ID=', groups[groupRow,'EBXCodelist'],'->',
                     Dimensions[dimRow,]$EBXCodelist),' does not have any data')
        }
        hierName <- paste0(dimName,'.',groups[groupRow,'Acronym'],'.Groups')
        print(paste0('=',hierName,' ID=',groups[groupRow,'EBXCodelist'],'->',Dimensions[dimRow,]$EBXCodelist))
        assign(hierName, hierarchy)
        hierarchy <- hierarchy[,c(1,2)]
        colnames(hierarchy) <- c('Group', 'Member')
        assign(paste0(hierName,'.2'), GroupAsList(hierarchy))

        objectlist <- c(objectlist,hierName)
        objectlist <- c(objectlist,paste0(hierName,'.2'))
      }
    }
  }

  print(paste0('=== saved ',length(objectlist),' to ',datasetName,'.RData, size=', sum(sapply(objectlist,function(x){object.size(get(x))})) ))
  save(list=objectlist, file = paste0('Timeseries_',timeseries_ID,'.RData'))
}

