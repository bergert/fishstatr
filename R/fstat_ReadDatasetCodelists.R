#' @title Read a complete group hierarchy, for use with applications
#'
#' @param metadata FishStat metadata; obtained using \code{\link{ReadMetadata}}
#' @param dataset_ID dataset Identifier obtained using \code{\link{GetDatasets}}
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
#' @importFrom dplyr group_by group_map ungroup "%>%"
#' @importFrom data.table data.table
#' @importFrom utils object.size
#' @importFrom faoebx5 EBXRead
#'
#' @export
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
ReadDatasetCodelists <- function(metadata, dataset_ID) {

  if (!is.list(metadata) || length(names(metadata))!=11) {
    stop('metadata is not valid for FishStat')
  }

  objectlist <- c('Dimensions')
  datasetName <- metadata$Dataset[metadata$Dataset$Identifier==dataset_ID,'Acronym']
  Dimensions <- GetDatasetDimensions(metadata, dataset_ID)
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

