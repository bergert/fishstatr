#' @title Read FishStat Metadata
#'
#' @description This function returns the FishStat metadata which is used for subsequent operations
#'
#' @return The returned object is used by other methods
#'
#' @importFrom faoebx5 ReadEBXGroup ReadEBXCodeList
#' @import data.table
#'
#' @examples
#'
#' \dontrun{
#' install.packages("C:/Programs/GithubTom/faoebx5_1.0.0.tar.gz", repos = NULL, type="source")
#' devtools::install_github('bergertom/faoebx5')
#' library(faoebx5)
#' SetupEBXConnection('Fishery','Fishery','http://server.name/ebx-dataservices/connector','user', 'pass')
#' metadata <- ReadMetadata()
#' }
#'
#' @export
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
ReadMetadata <- function() {


  fsj_Workspace  <- ReadEBXCodeList('FSJ_WORKSPACE')
  fsj_Dataset    <- ReadEBXCodeList('FSJ_DATASET')
  fsj_Timeseries <- ReadEBXCodeList('FSJ_TIMSERIES')

  fsj_Concept   <- ReadEBXCodeList('FSJ_CONCEPT')
  fsj_Dimension <- ReadEBXCodeList('FSJ_DIMENSION')
  fsj_Relation  <- ReadEBXCodeList('FSJ_RELATION')
  fsj_Attribute <- ReadEBXCodeList('FSJ_ATTRIBUTE')
  fsj_Measure   <- ReadEBXCodeList('FSJ_MEASURE')

  fsj_Workspace2Dataset <- ReadEBXGroup('FSJ_GROUP_WORKSPACE_DATASET')
  fsj_Dataset2Concept   <- ReadEBXGroup('FSJ_GROUP_DATASET_CONCEPT')
  fsj_Dataset2Relation  <- ReadEBXGroup('FSJ_GROUP_DATASET_RELATION')

  fsj_Metadata <- list('Workspace'  = fsj_Workspace,
                       'Dataset'    = fsj_Dataset,
                       'Timeseries' = fsj_Timeseries,
                       'Concept'    = fsj_Concept,
                       'Dimension'  = fsj_Dimension,
                       'Relation'   = fsj_Relation,
                       'Attribute'  = fsj_Attribute,
                       'Measure'    = fsj_Measure,
                       'Workspace2Dataset' = fsj_Workspace2Dataset,
                       'Dataset2Concept'   = fsj_Dataset2Concept,
                       'Dataset2Relation'  = fsj_Dataset2Relation)

  return(fsj_Metadata)
}
