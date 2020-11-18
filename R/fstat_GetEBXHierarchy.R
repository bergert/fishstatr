#' @title Get the connections between two hierarchies
#'
#' @description Using the EBX5 metadata, calculates the grouping from parent to child group,
#' based on codelist Identifier; as defined in 'EBXGroup.to' and 'EBXGroup.from'.
#' This is a basic function of this package, it is exposed primarily for troubleshooting.
#' Users will normally use \code{\link{ReadEBXHierarchy}} or \code{\link{ReadDatasetCodelists}}.
#'
#' @param sdmx_parent_codelist parent codelist name, in SDMX style.
#' @param sdmx_child_codelist child codelist name, in SDMX style.
#' Available code lists are shown by function \code{\link{GetEBXGroups}} in the field "Acronym".
#' The actual group ID and location in EBX5 (branch, instance, code-list-name) are resolved
#' using the Metadata structure.
#'
#' @seealso \code{\link{ReadEBXGroup}} \code{\link{GetDimensionGroups}}
#'
#' @details The solution may invaolve several steps (a list of group ID's), and may involve
#' multiple step groupings, which have to be merged for the final result:
#'    MAJOR(306)->ODER(307)->FAMILY(302)->SPECIES(301)
#'    MAJOR(306)->ODER(307)->SPECIES(301)
#' In the simplest case, if a matching group exists,
#' list(ebx_parent_ID, ebx_child_ID) is returned
#'
#' @return the resolved grouping details in the format int=parent list=(child1,child2, child3...)
#'
#' @importFrom dplyr group_by group_map
#' @importFrom data.table data.table
#' @export
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
GetEBXHierarchy <- function(sdmx_parent_codelist, sdmx_child_codelist) {

  if (missing(sdmx_parent_codelist)) {
    stop('define a sdmx_parent_group')
  }
  if (missing(sdmx_child_codelist)) {
    stop('define a sdmx_child_codelist')
  }

  ebx5.cl_data <- GetEBXCodeLists()
  if (length(ebx5.cl_data$Name[ebx5.cl_data$Acronym == sdmx_parent_codelist]) == 0) {
    stop('Cannot find a codelist with acronym=<', sdmx_parent_codelist, '> defined in EBX metadata')
  }
  if (length(ebx5.cl_data$Name[ebx5.cl_data$Acronym == sdmx_child_codelist]) == 0) {
    stop('Cannot find a codelist with acronym=<', sdmx_child_codelist, '> defined in EBX metadata')
  }

  parent_codelistID<- ebx5.cl_data$Identifier[ebx5.cl_data$Acronym == sdmx_parent_codelist]
  child_codelistID <- ebx5.cl_data$Identifier[ebx5.cl_data$Acronym == sdmx_child_codelist]

  return(ReadEBXHierarchy(
    dim_level1_ID = parent_codelistID,
    dim_level2_ID  = child_codelistID ))
}
