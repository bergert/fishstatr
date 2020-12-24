#' @title Get the connections between two hierarchies; internal
#'
#' @description Using the EBX5 metadata, calculates the grouping from parent to child group,
#' based on codelist Identifier; as defined in 'EBXGroup.to' and 'EBXGroup.from'.
#' This is a basic function of this package, it is exposed primarily for troubleshooting.
#' Users will normally use \code{\link{ReadEBXHierarchy}} or \code{\link{ReadDatasetCodelists}}.
#'
#' @param ebx_parent_codelist_ID parent, using the EBX5 codelist Identifier
#' @param ebx_child_codelist_ID child, using the EBX5 codelist Identifier
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
#' @return group ID's to resolve path from parent to child;
#' in the format int=parent list=(child1,child2, child3...)
#'
#' @importFrom data.table data.table
#' @export
#'
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
GetGroupConnectionIDs <- function(ebx_parent_codelist_ID, ebx_child_codelist_ID) {

  ebx5.gr_data <- GetEBXGroups()

  if (missing(ebx_parent_codelist_ID)) {
    stop('define a numeric ebx_parent_codelist_ID')
  }
  if (missing(ebx_child_codelist_ID)) {
    stop('define a numeric ebx_child_codelist_ID')
  }

  if (ebx_parent_codelist_ID == ebx_child_codelist_ID) {
    # this is the case of a grouping which is not modeled explicitly,
    # but has all levels lumped together: show the grouping to self and stop
    return (list(c(ebx_parent_codelist_ID, ebx_child_codelist_ID)))
  }

  query <- ebx5.gr_data[ebx5.gr_data$from == ebx_parent_codelist_ID,'to']
  if (length(query) == 0) {
    # this parent has no children
    return (list())
  }

  result <- list()
  for (child in query) {
    if (child == ebx_parent_codelist_ID | child == ebx_child_codelist_ID) {
      if (child == ebx_parent_codelist_ID) {
        # this is a group pointing to itself, so hierarchy ends here
        if (child == ebx_child_codelist_ID) {
          result[[length(result)+1]] <- c(child, child)
        }
      }
      else {
        # we found the solution
        result[[length(result)+1]] <- c(ebx_parent_codelist_ID, ebx_child_codelist_ID)
      }
    }
    else {
      # recursively resolve the child
      indirect <- GetGroupConnectionIDs(child, ebx_child_codelist_ID)
      for (partialResult in indirect) {
        # add the partialResult to the final result
        result[[length(result)+1]] <- c(ebx_parent_codelist_ID, partialResult)
      }
    }
  }
  return(result)
}
