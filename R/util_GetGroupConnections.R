#' @title Get the connections between two hierarchies
#'
#' @description Using the EBX5 metadata, calculates the grouping from parent to child group,
#' based on codelist Identifier; as defined in 'EBXGroup.to' and 'EBXGroup.from'.
#' This is a basic function of this package, it is exposed primarily for troubleshooting.
#' Users will normally use \code{\link{GetEBXHierarchy}}.
#'
#' @param metadata FishStat metadata; obtained using \code{\link{ReadMetadata}}
#' @param ebx_parent_group parent, using the EBX5 codelist Identifier
#' @param ebx_child_group child, using the EBX5 codelist Identifier
#'
#' @seealso \code{\link{[faoebx5]ReadEBXGroup}} \code{\link{GetDimensionGroups}}
#'
#' @details The solution may invaolve several steps (a list of group ID's), and may involve
#' multiple step groupings, which have to be merged for the final result:
#'    MAJOR(306)->ODER(307)->FAMILY(302)->SPECIES(301)
#'    MAJOR(306)->ODER(307)->SPECIES(301)
#' In the simplest case, if a matching group exists,
#' list(ebx_parent_group, ebx_child_group) is returned
#'
#' @return grouping in the format int=parent list=(child1,child2, child3...)
#'
#' @importFrom dplyr group_by group_map
#' @importFrom data.table data.table
#' @importFrom faoebx5 ReadEBXGroup
#' @export
#'
#' @examples
#' \dontrun{
#' GetGroupConnections(306,301)
#' [[1]]
#' [1] "306" "307" "302" "301"
#' [[2]]
#' [1] "306" "307" "301"
#' }
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
GetGroupConnections <- function(ebx_parent_group, ebx_child_group) {

   if (!exists("ebx5.gr_data")) {
     stop('ebx5.gr_data is not loaded; you must run ReadMetadata()')
   }

  if (missing(ebx_parent_group)) {
    stop('define a numeric ebx_parent_group')
  }
  if (missing(ebx_child_group)) {
    stop('define a numeric ebx_child_group')
  }

  if (ebx_parent_group == ebx_child_group) {
    # this is the case of a grouping which is not modeled explicitly,
    # but has all levels lumped together: show the grouping to self and stop
    return (list(c(ebx_parent_group, ebx_child_group)))
  }

  query <- ebx5.gr_data[ebx5.gr_data$from == ebx_parent_group,'to']
  if (length(query) == 0) {
    # this parent has no children
    return (list())
  }

  result <- list()
  for (child in query) {
    if (child == ebx_parent_group | child == ebx_child_group) {
      if (child == ebx_parent_group) {
        # this is a group pointing to itself, so hierarchy ends here
        if (child == ebx_child_group) {
          result[[length(result)+1]] <- c(child, child)
        }
      }
      else {
        # we found the solution
        result[[length(result)+1]] <- c(ebx_parent_group, ebx_child_group)
      }
    }
    else {
      # recursively resolve the child
      indirect <- GetGroupConnections(child, ebx_child_group)
      for (partialResult in indirect) {
        # add the partialResult to the final result
        result[[length(result)+1]] <- c(ebx_parent_group, partialResult)
      }
    }
  }
  return(result)
}
