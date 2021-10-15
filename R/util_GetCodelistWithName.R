utils::globalVariables(c("."))
#' @title Set the name column, as defined in Metadata
#'
#' @param metadata FishStat metadata; obtained using \code{\link{ReadMetadata}}
#' @param fsj_ConceptID FishStat Concept ID
#'
#' @description This function reads the codelist and adds a Name column
#'
#' @return a data frame with a computed Name column
#'
#' @importFrom data.table data.table
#'
#' @export
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
GetCodelistWithName <- function(metadata, fsj_ConceptID) {

  if (nrow(metadata$Concept[metadata$Concept$Identifier == fsj_ConceptID,]) == 0) {
    stop('fsj_ConceptID <',fsj_ConceptID,'> is not defined in metadata$Concept')
  }

  result <- GetCodelistByID(metadata$Concept[metadata$Concept$Identifier == fsj_ConceptID,'EBXCodelist'])
  if("Name" %in% colnames(result)) {
    return (result)
  }

  ThisConcept <- metadata$Concept[metadata$Concept$Identifier == fsj_ConceptID,]
  if (is.na(ThisConcept$NameColumn)) {
    result$Name <- result$Name_En
    return (result)
  }

  # get column names
  ColumnNames <- strsplit(ThisConcept$NameColumn, split="+", fixed=TRUE)
  ColumnNames <- ColumnNames[[1]]

  # extract columns
  newCols <- result[, ColumnNames]

  # concatenate
  result$Name <- do.call(paste, as.data.frame(newCols, stringsAsFactors=FALSE))

  return (result)
}
