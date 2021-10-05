utils::globalVariables(c("."))
#' @title Lookup a unit value; internal
#'
#' @param metadata FishStat metadata; obtained using \code{\link{ReadMetadata}}
#' @param unit_value the unit value, or comma separated unit values
#'
#' @description This function retruns the full unit information from metadata
#'
#' @return a data frame with one or more (usually 2) rows
#'
#' @importFrom data.table data.table
#'
#' @export
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
LookupUnit <- function(metadata, unit_value) {

  #values <- strsplit("Q_tlw,Q_no_1", ",")
  values <- strsplit(unit_value, ",")
  return (metadata$Unit[metadata$Unit$Code %in% values[[1]],])
}
