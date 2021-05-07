#' @title Get FishStat CSV files
#'
#' @description CSV files are files, which are created as download file. They are virtual or synthetic, as
#' the attributes are picked from a single EBX code-list. Additional attributes can be defined from
#' parent hierachies. The grouping is resolved by fishstatR, and shown with the defined column header in the CSV.
#' It is not possible to mix attributes from several code-lists.
#'
#' @return Returns an object of the class \code{\link[data.table]{data.table}}
#' Idetifier = to lokup attributes
#' Acronym = the name of the CSV file (passed to ReadCSVfile)
#' Codelist = codel-list CSV is based on
#'
#' @export
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
GetCSVfiles <- function() {

  return (ReadEBXCodeList('CSV_METADATA_CONCEPT'))
}
