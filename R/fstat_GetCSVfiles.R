#' @title Get FishStat CSV files
#'
#' @description CSV files are files, which are created as flat structure. They are virtual or synthetic, as
#' the attributes are picked from a single EBX code-list. Additional attributes can be defined from
#' parent hierachies. The grouping is resolved by fishstatR, and shown with the defined column header in the CSV.
#' It is not possible to mix attributes from several code-lists.
#'
#' @return Returns an object of the class \code{\link[data.table]{data.table}}
#'
#' @export
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
GetCSVfiles <- function() {

  return (ReadEBXCodeList('CSV_METADATA_CONCEPT'))
}
