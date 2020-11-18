#' @title Perform a read request to faoebx5; internal
#'
#' @description lookup the sdmx_name, and forward the request for for action to faoebx5
#'
#' @param .sdmx_name table name, in SDMX style.
#' Available code lists are shown by function \code{\link{GetEBXCodeLists}}
#' and Groups are shown using by function \code{\link{GetEBXGroups}}
#' The actual codelist location in EBX5 (branch, instance, code-list-name) are resolved
#' using the metadata structure.
#' @param .isCodeList TRUE to update a code-list, FALSE to update a group
#'
#' @return boolean
#'
#' @details The data columns provided must follow the code-list which is being updated.
#' All keys of the code-list must be given, as well as any fields which are mandatory.
#' Using the SOAP-API the column name is not the label visible in EBX, but the field name.
#' Requires that the EBX5 connection was configured using \code{\link{SetupEBXConnection}}.
#'
#'
#' @importFrom faoebx5 GetEBXConnection EBXRead
#'
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
request_read <- function(.sdmx_name, .isCodeList = TRUE) {

  if(missing(.sdmx_name)) {
    stop('Please, provide the table name.')
  }

  #-- connection details ----
  connection <- getEbxConnection()

  #-- read metadata; if not already loaded
  if (.isCodeList) {
    ebx5.cl_data <- GetEBXCodeLists(connection)
    if (length(ebx5.cl_data$Name[ebx5.cl_data$Acronym == .sdmx_name]) == 0) {
      stop('Cannot find a codelist with acronym=<', .sdmx_name, '> defined in EBX metadata')
    }
    #-- resolve the acutal location using metadata ----
    branch   <- as.character(ebx5.cl_data$Branch[ebx5.cl_data$Acronym == .sdmx_name])
    instance <- as.character(ebx5.cl_data$Instance[ebx5.cl_data$Acronym == .sdmx_name])
    folder   <- as.character(ebx5.cl_data$Folder[ebx5.cl_data$Acronym == .sdmx_name])
    cl_name  <- as.character(ebx5.cl_data$Name[ebx5.cl_data$Acronym == .sdmx_name])
  }
  else {
    ebx5.gr_data <- GetEBXGroups(connection)
    if (length(ebx5.gr_data$Name[ebx5.gr_data$Acronym == .sdmx_name]) == 0) {
      stop('Cannot find a group with acronym=<', .sdmx_name, '> defined in EBX metadata')
    }
    #-- resolve the acutal location using metadata ----
    branch   <- as.character(ebx5.gr_data$Branch[ebx5.cl_data$Acronym == .sdmx_name])
    instance <- as.character(ebx5.gr_data$Instance[ebx5.cl_data$Acronym == .sdmx_name])
    folder   <- as.character(ebx5.gr_data$Folder[ebx5.cl_data$Acronym == .sdmx_name])
    cl_name  <- as.character(ebx5.gr_data$Name[ebx5.cl_data$Acronym == .sdmx_name])
  }

  if (is.na(branch) | is.na(instance) | is.na(cl_name) | is.na(folder)) {
    stop('Cannot find branch,instance,folder for ', .sdmx_name)
  }

  ### get folder2 if one is specified
  folder2 <- ''
  if (lengths(strsplit(folder, "/")) == 2) {
    folderlist <- strsplit(folder, "/")[[1]]
    folder <- folderlist[[1]]
    folder2 <- folderlist[[2]]
  }

  #-- action to EBX5
  return ( EBXRead(branch=branch, instance=instance, folder=folder, folder2=folder2, table=cl_name))
}
