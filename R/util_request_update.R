#' @title Perform an update request to faoebx5; internal
#'
#' @description lookup the sdmx_name, and forward the request for for action to faoebx5
#'
#' @inheritParams request_read
#' @param .data the \code{\link[base]{data.frame}} containing all columns to be be inserted
#' @param .verb ebx5 update verb ('insert','update','delete')
#'
#' @return boolean
#'
#' @details The data columns provided must follow the code-list which is being updated.
#' All keys of the code-list must be given, as well as any fields which are mandatory.
#' Using the SOAP-API the column name is not the label visible in EBX, but the field name.
#' Requires that the EBX5 connection was configured using \code{\link{SetupEBXConnection}}.
#'
#' @importFrom faoebx5 GetEBXConnection EBXInsert EBXUpdate EBXRemove
#'
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
request_update <- function(.data, .sdmx_name, .verb, .isCodeList = TRUE) {

  if(missing(.data) || missing(.sdmx_name)) {
    stop('Please, provide the data and table name.')
  }

  if (!.verb %in% c('insert','update','delete')) {
    stop('Please, provide a valid verb.', .verb)
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
    branch   <- as.character(ebx5.gr_data$Branch[ebx5.gr_data$Acronym == .sdmx_name])
    instance <- as.character(ebx5.gr_data$Instance[ebx5.gr_data$Acronym == .sdmx_name])
    folder   <- as.character(ebx5.gr_data$Folder[ebx5.gr_data$Acronym == .sdmx_name])
    cl_name  <- as.character(ebx5.gr_data$Name[ebx5.gr_data$Acronym == .sdmx_name])
  }

  if (nchar(branch)==0 | nchar(instance)==0 | nchar(cl_name)==0 | nchar(folder)==0) {
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
  if (.verb == 'insert') {
    return ( EBXInsert(branch=branch, instance=instance, folder=folder, folder2=folder2,
                       table=cl_name, data=.data))
  }
  if (.verb == 'update') {
    return ( EBXUpdate(branch=branch, instance=instance, folder=folder, folder2=folder2,
                       table=cl_name, data=.data))
  }
  if (.verb == 'delete') {
    return ( EBXRemove(branch=branch, instance=instance,
                       table=cl_name, data=.data))
  }
}

