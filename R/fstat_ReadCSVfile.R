#' @title Read FishStat CSV files
#'
#' @description CSV files are files, which are created as flat structure. They are virtual or synthetic, as
#' the attributes are picked from a single EBX code-list. Additional attributes can be defined from
#' parent hierachies. The grouping is resolved by fishstatR, and shown with the defined column header in the CSV.
#' It is not possible to mix attributes from several code-lists.
#' This function returns a data.frame which is ready (as-is) to be saved to a CSV file.
#'
#' @param csv_file acronym obtained by \code{\link{GetCSVfiles}}
#'
#' @return Returns the CSV file as an object of the class \code{\link[data.frame]{data.frame}}
#'
#' @export
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
ReadCSVfile <- function(csv_file) {

  # read CSV metadata
  csvFiles <- ReadEBXCodeList('CSV_METADATA_CONCEPT')
  csvAttributes <- ReadEBXCodeList('CSV_METADATA_ATTRIBUTE')
  # read EBX metadata
  ebxAttributes <- GetEBXAttributes()

  # resolve the CSV file name
  if (nrow(csvFiles[csvFiles$Acronym == csv_file,]) == 0) {
    stop('Cannot find [csv_file] in ReadEBXCodeList(CSV_METADATA_CONCEPT)')
  }
  csvFile <- csvFiles[csvFiles$Acronym == csv_file,]

  # get CSV attributes
  attributes <- csvAttributes[csvAttributes$CSV_Concept == csvFile$Identifier,]
  if (nrow(attributes) == 0) {
    stop('Cannot attributes for [csv_file] in ReadEBXCodeList(CSV_METADATA_ATTRIBUTE)')
  }

  # add the attribute (column) name
  attributes0 <- ebxAttributes[,c('Identifier','EBX_Name','EBX_Codelist','Type')]
  colnames(attributes0)[colnames(attributes0)=="Identifier"] <- "EBX_Attribute"
  attributes <- merge(attributes, attributes0, by='EBX_Attribute')

  ###############################################################################
  # first, do base code-list attributes
  attributes1 <- attributes[attributes$EBX_Codelist==csvFile$EBX_Codelist, ]

  # divive by ML and not ML
  ml_attributes <- attributes1[attributes1$Type == 'MULTILINGUAL_STRING', ]
  nonML_attributes <- attributes1[attributes1$Type != 'MULTILINGUAL_STRING', ]

  # for ML, append all 6 language codes
  if (nrow(ml_attributes) > 0) {
    base_attributes <- createMlEBxnames(ml_attributes)
    base_attributes <- rbind(base_attributes, nonML_attributes)
  } else {
    base_attributes <- nonML_attributes
  }

  # get the main code-list
  codelist <- GetCodelistByID(csvFile$EBX_Codelist)

  # get columns from code-list
  col_names <- base_attributes[base_attributes$EBX_Codelist==csvFile$EBX_Codelist,'EBX_Name']
  # filter out columns which don't exist in code-list
  col_names <- col_names[col_names %in% names(codelist)]
  # Identifier required for grouping
  if (sum(col_names %in% "Identifier") == 0) {
    col_names <- c(col_names,'Identifier')
  }

  # subset the columns
  codelist <- codelist[,col_names]

  # rename columns
  for (myrow in 1:nrow(base_attributes)) {
    row <- base_attributes[myrow,]
    if (!is.na(row$Acronym)) {
      colnames(codelist)[colnames(codelist)==row$EBX_Name] <- row$Acronym
    }
  }

  ###############################################################################
  # second, group lookup
  attributes1 <- attributes[attributes$EBX_Codelist!=csvFile$EBX_Codelist, ]

  # divive by ML and not ML
  ml_attributes <- attributes1[attributes1$Type == 'MULTILINGUAL_STRING', ]
  nonML_attributes <- attributes1[attributes1$Type != 'MULTILINGUAL_STRING', ]

  # for ML, append all 6 language codes
  if (nrow(ml_attributes) > 0) {
    ml_attributes$EBX_Name <- paste0(ml_attributes$EBX_Name,"_En")
    attributes2 <- rbind(ml_attributes, nonML_attributes)
  } else {
    attributes2 <-  nonML_attributes
  }

  for (myrow in 1:nrow(attributes2)) {
     group <- attributes2[myrow,]

     # get the group code-list
     codelist2 <- GetCodelistByID(group$EBX_Codelist)
     codelist2 <- codelist2[,c('Identifier', group$EBX_Name)]
     colnames(codelist2)[colnames(codelist2)=="Identifier"] <- "Group"

     # read grouping from EBX
     grouping <- ReadEBXHierarchy(group$EBX_Codelist,csvFile$EBX_Codelist)
     grouping <- grouping[,1:2]
     colnames(grouping)<- c("Group","Identifier")

     # combine with group, adding member Identifier
     codelist2 <- merge(codelist2, grouping, by="Group")
     codelist2$Group <- NULL

     # rename to final column name
     colnames(codelist2)[colnames(codelist2)==group$EBX_Name] <- group$Acronym

     # merge with code-list
     codelist <- merge(codelist, codelist2, by="Identifier")
  }

  base_attributes <- rbind(base_attributes, attributes2)
  ###############################################################################

  # sort attributes
  base_attributes <- base_attributes[order(base_attributes$Sort),]

  # get column names (either the attribute name, or acronym if specified)
  col_names <- ifelse(is.na(base_attributes$Acronym), base_attributes$EBX_Name, base_attributes$Acronym)

  # remove columns not found in codelist (example: Name_Ru)
  col_names <- col_names[col_names %in% names(codelist)]

  # filter and sort columns
  codelist <- codelist[, col_names]

  return(codelist)
}

# utility function to duplicate a row, adding the 6 languages codes
createMlEBxnames <- function(df) {
  result <- df[0,]
  for (myrow in 1:nrow(df)) {
    line <- df[myrow, ]
    name <- line$EBX_Name
    acronym <- line$Acronym

    line$EBX_Name <- paste0(name,'_En')
    if (!is.na(line$Acronym)) {
      line$Acronym <- paste0(acronym,'_En')
    }
    result <- rbind(result, line)

    line$EBX_Name <- paste0(name,'_Fr')
    if (!is.na(line$Acronym)) {
      line$Acronym <- paste0(acronym,'_Fr')
    }
    result <- rbind(result, line)

    line$EBX_Name <- paste0(name,'_Es')
    if (!is.na(line$Acronym)) {
      line$Acronym <- paste0(acronym,'_Es')
    }
    result <- rbind(result, line)

    line$EBX_Name <- paste0(name,'_Ar')
    if (!is.na(line$Acronym)) {
      line$Acronym <- paste0(acronym,'_Ar')
    }
    result <- rbind(result, line)

    line$EBX_Name <- paste0(name,'_Cn')
    if (!is.na(line$Acronym)) {
      line$Acronym <- paste0(acronym,'_Cn')
    }
    result <- rbind(result, line)

    line$EBX_Name <- paste0(name,'_Ru')
    if (!is.na(line$Acronym)) {
      line$Acronym <- paste0(acronym,'_Ru')
    }
    result <- rbind(result, line)
  }
  return (result)
}
