# Utility functions for dartTools ETL
# 
# Author: Marvin Steijaert
###############################################################################

#' utility function to download files from web and ftp locations
#' @param url url
#' @param directory output directory 
#' @param filename name of output file (if NULL, this is automatically generated from url)
#' @param writeLog Logical value indicating if a log file is written with original comamnd and timestamp
#' @return NULL
#' @author Marvin Steijaert
#' @export
downloadWebOrFtp <- function(url, directory, filename=NULL, writeLog = TRUE){
  if(is.null(filename)) filename <- gsub("^.*\\=(.*)$","\\1",basename(url))
#  finalFilename <- file.path(directory, gsub("\\.gz$","", gsub("\\.zip$","", filename)))
  finalFilename <- normalizePath(file.path(directory, filename), mustWork = FALSE)
  if(file.exists(finalFilename)){
    cat("File",finalFilename,"already exists and will not be downloaded again.\n")
  } else {
    dir.create(directory, showWarnings = FALSE, recursive = TRUE)
    curl::curl_download(url, finalFilename)
    if(writeLog){
      logFilepath <- paste0(finalFilename,".log")
      logLines <- c(
          sprintf("filename: %s",filename),
          sprintf("url: %s",url),
          paste("download date:", format(Sys.time(), usetz=TRUE))
      )
      writeChar(paste(logLines, collapse = "\n"), con = logFilepath)
    }
  }
  return(invisible(finalFilename))
}


#' utility function to download files from web and ftp locations and read them as data.table
#' @param url url
#' @param directory output directory 
#' @param filename name out output file (if NULL, this is automatically generated from url)
#' @param fileInArchive name of file in zip archive
#' @param ... Extra parameters passed to the file reading function (data.table::fread() or openxlsx::read.xlsx())
#' @return NULL
#' @author Marvin Steijaert
#' @importFrom openxlsx read.xlsx
#' @export
readWebOrFtp <- function(url, directory, filename=NULL, fileInArchive = NULL, ...){
  finalFilename <- downloadWebOrFtp(url = url, directory = directory, filename = filename)
  if(!file.exists(finalFilename)) stop("File or directory ", finalFilename, " does not exist.")
  if(dir.exists(finalFilename)) stop(finalFilename, "is a directory, not a single file.")
  
  if (grepl("xlsx$",finalFilename)){
    res <- data.table(read.xlsx(xlsxFile = finalFilename, ...))
  } else if (grepl("zip$",finalFilename) && !is.null(fileInArchive)){
    res <- fread(cmd = paste("unzip -p", finalFilename, fileInArchive), ...)
  } else {
    # note: gzipped files are read by fread if R.utils package is installed 
    res <- fread(finalFilename, ...)
  }
  return(res)
}

removeReplacedTermsFromOBO <- function(inputFile, outputFile = inputFile){
  fileContents <- readLines(inputFile)
  termBreaks <- which(fileContents=="")
  linesWithReplaced <- which(grepl("^replaced_by.*$",fileContents))
  skippedLines <- c()
  for (thisLine in linesWithReplaced){
    thisSection <- (termBreaks[tail(which(termBreaks<thisLine),1)]+1) : termBreaks[head(which(termBreaks>thisLine),1)]
    skippedLines <- c(thisSection, skippedLines)
  } 
  writeLines(fileContents[-skippedLines], outputFile)
}

removeObsoleteTermsFromOBO <- function(inputFile, outputFile = inputFile){
  fileContents <- readLines(inputFile)
  termBreaks <- which(fileContents=="")
  linesWithReplaced <- which(grepl("is_obsolete: true$",fileContents))
  skippedLines <- c()
  for (thisLine in linesWithReplaced){
    thisSection <- (termBreaks[tail(which(termBreaks<thisLine),1)]+1) : termBreaks[head(which(termBreaks>thisLine),1)]
    skippedLines <- c(thisSection, skippedLines)
  } 
  writeLines(fileContents[-skippedLines], outputFile)
}