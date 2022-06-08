# R6 class for dealing with data
# 
# Author: Marvin Steijaert
###############################################################################

# https://cran.r-project.org/web/packages/R6/vignettes/Introduction.html

# R6 Classed defined by this package:
# * Data
# * Substances (subclass of Data)
# * PhenoPathway (subclass of Substances)
# * DartDB (subclass of PhenoPathway)
# * PathwayRanking

#' Class for storing data
#' 
#' @export
#' @import R6
#' @import data.table
#' @importFrom R.utils gzip
#' @importFrom yaml yaml.load_file write_yaml
Data <- R6Class("Data",
    public = list(
        #' @field tableColumns Named list with names of all the columns for each table
        tableColumns = list(), # named list with columns of all tables in self$tables
        #' @field tableNames Names of tables
        tableNames = c(), # names of tables in self$tables
        #' @field tables List with the actual data.table objects
        tables = list(), # named list with data tables
        #' @field columnConversionFunctions Named vector with functions (e.g. as.character) that applied after loading a columns with that name in any of the tables
        columnConversionFunctions = c(),
        #' @field metadata Named list with metadata
        metadata = list(),
        #' @description Create a new Data object
        initialize = function(){
          self$tableNames <- names(self$tableColumns)
          self$tables <- lapply(self$tableColumns, emptyDT) # empty tables
          self$checkTableConventions()
        },
        #' @description Print a summary of the contents of this object
        print = function(){
          cat("Instance of class", self$classname,"\n")
          tmp <- mapply(function(x,y){
                namesString <- if(ncol(y)>30){
                      paste(c(names(y)[1:29],"...",tail(names(y),1)),collapse=", ")
                    } else paste(names(y),collapse=", ")
                cat("Table ",x," (",ncol(y)," columns, ",nrow(y)," rows)\ncolumns: ",
                    namesString,"\n\n", sep = "")
              } ,
              self$tableNames, self$tables)
          invisible()
        },
        #' @description Dump the database to files
        #' @param dumpPath Directory path to which the output files are written
        #' @param dumpType Type of dump (currently only 'txt' is supported)
        #' @param gzip Set TRUE if dumped tables need to be gzipped
        #' @param overwrite If TRUE, existing files are overwritten
        dump = function(dumpPath = getOption("dartpaths_default_dumppath"), dumpType = "txt", gzip = FALSE, overwrite = FALSE) {
          tmp <- lapply(self$tableNames, function(x) self$writeTxtTable(dumpPath, x, gzip, overwrite))
          write_yaml(self$metadata, file.path(dumpPath,"metadata.yml"))
          message("Successfully created a database dump in ", dumpPath, " \n")
          invisible()
        },
        #' @description Load dumped files into the database
        #' @param dumpPath Directory path to which the output files are written
        #' @param dumpType Type of dump (currently only 'txt' is supported)
        #' @param verbose If TRUE, some extra information is printed
        #' @details If dumpPath refers to a zip file, this is assumed to be a zipped dump directory
        #' @return None
        #' @importFrom utils unzip
        loadDump = function(dumpPath = getOption("dartpaths_default_dumppath"), dumpType = "txt", verbose = FALSE) {
          dbHasRecords <- any(sapply(self$tables, function(x) x[,.N > 0]))
          if(dbHasRecords) stop("The target database object already contains records.\nThis method should only be used to load data into an empty database object")
          if(dumpType == "txt"){
            time <- proc.time()
            if(grepl("\\.zip$",dumpPath)){
              unzippedDir <- tempdir()
              unzip(zipfile = dumpPath, exdir = unzippedDir)
              unzippedSubdir <- file.path(unzippedDir,gsub("(.*).zip$","\\1",basename(dumpPath)))
              if(!dir.exists(unzippedSubdir)) stop("extracted zip data is not in expected directory ", unzippedSubdir)
              self$loadDump(dumpPath = unzippedSubdir, dumpType = "txt")
              unlink(unzippedDir, recursive = TRUE)
            } else {
              yamlFile <- file.path(dumpPath,"metadata.yml")
              if(file.exists(yamlFile)) self$metadata <- yaml.load_file(yamlFile)
              success <- sapply(self$tableNames, function(x){
                    thisTable <- readTxtTable(dumpPath,x)
                    if(verbose) print(sprintf("Loaded table %s (%i rows, %i columns)", x, nrow(thisTable), ncol(thisTable)))
                    if(is.null(thisTable)){
                      return(FALSE)
                    } else {
                      namesFound <- sort(names(thisTable))
                      namesRef <- sort(self$tableColumns[[x]])
                      if(! identical(namesFound, namesRef)){
                        stop("Mismatch between dump file and database definition for table ", x,
                            "\nDump file columns: ", paste(namesFound,collapse =","),
                            "\nDatabase definition columns: ", paste(namesRef,collapse =",")
                        )
                      }
                      
                      # apply conversions (e.g. type casting) specified in self$columnConversionFunctions
                      columnsToConvert <- intersect(names(thisTable), names(self$columnConversionFunctions))
                      for (thisColumn in columnsToConvert){
                        thisConversion <- self$columnConversionFunctions[[thisColumn]]
                        set(thisTable,,thisColumn, thisConversion(thisTable[[thisColumn]]))
                      }
                      
                      self$tables[[x]] <- thisTable
                      return(TRUE)
                    }
                  })
              if(all(success)){
                message("Successfully loaded database dump from ", dumpPath, "\n")
              } else if(any(success)){
                message("Successfully loaded database dump for ", sum(success)," / ",length(success)," tables from ", dumpPath, "\n")
              } else stop("None of the database tables could be found in dumpPath")
            }
            delta = proc.time() - time
            cat("Database load took", unname(delta["elapsed"]), "seconds\n")
          } else stop("invalid dumpType value")
          return(invisible())
        },
        #' @description Print all tables in the database
        #' @param nrows The number of rows which will be printed before truncation is enforced. 
        #' @param topn The number of rows to be printed from the beginning and end of tables with more than \code{nrows} rows.
        #' @return None 
        printAllTables = function(nrows = 5, topn = 2){
          ncols = 30
          for(thisTable in self$tableNames){
            cat("Table",thisTable,"\n")
            cat("---------------------------------------------------------\n")
            ncolsThisTable <- self$ncols(thisTable)
            if(ncolsThisTable > ncols){
              cat("Only showing first",ncols," out of ", ncolsThisTable ,"columns.\n")
              print(self$getData(thisTable)[,1:ncols], nrows = nrows, topn = topn)
            } else {
              print(self$getData(thisTable), nrows = nrows, topn = topn)
            }
            cat("=========================================================\n")
          } 
        },
        #' @description Get a table from the database
        #' @param tablename Name of the table
        #' @param copy If TRUE, a copy is made of the table. If FALSE, the original table is returned.
        #' @param allowEmpty If FALSE, an error is raised if the table is empty.
        #' @return data.table object
        getData = function(tablename, copy = TRUE, allowEmpty = TRUE){
          if(!tablename %in% self$tableNames) stop("Invalid tablename:",tablename,". Only the following names are allowed:\n",paste("  -",self$tableNames, collapse="\n"))
          if(!allowEmpty && nrow(self$tables[[tablename]]) == 0){
            stop(sprintf("Table %s does not (yet) contain any data.",tablename))
          }
          if(copy){
            return(data.table::copy(self$tables[[tablename]]))
          } else {
            return(self$tables[[tablename]])
          }
        },
        #' @description Get number of rows
        #' @param tablename Name of the table
        #' @return scalar
        nrows = function(tablename){
          if(!tablename %in% self$tableNames) stop("Invalid tablename:",tablename,". Only the following names are allowed:\n",paste("  -",self$tableNames, collapse="\n"))
          nrow(self$tables[[tablename]])
        },
        #' @description Get number of columns
        #' @param tablename Name of the table
        #' @return scalar
        ncols = function(tablename){
          if(!tablename %in% self$tableNames) stop("Invalid tablename:",tablename,". Only the following names are allowed:\n",paste("  -",self$tableNames, collapse="\n"))
          ncol(self$tables[[tablename]])
        },
        
        #' @description Add rows to a table from the database
        #' @param tablename Name of the table
        #' @param newdata Data to be added to the table
        #' @param fill TRUE fills missing columns in newdata with NAs. FALSE throws an error if columns are missing.
        #' @param ignoreNewColumns If FALSE, an error is given if newdata contains new columns. If TRUE, new columns are ignored.
        #' @param overwrite if TRUE, the existing table is entirely replaced by newdata
        #' @return None
        addData = function(tablename, newdata, fill = FALSE, ignoreNewColumns = TRUE, overwrite = FALSE){
          if(!fill && !all(self$tableColumns[[tablename]] %in% names(newdata))){
            stop("newdata does not contain the following columns:", paste(setdiff(self$tableColumns[[tablename]], names(newdata)), collapse = ","))
          }
          if(!all(names(newdata) %in% self$tableColumns[[tablename]])){
            if(ignoreNewColumns){
              warning("newdata contains the following columns that are not part of table ",tablename, "(and will be ignored) :\n",
                  paste(setdiff(names(newdata),self$tableColumns[[tablename]]), collapse = ","))
              selectedColumnNames <- intersect(self$tableColumns[[tablename]], names(newdata))
              newdata <- newdata[, selectedColumnNames , with = TRUE]
            } else stop("newdata contains the following columns that are not part of table ",tablename, ":\n",
                  paste(setdiff(names(newdata),self$tableColumns[[tablename]]), collapse = ","))
          }
          if(overwrite){
            self$tables[[tablename]] <- newdata
          } else {
            self$tables[[tablename]] <- unique(rbindlist(list(self$tables[[tablename]], newdata), fill = TRUE, use.names = TRUE))
          }
          invisible()
        },
        
        #' @description Add a new table to the database. Should be handled with care!
        #' @param tablename Name of the table
        #' @param tablecontents Data to be added to the new table
        #' @return None
        addTable = function(tablename, tablecontents){
          tablecontents <- setDT(unique(tablecontents))
          tableColumnNames <- names(tablecontents)
          if(any(sapply(tableColumnNames,nchar)==0)) stop("tablecontents should only contain non-empty column names")
          if(length(unique(tableColumnNames)) !=  length(tableColumnNames)) stop("tablecontents should only contain unique column names")
          if(toLower(tablename) %in% toLower(self$tableNames)) stop("table with same name (case-insensitive) already exist")
          
          self$tableNames[length(self$tableNames)+1] <- tablename
          self$tableColumns[[tablename]] <- tableColumnNames
          self$tables[[tablename]] <- tablecontents
          invisible()
        },
        
        #' @description check if table definitions follow conventions (used during initialization) 
        #' @return NULL
        #' @details throws error if conventions are violated
        checkTableConventions = function(){
          if(!all.equal(self$tableNames, tolower(self$tableNames))){
            stop("tableNames should be all lowercase")
          }
          allColumnNames <- unname(unlist(self$tableColumns))
          if(!all.equal(allColumnNames, tolower(allColumnNames))){
            stop("column names of all tables should be lowercase")
          }
        },
        #' @description to write a text dump for a single table
        #' @param dumpPath Directory path to which the output file is written
        #' @param tablename Name of table, used to create the output file name
        #' @param gzip Logical. If TRUE, the file is gzipped
        #' @param overwrite Logical. If TRUE, an existing file is overwritten
        #' @return None
        writeTxtTable = function(dumpPath, tablename, gzip = FALSE, overwrite = FALSE){
          dir.create(dumpPath, showWarnings = FALSE, recursive = TRUE)
          outputFile <- file.path(dumpPath,paste0(tablename,".txt"))
          outputFileGz <- paste0(outputFile,".gz")
          for (f in c(outputFile, outputFileGz)){
            if(file.exists(f) && overwrite == FALSE){
              warning("Dump file already exists for ", tablename, ". Skipping this table.")
              return(invisible())
            }
          }
          if(self$ncols(tablename) > 0){
            fwrite(self$tables[[tablename]], outputFile, sep='\t', na = "NA")
            if(gzip) gzip(filename=outputFile, destname=outputFileGz)
          }
        }
    )
)


#' Function to read dumped tables from text files
#' @param dumpPath Path of directory containing text file
#' @param tablename Name of table (i.e., basename of the text file)
#' @return data.table object
#' @author Marvin Steijaert
readTxtTable <- function(dumpPath, tablename){
  foundFiles = list.files(dumpPath, pattern = paste0("^",tablename, "[.]txt(.gz)*$"), full.names = TRUE)
  if(length(foundFiles)>1) stop("multiple files were found for table name", tablename)
  if(length(foundFiles)==0){
    warning("Dump file could not be found for table ", tablename)
    return(NULL)
  } else {
    return(fread(foundFiles, na.strings="NA"))
  }
}

#' Function to create a data.table with specified columns and zero rows
#' @param columnNames Vector with column names
#' @return data.table object
#' @author Marvin Steijaert
emptyDT <- function(columnNames){
  dt <- as.data.table(lapply(columnNames,function(x) logical(0)))
  setnames(dt, columnNames)
  dt
}

