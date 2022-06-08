# Using Rdkit from R
# 
# Author: Marvin Steijaert
###############################################################################

# set names of columns to NULL to avoid R CMD check complain about "no visual binding"
# https://www.r-bloggers.com/2019/08/no-visible-binding-for-global-variable/
smilesOrig <- NULL

#' python system call
#'
#' use system call to apply Rdkit normalization to SMILES representation of chemical structures
#'
#' @param inputVec single string or a vector with strings to be sent to python script via stdin
#' @param pyfile python file to be executed via system call
#' @param pythonPath full path of python executable
#' @param header If TRUE, the first array in the returned json is used as a header for the returned table
#' @param outputClass Class name of returned object (default: data.table)
#' @param useFiles If TRUE, files are used to exchange files between python and R. If FALSE, stdin/stdout is used.
#' @return data.table object
#' @author Marvin Steijaert
#' @importFrom jsonlite fromJSON
#' @import data.table
#' @import utils
callPythonScript <- function(inputVec, pyfile, pythonPath = getOption("dartpaths_python"),
    header = TRUE, outputClass = "data.table", useFiles = FALSE){
  
  if(length(inputVec) == 0) return(NULL)
  
  pythonPath <- normalizePath(pythonPath, mustWork = TRUE)
  pythonScript <- normalizePath(system.file("python", pyfile, package = "dartTools"), mustWork = TRUE)
  
  if (useFiles){
    inputFile <- tempfile(pattern = "std_input_",fileext = ".txt")
    outputFile <- tempfile(pattern = "std_output_",fileext = ".json")
    con <- file(inputFile, open = "w")
    writeLines(inputVec, con)
    close(con)
    exitcode <- system2(pythonPath,
        c(pythonScript, "--input_file", inputFile,
            "--output_file", outputFile))
    if(exitcode != 0) stop("Python script ", pythonScript, " failed with exit code ", exitcode,
          " while processing input file ",inputFile,".")
    res <- as.data.table(fromJSON(readLines(outputFile, warn = FALSE),
            simplifyDataFrame=TRUE))
    # replace empty strings by NA
    res <- res[,lapply(.SD, function(x) ifelse(x=="",NA,x))]
    # convert columns to most appropriate data type (as.is = TRUE avoids factors)
    res <- res[,lapply(.SD, function(x) type.convert(x, numerals = "no.loss", as.is = TRUE))]
    unlink(inputFile)
    unlink(outputFile)
  } else {
    res <- system2(pythonPath, pythonScript, input = inputVec, stdout = TRUE)
    if (isTRUE(attr(res,"status"))){
      stop("something went wrong while processing input using ", pythonScript)
    }
    if(!is.null(outputClass) && outputClass == "data.table"){
      res <- as.data.table(fromJSON(res), simplifyDataFrame=TRUE)
      if(header){
        res <- setnames(res[2:.N,], unname(unlist(res[1,])))
      }
      # replace empty strings by NA
      res <- res[,lapply(.SD, function(x) ifelse(x=="",NA,x))]
      # convert columns to most appropriate data type (as.is = TRUE avoids factors)
      res <- res[,lapply(.SD, function(x) type.convert(x, numerals = "no.loss", as.is = TRUE))]
    } else {
      res <- fromJSON(res, simplifyDataFrame=TRUE)
    }
  }
  return(res)
}

#' standardize smiles
#'
#' apply Rdkit normalization via reticulate binding to SMILES representation of chemical structures
#'
#' @param smiles single SMILES string or a vector with multiple SMILES
#' @param pythonPath full path of python executable
#' @param cache If TRUE, a cache file will be used to avoid repeated standardization of the same SMILES
#' @param chunkSize Number of smiles per chunk sent to standardizer
#' @return data.table object with standardized smiles
#' @details Rdkit (python) is called to take care of the standardization
#' @author Marvin Steijaert
#' @export
#' @importFrom jsonlite fromJSON
#' @import data.table
#' @examples \dontrun{
#' smiles <- c("N[C@@H](C)C(=O)O","O1CCCC[C@@H]1C")
#' result <- standardizeSmiles(smiles)
#' }
standardizeSmilesRdkit <- function(smiles, pythonPath = getOption("dartpaths_python"), cache = TRUE, chunkSize = 1000){
  
  if (!is.character(smiles)) stop("'smiles' must be a character vector")
  cacheFile <- getOption("dartpaths_smilescache", file.path(tempdir(), "smilesCache.txt"))
  
  # cacheTable is used both for caching and for avoiding standardization of replicates
  cacheTable <- if(cache && file.exists(cacheFile)) fread(cacheFile, fill = TRUE) else emptyDT(c("smilesOrig", "smiles"))
  smilesNew <- setdiff(smiles, cacheTable[,smilesOrig])
  if(length(smilesNew) > 0){
    smilesChunks = split(smilesNew,ceiling(seq_along(smilesNew)/chunkSize))
    newResultsList = list()
    listItem = 0
    for(thisChunk in smilesChunks){
      listItem = listItem + 1
      elapsed <- system.time({
            newResultsList[[listItem]] <- as.data.table(standardizeSmilesList(thisChunk))
          })
      print(sprintf("Chunk %d/%d: standardized %d smiles in %f seconds",
              listItem, length(smilesChunks), length(thisChunk), unname(elapsed[3])))
    }
    newResults <- rbindlist(newResultsList)
    setnames(newResults, c("smiles"))
    cacheTable <- rbindlist(list(cacheTable,cbind("smilesOrig" = smilesNew, newResults)), fill = TRUE)
    cacheTable <- cacheTable[,.SD[1], by = smilesOrig] # avoid duplicates or ambiguity
    if(cache) fwrite(cacheTable[!smiles %in% c("",NA_character_)], cacheFile, sep = "\t", quote = FALSE, na = NA)
    
  }
  inputSmiles = smiles # avoid ambiguity wrt object name
  return(cacheTable[.(inputSmiles), on = "smilesOrig"][,smilesOrig := NULL])
}


#' calculate Morgan fingerprints
#'
#' use python RDkit package to calculate morgan fingerprints
#'
#' @param smiles single SMILES string or a vector with multiple SMILES
#' @param pythonPath full path of python executable
#' @return data.table object with smiles and fingerprints
#' @author Marvin Steijaert
#' @export
#' @importFrom jsonlite fromJSON
#' @import data.table
#' @examples \dontrun{
#' smiles <- c("N[C@@H](C)C(=O)O","O1CCCC[C@@H]1C")
#' result <- fingerprintsRdkit(smiles)
#' }
fingerprintsRdkit <- function(smiles, pythonPath = getOption("dartpaths_python")){
  
  callPythonScript(smiles, "morganfpCmd.py", pythonPath, header = TRUE)
}

#' smilesToPng
#'
#' use rdkit to create png image (html img element with base64 encoded image)
#' 
#' @param smiles single SMILES string or a vector with multiple SMILES
#' @param pythonPath full path of python executable
#' @return vector with character strings
#' @author Marvin Steijaert
#' @export
#' @importFrom jsonlite fromJSON
#' @examples \dontrun{
#' smiles <- c("N[C@@H](C)C(=O)O","O1CCCC[C@@H]1C")
#' result <- smilesToPng(smiles)
#' }
smilesToPng <- function(smiles, pythonPath = getOption("dartpaths_python")){
  callPythonScript(smiles, "drawMolecule.py", pythonPath, outputClass = NULL)
}


#' inchiToSmiles
#'
#' use rdkit to convert a vector of inchi to a vector of smiles
#' 
#' @param inchis single inchi string or a vector with multiple inchis
#' @param pythonPath full path of python executable
#' @return vector with character strings
#' @author Marvin Steijaert
#' @export
#' @importFrom jsonlite fromJSON
#' @examples \dontrun{
#' smiles <- c("N[C@@H](C)C(=O)O","O1CCCC[C@@H]1C")
#' result <- smilesToPng(smiles)
#' }
inchiToSmiles <- function(inchis, pythonPath = getOption("dartpaths_python")){
  callPythonScript(inchis, "inchi2smiles.py", pythonPath, outputClass = NULL)
}

#' Auxiliary function for standardizeSmilesRdkit
#' @param smiles List of smiles strings
#' @param pythonBinding Character string indicating if system call ("system2") or reticulate ("reticulate") is used to call python module
#' @return data.table object
#' @author Marvin Steijaert
#' @import reticulate
standardizeSmilesList <- function(smiles, pythonBinding = "system2"){
  # pythonBinding="system2" is easier to debug
  if (pythonBinding == "reticulate"){
    # reticulate:  	
    use_python(python = getOption("dartpaths_python"), required = TRUE)
    pythonModule = "standardize_rdkit" 
    module <- import_from_path(pythonModule,
        path = system.file("python", package = "dartTools"),
        convert = TRUE, delay_load = FALSE)
    module$standardize_smiles_list(smiles)
  } else if (pythonBinding == "system2"){
    callPythonScript(smiles, "standardize_rdkit.py", useFiles = TRUE)
  } else stop("pythonBinding has unsupported value")
}