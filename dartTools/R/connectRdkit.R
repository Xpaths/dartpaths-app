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
#' result <- standardizeSmilesRdkit(smiles)
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
  
  #callPythonScript(smiles, "morganfpCmd.py", pythonPath, header = TRUE)
  
  if(length(smiles) == 0) return(NULL)
  
  # using strategy "sequential" (fastest in this case)
  res <- as.data.table(t(setDT(processSmilesWithReticulate(smiles, "morgan_fp_bitvector_int", timeout = 60, strategy = "sequential"))))
  # replace NaN by NA (for consistency)
  for(j in names(res)) {
    set(res, which(is.nan(res[[j]])), j, NA)
  }
  names(res) <- paste0("fp",seq_len(ncol(res))-1)
  res[, smiles := smiles]
  setcolorder(res, "smiles")
  
  return(res)
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
  #callPythonScript(smiles, "drawMolecule.py", pythonPath, outputClass = NULL)
  processSmilesWithReticulate(smiles, "draw_smiles")
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
#' inchis <- c("InChI=1S/C2H6O/c1-2-3/h3H,2H2,1H3","InChI=1S/C6H8O6/c7-1-2(8)5-3(9)4(10)6(11)12-5/h2,5,7-10H,1H2/t2-,5+/m0/s1")
#' result <- inchiToSmiles(inchis)
#' }
inchiToSmiles <- function(inchis, pythonPath = getOption("dartpaths_python")){
  #callPythonScript(inchis, "inchi2smiles.py", pythonPath, outputClass = NULL)
  processSmilesWithReticulate(inchis, "inchi_to_smiles")
}

#' Auxiliary function for standardizeSmilesRdkit
#' @param smiles List of smiles strings
#' @param pythonBinding Character string indicating if system call ("system2") or reticulate ("reticulate") is used to call python module
#' @return data.table object
#' @author Marvin Steijaert
#' @import reticulate
standardizeSmilesList <- function(smiles, pythonBinding = "reticulate"){
  if (pythonBinding == "reticulate"){
    res = processSmilesWithReticulate(smiles, "standardize_smiles", timeout = 60)
    res = data.table(res)
    setnames(res,"V1")
    res[V1=="",V1 := NA]
    return(res)
  } else if (pythonBinding == "system2"){
    callPythonScript(smiles, "standardize_rdkit.py", useFiles = TRUE)
  } else stop("pythonBinding has unsupported value")
}


#' Auxiliary function to processes smiles in parallel
#' @param smiles Vector with smiles strings.
#' @param python_function_name One of the available function names for parallel procesing.
#' @param timeout Number of seconds after which a worker is interrupted
#' @param nWorkers Number of workers (optional)
#' @param strategy Strategy for processing multiple smiles (can be "multiprocessing", "threading" or "sequential")
#' @return processed output of smiles
#' @author Marvin Steijaert
#' @export
processSmilesWithReticulate <- function(smiles,
    python_function_name = c("standardize_smiles", "draw_smiles", "morgan_fp_bitvector_int", "inchi_to_smiles"),
    timeout = 10, nWorkers = parallel::detectCores() -1,
    strategy = getOption("dartpaths_python_strategy")){
  
  python_function_name = match.arg(python_function_name)
  
  use_python(python = getOption("dartpaths_python"), required = TRUE)
  
  # make sure that modules can be found
  pythonModulesDir <- system.file("python", package = "dartTools")
  sys <- import("sys", convert = FALSE)
  sys$path$insert(0L, pythonModulesDir)
  
  process_parallel <- import_from_path("process_parallel",
      path = pythonModulesDir,
      convert = TRUE, delay_load = FALSE)
  
  # reticulate translates NA_character_ to "NA".
  # replace by empty strings to avoid unexpected behavior
  smiles[is.na(smiles)] = ""
  process_parallel$process_smiles(python_function_name, smiles,
      n_workers = as.integer(nWorkers),
      strategy = strategy,
      timeout = timeout)
}
