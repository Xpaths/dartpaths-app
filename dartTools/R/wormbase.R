#' Function connecting to the wormbase database for specific molecule
#'
#' @param prefix A character string to complete the url. Defaults to NULL.
#' @param identifier Wormbase identifier for a molecule. Defaults to NULL.
#' @param suffix A character string to complete the url. Defaults to NULL.
#' @param baseurl The wormbase base url: "http://rest.wormbase.org/rest".
#' @return A processed JSON object with the required information of the identifier.
#' @author Marijke Van Moerbeke
#' @importFrom curl curl
#' @importFrom jsonlite fromJSON
#' @export
#' @examples
#' \dontrun{
#' result <- wormbaseAPI(prefix = "widget/molecule", identifier ="WBMol:00003509", suffix = "affected")
#' }
wormbaseAPI <- function(prefix = NULL, identifier = NULL, suffix = NULL,  baseurl = "http://rest.wormbase.org/rest"){
  url <- paste(baseurl, prefix, identifier, suffix, "/", sep = "/")
  url <- gsub(":/","://",gsub("/+","/", url)) # remove "//", but keep "://" (e.g. "http://")
  url <- gsub("/$","",url)
  #cat(url, "\n")
  con <- curl(url = url) 
  res <- tryCatch(
      {  
        fromJSON(readLines(con, warn = FALSE))
      }, 
      error = function(cond) {
        message("Timeout. Skipping.")
        return(NULL)
      })
  close(con)   
  return(res)
}


#' Function to help the processing of the retrieved Wormbase data
#'
#' @param text The 'details' section of the 'affected genes' table of the molecule.
#' @return The 'details' section of the 'affected genes' table of the molecule in a character vector.
#' @author Marijke Van Moerbeke
#' @export
retrieveDetails <- function(text){
  
  if(class(text)=="data.frame"){
    details <- text[1,2]
  }
  else if(class(text)=="list"){
    details <- paste0(text[[1]]$label, " - ",text[[2]])
  }
  else{
    details = ''
  }
  return(details)
}



#' Function to retrieve and process the 'affected genes' of a single molecule in the Wormbase database
#'
#' @param molecule The molecule to retrieve. Defaults to NULL.
#' @param verbose Logical value to switch on/off extra verbosity.
#' @return A data.table containing the processed 'affected genes' table of the molecule.
#' @author Marijke Van Moerbeke
#' @export
wormbaseMoleculeData <- function(molecule = NULL, verbose = FALSE){
  
  if(!is.null(molecule)){
    if (verbose) print(molecule)
    affectedGenes <- wormbaseAPI(prefix = "widget/molecule", identifier = molecule, suffix = "affected")
    
    
    affectedGenesDetails = ''
    affectedGenesPhenotype = ''
    affectedGenesLabel = ''
    affectedGenesPhenotypeId = ''
    affectedGenesSpecies = ''
    
    if(is.null(affectedGenes)){
      moleculeName=''
      data <- data.frame(molID = molecule, molName = moleculeName, 'gene' =  affectedGenesLabel,
          "phenotypename" = affectedGenesPhenotype,
          "phenotypeid" = affectedGenesPhenotypeId,
          "details" = affectedGenesDetails,
          "species" = affectedGenesSpecies,
          stringsAsFactors = FALSE)
      
    }
    else{
      moleculeName <- affectedGenes$fields$name$data$label
      
      if(length(affectedGenes$fields$affected_genes$data)!=0){
        
        affectedGenesDetails <- sapply(affectedGenes$fields$affected_genes$data$affected$text, retrieveDetails)   
        affectedGenesPhenotype <- affectedGenes$fields$affected_genes$data$phenotype$label
        affectedGenesPhenotypeId <- affectedGenes$fields$affected_genes$data$phenotype$id
        affectedGenesLabel <- affectedGenes$fields$affected_genes$data$affected_gene$label
        affectedGenesSpecies <- affectedGenes$fields$affected_genes$data$affected_gene$taxonomy
      }
      
      data <- data.frame(molID = molecule, molName = moleculeName,'gene' =  affectedGenesLabel,
          'phenotypename' = affectedGenesPhenotype,
          'phenotypeid' = affectedGenesPhenotypeId,
          'details' = affectedGenesDetails,
          'species' = affectedGenesSpecies,
          stringsAsFactors = FALSE)
      data <- data[order(data$gene),]
    } 
  }
  # change to data.table
  setDT(data)
  
  return(data)
}


#' Function to retrieve and process the 'affected genes' of a vector of molecules in the Wormbase database
#'
#' @param molecules A character vector of the molecule to retrieve. Defaults to NULL.
#' @return A data.table containing the processed 'affected genes' of the molecules.
#' @author Marijke Van Moerbeke
#' @export
#' @examples 
#' \donttest{
#' result <- wormbaseDataRetrieval(molecules = c("WBMol:00000280", "WBMol:00000373", "WBMol:00000896"))
#' }
wormbaseDataRetrieval <- function(molecules =  NULL){
  
  dbVersion <- wormbaseAPI(prefix = "database", suffix = "version")$data
  
  message(paste("retrieving data from Wormbase - version ", dbVersion, "."))
  
  pb <- txtProgressBar(min = 0, max = length(molecules), style = 3)
  moleculesData <- lapply(molecules, function(x){
        res <- wormbaseMoleculeData(x)
        setTxtProgressBar(pb, getTxtProgressBar(pb) + 1)
        return(res)
      })
  close(pb)
  moleculesDb <- rbindlist(moleculesData)
  
  return(moleculesDb)
}

#' Function to retrieve and process the smiles of a vector of molecules in the Wormbase database
#'
#' @param molecules A character vector of the molecule to retrieve.
#' @return Vector with the smiles retrieved for the molecules (NA if missing).
#' @author Marvin Steijaert
#' @export
#' @importFrom utils txtProgressBar
#' @examples 
#' \donttest{
#' result <- wormBaseSmilesRetrieval(molecules = c("WBMol:00000001", "WBMol:00000002", "WBMol:00000003"))
#' }
wormBaseSmilesRetrieval <- function(molecules){
  pb <- txtProgressBar(min = 0, max = length(molecules), style = 3)
  smiles <- lapply(molecules, function(x){
        res <- wormbaseAPI(prefix = "field/molecule", identifier = x, suffix = "smiles",  baseurl = "http://rest.wormbase.org/rest")
        setTxtProgressBar(pb, getTxtProgressBar(pb) + 1)
        return(res$smiles$data)
      })
  smiles <- sapply(smiles, function(x) ifelse(is.null(x), NA_character_, x))
  close(pb)
  cat("\n")
  return(smiles)
}

