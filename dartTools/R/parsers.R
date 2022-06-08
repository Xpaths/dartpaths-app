#' get the value for the specified key from a (vector of) hstore string(s)
#' @param hstore_str vector of hstore strings
#' @param hstore_key key
#' @return vector of values
#' @author Marvin Steijaert
getHstoreElement <- function(hstore_str, hstore_key){
  pattern <- paste0("^.*\"", hstore_key, "\"=>\"([^\"]*)\".*$")
  ifelse(grepl(pattern, hstore_str),
      gsub(pattern, "\\1", hstore_str),
      NA_character_)
}


#' get all keys in a hstore string
#' @param hstoreStr hstore string 
#' @return vector with keys
#' @author Marvin Steijaert
#' @export
getHstoreKeys <- function(hstoreStr){
  pattern <- paste0("\"([^\"]*)\"=>")
  patternMatches = gregexpr(pattern,hstoreStr)
  patternStarts = patternMatches[[1]]+1
  patternEnds = patternStarts + attr(patternMatches[[1]],"match.length") -5
  mapply(function(x,y) substr(hstoreStr, x, y), patternStarts, patternEnds)
}

#' get value for specified key from json string
#' @param jsonstr json string
#' @param jsonkey key of item
#' @return character string
#' @importFrom jsonlite fromJSON
#' @author Marvin Steijaert
getJsonElement <- function(jsonstr, jsonkey){
  js <- fromJSON(jsonstr)
  if(jsonkey %in% names(js)) js[jsonkey] else NA_character_
}

#' get the value for the same key from multiple json string
#' @param vec vector of json strings
#' @param jsonkey key of item
#' @return character string
#' @author Marvin Steijaert
getAllJsonElements <- function(vec, jsonkey){
  unlist(unname(sapply(vec, function(x) getJsonElement(x, jsonkey), USE.NAMES = FALSE)))
}


#' Parser for Wormbase ace file with Molecule data
#' @param filepath Path of .ace file 
#' @return data.table object
#' @details Creates a data.table that can be fed to addSubstances method of Substances class
#' @import data.table
#' @author Marvin Steijaert
#' @export
wormbaseAceMoleculeParser <- function(filepath){
  fileContents <- readLines(filepath)
  moleculeStarts <- which(grepl("^Molecule :", fileContents))
  moleculeEnds <- c(moleculeStarts[-1]-2, length(fileContents))
  parsedItems <- lapply(seq_along(moleculeStarts), function(item){
        wormbaseAceElementParser(fileContents[moleculeStarts[item]: moleculeEnds[item]])
      })
  res <- rbindlist(parsedItems, use.names = TRUE)
  return(res)
}


#' helper function for wormbaseAceMoleculeParser
#' @param element element from wormbase ace file  
#' @return data.table object
#' @author Marvin Steijaert
wormbaseAceElementParser <- function(element){
  pattern <- "([A-Za-z0-9_ ]+[A-Za-z0-9])[[:space:]:\"]+(.*)"#"([A-Za-z0-9]+)[\\s:]+(.*)"
  fields <- lapply(element, function(x){
        keyfield <- gsub(pattern,"\\1",x)
        valuefield <- gsub(pattern,"\\2",x)
        list(keyfield, 
            strsplit(valuefield,"[:space:]*\"+[[:space:]]*\"*")
        )
      })
  fieldKeys <- sapply(fields, function(x) x[[1]])
  fieldValues <- sapply(fields, function(x) x[[2]])
  smiles = unlist(fieldValues[fieldKeys %in% c("SMILES")])
  iupac_names = unlist(fieldValues[fieldKeys %in% c("IUPAC")])
  public_names = unlist(fieldValues[fieldKeys %in% c("Public_name")])
  
  if (length(smiles) > 1){
    warning("Molecule ",fieldValues[fieldKeys %in% c("Molecule")], " contains multiple smiles: ", paste(smiles,collapse = ", "), " and will be skipped")
    NULL
  } else if(length(smiles) == 1){
    data.table(
        smiles = smiles,
        name = c(iupac_names,public_names),
        wbmol = unlist(fieldValues[fieldKeys %in% c("Molecule")])
    )
  } else NULL
}
