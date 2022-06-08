# set names of columns to NULL to avoid R CMD check complain about "no visual binding"
# https://www.r-bloggers.com/2019/08/no-visible-binding-for-global-variable/
Notes <- cluster <- V1 <- NULL

#' Function to convert integer representation of CAS numbers to character representation
#'
#' @param vec integer vector with CAS numbers 
#' @return character vector with CAS numbers
#' @author Marvin Steijaert
#' @export
intToCAS <- function(vec){
  # Ni...N3 - N2N1 - R
  vecStr <- sapply(vec, toString)
  vecLengths <- sapply(vecStr,nchar)
  paste(substr(vecStr, 1, vecLengths-3),
      substr(vecStr, vecLengths-2, vecLengths-1),
      substr(vecStr, vecLengths, vecLengths), sep = "-")
}

#' Function to convert integer representation of EC numbers to character representation
#'
#' @param vec integer vector with EC numbers 
#' @return character vector with EC numbers
#' @author Marvin Steijaert
#' @export
intToEC <- function(vec){
  # N1N2N3 - N4N5N6 - R (always 7 digits)
  vecStr <- sapply(vec, toString)
  vecLengths <- sapply(vecStr,nchar)
  if(!all(vecLengths == 7)) stop("EC numbers should have exactly 7 digits")
  paste(substr(vecStr, 1, 3), substr(vecStr, 4, 6), substr(vecStr, 7, 7), sep = "-")
}

#' Function to convert character representation of EC numbers to integer representation
#'
#' @param vec character vector with EC numbers 
#' @return integer vector with EC numbers
#' @author Marvin Steijaert
#' @export
ECtoInt <- function(vec){
  as.integer(gsub("-","",vec))
}

#' Function to convert character representation of CAS numbers to integer representation
#'
#' @param vec character vector with CAS numbers 
#' @return integer vector with CAS numbers
#' @author Marvin Steijaert
#' @export
CAStoInt <- ECtoInt


#' Multi-column clustering
#'
#' Function that allows clustering of rows in a table that have matching values for one the columns
#'
#' @param inputTable data.table object 
#' @param clusterColumns Columns for which at least one item should match
#' @param uniqueColumns Columns for which all items should match
#' @param verbose Toggle verbosity 
#' @return NULL
#' @details
#' An additional column 'cluster' is added to the input table (modified in-place)
#' NA values in clusterColumns are ignored
#' @author Marvin Steijaert
#' @export
clusterByMatchingEntries <- function(inputTable, clusterColumns, uniqueColumns, verbose = FALSE){
  if("cluster" %in% names(inputTable)) warning("inputTable already contains a column named 'cluster'")
  inputTable[,cluster := .I]
  nBefore = 2
  nAfter = 1
  nIterations = 0
  while(nBefore != nAfter){
    nIterations = nIterations + 1
    nBefore = inputTable[,length(unique(cluster))]
    if(verbose) cat("starting with",inputTable[,length(unique(cluster))],"clusters \n")
    for (clCol in clusterColumns){
      inputTable[!is.na(get(clCol)), cluster:= min(cluster), by = c(clCol, uniqueColumns)]
      if(verbose) cat(inputTable[,length(unique(cluster))],"after clustering on", clCol,"\n")
      if(verbose && "substancetype" %in% names(inputTable)) print(inputTable[, .(N = length(unique(cluster))), by = substancetype])
    }
    nAfter = inputTable[,length(unique(cluster))]
    if(verbose) cat("iteration",nIterations, "clusters before:", nBefore, ", clusters after:", nAfter,"\n")
  }
  invisible()
}


#' Function to filter entries
#'
#' @param vec Character vector 
#' @param type Type of entries ("smiles", "names")
#' @param mode How to filter. "replace" replaces by NA, "invalid" returns only invalid entries, "valid" return only valid entries
#' @return Vector. Contents depend on mode argument
#' @author Marvin Steijaert
#' @export
#' @examples \dontrun{
#'   filterInvalid(c("CCCCC","NA"," - "), "smiles")
#'   filterInvalid(c("oxygen","NA"," - ", "this is a substance description not a name"), "substancenames")
#' }
filterInvalid <- function(vec, type = "smiles", mode = "replace"){
  type = tolower(type) # make sure to also use lower cases in following switch
  invalidEntries <- switch(type,
      smiles = findInvalidSmiles(vec),
      substancenames = findInvalidSubstanceNames(vec),
      ec = findInvalidEC(vec),
      cas = findInvalidCAS(vec),
      other = findInvalidOther(vec)
  )
  switch(mode,
      replace = ifelse(invalidEntries, NA_character_, vec),
      invalid = vec[invalidEntries],
      valid = vec[invalidEntries]
  )
}

findInvalidOther <- function(vec){
  grepl("^\\s[[:punct:]]+\\s$",vec) | # "-", " - ", etc
      grepl("^\\s*NA|na\\s*$",vec) | # NA, na
      grepl("available",vec) |
      grepl("^\\s[[:punct:]]+\\s$",vec) | # "-", " - ", etc
      nchar(vec) == 0 # empty strings
}

findInvalidSmiles <- function(vec){
  grepl("^\\s[[:punct:]]+\\s$",vec) | # "-", " - ", etc
      grepl("^\\s*NA|na\\s*$",vec) | # NA, na
      grepl("available",vec) | 
      grepl("^[Nn][.//][Aa]",vec) | # anything starting with N.A. n/a, etc
      grepl("\\s",vec) |# anything with whitespace characters
      # TODO: we might want to allow newlines (along with semi-colons, this can indicate multiple constituents)
      nchar(vec) == 0 # empty strings
}

findInvalidSubstanceNames <- function(vec){
  
  lowervec <- tolower(vec)
  
  grepl("^\\s*[[:punct:]]+\\s*$",vec) | # "-", " - ", etc
      grepl("^([[:punct:]CcOoNnHSKFPBVW0-9]|He|Si|Mg|Fe|Al|Mg|Ca|Na|Ni|Si|Br|Zn)+$",vec) | # typical characters that occur in smiles, CAS/EC and molecular formulas, but not in names 
      grepl("^\\s*NA|na\\s*$",vec) | # NA, na
      grepl("_|#|~",vec) |     
      grepl(intToUtf8("9679"),vec) | # bold dot (non-ascii character)
      grepl("@",vec) |
      grepl("C[0-9]+H[0-9]+", vec) |
      grepl("applicable",lowervec) |
      grepl("listed",lowervec) |
      grepl("unspecified",lowervec) |
      grepl("not applicable",lowervec) |
      grepl("not determinable",lowervec) |
      grepl("available",lowervec) | # word expected in substance description, not in a substance name
      #grepl(" of ",vec) | # word expected in substance description, not in a substance name
      #grepl(" and ",vec) | # word expected in substance description, not in a substance name
      #grepl(" the ",vec) | # word expected in substance description, not in a substance name
      #grepl("reaction",vec) | # word expected in substance description, not in a substance name
      #grepl("mixed",vec) | # word expected in substance description, not in a substance name
      #grepl("product",vec) | # word expected in substance description, not in a substance name
      #grepl("distillates",vec) | # word expected in substance description, not in a substance name
      #grepl("substance",vec) | # word expected in substance description, not in a substance name
      #grepl("compound",vec) | # word expected in substance description, not in a substance name
      grepl("[Nn][.//][Aa]",vec) | # anything starting with N.A. n/a, etc
      nchar(vec) < 3 | # TODO: what would be a reasonable filter?
      nchar(vec) > 200 | # arbitrary filter. Remove long names with visualisation in mind 
      nchar(vec) == 0 | # empty strings
      grepl("inchi", lowervec) |
      grepl(";", lowervec) | # less stringent alternative : grepl(";;", lowervec)
      lowervec %in% c("uvcb substance", "uvcb", "too complex", "substance is uvcb", "none", "no molecular formula", "unknown", "variable")
}

findInvalidEC <- function(vec){
  !grepl("^[0-9]+-[0-9]+-[0-9]+$",vec)
}

findInvalidCAS <- findInvalidEC


#' Function to remove non-informative rows from a data.table
#'
#' @param inputTable data.table 
#' @param infoColumns names of columns in in data.table
#' @return list with items inputTable (adjusted version of inputTable) and informative (logical vector indicating if row in original table is informative)
#' @author Marvin Steijaert
#' @details Rows in which all specified columns have NA value are removed
removeNonInformativeEntries <- function(inputTable, infoColumns){
  if(length(infoColumns) == 0) stop("infoColumns should be a vector with one or more column names")
  if(! all(infoColumns %in% names(inputTable)) ) stop("infoColumns should contain names of columns in inputTable")
  nonInformative <- inputTable[, lapply(.SD, is.na), .SDcols=infoColumns][, apply(.SD, 1, all)]
  informative <- !nonInformative
  inputTable <- inputTable[informative]
  return(list(inputTable = inputTable, informative = informative))
}



#' Function to standardize species names
#'
#' @param species Vector with species names 
#' @return Vector with standardized species names
#' @export
standardizeSpecies <- function(species){
  species <- tolower(species)
 
  synonyms = list(
      mouse = c("mouse","mice"),
      human = c("homo","human"),
      pig = c("^(?!guinee).*pig.*", "swine"),
      duck = c("duck"),
      cow = c("cattle", "cow"),
      hamster = c("hamster"),
      chicken = c("hen"),
      rat = c("rat"),
      quail = "quail",
      pheasant = "pheasant",
      monkey = "monkey",
      mink = "mink"
  )
  
  for(i in seq_along(synonyms)){
    for(synonym in synonyms[[i]]){
      species[grepl(synonym, species, perl = TRUE)] = names(synonyms)[i]
    }
  }
  species[grepl("other", species)] = NA
  species[grepl("not reported", species)] = NA
  return(species)
}
