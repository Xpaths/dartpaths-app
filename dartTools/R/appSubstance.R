#' Helper function for substance query in Shiny app
#' @param substancesObject Object that inherits from Substances (R6 class) 
#' @param mySearch Character string with query 
#' @param type Type of (external) substance identifier
#' @param partialMatch Boolean indicating if a partial match is allowed
#' @return vector with (integer) substanceids
#' @author Marvin Steijaert
#' @export
#' @importFrom stats na.omit
matchSubstanceQuery <- function(substancesObject, mySearch, type, partialMatch = TRUE){
  
  res <- substancesObject$getSubstanceExtIds(type, requireURL = (type == "name"))
  if(partialMatch){
    res <- unique(rbindlist(lapply(mySearch,
                function(searchItem) res[grepl(searchItem, get(type), ignore.case = TRUE)]
            )))
    # order by length of match to priority full matches
    res <- res[order(nchar(get(type)))]
  } else {
    res <- na.omit(res[mySearch, on = type])
  }
  
  if(nrow(res) > 0){
    unique(res$substanceid)
  } else stop("No match found")
}


