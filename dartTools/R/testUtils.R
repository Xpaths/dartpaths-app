# Utility functions for package testing and debugging
# 
# Author: Marvin Steijaert
###############################################################################


#' Utility function to generate example data for unit testing
#' @param dataType Character string with type of vector (currently, only 'substance' is supported)
#' @return data.table object
#' @author Marvin Steijaert
#' @export
getExampleData <- function(dataType = "substance")(
        if(dataType == "substance"){
          data.table(
              smiles = c("CCN","NCC","CC",NA,""),
              substancetype = c("mono","mono","mono","uvcb","uvcb"),
              url = rep("https://cran.r-project.org/",5),
              EC = c("1-1-1","2-2-2","3-3-3","4-4-4","4-4-4")
          )
        } else NULL
        )

#' Utility to check environment
#' @return NULL
#' @author Marvin Steijaert
#' @export
checkEnvironment <- function(){
  exitCode <- system2(normalizePath(getOption("dartpaths_python")), "--version", stdout = NULL)
  if(exitCode !=0){
    stop("No python executable can be found at location specified by getOption('dartpaths_python'))")
  }
  message("Python is available: ",
      system2(normalizePath(getOption("dartpaths_python")),"--version", stdout = TRUE))
  
  exitCode <- system2(normalizePath(getOption("dartpaths_python")),
      "-c 'import rdkit; print(rdkit.__version__)'", stdout = NULL)
  if(exitCode !=0){
    stop("RDKit is not available")
  }
  message("RDKit is available: ", 
      system2(normalizePath(getOption("dartpaths_python")),
          "-c 'import rdkit; print(rdkit.__version__)'", stdout = TRUE))
}