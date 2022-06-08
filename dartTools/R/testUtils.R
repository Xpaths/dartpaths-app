# Utility functions for package testing and debugging
# 
# Author: Marvin Steijaert
###############################################################################


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