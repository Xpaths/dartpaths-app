# Project: DARTpaths_git
# 
# Author: mvarewyck
###############################################################################

# Session info
output$info_session <- renderUI({
      
      res <- lapply(capture.output(oaReporting::printSessionInfoMarkdown()),
          function(x) tags$p(x))
      
      pythonPath <- normalizePath(getOption("dartpaths_python"))
      res <- c(res, list(
              tags$p(system2(pythonPath,"--version", stdout = TRUE)),
              tags$p(system2(pythonPath,
                      "-c \"import rdkit; print(f'RDKit version {rdkit.__version__}')\"", stdout = TRUE)),
              tags$p("* other installed Python packages:"),
              tags$p(paste(system2(pythonPath,"-m pip list --format freeze", stdout = TRUE),collapse = ", "))
          ))
      res
      
    })