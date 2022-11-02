# will be executed when loading the package
.onLoad <- function(libname, pkgname) {
  
  ## set options if not yet set
  # first check environment variables
  existingOptions <- names(options())
  if(! "dartpaths_python" %in% existingOptions){
    # path with python executable
    options("dartpaths_python" = Sys.getenv("DARTPATHS_PYTHON", "~/miniconda3/envs/rdkit/bin/python") )
  }
  if(! "dartpaths_default_dumppath" %in% existingOptions){
    # path with dartpaths database dump
    options("dartpaths_default_dumppath" =
            Sys.getenv("DARTPATHS_DUMPPATH", "/opt/dartpaths/dbdump"))
  }
  if(! "dartpaths_smilescache" %in% existingOptions){
    # path of smiles cache txt file
    options("dartpaths_smilescache" =
            Sys.getenv("DARTPATHS_SMILESCACHE", "/opt/dartpaths/smilesCache.txt"))
  }
  if(! "dartpaths_python_strategy" %in% existingOptions){
    # strategy for processing multiple smiles in python.
    # can be "multiprocessing", "threading" or "sequential"
    options("dartpaths_python_strategy" =
            Sys.getenv("DARTPATHS_PYTHON_STRATEGY",
                if (.Platform$OS.type=="windows") "sequential" else "multiprocessing")
    )
  }
  
  Sys.setlocale('LC_ALL','C') # make sure to support non-ascii characters in e.g. compound names
}