# will be executed when loading the package
.onLoad <- function(libname, pkgname) {
  
  ## set options if not yet set
  # first check environment variables
  existingOptions <- names(options())
  if(! "dartpaths_python" %in% existingOptions){
    options("dartpaths_python" = Sys.getenv("DARTPATHS_PYTHON", "~/miniconda3/envs/rdkit/bin/python") )
  }
  if(! "dartpaths_default_dumppath" %in% existingOptions){
    options("dartpaths_default_dumppath" =
            Sys.getenv("DARTPATHS_DUMPPATH", "/opt/dartpaths/dbdump"))
  }
  if(! "dartpaths_smilescache" %in% existingOptions){
    options("dartpaths_smilescache" =
            Sys.getenv("DARTPATHS_SMILESCACHE", "/opt/dartpaths/smilesCache.txt"))
  }
  
  Sys.setlocale('LC_ALL','C') # make sure to support non-ascii characters in e.g. compound names
}