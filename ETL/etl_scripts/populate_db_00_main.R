# Script for ETL
# 
# Author: Marvin Steijaert
###############################################################################

library(RPostgreSQL) # system requirement: libpq-dev
library(data.table)

dartpathsGitDir <- "~/git/dartpaths"
rawDataDir <- file.path(dartpathsGitDir, "ETL/data_raw")
sharedDataDir <- file.path(dartpathsGitDir, "ETL/data_shared")
manualDownloadDataDir <- file.path(dartpathsGitDir, "ETL/data_manual_download")
intermediateDataDir <- file.path(dartpathsGitDir, "ETL/data_intermediate")
etlScriptsDir <- file.path(dartpathsGitDir, "ETL/etl_scripts/")
dir.create(intermediateDataDir, showWarning = FALSE)

# Avoid timeout when downloading large files
options(timeout = max(600, getOption("timeout")))

devtools::load_all(file.path(dartpathsGitDir, "dartTools"))

dumpdir <- file.path(dartpathsGitDir,"ETL/data_app/txt_database_dump")
options("dartpaths_smilescache" =  file.path(dartpathsGitDir,"ETL/data_app/smilesCache.txt"))
options("dartpaths_default_dumppath" = dumpdir)
options("dartpaths_python" = "~/miniconda3/envs/rdkit/bin/python")

database <- DartDB$new()
if(file.exists(dumpdir)) stop("dumpdir already exist, proceeding with this script might overwrite or duplicate existing data")
dir.create(dumpdir, showWarnings = FALSE, recursive = TRUE)

onlyLargestFragments = TRUE
if(!onlyLargestFragments){
  Sys.setenv(DART_fragment = "false")
  Sys.setenv(DART_charge = "false")
} else {
  # Note: only use the shared smilesCache if you make use of the same standardization
  file.copy(file.path(sharedDataDir,"smilesCache.txt"), getOption("dartpaths_smilescache"))
}

# Species
# Taxon id according to https://www.ncbi.nlm.nih.gov/taxonomy
speciesTable <- rbind(
    data.table(speciesfull = "Dictyostelium discoideum", taxon = "44689", speciescommon = "slime mold"),
    data.table(speciesfull = "Caenorhabditis elegans", taxon = "6239", speciescommon = "nematode"),
    data.table(speciesfull = "Danio rerio", taxon = "7955", speciescommon = "zebrafish"),
    data.table(speciesfull = "Mus musculus", taxon = "10090", speciescommon = "mouse"),
    data.table(speciesfull = "Homo sapiens", taxon = "9606", speciescommon = "human"),
    data.table(speciesfull = "Rattus norvegicus", taxon = "10116", speciescommon = "rat"),
    data.table(speciesfull = "Oryctolagus cuniculus", taxon = "9986", speciescommon = "rabbit"),
    data.table(speciesfull = "Drosophila melanogaster", taxon = "7227", speciescommon = "fruit fly")
)
speciesTable[, speciesshort := gsub("([A-Z])[a-z]* ([a-z]*)","\\1. \\2", speciesfull, perl = TRUE)]
database$addData("species", speciesTable, fill = TRUE)

# Run scripts for individual data sources
source(file.path(etlScriptsDir,"populate_db_01_qsartoolbox.R"))
source(file.path(etlScriptsDir,"populate_db_02_echa.R"))
source(file.path(etlScriptsDir,"populate_db_03_uvcb_concawe_loa.R"))
source(file.path(etlScriptsDir,"populate_db_04_wormbase.R"))
source(file.path(etlScriptsDir,"populate_db_05_epa.R"))
source(file.path(etlScriptsDir,"populate_db_06_orthology_gene_pheno.R"))
source(file.path(etlScriptsDir,"populate_db_07_zfin.R"))
source(file.path(etlScriptsDir,"populate_db_08_phenotype_enrichment.R")) # takes approx. 2 hours
source(file.path(etlScriptsDir,"populate_db_09_extracted_phenotypes.R"))
source(file.path(etlScriptsDir,"populate_db_10_pathway_ranking_simulations.R"))

## create morgan fingerprints
database$updateDescriptors("morgan")

## merge substances that have identical external substance identifiers
database$mergeSubstances()

## create dump
database$dump(dumpdir)

# Inspection of database
database$inspect()
database$printAllTables()
