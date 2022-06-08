# Dartpaths ETL : orthology, gene -> phenotype mapping
# 
# Author: Marvin Steijaert
###############################################################################

## orthology data
# ENSEMBL Compara data can be obtained through multiple channels. Through the ftp site rat and rabbit are not available.
# Through Biomart, dicty orthology is not available. Therefore a combination of both sources is necessary.
dir.create(file.path(rawDataDir, "ensembl"),showWarnings = FALSE, recursive = TRUE)

# human orthologs for species in Biomart
biomartSpeciesTable <- database$getData("species")
biomartSpeciesTable[,speciesconcat := tolower(gsub("[\\. ]","", speciesshort))]
biomartSpeciesTable <- biomartSpeciesTable[speciesconcat %in% c("ocuniculus", "rnorvegicus", "mmusculus", "drerio", "celegans","dmelanogaster")]
biomartQuery <- '<?xml version="1.0" encoding="UTF-8"?>
    <!DOCTYPE Query>
    <Query  virtualSchemaName = "default" formatter = "TSV" header = "1" uniqueRows = "0" count = "" datasetConfigVersion = "0.6" >
    <Dataset name = "hsapiens_gene_ensembl" interface = "default" >
    <Attribute name = "ensembl_gene_id" />
    <Attribute name = "ensembl_transcript_id" />
    <Attribute name = "SPECIES_homolog_ensembl_gene" />
    <Attribute name = "SPECIES_homolog_associated_gene_name" />
    <Attribute name = "SPECIES_homolog_orthology_type" />
    <Attribute name = "SPECIES_homolog_ensembl_peptide" />
    <Attribute name = "SPECIES_homolog_orthology_confidence" />
    <Attribute name = "SPECIES_homolog_perc_id_r1" />
    </Dataset>
    </Query>'
biomartQueryColumnNames <- c("geneid", "protein_or_transcriptid", "homolog_geneid",
    "homolog_gene", "homologytype", "homolog_protein_or_transcriptid",
    "confidence","percent_identical" ) # names in order of query

queryBiomart <- function(query, filename, overwrite = FALSE){
  if(file.exists(filename) && !overwrite){
    cat("File already exists:",filename,"and will not be downloaded again.\n")
  } else {
    query <- gsub("\\n", "",gsub("\\t", "", query))
    exitCode <- system2("wget",c("-O",filename,
            paste0("'","http://www.ensembl.org/biomart/martservice?query=",query,"'")),
        stdout = FALSE, stderr = FALSE)
    if(exitCode!=0) stop("Something went wrong while perfoming biomart query")
  }
  fread(filename)
}

for(thisSpecies in biomartSpeciesTable[,speciesconcat]){
  thisSpeciesQuery <- gsub("SPECIES",thisSpecies,biomartQuery)
  comparaData <- queryBiomart(thisSpeciesQuery, file.path(rawDataDir, "ensembl",paste0(thisSpecies,"_homology.txt")))
  setnames(comparaData, biomartQueryColumnNames)
  comparaData[, speciesfull:= biomartSpeciesTable[speciesconcat == thisSpecies,  speciesfull]]
  comparaData <- comparaData[!homolog_geneid %in% c(NA_character_, "")]
  database$addData("orthology", comparaData, fill = TRUE)
} 

# human orthologs for other species
selectedSpecies <- c("dictyostelium_discoideum")
comparaOtherSpecies <- readWebOrFtp("ftp://ftp.ensemblgenomes.org/pub/release-51/pan_ensembl/tsv/ensembl-compara/homologies/homo_sapiens/Compara.104.protein_default.homologies.tsv.gz",
    file.path(rawDataDir, "ensembl"))
comparaOtherSpecies <- comparaOtherSpecies[homology_species %in% selectedSpecies]
comparaOtherSpecies <- comparaOtherSpecies[,.(geneid = gene_stable_id,
        protein_or_transcriptid = protein_stable_id,
        homologytype = homology_type,
        speciesfull = gsub("_", " ", gsub("^([a-z])","\\U\\1",homology_species, perl = TRUE)),
        homolog_geneid = homology_gene_stable_id,
        homolog_protein_or_transcriptid = homology_protein_stable_id,
        #homolog_gene = NA_character_, # not included in downloaded data file
        percent_identical = identity,
        confidence = is_high_confidence)]
# merge to get dicty gene names
dictyGenes <- readWebOrFtp("http://dictybase.org/db/cgi-bin/dictyBase/download/download.pl?area=general&ID=gene_information.txt", file.path(rawDataDir,"dictybase"))
comparaOtherSpecies <- merge(comparaOtherSpecies, dictyGenes[,.(`GENE ID`, `Gene Name`)], by.x = "homolog_geneid", by.y = "GENE ID", all.x = TRUE)
setnames(comparaOtherSpecies, "Gene Name", "homolog_gene")    
database$addData("orthology", comparaOtherSpecies, fill = TRUE)

# orthology extension from phylogeny pipeline
# hedgehog and angiogenesis pathways
phylogenyFile <- file.path(sharedDataDir, "phylogeny_pipeline/orthologs_protein-phylogeny_pipeline_20201028.xlsx")
phylogenyData <- rbind(
    as.data.table(openxlsx::read.xlsx(phylogenyFile, sheet = 2, rows = 8:35, cols = 3:11)),
    as.data.table(openxlsx::read.xlsx(phylogenyFile, sheet = 4, rows = 6:8, cols = 3:11)))
phylogenyData <- phylogenyData[!is.na(speciesfull)]
phylogenyData[confidence %in% c("N/A"), confidence:= NA]
phylogenyData[, speciesfull:= gsub("^([A-Z][a-z]*)([A-Z][a-z]*)","\\1 \\L\\2",speciesfull,perl = TRUE)]
phylogenyData[, homologytype:=paste0("ortholog_", homologytype, "_manual")]
database$addData("orthology", phylogenyData, fill = TRUE)
# WNT pathway
wntSpeciesPrefix <- c("Celegans", "Rabbit", "Rat", "Zebrafish")
wntSpeciesFull <- c("Caenorhabditis elegans", "Oryctolagus cuniculus", "Rattus norvegicus", "Danio rerio")
wntFilenames <- file.path(sharedDataDir, "phylogeny_pipeline", "WNT", paste0(wntSpeciesPrefix, "_orthology_finegrained_additions.txt"))
phylogenyList <- lapply(wntFilenames, fread)
tmp <- mapply(function(dt, species) dt[, speciesfull:= species], phylogenyList, wntSpeciesFull, SIMPLIFY = FALSE)
tmp <- lapply(phylogenyList, setnames, c("geneid", "protein_or_transcriptid", "homologytype", "homolog_geneid", "homolog_protein_or_transcriptid", "status_ensembl", "speciesfull"))
wntPhylogeny <- rbindlist(phylogenyList)
wntPhylogeny[, status_ensembl := NULL]
wntPhylogeny[, homologytype:=gsub("ortholog_", "", homologytype )]
wntPhylogeny[, homologytype:=paste0("ortholog_", homologytype, "_manual")]
database$addData("orthology", wntPhylogeny, fill = TRUE)

# Ensembl human genes
# file was created with following API call
# "https://www.genenames.org/cgi-bin/download/custom?col=gd_app_sym&col=gd_app_name&col=md_ensembl_id&status=Approved&status_opt=2&where=&order_by=gd_app_sym_sort&format=text&limit=&hgnc_dbtag=on&submit=submit",
humanGenes <- readWebOrFtp(
    url = "https://www.genenames.org/cgi-bin/download/custom?col=gd_app_sym&col=gd_app_name&col=md_ensembl_id&status=Approved&status_opt=2&where=&order_by=gd_app_sym_sort&format=text&limit=&hgnc_dbtag=on&submit=submit",
    directory = file.path(rawDataDir, "genenames"),filename = "ensembl_human_genes.txt")
setnames(humanGenes, c("gene", "genelongname", "geneid"))
database$addData("humangenes", humanGenes, fill = TRUE)

## generate and add genepheno table
source(file.path(etlScriptsDir,"genepheno_etl.R"))
library(magrittr)
library(dplyr)
# TODO install dependencies
# BiocManager::install("Rgraphviz")
# install.packages("ontologyPlot")
# BiocManager::install("biomaRt")

# robot tool for owl to obo conversion : http://robot.obolibrary.org/
robotDir <- file.path(rawDataDir,"robot")
robotExec <- downloadWebOrFtp("https://raw.githubusercontent.com/ontodev/robot/master/bin/robot", robotDir)
downloadWebOrFtp("https://github.com/ontodev/robot/releases/download/v1.8.1/robot.jar", robotDir)
Sys.chmod(robotExec, "777")
genePhenoRawDataDir <- file.path(rawDataDir,"genepheno")
genePhenoProcessedDataDir <- file.path(intermediateDataDir,"genepheno")
genePhenoFile <- file.path(genePhenoProcessedDataDir, "genePheno.csv")
if(!file.exists(genePhenoFile)){
  dir.create(genePhenoRawDataDir, showWarnings = FALSE)
  dir.create(genePhenoProcessedDataDir, showWarnings = FALSE)
  DownloadOntologies(rawDataDir = genePhenoRawDataDir, robotCommand = robotExec) 
  ProcessOntologies(rawDataDir = genePhenoRawDataDir, persistentDataDir = genePhenoProcessedDataDir)
  DownloadChemPheno(rawDataDir = genePhenoRawDataDir) # TODO: add logic to deal with failing downloads
  DownloadGenePheno(rawDataDir = genePhenoRawDataDir)
  ParseZFINphenotype(rawDataDir = genePhenoRawDataDir, persistentDataDir = genePhenoProcessedDataDir)
  ProcessGenePheno(rawDataDir = genePhenoRawDataDir, persistentDataDir = genePhenoProcessedDataDir)
}
genepheno <- fread(genePhenoFile)
setnames(genepheno, tolower(names(genepheno)))
database$addData("genepheno", genepheno, fill = FALSE)

## ensembl2reactome
DownloadPathway(database = database, persistentDataDir = genePhenoProcessedDataDir)
ensembl2reactomeLowest <- fread(file.path(genePhenoProcessedDataDir,"ensembl2reactome.csv"))
setnames(ensembl2reactomeLowest, tolower(names(ensembl2reactomeLowest)))
ensembl2reactomeLowest[,lowest := TRUE]
ensembl2reactomeAll = fread(file.path(genePhenoProcessedDataDir,"ensembl2reactome_all.csv"))
setnames(ensembl2reactomeAll, tolower(names(ensembl2reactomeAll)))
ensembl2reactomeCombined = merge(ensembl2reactomeAll, ensembl2reactomeLowest, all.x = TRUE)
ensembl2reactomeCombined[is.na(lowest), lowest:=FALSE]
existingTableSize <- database$getData("ensembl2reactome")[,.N]
if (existingTableSize){
  errorMessage <- paste("Table ensembl2reactome is already populated.",
      "Existing table has", existingTableSize, "records; new data has", ensembl2reactomeCombined[,.N] ,"records.\n",
      "Please remove table from database if you want to repopulate it.")
  stop(errorMessage)
} else {
  database$addData("ensembl2reactome", ensembl2reactomeCombined, fill = FALSE)
}

# Reactome human pathway relations
reactomeDir <- file.path(rawDataDir, "reactome")
reactomePathwaysRelation <- readWebOrFtp(url = "https://reactome.org/download/current/ReactomePathwaysRelation.txt",
    directory = reactomeDir, filename=NULL, header = FALSE)
setnames(reactomePathwaysRelation, c("reactome_pathway_parent", "reactome_pathway_child"))
database$addData("pathwayrelations", reactomePathwaysRelation, fill = FALSE)
