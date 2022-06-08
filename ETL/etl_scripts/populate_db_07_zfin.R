# Dartpaths ETL : ZFIN (zebrafish)
# 
# Author: Marvin Steijaert
###############################################################################

zfinRawDataDir <- file.path(rawDataDir,"zfin")
chebiSmilesFile <- file.path(intermediateDataDir,"zfin", "chebiId_smiles.txt")
if(file.exists(chebiSmilesFile)){
  chebi <- fread(chebiSmilesFile)
} else {
  chebi <- readWebOrFtp("ftp://ftp.ebi.ac.uk/pub/databases/chebi/Flat_file_tab_delimited/chebiId_inchi.tsv", zfinRawDataDir)
  chebi[, smiles := inchiToSmiles(InChI)]
  dir.create(dirname(chebiSmilesFile), showWarnings = FALSE)
  fwrite(chebi, chebiSmilesFile)
}
chebicmpdsFile <- downloadWebOrFtp("ftp://ftp.ebi.ac.uk/pub/databases/chebi/Flat_file_tab_delimited/compounds.tsv.gz", zfinRawDataDir)
# use sed to prevent early stopping of fread due to double tabs
chebicmpds <- fread(cmd = paste("gunzip -c", chebicmpdsFile," | sed 's|[\t][\t]|[\t]|g'"))
# not used: chebinames <- fread("ftp://ftp.ebi.ac.uk/pub/databases/chebi/Flat_file_tab_delimited/names.tsv.gz")
# not used: chebirelation <- fread("ftp://ftp.ebi.ac.uk/pub/databases/chebi/Flat_file_tab_delimited/relation.tsv")
chebimerge <- merge(chebicmpds, chebi, by.x = "ID", by.y = "CHEBI_ID", all = TRUE)
chebimerge <- chebimerge[!STATUS %in% "D"] # remove "D" (deleted) identifiers
chebimerge <- chebimerge[, .(chebi = CHEBI_ACCESSION, name = NAME, smiles)]
chebimerge[, url:= paste0("https://www.ebi.ac.uk/chebi/searchId.do?chebiId=",chebi)]
chebimerge[name %in% "null", name := NA_character_]
chebimerge[, substancetype := "mono"] # Note this is not always true. E.g. CHEBI:9513 is a racemic mixtures (multiconstituent), but there are no ways to determine this 
chebimerge <- unique(chebimerge[!(is.na(name) & is.na(smiles)), ])

# get CAS information from chebi.obo (to deal also with chebi without structure information)
library(ontologyIndex)
chebiOntologyFile <- downloadWebOrFtp("http://purl.obolibrary.org/obo/chebi.obo", zfinRawDataDir)
ontology <- get_ontology(chebiOntologyFile, extract_tags = "everything")
casFromList <- function(vec) unique(gsub("^CAS:","",vec[grepl("^CAS",vec)]))
chebiCAS <- lapply(ontology$xref[chebimerge[, chebi]], casFromList)
chebimerge <- chebimerge[, c(.SD, list(CAS = chebiCAS[[chebi]])), by = chebi] # replicates row if there are multiple CAS with same chebi

# read file phenotypeFishChem.csv (this file was created as part of populate_db_06_orthology_gene_pheno.R)
zfin <- fread(file.path(genePhenoProcessedDataDir, "phenotypeFishChem.csv"))
zfin <- zfin[,.(
        species = "zebrafish",
        sourcedb = "ZFIN",
        sourceid = Environment.ID,
        variantid = Fish.ID,
        variantname = Fish.Name,
        conditions = ZECO.Term.Name, # Zebrafish Experimental Conditions Ontology
        chebi = `Chebi.Term.ID..Chebi.ID.`,
        # chebiname = `Chebi.Term.Name`,
        phenotypeid = Phenotype.ID,
        phenotypename = Phenotype.Name,
        gene = Gene.Symbol,
        details = NA,
        url = paste0("https://zfin.org/",Environment.ID)
    )]
zfin[gene == "", gene := NA]
# remove CHEBI identifiers that represent a class of substances rather than a single substance/compound
unwantedChebiIds = c("CHEBI:52217")
zfin <- zfin[!chebi %in% unwantedChebiIds]
database$addIdTypes("chebi")

# add substances to database
system.time(tmp <- database$addSubstances(chebimerge[chebi %in% zfin[,chebi]], verbose = TRUE, returnTable = TRUE))  
# add activity to database
database$addSubstanceActivity(zfin[chebi %in% chebimerge[,chebi]], "chebi", type = "phenotypes")

