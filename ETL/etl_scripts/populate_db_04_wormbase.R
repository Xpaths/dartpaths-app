# Dartpaths ETL : Wormbase
# 
# Author: Marvin Steijaert
###############################################################################

wbRawDataDir <- file.path(rawDataDir,"wormbase")

# 1. molecules
wormbaseSmilesFiles <- file.path(wbRawDataDir, "wormbaseMolsWithSmiles.txt")
if(file.exists(wormbaseSmilesFiles)){
  wormbaseMols <- fread(wormbaseSmilesFiles)
} else {
  query <- '<query name="" model="genomic" view="Molecule.primaryIdentifier Molecule.name" longDescription="" sortOrder="Molecule.primaryIdentifier asc"></query>'
  wormbaseMols <- readWebOrFtp(URLencode(paste0("http://intermine.wormbase.org/tools/wormmine/service/query/results?query=", query)),
      directory = wbRawDataDir, filename = "wbmols_query_result.txt", header = FALSE, col.names = c("wbmol", "name"))
  wormbaseMols[, smiles := wormBaseSmilesRetrieval(wbmol)]
  fwrite(wormbaseMols, wormbaseSmilesFiles)
}
wormbaseMols[smiles == "", smiles:=NA]
wormbaseMols[!is.na(smiles), substancetype := "mono"]
database$addIdTypes("wbmol")

# patch for diethylstilbestrol (DES)
wormbaseMols[tolower(name) == "diethylstilbestrol" & is.na(smiles),
    `:=`(smiles= "CCC(=C(CC)C1=CC=C(C=C1)O)C2=CC=C(C=C2)O", substancetype = "mono")]
wormbaseMols[,url:= paste0("https://wormbase.org/resources/molecule/",wbmol)]
tmp <- database$addSubstances(wormbaseMols, verbose = TRUE, returnTable = TRUE)

# 2. activity data
wbIntermediateFile <- file.path(intermediateDataDir, "wbphenotypes.txt")
if (file.exists(wbIntermediateFile)){
  wbPhenotypes <- fread(wbIntermediateFile)
} else {
  wbPhenotypes <- wormbaseDataRetrieval(wormbaseMols[,wbmol])
  wbPhenotypes[species %in% c("","all", "c_elegans"), species:= "C. elegans"] # only C. elegans data was found
  wbPhenotypes[gene == "", gene:= NA]
  wbPhenotypes[phenotypename == "", phenotypename:= NA]
  wbPhenotypes[phenotypeid == "", phenotypeid:= NA]
  wbPhenotypes[details == "", details:= NA]
  fwrite(wbPhenotypes, wbIntermediateFile, sep="\t")
}
# TODO In the future we might extend wormbaseDataRetrieval to retreive variantid and variantname
database$addSubstanceActivity(
    wbPhenotypes[!is.na(gene) & !is.na(phenotypeid)][,
        .(wbmol = molID,
            species = species,
            variantid = NA, 
            variantname = NA,
            conditions = NA,
            gene = gene,
            phenotypeid = phenotypeid,
            phenotypename = phenotypename,
            details = details,
            url = paste0("https://wormbase.org/resources/molecule/",molID),
            sourcedb = "wormbase",
            sourceid = NA)],
    substanceIdentifier = "wbmol",
    type = "phenotypes")
