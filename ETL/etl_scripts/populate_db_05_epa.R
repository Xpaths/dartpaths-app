# Dartpaths ETL : EPA InVitroDB
# 
# Author: Marvin Steijaert
###############################################################################

## InVitroDB version 3.2
# TODO: upgrade to version 3.3. This will take some work as zip file contents are entirely different.
epaDir <- file.path(rawDataDir, "epa")

# TODO: following gives Error in curl::curl_download(url, finalFilename) : 
#    SSL certificate problem: unable to get local issuer certificate
#if(!nchar(curl:::get_bundle())){
#  curl:::set_bundle("/etc/ssl/certs")
#}
invitroDbUrl <- "https://gaftp.epa.gov/Comptox/High_Throughput_Screening_Data/InVitroDB_V3.2/Summary_Files/INVITRODB_V3_2_SUMMARY.zip"
ivdbAssayInfo <- readWebOrFtp(
    url = invitroDbUrl,
    directory = epaDir, fileInArchive = "Assay_Summary_190708.csv")

# only keep 357 single readout assays (out of 1473) with a gene annotation
ivdbAssayInfo <- ivdbAssayInfo[,
    .(assay_name,organism,biological_process_target,intended_target_family, intended_target_gene_symbol,content_readout_type )]
ivdbAssayInfo <- unique(ivdbAssayInfo[content_readout_type %in% "single" & !is.na(intended_target_gene_symbol)])

# From README_INVITRODB_V3_2_SUMMARY.pdf :
# The hit-call matrix contains NA, 0, 1, and -1 values.
# NA indicates the chemical was not tested in the multiple-concentration screening format,
# 0 indicates the chemical was determined inactive, “1” indicates the chemical was determined active
# -1 indicates that the activity could not be determined
# We only keep value 0 and 1 (remove NA and -1 records)
# AC50 (in μM) is only available for hits. The non-informative 1e6 value (=1M) for non-hits are removed to avoid confusion
ivdbHitMatrix <- readWebOrFtp(url = invitroDbUrl, directory = epaDir,
    fileInArchive = "hitc_Matrix_190708.csv") 
ivdbHits <- melt(ivdbHitMatrix, id.vars = "V1")[value %in% c(0,1)] # 2974648 rows
setnames(ivdbHits, c("molecule","assay_name", "hitcall"))
ivdbAC50Matrix <- readWebOrFtp(url = invitroDbUrl, directory = epaDir,
    fileInArchive = "ac50_Matrix_190708.csv")
ivdbAC50 <- melt(ivdbAC50Matrix, id.vars = "V1")[!is.na(value) & value!=1e6] # 271280 rows
setnames(ivdbAC50, c("molecule","assay_name", "ac50"))
ivdbHitsMerged <- merge(ivdbHits, ivdbAC50, by = c("molecule","assay_name"), all = TRUE) # 2974648 rows
ivdbHitsMerged<- merge(ivdbHitsMerged, ivdbAssayInfo, by = "assay_name") # 222983 rows (only single readout assays)

## Annotate with DSSTOX Substance Identifier (DTXSID)
# 
# DSSTox_v2000_full.smi can be created from
# https://gaftp.epa.gov/Comptox/Sustainable_Chemistry_Data/Chemistry_Dashboard/DSSTox_v2000_full.zip
# using ETL/etl_scripts/convert_comptox_sdf_to_smi.py
ivdbChemicals <- readWebOrFtp(url = invitroDbUrl, directory = epaDir,
    fileInArchive = "Chemical_Summary_190708.csv", select = c("code","dsstox_substance_id"))
setnames(ivdbChemicals, c("molecule","dtxsid"))
ivdbHitsMerged <- merge(ivdbHitsMerged, ivdbChemicals, all.x = TRUE, by = "molecule")[, molecule := NULL]
ivdbHitsMerged[, url := paste0("https://comptox.epa.gov/dashboard/dsstoxdb/results?search=",dtxsid,"#bioactivity")]
ivdbHitsMerged[, sourcedb := "EPA invitroDB"]
dsstoxSmi <- normalizePath(file.path(intermediateDataDir,"epa", "DSSTox_v2000_full.smi"), mustWork = FALSE)
dir.create(dirname(dsstoxSmi), showWarnings = FALSE, recursive = TRUE)
if (!file.exists(dsstoxSmi)){
  dsstoxZip <- downloadWebOrFtp(url = "https://gaftp.epa.gov/Comptox/Sustainable_Chemistry_Data/Chemistry_Dashboard/DSSTox_v2000_full.zip", directory = epaDir) 
  dsstoxSdf <- normalizePath(file.path(epaDir,"DSSTox_v2000_full.sdf"), mustWork = FALSE)
  unzip(dsstoxZip, exdir = dirname(dsstoxSdf))
  system(paste(getOption("dartpaths_python"),
          file.path(dartpathsGitDir,"ETL/etl_scripts/convert_comptox_sdf_to_smi.py"),
          "--inputfile", dsstoxSdf, "--outputfile", dsstoxSmi))
  unlink(dsstoxSdf)
}
dsstox <- fread(dsstoxSmi, fill = TRUE, sep ="\t",
    select = c("SMILES","Preferred_name","DSSTox_Compound_id","DSSTox_Substance_id","CASRN","Dashboard_URL"))[!is.na(`Dashboard_URL`)]
dsstox <- unique(dsstox)
dsstox[, substancetype := if(.N>1) "multi" else "mono" , by = DSSTox_Substance_id]
dsstox <- dsstox[,.(dtxsid = DSSTox_Substance_id, smiles = SMILES, name = Preferred_name, substancetype, CAS = CASRN, url = Dashboard_URL)]
dsstox <- dsstox[dtxsid %in% ivdbHitsMerged[,dtxsid]]
# fix outdated urls:
dsstox[,url:= paste0("https://comptox.epa.gov/dashboard/chemical/details/",dtxsid)]
#unlink(dsstoxSmi)


# add substances to database
database$addIdTypes("dtxsid")
system.time(tmp <- database$addSubstances(dsstox, verbose = TRUE, returnTable = TRUE))
# add activity to databae
setnames(ivdbHitsMerged,
    c("assay_name", "hitcall", "ac50", "organism", "biological_process_target", "intended_target_gene_symbol"),
    c("assay", "hitcall", "ac50", "species", "biologicalprocess", "gene"))
database$addSubstanceActivity(ivdbHitsMerged, "dtxsid", type = "invitro")
