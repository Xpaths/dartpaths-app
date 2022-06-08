# Dartpaths ETL : data from Echa website
# 
# Author: Marvin Steijaert
###############################################################################

rawDataDirEcha <- file.path(rawDataDir, "echa")

# CAS numbers from Echa website
# assumes that file has been manually downloaded from
# from https://echa.europa.eu/nl/universe-of-registered-substances
substancesExportFile <-  file.path(manualDownloadDataDir, "echa", "chemical_universe_list_en_dec2020.csv")
if(file.exists(substancesExportFile)){
  reachSubstances <- fread(substancesExportFile, header = TRUE, sep = ",")[`Registration type`=="active registrations(s) under REACH"]
  setnames(reachSubstances, c("Substance Name", "Infocard URL"), c("name", "url"))
  reachSubstances <- reachSubstances[url!=""]
  system.time(tmp <- database$addSubstances(reachSubstances, structureColumn = NULL, verbose = TRUE, returnTable = TRUE))
} else warning("File ", substancesExportFile, " does not exist. So this data cannot be added to the database")

# Pre-registered substances
# assumes that file has been manually downloaded from
# https://echa.europa.eu/nl/information-on-chemicals/pre-registered-substances
  preregSubstancesFile <- file.path(manualDownloadDataDir, "echa", "prs_complete_20090327_en.csv")
if(file.exists(preregSubstancesFile)){
  preregSubstances <- fread(preregSubstancesFile, header = TRUE, sep = ";", quote = '"') # line 64244 contains improper quoting, fread will give a warning
  setnames(preregSubstances, c("EC-Number","CAS-Number","Name"), c("EC","CAS","name"))
  system.time(tmp <- database$addSubstances(preregSubstances, structureColumn = NULL, verbose = TRUE, returnTable = TRUE))
} else warning("File ", preregSubstancesFile, " does not exist. So this data cannot be added to the database")
  
# CLP data
# see: https://echa.europa.eu/en/information-on-chemicals/annex-vi-to-clp
# hazardcategory is not yet supportd by the database/app
# this cannot be added as a separate column, but would require an additional table or adding this information to the
# same column as the hazardcodes

# clpUrl <- "https://echa.europa.eu/documents/10162/17218/annex_vi_clp_table_atp14_en.xlsx" # In force from 9 September 2021
clpUrl <- "https://echa.europa.eu/documents/10162/17218/annex_vi_clp_table_atp15_en.xlsx" # In force from 1 March 2022
clpFile <- downloadWebOrFtp(url = clpUrl, directory = rawDataDirEcha)
clpData <- as.data.table(openxlsx::read.xlsx(clpFile, startRow = 6))
clpData <- readWebOrFtp(url = clpUrl, directory = rawDataDirEcha, startRow = 6)

setnames(clpData, paste0("X", seq_len(ncol(clpData))))
clpData <- clpData[,.(
				EC = gsub("\\[[0-9]+\\]"," ",X3),
				CAS = gsub("\\[[0-9]+\\]"," ",X4),
                hazardcategory =strsplit(X5,"[^-[:alnum:]\\. ]+"),
				hazardcode = mapply(function(x,y) union(x,y), strsplit(X6,"[^[:alnum:]]+"), strsplit(X8,"[^[:alnum:]]+")))]
collapseList <- function(x){
	ifelse(length(x), paste(x, collapse=","), NA_character_)
}
clpData[, hazardcode := sapply(hazardcode,collapseList)]
clpData[, hazardcategory := sapply(hazardcategory,collapseList)] 
clpData = clpData[, .(CAS = strsplit(CAS, "\\s+")[[1]]), by = .(EC, hazardcode, hazardcategory)]
clpData = clpData[, .(EC = strsplit(EC, "\\s+")[[1]]), by = .(CAS, hazardcode, hazardcategory)]
clpData[, url := "https://echa.europa.eu/information-on-chemicals/annex-vi-to-clp"]
clpData <- clpData[!is.na(hazardcode) & !(is.na(EC)&is.na(CAS))][,ind:=.I]
clpDataCAS <- unique(clpData[!is.na(CAS) & !CAS%in%"-",.(CAS = unlist(CAS), hazardcode), by = ind][,.(CAS,hazardcode)])
clpDataEC <- unique(clpData[!is.na(EC) & !EC%in%"-",.(EC = unlist(EC), hazardcode), by = ind][,.(EC,hazardcode)])
# add both clpDataCAS and clpDataEC (duplicates will be ignored) to avoid missing records
database$addSubstanceActivity(clpDataEC, "EC", type = "clp")
database$addSubstanceActivity(clpDataCAS, "CAS", type = "clp")


