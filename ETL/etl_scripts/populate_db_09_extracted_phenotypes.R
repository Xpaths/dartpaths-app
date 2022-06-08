# Dartpaths ETL : add extracted phenotypes from literature
# 
# Author: Marvin Steijaert
###############################################################################

## Add phenotypes extracted from literature

# DES phenotypes
library(tidyxl)
substanceNames <- database$getSubstanceExtIds("name")
idDES <- substanceNames[grepl("^diethylstilbestrol$",name, ignore.case = TRUE), substanceid]
if(length(idDES)!=1){
  warning("Multiple substances with name diethylstilbestrol. Assigning records to first substance.")
  idDES <- idDES[1]
} 


extraPhenotypesFile1 <- file.path(sharedDataDir, "mammalian_phenotypes/Diethylstilbestrol_AutomatedExtraction_Curated.xlsx")
cells <- xlsx_cells(extraPhenotypesFile1)
formats <- xlsx_formats(extraPhenotypesFile1)
greenRows <- cells[cells$local_format_id %in% which( formats$local$fill$patternFill$fgColor$rgb == "FF9BBB59"),"row" ]$row
extraPhenotypes1 <- setDT(openxlsx::read.xlsx(extraPhenotypesFile1, sheet = 1))
extraPhenotypesForDB1 <- extraPhenotypes1[greenRows-1,][,
    .(
        substanceid = idDES,
        species = "mammalian",
        variantid = NA_character_,
        variantname = NA_character_,
        conditions = NA_character_,
        phenotypeid = TERMID,
        phenotypename = gsub("\\s*$","",TERM),
        "gene" = NA_character_,
        details = `MATCHING.SENTENCE`,
        url = NA,
        sourcedb = "pipeline + manual curation",
        sourceid = NA
    )]
database$addSubstanceActivity(extraPhenotypesForDB1 , substanceIdentifier = "substanceid", type = "phenotypes")

extraPhenotypesFile1 <- file.path(sharedDataDir, "mammalian_phenotypes/Diethylstilbestrol_ManualPhenotypeExtraction.xlsx")
extraPhenotypes2 <- setDT(openxlsx::read.xlsx(extraPhenotypesFile1, sheet = 1))
extraPhenotypesForDB2 <- extraPhenotypes2[,
    .(
        substanceid = idDES,
        species = ifelse(ORGANISM %in% c("rat", "human"), "mammalian", ORGANISM),
        variantid = NA_character_,
        variantname = NA_character_,
        conditions = NA_character_,
        phenotypeid = TERMID,
        phenotypename = gsub("\\s*$","",TERM),
        "gene" = NA_character_,
        details = `MATCHING.SENTENCE`,
        url = METADATA,
        sourcedb = "manual extraction",
        sourceid = NA
    )]

database$addSubstanceActivity(extraPhenotypesForDB2 , substanceIdentifier = "substanceid", type = "phenotypes")
