# Dartpaths ETL : UVCB/substance categories
# 
# Author: Marvin Steijaert
###############################################################################

## CONCAWE categories
# https://www.concawe.eu/reach/
concaweMembers <- readWebOrFtp(url = "https://www.concawe.eu/wp-content/uploads/INVENTORY_17_2021.xlsx",
    file.path(rawDataDir,"concawe"),
    sheet = 2, startRow = 9)
setnames(concaweMembers, c("CATEGORY","Name", "EC.Description"), c("category","name", "description"))
concaweMembers[,Remarks:= NULL]
concaweMembers[,`CMR?`:= NULL]
concaweMembers[!is.na(description), substancetype := "uvcb"] 
concaweProfiles <- NULL # Not yet available
concaweInfo <- data.table(category = concaweMembers[,unique(category)],
    description = "",
    url = "https://www.concawe.eu/reach")
database$addSubstanceCategory(categoryInfo = concaweInfo, categoryMembers = concaweMembers,
    categoryProfiles = concaweProfiles, verbose = TRUE)

## LOA-REACH categories
# pdf files can be downloaded from https://loa-reach.com/categories.html, but do not allow
# straightforward and consistent automated extraction
# Hence, we read a text file loa_category_constituents_210920.txt that was manually composed from those pdf files
# Category B does not have any constituents with a CAS annotation
# Category M is no longer listed on https://loa-reach.com/categories.html.
# Categories N and P do not have a pdf with composition
# Categories F and I does not (seem to) exist
loaConstituents = readLines(file.path(sharedDataDir, "loa", "loa_category_constituents_210920.txt"))
splits = which(grepl("^#\\s*Category",loaConstituents))
allTables <- list()
for (tableNum in head(seq_along(splits),-1)){
  thisCategory <- gsub("^#\\s*Category ([A-Z]).*","\\1",loaConstituents[splits[tableNum]])
  thisTable <- fread(
      paste(loaConstituents[(splits[tableNum]+1):(splits[tableNum+1]-1)], collapse = "\n"),
      fill = TRUE, sep = "\t",
      col.names = c("name", "CAS","typicalconcentration","concentrationrange","remarks"))
  thisTable[,remarks := NULL]
  thisTable[,category := thisCategory ]
  allTables[[length(allTables)+1]] <- thisTable
}
loaProfiles <- rbindlist(allTables)
loaProfiles <- loaProfiles[!is.na(CAS)]
loaProfiles[, category := paste("LOA category",category)]
loaInfo <- data.table(category = loaProfiles[,unique(category)],
    description = "",
    url = "https://loa-reach.com/categories.html")
# file loa_category_members_210920.csv was manually created using contents of 
# https://loa-reach.com/substance-information-and-obtaining-letters-of-access.html
loaMembers = fread(file.path(sharedDataDir, "loa", "loa_category_members_210920.csv"))
loaMembers <- loaMembers[Category != "MC"]
setnames(loaMembers, c("category", "CAS", "EC", "name", "description"))
loaMembers[, category := paste("LOA category",category)]
loaMembers[,substancetype := "uvcb"]
loaMembers[category %in% "N", substancetype := NA_character_] # This category consists primarily of 1-butene, 2-butene, 2-methylpropene and butene mixed isomers.
loaMembers[category %in% "P", substancetype := "mono"] # This category covers mono-constituent C1-C4 alkanes. 

database$addSubstanceCategory(categoryInfo = loaInfo, categoryMembers = loaMembers,
    categoryProfiles = loaProfiles, verbose = TRUE)

