# Dartpaths ETL : run Monte Carlo simulations for pathway ranking
# 
# Author: Marvin Steijaert
###############################################################################

## Run pathway ranking simulations
filterICw = 0.2

pathwayRankingObject <- PathwayRanking$new(database = database, substanceid = NULL,
    pathwayLevels = 2:database$getHumanPathwayLevels()[,max(level)],
    sizePathwayMin = 4, sizePathwayMax = 200, filterICw = filterICw)

# optionally print the distribution of number of unique ontology terms (for selected filterICw) per substance
# (can be used to set relevant querySizes)
if(FALSE){
  for (ontology in c("MP","ZP","WBPhenotype")){
    print(ontology)
    thisOntologyTerms <- pathwayRankingObject$database$getData("genepheno")[
        grepl(paste0("^", ontology,":"),go_id)]
    uniqueTerms <- pathwayRankingObject$createCasePhenotype(queryPhenotype = NULL,
        querySpecies = pathwayRankingObject$ontologyToSpeciesName(ontology),
        filterICw = pathwayRankingObject$filterICw)[,unique(go_id)]
    print(database$tables$substancephenotypes[phenotypeid %in% uniqueTerms,length(unique(phenotypeid)), by= substanceid][,table(V1)])
  }
}
if(is.null(filterICw)){
  querySizesMP = 1:21
  querySizesWBPhenotype = 1:20
  querySizesZP = c(1:30, 98, 514)
} else if(filterICw == "mean"){
  querySizesMP = 1:10
  querySizesWBPhenotype = 1:20
  querySizesZP = c(1:30, 83, 208)
} else if(filterICw == 0.2){
  querySizesMP = 1:20
  querySizesWBPhenotype = 1:20
  querySizesZP = c(1:26, 55, 149)
} else stop("please specify query sizes")

nRunsMP <- 10000 # more runs for MP, takes approximately 1 day (optionally set to 1000 to speed up)
nRunsWBPhenotype <- 1000 # 1000 runs takes several hours
nRunsZP <- 1000 # 1000 runs takes several hours
backupFile <- file.path(intermediateDataDir, paste0("monte_carlo_all_levels_", nRuns,
        "_runs_mean_filteric_", ifelse(is.null(filterICw),"null",filterICw), ".txt.gz"))
if(!file.exists(backupFile)){
  simulations <- rbindlist(list(
          pathwayRankingObject$runSimulations("MP", nRuns = nRunsMP, querySizes = querySizesMP),
          pathwayRankingObject$runSimulations("WBPhenotype", nRuns = nRunsWBPhenotype, querySizes = querySizesWBPhenotype),
          pathwayRankingObject$runSimulations("ZP", nRuns = nRunsZP, querySizes = querySizesZP)
      ))
  fwrite(simulations, backupFile, sep = "\t")  
} else {
  simulations <- fread(backupFile)
}

database$addData("pathwaysimulations", simulations, overwrite = TRUE)