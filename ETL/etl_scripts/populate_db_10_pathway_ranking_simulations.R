# Dartpaths ETL : run Monte Carlo simulations for pathway ranking
# 
# Author: Marvin Steijaert
###############################################################################

## Run pathway ranking simulations

pathwayRankingObject <- PathwayRanking$new(database = database, substanceid = NULL,
    pathwayLevels = 3:database$getHumanPathwayLevels()[,max(level)],
    sizePathwayMin = 4, sizePathwayMax = 200)

# 1000 runs (takes several hours)
nRuns <- 1000
backupFile <- file.path(intermediateDataDir, paste0("monte_carlo_all_levels_",nRuns,"_runs.txt.gz"))
if(!file.exists(backupFile)){
  simulations <- rbindlist(list(
          pathwayRankingObject$runSimulations("MP", nRuns = nRuns, querySizes = 1:21),
          pathwayRankingObject$runSimulations("WBPhenotype", nRuns = nRuns, querySizes = 1:20),
          pathwayRankingObject$runSimulations("ZP", nRuns = nRuns, querySizes = c(1:30, 98, 514))
      ))
  fwrite(simulations, backupFile, sep = "\t")  
} else {
  simulations <- fread(backupFile)
}

database$addData("pathwaysimulations", simulations)