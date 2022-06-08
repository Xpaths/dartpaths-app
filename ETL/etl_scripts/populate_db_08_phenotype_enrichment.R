# Dartpaths ETL : orthology, gene -> phenotype mapping
# 
# Author: Marvin Steijaert
###############################################################################

### Phenotype enrichment
enrichmentDir <- normalizePath(file.path(rawDataDir, "phenotype_enrichment"), mustWork = FALSE)
enrichmentDbDir <- file.path(enrichmentDir, "databases")
enrichmentPhenoDir <- file.path(enrichmentDbDir, "phenotype")
enrichmentOntologyDir <- file.path(enrichmentDbDir, "ontology")
enrichmentOrthologsDir <- file.path(enrichmentDbDir, "orthologs")

tmp <- sapply(c(enrichmentDbDir, enrichmentPhenoDir, enrichmentOntologyDir, enrichmentOrthologsDir), dir.create, showWarnings = FALSE, recursive = TRUE)

## run phenotype_enrichment.py script
pythonExecutable <- normalizePath(getOption("dartpaths_python"))
pythonScript <- normalizePath(file.path(etlScriptsDir, "phenotype_enrichment.py"))
pathwayName <- "xxx" # required by python script, but currently not used
#topLevelPathwayName <- "yyy" # required by python script, but currently not used
humanPathwayIds <- database$getHumanPathways(levels = 3:20, sizePathwayMin = 4, sizePathwayMax  = 200, renameEvent = TRUE)[,
    unique(reactome_pathway_stable_identifier)]

# create function to parse results from phenotype enrichment
phenotypeEnrichmentToTable <- function(resultFile, pathwayId){ 
  thisResult <- readLines(resultFile)
  if (sum(grepl("^Result$",thisResult))>1) stop("file contains multiple results")
  startingPoints <- which(grepl("^orthologs:",thisResult))
  endPoints <- c(startingPoints[-1]-1,length(thisResult))
  result <- data.table()
  for (chunkId in seq_along(startingPoints)){
    thisChunk <- thisResult[startingPoints[chunkId]:endPoints[chunkId]]
    if(length(thisChunk) > 4){
      thisChunkTable <- suppressWarnings(fread(text = gsub("\"$","",gsub("^\"","", thisChunk[-1:-3])), header = TRUE, sep = "\t", fill = TRUE)) # get rid of unneeded quotes
      thisSpecies = gsub("organism: ", "", thisChunk[3])
      if (thisSpecies == "slimemould"){
        thisSpecies = "slimemold"
      }
      if ("Affected Term" %in% names(thisChunkTable)){
        setnames(thisChunkTable,"Affected Term", "Phenotype Name")
      }
      if ("Enriched Phenotype" %in% names(thisChunkTable) && ! "Phenotype Name" %in% names(thisChunkTable)){
        setnames(thisChunkTable,"Enriched Phenotype", "Phenotype Name")
      }
      thisChunkTable[, species:= thisSpecies]
      thisChunkTable[, reactome_pathway_stable_identifier := pathwayId]
      result <- rbind(result, thisChunkTable, fill = TRUE)
    }
  }
  if ("reactome_pathway_stable_identifier" %in% names(result)) setcolorder(result, c("reactome_pathway_stable_identifier","species"))
  return(result)
}

setwd(enrichmentDir)

runAndParseEnrichment <- function(pathwayId, scriptOutput = FALSE, nDaysUpdateInputData = 300, raiseError = FALSE){
  # run enrichment script
  resultFile <- file.path(enrichmentDir, paste0(pathwayName, "_", pathwayId, "_Enrichment_Results"), "combined_result.txt")
  if (file.exists(resultFile)){
    exitCode <- 0
  } else {
    exitCode <- system2(pythonExecutable, c(pythonScript, pathwayId,
            pathwayName, paste0(enrichmentDir,"/"), nDaysUpdateInputData),
        stdout = ifelse(scriptOutput, "", FALSE),
        stderr =  ifelse(scriptOutput, "", FALSE))
  }
  if (exitCode == 0){
    # parse output and return data.table
    res <- phenotypeEnrichmentToTable(resultFile, pathwayId)
    return(res)
  } else if(raiseError){
    stop("Something when wrong while processing pathway ",pathwayId, " with phenotype_enrichment.py\n")
  } else {
    cat("Skipped pathway",pathwayId, "after failure in phenotype_enrichment.py:\n")
    return(NULL)
  }       
}

cat("Starting phenotype enrichment for ", length(humanPathwayIds), "pathways...\n")
scriptOutput <- FALSE # set to TRUE to print to console
if(require(pbmcapply)){
  # parallel, requires pbmcapply package
  # process first pathway separately to avoid parallel downloading of identical files
  tmp <- runAndParseEnrichment(humanPathwayIds[1], scriptOutput = TRUE, raiseError = TRUE)
  enrichmentList <- pbmcapply::pbmclapply(humanPathwayIds, runAndParseEnrichment, scriptOutput = scriptOutput, mc.cores = max(1, detectCores()/2))  
} else {
  # sequential
  enrichmentList <- lapply(humanPathwayIds, runAndParseEnrichment, scriptOutput = scriptOutput)
}
cat("Finished phenotype enrichment. Processed", length(enrichmentList), "pathways (including",sum(sapply(enrichmentList, is.null)), "failures)\n")
# bind results to a single table
enrichmentResult <- rbindlist(enrichmentList, fill = TRUE)
# write raw results
fwrite(enrichmentResult, file.path(intermediateDataDir, "phenotype_enrichment_level3_and_higher_pathways.txt"), sep = "\t")

# add results to database
setnames(enrichmentResult,
    c("Enriched Phenotype", "Phenotype Name", "P-value", "n", "m", "i", "rank", "q-value", "adjusted-p-value", "rejected", "Overlap Genes"),
    c("phenotypeid", "phenotypename","pvalue", "n", "m", "i", "rank", "qvalue", "adjusted_pvalue", "rejected", "overlapping_genes"))
database$addData(
    "pathwayphenotypes",
    enrichmentResult[,unique(.SD),.SDcols = database$tableColumns$pathwayphenotypes], overwrite = FALSE
)
