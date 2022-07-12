# R6 class for pathway ranking
# 
# Author: Marvin Steijaert
###############################################################################

#' Class for pathway ranking of a selected substance
#' 
#' @export
#' @import R6
#' @import digest
PathwayRanking <- R6Class("PathwayRanking",
    public = list(
        #' @field classname Name of the class
        classname = "PathwayRanking",
        #' @field database Reference to DartDB object
        database = NULL,
        #' @field substanceid Identifier of substance(s) in self$database
        substanceid = 0,
        #' @field mammalianPhenotypes Field with all observed mammalian phenotypes for the substance(s)
        mammalianPhenotypes = NULL,
        #' @field nonMammalianPhenotypes Field with all observed non-mammalian phenotypes for the substance(s)
        nonMammalianPhenotypes = NULL,
        #' @field allInVitro Field with all in vitro data for the substance(s)
        allInVitro = NULL, 
        #' @field humanGenesList Human pathway data
        humanGenesList = NULL,
        #' @field pathwayLevels Level(s) of selected human pathways
        pathwayLevels = NULL,
        #' @field pathwaySummaryLevels Level(s) of selected human pathways used for summary
        pathwaySummaryLevels = NULL,
        #' @field sizePathwayMin Minimum number of genes in selected human pathways
        sizePathwayMin = NULL,
        #' @field sizePathwayMax Maximum number of genes in selected human pathways
        sizePathwayMax = NULL,
        #' @field cache Cache for pathway ranking with different data selections
        cache = list(pathwayRanking = list()),
        #' @field defaultSelection default data selections
        defaultSelection = NULL,
        #' @field mammalianPhenotypeOntologies Supported ontologies for mammalian phenotypes
        mammalianPhenotypeOntologies = c("MP"),
        #' @field nonMammalianPhenotypeOntologies Supported ontologies for non-mammalian phenotypes
        nonMammalianPhenotypeOntologies = c("ZP", "WBPhenotype"),
        #' @field phenotypeRankingLowestLevel Logical, indicates if phenotype-based ranking should use lowest level + aggregation
        phenotypeRankingLowestLevel = FALSE,
        #' @field simulationResults Simulation results used for p-value estimation
        simulationResults = NULL,
        #' @description Create a new PathwayRanking object
        #' @param database DartDB object
        #' @param substanceid selected substanceid in database
        #' @param pathwayLevels optional level(s) of selected pathways
        #' @param pathwaySummaryLevels Level(s) of selected human pathways used for summary
        #' @param sizePathwayMin minimum number of genes in selected HUMAN pathways
        #' @param sizePathwayMax maximum number of genes in selected HUMAN pathways
        #' @param phenotypeRankingLowestLevel logical, indicates if phenotype-based ranking should use lowest level + aggregation
        initialize = function(database, substanceid,
            pathwayLevels = NULL, pathwaySummaryLevels = if(length(pathwayLevels)) min(pathwayLevels), 
            sizePathwayMin = 0, sizePathwayMax = Inf, phenotypeRankingLowestLevel = FALSE) {
          
          # store init arguments
          self$database <- database
          self$substanceid <- substanceid
          self$pathwayLevels <- pathwayLevels
          self$pathwaySummaryLevels <- pathwaySummaryLevels
          self$sizePathwayMin <- sizePathwayMin
          self$sizePathwayMax <- sizePathwayMax
          self$phenotypeRankingLowestLevel <- phenotypeRankingLowestLevel
          
          simulations <- self$database$getData("pathwaysimulations")
          self$simulationResults <- if (is.data.table(simulations) && simulations[,.N]>0) {
                simulations
              } else NULL
          
          if (!is.null(substanceid)){
            # lookup data for substanceid
            self$allInVitro <- database$getSubstanceActivity(self$substanceid, summarize = FALSE)$invitro[
                order(ac50, species == "human", decreasing = TRUE)]
            allPhenotypes <- database$getData("substancephenotypes")[
                substanceid %in% self$substanceid & !is.na(phenotypeid) & phenotypeid !=""]
            
            # Pathway ranking for individual phenotype ontologies
            ontologyPattern <- "^([A-Za-z]+):.*$"
            allPhenotypes[, phenotypeOntology := gsub(ontologyPattern,"\\1",
                    ifelse(grepl(ontologyPattern,phenotypeid),
                        phenotypeid,NA))]
            
            self$mammalianPhenotypes <- allPhenotypes[phenotypeOntology %in% self$mammalianPhenotypeOntologies] 
            self$nonMammalianPhenotypes <- allPhenotypes[phenotypeOntology %in% self$nonMammalianPhenotypeOntologies]
            
            # create default selection mask for rows in in vitro and phenotype data
            self$defaultSelection = list(
                inVitro = rep(TRUE, nrow(self$allInVitro)),
                mammalianPhenotypes = rep(TRUE, nrow(self$mammalianPhenotypes)),
                nonMammalianPhenotypes = rep(TRUE, nrow(self$nonMammalianPhenotypes))
            )
          }
          
          # lookup human pathway data for specified levels, sizePathwayMin, sizePathwayMax
          # for in vitro counts use all levels: so lowestLevelOnly = FALSE
          self$humanGenesList <- database$getHumanPathwayGenes(levels = pathwayLevels,
              sizePathwayMin = sizePathwayMin, sizePathwayMax = sizePathwayMax,
              renameEvent = TRUE, lowestLevelOnly = FALSE)
          
        },
        #' @description Calculates pathway ranking (without using cache)
        #' @param inVitroSelection logical vector indicating which rows in self$allInVitro should be used
        #' @param mammalianPhenoSelection logical vector indicating which rows in self$mammalianPhenotypes should be used
        #' @param nonMammalianPhenoSelection logical vector indicating which rows in self$nonMammalianPhenotypes should be used
        .rankPathways = function(inVitroSelection, mammalianPhenoSelection, nonMammalianPhenoSelection){
          
          # apply selection
          inVitroTable <- self$allInVitro[inVitroSelection]
          mammalianPhenoTable <- self$mammalianPhenotypes[mammalianPhenoSelection]
          nonMammalianPhenoTable <- self$nonMammalianPhenotypes[nonMammalianPhenoSelection]
          phenotypeTable <- rbind(mammalianPhenoTable, nonMammalianPhenoTable)
          
          results <- list()
          results[["contributingPhenotypes"]] <- NULL
          results[["selectedMammalianPhenotypes"]] <- copy(mammalianPhenoTable)
          results[["selectedNonMammalianPhenotypes"]] <- copy(nonMammalianPhenoTable)
          results[["selectedInVitro"]] <- copy(inVitroTable)
          
          ## Count in vitro hits and add in vitro ranking
          
          # humanGenesList: list with data.tables "pathways" and "pathwayGenes
          humanGenesList <- copy(self$humanGenesList)
          humanGenesList$pathwayGenes[,url:=NULL]
          
          # add pathways
          thisSubstanceGenes <- unique(merge(
                  inVitroTable,
                  humanGenesList$pathwayGenes,
                  by = "gene"))
          
          # count hits and measured genes per pathway
          thisSubstanceUniqueGenes <- thisSubstanceGenes[,.(hit = any(hitcall)),
              by = c("geneid", "gene", "genelongname", "reactome_pathway_stable_identifier",
                  "event_name", "level", "nGenesPathway")] 
          thisSubstancePathways <- thisSubstanceUniqueGenes[,
              .(
                  genes = paste(unique(gene[hit]), collapse = ","),
                  invitroPwHits = sum(hit),
                  invitroPwMeasured = .N),
              by = .(reactome_pathway_stable_identifier)]
          
          # Save a table of genes (hits only) contributing to pathway ranking
          # One record per gene-pathway combination (combining all species and assays for that gene)
          results[["contributingInVitroGenes"]] <- thisSubstanceGenes[hitcall == TRUE,
              .(ac50 = maxWithNA(ac50)), by = .(geneid, gene,
                  genelongname, reactome_pathway_stable_identifier, event_name, level)]
          
          # In vitro pathway ranking score
          aggregateToLevels <- if(is.null(self$pathwaySummaryLevels)) {
                self$database$getHumanPathwayLevels()[,min(level)] : self$database$getHumanPathwayLevels()[,max(level)]
                
              } else self$pathwaySummaryLevels
          summaryRanking <- humanGenesList$pathways[level %in% aggregateToLevels]
          # TODO: consider storing levelMap as a class attribute
          levelMap <- rbindlist(lapply(aggregateToLevels,self$database$getHumanPathwaysForLevel))
          results[["levelMap"]] = copy(levelMap)
          individualPathwayScores <- copy(humanGenesList$pathways)
          summaryRanking <- merge(summaryRanking, thisSubstancePathways, by = "reactome_pathway_stable_identifier", all.x = TRUE)
          summaryRanking[is.na(nGenesPathway), nGenesPathway := 0]
          summaryRanking[is.na(genes), genes := ""]
          summaryRanking[is.na(invitroPwHits), invitroPwHits := 0]
          summaryRanking[is.na(invitroPwMeasured), invitroPwMeasured := 0]
          summaryRanking[, invitroSubstanceHits := thisSubstanceUniqueGenes[hit == TRUE, length(unique(gene))]] # number of human genes with at least one hit
          summaryRanking[, invitroSubstanceMeasured := thisSubstanceUniqueGenes[, length(unique(gene))]] # number of measured human genes
          summaryRanking[, scoreInVitro := ifelse(invitroPwMeasured>0, invitroPwHits/(invitroSubstanceHits+invitroPwMeasured-invitroPwHits),0)] # Jaccard index
          
          ## Phenotype-based ranking of pathways
          phenoOntologies <- intersect(
              phenotypeTable[,unique(phenotypeOntology)],
              c(self$mammalianPhenotypeOntologies, self$nonMammalianPhenotypeOntologies))
          
          for(ontology in phenoOntologies){
            thisOntologyResults <- self$phenotypePathwayByOntology(
                queryPhenotype = phenotypeTable[phenotypeOntology==ontology, unique(phenotypeid)],
                go_id_prefix = ontology,
                filterICw = NULL,
                lowestLevelOnly = self$phenotypeRankingLowestLevel,
                pValues = TRUE)
            
            if(is.null(thisOntologyResults$ranking) || nrow(thisOntologyResults$ranking) == 0) next
            
            thisOntologyResults$contributingPhenotypes[, mammalian:=(ontology %in% self$mammalianPhenotypeOntologies)]  
            
            # aggregate pathway scores to parent level(s)
            mergedRanking <- merge(thisOntologyResults$ranking, levelMap, by = "reactome_pathway_stable_identifier")
            results[[paste0(ontology,"_indivPathways")]] <- thisOntologyResults$ranking
            results[[ontology]] <- mergedRanking
            contributingPhenotypesWithLevelParent <- merge(thisOntologyResults$contributingPhenotypes, levelMap,
                on = "reactome_pathway_stable_identifier", allow.cartesian = TRUE)
            results[[paste0(ontology,"_contributingPhenotypes_individualPathways")]] <- thisOntologyResults$contributingPhenotypes
            results[[paste0(ontology,"_contributingPhenotypes_aggregatedPathways")]] <- contributingPhenotypesWithLevelParent
            resHuman <- mergedRanking[ , .(p_min_phenotype = minWithNA(pvalue)), by = level_parent]
            setnames(resHuman, "level_parent", "reactome_pathway_stable_identifier")
            
            # append aggregated scores for this ontology
            summaryRanking <- merge(summaryRanking, resHuman, by = "reactome_pathway_stable_identifier", all.x = TRUE)
            setnames(summaryRanking, "p_min_phenotype", paste0("p_min_",ontology))
            
            # append individual scores for this ontology
            individualPathwayScores <- merge(individualPathwayScores,
                thisOntologyResults$ranking[,.(reactome_pathway_stable_identifier, p_phenotype_ = pvalue)],
                by = "reactome_pathway_stable_identifier", all.x = TRUE)
            #individualPathwayScores[is.na(p_phenotype_), p_phenotype_ := 1] # p-value=1 if no evidence was found
            setnames(individualPathwayScores, "p_phenotype_", paste0("p_", ontology))
            
            # append new contributing phenotypes
            thisOntologyContributionPhenotypes <- merge(
                summaryRanking[,.(reactome_pathway_stable_identifier, event_name, level)],
                unique(contributingPhenotypesWithLevelParent[ ,
                        .(reactome_pathway_stable_identifier = level_parent, go_id, go_id_name, mammalian)]),
                by = "reactome_pathway_stable_identifier")[!is.na(event_name)]
            
            results$contributingPhenotypes = rbind(
                results$contributingPhenotypes,
                thisOntologyContributionPhenotypes)
          }
          
          ## count number of contributing phenotypes
          if (is.null(results$contributingPhenotypes) || nrow(results$contributingPhenotypes) == 0){
            summaryRanking[, contributingPhenotypesMammalian:=0]
            summaryRanking[, contributingPhenotypesNonMammalian:=0]
            summaryRanking[, totalPhenotypesMammalian:=0]
            summaryRanking[, totalPhenotypesNonMammalian:=0]
          } else {
            summaryRanking <- merge(summaryRanking,
                results$contributingPhenotypes[,.(
                        contributingPhenotypesMammalian = sum(mammalian),
                        contributingPhenotypesNonMammalian = sum(!mammalian)),
                    by=reactome_pathway_stable_identifier],
                all.x = TRUE)
            summaryRanking[is.na(contributingPhenotypesMammalian), contributingPhenotypesMammalian:=0]
            summaryRanking[is.na(contributingPhenotypesNonMammalian), contributingPhenotypesNonMammalian:=0]
            summaryRanking[, totalPhenotypesMammalian:=
                    results$contributingPhenotypes[mammalian == TRUE, length(unique(go_id))]]
            summaryRanking[, totalPhenotypesNonMammalian:=
                    results$contributingPhenotypes[mammalian == FALSE, length(unique(go_id))]]
          }
          
          ## combine scores of different ontologies
          adjustMethod <- "fdr"
          
          # p_adjusted_mammalian: Mammalian in vivo phenotypes
          if ("p_MP" %in% names(individualPathwayScores)){
            if (is.null(adjustMethod)){
              individualPathwayScores[, p_adjusted_mammalian := p_MP]
            } else {
              individualPathwayScores[, p_adjusted_mammalian := p.adjust(p_MP, method = adjustMethod)]
            }
          }  else {
            individualPathwayScores[, p_adjusted_mammalian := NA_real_]
          }
          # p_adjusted_NAM : NAM phenotypes
          # harmonic mean of ZP and WBPhenotype scores
          if(any(c("p_ZP", "p_WBPhenotype") %in% names(individualPathwayScores))){
            # non-existing fields like individualPathwayScores[["scoreXX"]] will yield NULL, which will be ignored by harmonicMeanColumnWise
            individualPathwayScores[, p_adjusted_NAM := 
                    harmonicMeanColumnWise(
                        individualPathwayScores[["p_ZP"]],
                        individualPathwayScores[["p_WBPhenotype"]],
                        na.rm = TRUE, # NA indicates that no p-value could be estimated
                        adjustMethod  = adjustMethod
                    )]
          } else {
            individualPathwayScores[, p_adjusted_NAM := NA]
          }
          # p_adjusted_all : Summary of Mammalian and NAM
          # harmonic mean of all ontologies
          if(any(c("p_MP", "p_ZP", "p_WBPhenotype") %in% names(individualPathwayScores))){
            # non-existing fields like individualPathwayScores[["scoreXX"]] will yield NULL, which will be ignored by harmonicMeanColumnWise
            individualPathwayScores[, p_adjusted_all := 
                    harmonicMeanColumnWise(
                        individualPathwayScores[["p_MP"]],
                        individualPathwayScores[["p_ZP"]],
                        individualPathwayScores[["p_WBPhenotype"]],
                        na.rm = TRUE, # NA indicates that no p-value could be estimated
                        adjustMethod  = adjustMethod
                    )]
          } else {
            individualPathwayScores[, p_adjusted_all := NA]
          }
          
          # add minimum scores of child pathways to summary table
          individualPathwayScores <- merge(individualPathwayScores,levelMap, by = "reactome_pathway_stable_identifier" ) # merge with levelMap to add parent_level
          minScores <- individualPathwayScores[ , .(
                  min_p_adjusted_all = minWithNA(p_adjusted_all),
                  min_p_adjusted_mammalian = minWithNA(p_adjusted_mammalian),
                  min_p_adjusted_NAM = minWithNA(p_adjusted_NAM)),
              by = level_parent]
          setnames(minScores, "level_parent", "reactome_pathway_stable_identifier")
          summaryRanking <- merge(summaryRanking, minScores, by = "reactome_pathway_stable_identifier", all.x = TRUE)
          
          results[["individualPathwayScores"]] <- individualPathwayScores
          results[["summary"]] <- summaryRanking[!is.na(min_p_adjusted_all) | scoreInVitro > 0][
              order(min_p_adjusted_all, -scoreInVitro, decreasing = FALSE)]
          
          return(results)
        },
        #' @description Calculates pathway ranking (using cache for speedup)
        #' @param inVitroSelection (optional) logical vector indicating which rows in self$allInVitro should be used
        #' @param mammalianPhenoSelection logical vector indicating which rows in self$mammalianPhenotypes should be used
        #' @param nonMammalianPhenoSelection logical vector indicating which rows in self$nonMammalianPhenotypes should be used
        rankPathways = function(
            inVitroSelection = self$defaultSelection$inVitro,
            mammalianPhenoSelection = self$defaultSelection$mammalianPhenotypes,
            nonMammalianPhenoSelection = self$defaultSelection$nonMammalianPhenotypes
        ){
        
          cat(sprintf("Calculating pathway ranking with %d in vitro, %d mammalian and %d non-mammalian records...\n",
              sum(inVitroSelection), sum(mammalianPhenoSelection), sum(nonMammalianPhenoSelection)))
          hash <- digest(list(inVitroSelection, mammalianPhenoSelection, nonMammalianPhenoSelection))
          if (! hash %in% names(self$cache$pathwayRanking)){
            startTime <- Sys.time()
            self$cache$pathwayRanking[[hash]] <- self$.rankPathways(inVitroSelection, mammalianPhenoSelection, nonMammalianPhenoSelection)
            endTime <- Sys.time()
            cat("Calculation of pathway ranking took", round(endTime - startTime), "s\n")
          } else {
            cat("Returning cached result of pathway ranking\n")
          }
          return(copy(self$cache$pathwayRanking[[hash]]))
        },
        #' @description variant of phenotypePathway that allows specifying ontology instead of species
        #' @param queryPhenotype list of phenotypes, e.g. c("ZP:0004424","ZP:0008523")
        #' @param go_id_prefix prefix of ontology as used in queryPhenotype. (e.g., "ZP", "MP")
        #' @param filterICw number that scales the meanICweight, e.g. 1
        #' @param lowestLevelOnly Only include lowest level genes for pathways
        #' @param pValues If TRUE, p-value estimaes are calculated.
        #' @return list with data.tables for ranking and contributing phenotypes per pathway
        #' @author Monique van der Voet
        #' @examples
        #' \dontrun{
        #' ranking <- phenotypePathwayByOntology(
        #' queryPhenotype = c("ZP:0004424","ZP:0008523","ZP:0020346","ZP:0000038","ZP:0001036","ZP:0001211","ZP:0004424"),
        #' go_id_prefix = "ZP",
        #' filterICw = NULL
        #' )
        #' }
        phenotypePathwayByOntology = function(queryPhenotype,
            go_id_prefix,
            filterICw = NULL,
            lowestLevelOnly = FALSE,
            pValues = TRUE) {
          
          if (length(queryPhenotype)==0) return(NULL)
          
          if (pValues){
            simulations <- self$simulationResults
            if (is.null(simulations)) stop("No simulation results available for p-value estimation.")
          }
          
          pathwaySpecies <- switch(
              # remove trailing colon to support both "ZP" and "ZP:"
              gsub("\\^","",
                  gsub(":$","",go_id_prefix)),
              ZP = "Danio rerio",
              MP = "Mus musculus",
              WBPhenotype = "Caenorhabditis elegans"
          )
          
          # casePheno: icweights for obserserved phenotypes (go_id)
          casePheno <- self$createCasePhenotype(queryPhenotype = queryPhenotype, querySpecies = NULL, filterICw = filterICw)
          
          # profile: score for each gene
          profileList <- self$createProfile(casePheno = casePheno, querySpecies = NULL, go_id_prefix = go_id_prefix)
          profile <- profileList$geneScores
          
          # targetPathway: count genes per pathway that are associated with pathway
          targetPathway <- self$matchProfile(profile, querySpecies = pathwaySpecies, lowestLevelOnly = lowestLevelOnly)
          ranking <- self$rankAUC(targetPathway, profile)
          
          # p-values
          if (pValues){
            ranking[, pvalue := self$estimatePvalueVector(go_id_prefix, length(unique(queryPhenotype)), reactome_pathway_stable_identifier, rankPercent)]
          }
          
          # list phenotypes contributing to pathway ranking
          contributingPhenotypes <- merge(profileList$geneObservedPhenotypes[,.(db_object_id, go_id, go_id_name, taxon)],
              targetPathway[,.(reactome_pathway_stable_identifier, event_name, source_database_identifier)],
              by.x = "db_object_id", by.y = "source_database_identifier")
          contributingPhenotypes[, db_object_id := NULL]
          contributingPhenotypes <- unique(contributingPhenotypes)
          
          return(list(
                  ranking = ranking,
                  contributingPhenotypes = contributingPhenotypes
              ))
        },
        #' @description Create a test case from a list of phenotypes (phenotype>gene>pathway)
        #' @param queryPhenotype list of phenotypes, e.g. c("ZP:0004424","ZP:0008523")
        #' @param querySpecies (optional) character string with species, e.g. "Caenorhabditis elegans"
        #' @param filterICw Filter to use for IC, should be, NULL (no filtering) "mean" or number 0-1.
        #' @return casePheno data.table with GO_ID and ICweight
        #' @author Monique van der Voet
        #' @examples
        #' \dontrun{
        #' self$createCasePhenotype(queryPhenotype = c("ZP:0004424","ZP:0008523"), "Danio rerio", 1)
        #' }
        createCasePhenotype = function(queryPhenotype,
            querySpecies = NULL,
            filterICw = NULL) {
          
          # phenotypes of genes
          genePheno <- self$database$getData("genepheno")
          
          # Retrieve phenotype and IC weight
          casePheno <- genePheno
          if (!is.null(querySpecies)){
            casePheno <- casePheno[taxon == self$database$retrieveTaxon(querySpecies)]
          }
          casePheno <- casePheno[go_id %in% queryPhenotype]
          
          # optional extra filter based on icweight
          if (!is.null(filterICw) && filterICw == "mean") {
            casePheno <- casePheno[icweight >= mean(icweight)]
          } else if (!is.null(filterICw)) {
            casePheno <- casePheno[icweight >= filterICw]
          }
          
          if (casePheno[,length(unique(icweight)), by = go_id][,any(V1>1)]){
            warning("Database table genepheno contains multiple icweight values for the same go_id. Usign the first occuring icweight as a workaround.")
            casePheno <- casePheno[,.(icweight = icweight[1]), by = go_id]
          } else {
            casePheno <- unique(casePheno[,.(go_id, icweight)])
          }
          
          return(casePheno)
        },
        
        #' @details Create a signature profile of phenotypes, a list of genes with at least one match, counting the total matches
        #'
        #' @param casePheno data.table with go_id and ICweight
        #' @param querySpecies Full scientic name of species (e.g. "Homo Sapiens")
        #' @param go_id_prefix Prefix of go_id, used for filtering instead of querySpecies.
        #' @return profile data.table
        #' @author Monique van der Voet
        createProfile = function(casePheno, querySpecies = NULL, go_id_prefix = NULL) {
          if(is.null(querySpecies)==is.null(go_id_prefix)) stop("Either querySpecies or go_id_prefix should be provided, not both.")
          
          # Get table that lists phenotypes for genes
          genePheno <- self$database$getData("genepheno", copy = FALSE)
          if(is.null(go_id_prefix)){
            profileLong <- genePheno[
                taxon == self$database$retrieveTaxon(querySpecies)]
            
          } else {
            if (substr(go_id_prefix,1,1)!="^") go_id_prefix <- paste0("^",go_id_prefix)
            profileLong <- genePheno[grepl(go_id_prefix, go_id)]
          }
          
          # new implementation: long format profile avoids expensive grepl operations
          geneObservedPhenotypes <- profileLong[go_id %chin% casePheno$go_id] # keep only observed phenotypes
          genesScores <- geneObservedPhenotypes[, .(score = mean(icweight)), by = .(db_object_id, db_object_symbol)]
          profileList <- list(
              geneScores = genesScores,
              geneObservedPhenotypes = geneObservedPhenotypes
          )
          return(profileList)
        },
        
        #' @details Retrieve list of pathways that include at least one of the genes
        #' @param profile data.table from class method createProfile
        #' @param querySpecies character string with species, e.g. "Caenorhabditis elegans"
        #' @param lowestLevelOnly Only include lowest level genes for pathways
        #' @return targetPathway
        #' @author Monique van der Voet
        #' @examples
        #' \dontrun{
        #' self$matchProfile(profile, querySpecies)
        #' }
        matchProfile = function(profile, querySpecies, lowestLevelOnly = FALSE) {
          
          # Genes in all pathways
          ensembl2reactome <- self$database$getData("ensembl2reactome")
          if(lowestLevelOnly){
            ensembl2reactome <- ensembl2reactome[lowest == TRUE]
          }
          
          # Match gene profile to pathways, count hits
          targetPathway <- ensembl2reactome[species == querySpecies,
              .(reactome_pathway_stable_identifier, event_name, source_database_identifier)]
          # map target pathways to human pathways
          # now: replace species code by HSA (non-existing human pathways will be discarded)
          targetPathway[,reactome_pathway_stable_identifier := gsub("(^R-)([A-Z]+)(-[0-9]+$)","\\1HSA\\3",reactome_pathway_stable_identifier),]
          
          return(targetPathway)
        },
        
        
        #' @description Rank pathways using AUC
        #' @param targetPathway data.table from class method matchProfile
        #' @param profile data.table from class method createProfile
        #' @return ranking
        #' @author Monique van der Voet, Marvin Steijaert
        #' @examples
        #' \dontrun{
        #' self$rankAUC(targetPathway, profile)
        #' }
        rankAUC = function(targetPathway, profile) {
          
          # profile contains the genes associated with the observed phenotypes and the corresponding score
          # (mean ICweight for the asscociated phenotypes of that gene)
          
          if(nrow(profile) == 0) return(NULL) 
          
          # Lookup pathway id and name for human pathways of interest
          ranking <- copy(self$humanGenesList$pathways)
          ranking[, nGenesPathway:= NULL] # remove column with human gene counts as this might cause confusion
          profileC <- copy(profile)
          
          # Check each potential pathway and calculate a ranking score
          evaluatedPathways <- intersect(ranking[,reactome_pathway_stable_identifier],
              targetPathway[,unique(reactome_pathway_stable_identifier)])
          
          # split is faster than data.table lookups in a for loop
          targetPathwaysGenes <- lapply(
              split(targetPathway, targetPathway$reactome_pathway_stable_identifier),
              function(x) x[["source_database_identifier"]])
          
          rankScores <- rep(NA_real_, length(evaluatedPathways))
          rankMaxScores <- rep(NA_real_, length(evaluatedPathways))
          pathwayIndex <- 0
          
          for (thisPathway in evaluatedPathways){
            pathwayIndex <- pathwayIndex + 1
            targetGeneIds <- targetPathwaysGenes[[thisPathway]]
            
            isHit <- profileC$db_object_id %chin% targetGeneIds
            
            if (any(isHit)){
              profileC$hits = isHit
              # setorder is faster than profileC[order(score, hits, decreasing = TRUE), hits]
              setorder(profileC, -score, -hits)
              rankScores[pathwayIndex] <- sum(cumsum(profileC$hits))
              # debugging code:
              # browser()
              # plot(profileC[order(score, hits, decreasing = TRUE)][,cumsum(hits)])
            } else {
              rankScores[pathwayIndex] <- 0
            }
            rankMaxScores[pathwayIndex] <- getRankMax(nrowProfile=nrow(profileC), nrowTargetGenes=length(targetGeneIds))
          }
          
          ranking <- merge(ranking,
              data.table(reactome_pathway_stable_identifier = evaluatedPathways, rankScore = rankScores, rankMax = rankMaxScores),
              by = "reactome_pathway_stable_identifier")
          
          # Calculate percentage for relative ranking
          ranking[,rankPercent := 100 / rankMax * rankScore]
          
          # Sort
          ranking <- ranking[order(rankPercent, decreasing = TRUE)]
          
          return(ranking)
        },
        #' @description Run Monte Carlo simulations for phenotypePathwayByOntology
        #' @param ontology name (prefix of phenotype identifiers) of ontology
        #' @param nRuns number of simulations
        #' @param querySizes Vector with sizes of phenotype queries
        #' @param mc.cores Passed to parallel::mcmapply.
        #' @param digits Integer value passed to round(). If NULL or non-numeric, rounding will be skipped.
        #' @return data.table with simulation results
        #' @author Marvin Steijaert
        #' @importFrom parallel mcmapply detectCores
        #' @details Use rounding (digits parameter) to reduce the size on disk and time to load.
        #' @examples
        #' \dontrun{
        #' self$runSimulations(ontology = "ZP", nRuns = 1000, querySizes = c(10,20,30,40))
        #' }
        runSimulations = function(ontology, nRuns = 1000, querySizes = c(10,20,30,40),
            mc.cores = max(parallel::detectCores() - 1, 1), digits = 2L){
          
          startTime <- proc.time()
          
          thisOntologyTerms <- self$database$getData("genepheno")[grepl(paste0("^", ontology,":"),go_id)]
          uniqueTerms <- thisOntologyTerms[,unique(go_id)]
          
          res <- mcmapply(function(runIndex, nPhenotypes){
                self$phenotypePathwayByOntology(
                    queryPhenotype = sample(uniqueTerms, nPhenotypes, FALSE),
                    go_id_prefix = ontology,
                    filterICw = NULL,
                    lowestLevelOnly = self$phenotypeRankingLowestLevel,
                    pValues = FALSE)$ranking[,.(reactome_pathway_stable_identifier, rankPercent)][rankPercent > 0][,
                    `:=` (n_phenotypes = nPhenotypes, ontology = ontology, n_runs = nRuns, run_index = runIndex)]
              },
              rep(seq_len(nRuns), length(querySizes)),
              rep(querySizes, each = nRuns),
              SIMPLIFY = FALSE,
              mc.cores = mc.cores,
              mc.preschedule = FALSE)
          
          resLong <- rbindlist(res)
          setnames(resLong, "rankPercent", "rank_percent")
          
          endTime <- proc.time()
          cat("Pathway ranking simulations for ontology", ontology, "(", nRuns ,"runs and ", length(querySizes),"query sizes) took",
              round(endTime - startTime)[3], "s\n")

          # round to reduce disk 
          if(is.numeric(digits)){
            resLong[, rank_percent := round(rank_percent, digits)]
          }
          
          # convert to a wide format with rank_percent values in a list of vectors
          resWide <- resLong[, .(rank_percent_list=list(rank_percent)), by = .(reactome_pathway_stable_identifier, n_phenotypes, ontology, n_runs)]
          
          return(resWide)
        },
        #' @description Estimate p-value using Monte Carlo simulations
        #' @param ontology Name (prefix of phenotype identifiers) of ontology
        #' @param querySize Number of phenotypes in query
        #' @param pathway Pathway identifier
        #' @param rankPercent Original rankPercent score returned by self$rankAUC
        #' @return p-value
        #' @author Marvin Steijaert
        #' @importFrom assertthat assert_that
        #' @examples
        #' \dontrun{
        #' self$estimatePvalue(ontology = "ZP", querySize= 10, pathway = "R-HSA-8856828", rankPercent = 12.3)
        #' }
        estimatePvalue = function(ontology, querySize, pathway, rankPercent){
          assert_that(length(pathway) == length(rankPercent))
          assert_that(length(ontology) == 1)
          assert_that(length(querySize) == 1)
          
          if(length(rankPercent) > 1) return(mapply(
                    function(x,y) self$estimatePvalue(ontology, querySize, pathway = x, rankPercent = y),
                    pathway, rankPercent,
                    SIMPLIFY = TRUE))
          
          simulations <- self$simulationResults          
          ontologyQuery <- ontology
          subTable <- simulations[ontology == ontologyQuery &
                  reactome_pathway_stable_identifier == pathway &
                  n_phenotypes == querySize]
          if (subTable[,.N] == 0 ){
            # try closest higher number of phenotypes as a proxy
            subTable2 <- simulations[ontology == ontologyQuery &
                    reactome_pathway_stable_identifier == pathway &
                    n_phenotypes > querySize]
            if(subTable2[,.N]){
              #warning("No simulation results available for the provided number of phenotypes. Using a higher number of phenotypes to estimate an upper bound for the p-value.")
              querySizeNext <- subTable2[, min(n_phenotypes)]
              self$estimatePvalue(ontology, querySizeNext, pathway, rankPercent)
            } else NA_real_
          } else {
            (sum(subTable[,rank_percent_list[[1]]]>rankPercent) + 1)/(subTable[1,n_runs] + 1)
          }
        },
        #' @description Estimate vector of p-values using Monte Carlo simulations
        #' @param ontology Name (prefix of phenotype identifiers) of ontology
        #' @param querySize Number of phenotypes in query
        #' @param pathway Vector with pathway identifiers
        #' @param rankPercent Vector with original rankPercent scores returned by self$rankAUC
        #' @param skipNoSimulations Boolean, if TRUE, pathways without simulation results will get an NA value. If FALSE, a pvalue will be calculated (always lowest possible or 1 for rankPercent==0).
        #' @return vector with p-values
        #' @author Marvin Steijaert
        #' @importFrom assertthat assert_that
        #' @examples
        #' \dontrun{
        #' self$estimatePvalueVector(ontology = "ZP", querySize= 10, pathway = c("R-HSA-8856828","R-HSA-2219528"), rankPercent = c(9.5,12.3))
        #' }
        estimatePvalueVector = function(ontology, querySize, pathway, rankPercent, skipNoSimulations = FALSE){
          assert_that(length(pathway) == length(rankPercent))
          assert_that(length(ontology) == 1)
          assert_that(length(querySize) == 1)
          
          simulations <- self$simulationResults          
          ontologyQuery <- ontology
          subTable <- simulations[ontology == ontologyQuery & n_phenotypes == querySize]
          if (subTable[,.N] == 0 ){
            # try closest higher number of phenotypes as a proxy
            subTable2 <- simulations[ontology == ontologyQuery & n_phenotypes > querySize]
            if(subTable2[,.N]){
              #warning("No simulation results available for the provided number of phenotypes. Using a higher number of phenotypes to estimate an upper bound for the p-value.")
              querySizeNext <- subTable2[, min(n_phenotypes)]
              self$estimatePvalueVector(ontology, querySizeNext, pathway, rankPercent, skipNoSimulations)
            } else rep(NA_real_, length(rankPercent))
          } else {
            denominator <- subTable[1,n_runs] + 1
            
            rankPercentLookup <- lapply(
                split(subTable, subTable$reactome_pathway_stable_identifier),
                function(x) x[,rank_percent_list[[1]]])
            
            mapply(
                function(x,y){
                  split
                  rankPercentSims <- rankPercentLookup[[x]]
                  if(is.null(rankPercentSims)){
                    if (skipNoSimulations){
                      NA
                    } else ifelse(y>0, 1/denominator, 1)
                  } else {
                    numerator <- sum(rankPercentSims >= y) + 1
                    numerator/denominator
                  }
                },
                pathway, rankPercent, SIMPLIFY = TRUE)
          }
        },
        #' @description Plot simulation-based p-value estimate for different number of simulations
        #' @param ontology Name (prefix of phenotype identifiers) of ontology
        #' @param querySize Number of phenotypes in query
        #' @param pathway Pathway identifier
        #' @param rankPercent Original rankPercent score returned by self$rankAUC
        #' @return NULL
        #' @author Marvin Steijaert
        #' @examples
        #' \dontrun{
        #' self$plotPvalue(ontology = "ZP", querySize= 10, pathway = "R-HSA-8856828", rankPercent = 12.3)
        #' }
        plotPvalue = function(ontology, querySize, pathway, rankPercent){
          ontologyQuery <- ontology
          
          simulations <- self$simulationResults
          subTable <- simulations[ontology == ontologyQuery &
                  reactome_pathway_stable_identifier == pathway &
                  n_phenotypes == querySize]
          if (subTable[,.N] == 0 ){
            stop("Insufficient samples to create plot")
          } else {
            simulatedResults <- subTable[,rank_percent_list[[1]]]
            # add missing zeros and reshuffle (original location of zeros is not stored in table)
            nRuns <- subTable[1,n_runs]
            simulatedResults <- c(simulatedResults, rep(0, nRuns - length(simulatedResults)))
            simulatedResults <- sample(simulatedResults, size = nRuns) 
            plot(sapply(1:nRuns, function(x) (sum(simulatedResults[1:x] >= rankPercent) + 1)/(x + 1)), xlab = "number of samples", ylab = "p-value estimate")
          }
        },
        #' @description Plot histogram with simulated rankPercent scores
        #' @param ontology Name (prefix of phenotype identifiers) of ontology
        #' @param querySize Number of phenotypes in query
        #' @param pathway Pathway identifier
        #' @param rankPercent Original rankPercent score returned by self$rankAUC (optional). Will be shown with a red line.
        #' @return NULL
        #' @author Marvin Steijaert
        #' @examples
        #' \dontrun{
        #' self$plotSimulationsHistogram(ontology = "ZP", querySize= 10, pathway = "R-HSA-8856828", rankPercent = 12.3)
        #' }
        plotSimulationsHistogram = function(ontology, querySize, pathway, rankPercent = NULL){
          ontologyQuery <- ontology
          
          simulations <- self$simulationResults
          subTable <- simulations[ontology == ontologyQuery &
                  reactome_pathway_stable_identifier == pathway &
                  n_phenotypes == querySize]
          if (subTable[,.N] == 0 ){
            stop("Insufficient samples to create plot")
          } else {
            simulatedResults <- subTable[,rank_percent_list[[1]]]
            # add missing zeros
            nRuns <- subTable[1,n_runs]
            simulatedResults <- c(simulatedResults, rep(0, nRuns - length(simulatedResults)))
            hist(simulatedResults, xlab = "rankPercent", xlim= c(0,100))
            
            if(!is.null(rankPercent)){
              abline(v = rankPercent, col = "red")
              greater = sum(simulatedResults > rankPercent)
              greaterOrEqual = sum(simulatedResults >= rankPercent)
              pvalue = (greaterOrEqual + 1) / (nRuns + 1)
              message("Simulation:\n n_runs=", nRuns,
                  "\n max (simulations) = " , max(simulatedResults),
                  "\n n (simulations > rankPercent) = ", greater,
                  "\n n (simulations >= rankPercent) = ", greaterOrEqual,
                  "\n p-value = (", greaterOrEqual," + 1)/(", nRuns, " + 1) = ", pvalue)
            }
            
          }
        }
    ),
    private = list()
)

#' Returns NA if only NA values are provided. Otherwise, the maximum of non-NA values is returned
#' @param vec numeric vector 
#' @return maximum value
maxWithNA <- function(vec){
  if(any(!is.na(vec))) max(vec,na.rm = TRUE) else NA_real_
}

#' Returns NA if only NA values are provided. Otherwise, the minimum of non-NA values is returned
#' @param vec numeric vector 
#' @return maximum value
minWithNA <- function(vec){
  if(any(!is.na(vec))) min(vec,na.rm = TRUE) else NA_real_
}

#' Returns NA if only NA values are provided. Otherwise, the harmonic mean of non-NA values is returned
#' @param vec numeric vector 
#' @return maximum value
harmonicMeanWithNA <- function(vec){
  if(any(!is.na(vec))) 1/mean(1/vec,na.rm = TRUE) else NA_real_
}

#' Return the element-wise harmonic mean of multiple vectors
#' @param ... one or more vectors (all with the same length)
#' @param na.rm a logical value indicating whether NA values should be stripped before the computation proceeds.
#' @param adjustMethod Name of applyied multiple testing correction (passed to p.adjust()). If NULL, original values are returned.
#' @return maximum value
#' @importFrom stats p.adjust
#' @details For convenience, ... can include NULL values. These are ignored.
#' @examples dartTools:::harmonicMeanColumnWise(c(0.1,0.2,0.3), c(0.3, 0.4, 0.5))
harmonicMeanColumnWise <- function(..., na.rm = FALSE, adjustMethod = NULL){
  
  columns <- list(...)
  
  # remove NULL items
  columns <- columns[which(sapply(columns, function(x) !is.null(x)))]
  
  lengths <- sapply(columns, length)
  assert_that(length(columns) >= 1)
  assert_that(length(unique(lengths)) == 1)
  meanFun <- if(na.rm) harmonicMeanWithNA else function(x) 1/mean(1/x)
  res <- sapply(seq_len(lengths[1]),
      function(item) meanFun(sapply(columns, function(col) col[item])))
  if(!is.null(adjustMethod)){
    res <- p.adjust(res, method = adjustMethod)
  }
  return(res)
}
