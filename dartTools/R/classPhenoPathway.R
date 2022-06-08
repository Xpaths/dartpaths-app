# R6 class for dealing with phenotype and pathway data
# 
# Author: Marvin Steijaert
###############################################################################

#' Class for dealing with Phenotype and Pathway data
#' 
#' @export
#' @import R6
#' @details For reasons of code visibility, the different components are implemented in a series of parent classes
PhenoPathway <- R6Class("PhenoPathway",
    inherit = Substances,
    public = list(
        #' @field classname Name of the class
        classname = "PhenoPathway",
        #' @field humanPathwayLevelsCache Cache data.table object with pathway level mapping
        humanPathwayLevelsCache = NULL,
        #' @description Get all human pathways, their level and parents
        #' @param returnParents Logical value indicating if parent pathways should be included
        #' @return data.table object
        #' @author Marvin Steijaert
        getHumanPathwayLevels = function(returnParents = TRUE){
          if(is.null(self$humanPathwayLevelsCache)){
            pathwayrelations = c("reactome_pathway_parent", "reactome_pathway_child")
            reactomePathwaysRelationHuman <- self$getData("pathwayrelations")[grepl("^R-HSA",reactome_pathway_parent)]
            
            # recursively search for parents until no more parents can be added
            humanPathwayParents <- copy(reactomePathwaysRelationHuman)
            setnames(humanPathwayParents, c("p0","c0"))
            level <- 0
            while (humanPathwayParents[,any(!is.na(c0))]){
              level <- level + 1
              levelTable <- copy(reactomePathwaysRelationHuman)
              setnames(levelTable, paste0(c("p","c"), level))
              humanPathwayParents = merge(humanPathwayParents, levelTable, by.x = paste0("c",level-1), by.y = paste0("p",level), all.y = TRUE)
            }
            humanPathwayParents[, c0 := NULL]
            humanPathwayParents[, p0 := NULL]
            setcolorder(humanPathwayParents, paste0("c",level))
            
            # add top level pathways
            pathwaysTop <- reactomePathwaysRelationHuman[, .(x = setdiff(reactome_pathway_parent,reactome_pathway_child))]
            setnames(pathwaysTop, paste0("c",level))
            humanPathwayParents <- rbind(humanPathwayParents, pathwaysTop, fill = TRUE)
            setnames(humanPathwayParents, c("reactome_pathway_stable_identifier", paste0("parent_", 1:(level-1))))
            
            # add level
            humanPathwayParents[, level:=apply(.SD,1,function(x) sum(!is.na(x)))]
            self$humanPathwayLevelsCache <- humanPathwayParents
          }
          # if multiple paths exist, use shortest path (i.e., lowest possible level)
          result <- self$humanPathwayLevelsCache[, .SD[which.min(level)], by = reactome_pathway_stable_identifier]
          if(returnParents){
            return(result)
          } else {
            return(result[,.(reactome_pathway_stable_identifier,level)])
          }
          
        },
        #' @description Get all human pathways upstream of provided pathway identifiers
        #' @param pathwayids Character vector with one or more Reactome pathway identifiers
        #' @author Marvin Steijaert
        #' @return vector with pathway identifiers
        getHumanPathwayParents = function(pathwayids){
          pathwayLevels = database$getHumanPathwayLevels(TRUE)
          thisPathwayLevels = pathwayLevels[reactome_pathway_stable_identifier %in% pathwayids]
          as.vector(na.omit(unlist(copy(thisPathwayLevels)[,level:=NULL][,reactome_pathway_stable_identifier:=NULL])))
        },
        #' @description Get all human pathways at and below a specified level and the corresponding parent pathways at the specified level
        #' @param level Integer specifying a level
        #' @author Marvin Steijaert
        #' @return data.table object
        getHumanPathwaysForLevel = function(level){
          
          selectLevelPathway <- function(x,goalLevel,pathwayName){
            levelsUp <- x$level[1] - goalLevel
            if(levelsUp == 0) as.character(pathwayName) else x[[paste0("parent_", levelsUp)]][1]  
          }
          goalLevel <- level
          humanPathwayParents <- self$getHumanPathwayLevels(returnParents = TRUE)
          result <- humanPathwayParents[level>=goalLevel][,
              .(level_parent = selectLevelPathway(.SD,goalLevel,.BY)), by = reactome_pathway_stable_identifier]
          return(result)
        },
        
        #' @description Function to get all human pathways and apply some filtering
        #' @param levels (optional) vector with selected level(s) of pathway in reactome hierarchy 
        #' @param sizePathwayMin minimum number of genes in pathway, e.g. 4
        #' @param sizePathwayMax maximum number of genes in pathway, e.g. 100
        #' @param asVector if TRUE, a named vector is returned. If FALSE, a data.table is returned
        #' @param renameEvent if TRUE, replicated pathway names will be rename to avoid ambiguity
        #' @param lowestLevelOnly logical, indicates if only the genes annotated at the lowest level are included
        #' @return table or vector with Reactome pathways 
        #' @author Marvin Steijaert
        getHumanPathways = function(levels = NULL, sizePathwayMin = 0, sizePathwayMax  = Inf, asVector = FALSE,
            renameEvent = FALSE, lowestLevelOnly = FALSE){
          
          pathways <- self$getHumanPathwayGenes(levels = levels,
              sizePathwayMin = sizePathwayMin, sizePathwayMax=sizePathwayMax,
              renameEvent = renameEvent, lowestLevelOnly = lowestLevelOnly)$pathways
          
          # return results as vector or data.table
          if (asVector){
            pathways <- pathways[order(event_name)]
            pathwaysVector <- pathways[,reactome_pathway_stable_identifier]
            names(pathwaysVector) <- pathways[,event_name]
            return(pathwaysVector)
          } else {
            return(pathways)
          }
        },
        #' @description Function to get all human pathways genes and apply some filtering
        #' @param levels (optional) vector with selected level(s) of pathway in reactome hierarchy 
        #' @param sizePathwayMin minimum number of genes in pathway, e.g. 4
        #' @param sizePathwayMax maximum number of genes in pathway, e.g. 100
        #' @param renameEvent if TRUE, replicated pathway names will be rename to avoid ambiguity
        #' @param lowestLevelOnly logical, indicates if only the genes annotated at the lowest level are included
        #' @return list with data.tables "pathways" and "pathwayGenes 
        #' @author Marvin Steijaert
        getHumanPathwayGenes = function(levels = NULL, sizePathwayMin = 0, sizePathwayMax  = Inf,
            renameEvent = FALSE, lowestLevelOnly = FALSE){
          
          # human annotations are all ENSG (ensembl gene) or ENSP (ensemble protein)
          # here we select on the genes
          pathwayGenes <- self$getData("ensembl2reactome", allowEmpty = FALSE)[
              species %in% "Homo sapiens" & substr(source_database_identifier,1,4) %in% "ENSG"]
          if(lowestLevelOnly){
            pathwayGenes <- pathwayGenes[lowest == TRUE]
          }
          
          # Add gene names and full gene name and remove genes not part of human genes table
          # (this corresponds to keeping the primary assembly from Ensembl and discarding alternative sequences)
          pathwayGenes <- merge(pathwayGenes, self$getData("humangenes"),
              by.x = "source_database_identifier", by.y = "geneid")
          
          # filter on pathway levels
          pathwayLevels <- self$getHumanPathwayLevels(returnParents = FALSE)
          pathwayGenes <- merge(pathwayGenes, pathwayLevels, by = "reactome_pathway_stable_identifier")
          if(!is.null(levels)){
            pathwayGenes <- pathwayGenes[level %in% levels]
          }
          
          # filter on pathway size
          pathways <- pathwayGenes[, .(nGenesPathway= length(unique(source_database_identifier))),
              by = .(reactome_pathway_stable_identifier, event_name, level)][
              nGenesPathway >= sizePathwayMin & nGenesPathway <= sizePathwayMax]
          pathwayGenes <- merge(
              unique(pathwayGenes[,.(reactome_pathway_stable_identifier, source_database_identifier, gene, genelongname, url)]),
              pathways, by = "reactome_pathway_stable_identifier") 
          
          # deal with replicated event_names (different Reactome pathways with identical name)
          valCount <- function(vec){sapply(vec,function(val){sum(vec == val)})}
          pathways[ , idsPerEvent := valCount(event_name)]
          if(pathways[idsPerEvent>1,.N]){
            # replicated event_names found
            if(renameEvent){
              pathways[idsPerEvent > 1, event_name := paste0(event_name, " (",reactome_pathway_stable_identifier,")")]
            } else {
              warning("Multiple pathways (reactome_pathway_stable_identifiers) were found with the same even_name.\n",
                  "Use renameEvent=TRUE to avoid ambiguity and suppress this warning message.")
            }
          }
          pathways[, idsPerEvent:= NULL]
          
          return(list(
                  pathways = pathways,
                  pathwayGenes = pathwayGenes
              ))
        },
        #' @description Retrieve taxon id from species name
        #' @param querySpecies character string with species, e.g. "Caenorhabditis elegans"
        #' @return queryTaxon
        #' @author Monique van der Voet
        #' @importFrom magrittr %>%
        retrieveTaxon = function(querySpecies) {
          lookup <- self$getData("species")
          return(lookup[speciesfull == querySpecies, as.character(taxon)])
          
        },
        
        #' @description get pathway orthology table
        #' @param pathwayid Reactome pathway identifier
        #' @param description Reactome pathway description
        #' @param renameEvent passed to self$getHumanPathwayGenes. If TRUE, replicated pathway names will be rename to avoid ambiguity
        #' @param ... additional parameters passed to self$getHumanPathwayGenes
        #' @return table
        #' @author Marvin Steijaert
        #' @examples
        #' \dontrun{
        #' self$getPathwayOrthology(pathwayid = "R-HSA-2029481")
        #' }
        #' @details Preferably used pathwayid instead of description. Description can be ambiguous
        #' (in rare cases, the same description is used for multiple pathwayids)
        getPathwayOrthology = function(pathwayid = NULL , description = NULL, renameEvent = FALSE, ...) {
          # replaces the old extractGeneData function
          
          orthology <- self$getData("orthology")
          pathwaydata <- self$getHumanPathwayGenes(renameEvent = renameEvent, ...)$pathwayGenes
          
          # select pathway
          if (!is.null(pathwayid)){
            thispathwaydata <- pathwaydata[reactome_pathway_stable_identifier == pathwayid]
          } else if (!is.null(description)){
            thispathwaydata <- pathwaydata[event_name == description]
            if (thispathwaydata[, length(unique(reactome_pathway_stable_identifier))]>1){
              warning("Multiple reactome_pathway_stable_identifier were found for the provided description/name:", description)
            } 
          } else stop("either pathwayid or pathway description should be provided")
          setnames(thispathwaydata, "source_database_identifier", "geneid")
          aggregateFun <- function(x) if(is.null(x) || length(x)==0) "0" else x[1]
          orthologyThisPathwayLong <- merge(orthology, thispathwaydata, by = "geneid")[,.(geneid, gene, genelongname, speciesfull, homologytype)]
          
          if(is.null(orthologyThisPathwayLong) || nrow(orthologyThisPathwayLong)==0){
            return(NULL)
          }
          
          orthologyThisPathway <- dcast(
              orthologyThisPathwayLong,
              fun.aggregate = aggregateFun,
              geneid+gene+genelongname~speciesfull,  value.var = "homologytype")
          speciesfullOrder <- c("Rattus norvegicus", "Mus musculus",
              "Oryctolagus cuniculus", "Danio rerio", "Drosophila melanogaster",
              "Caenorhabditis elegans", "Dictyostelium discoideum")
          
          # add pathway genes that are missing from orthology data
          missingGenes <- setdiff(thispathwaydata[,unique(geneid)], orthologyThisPathway[, geneid])
          if(length(missingGenes)){
            #cat("Adding",length(missingGenes), "missing genes to orthology table\n")
            orthologyThisPathway <- rbind(orthologyThisPathway,
                thispathwaydata[geneid %in% missingGenes,.(geneid, gene, genelongname)],
                fill = TRUE)
            orthologyThisPathway[,lapply(.SD,function(x) ifelse(is.na(x),"0",x))]
          }
          
          # fill in 0 for columns without data
          for (missingColumn in setdiff(speciesfullOrder, names(orthologyThisPathway))){
            set(orthologyThisPathway,,missingColumn,"0")
          }
          
          # Include general + gene-specific weblinks
          reactomeLink <- unique(thispathwaydata[, url])
          orthologyThisPathway[ , webLink := paste0("https://www.ensembl.org/Homo_sapiens/Gene/Summary?g=", geneid)]
          
          setnames(orthologyThisPathway, "gene", "human")
          setnames(orthologyThisPathway, "genelongname", "full name")           
          
          # Order rows by gene name
          setkey(orthologyThisPathway, 'human')
          # Order columns
          setcolorder(orthologyThisPathway, unique(c("geneid", "full name", "human", speciesfullOrder, names(orthologyThisPathway))))
          # common names instead of full scientific names
          speciesTable <- self$getData("species")
          setnames(orthologyThisPathway, speciesTable[, speciesfull], speciesTable[,speciescommon], skip_absent=TRUE)
          
          attr(orthologyThisPathway, "reactomeLink") <- reactomeLink
          
          return(orthologyThisPathway)
        },
        #' @description map genes in a table to the human orthologs
        #' @param inputTable data.table with at least columns 'gene' and 'species'
        #' @return data.table object
        #' @author Marvin Steijaert
        #' @details Only mouse and rat are currently mapped.
        mapGenesToHumanGenes = function(inputTable){
          if(!inherits(inputTable, "data.table") || !all(c("gene", "species") %in% names(inputTable))){
            stop("inputTable should be a data.table with at least columns 'gene' and 'species'")
          }
          # TODO: only mouse and rat are mapped. Needs to be extended
          genesToHumanGenes <- merge(
              self$getData("orthology")[speciesfull %in% c("Mus musculus","Rattus norvegicus")],
              self$getData("species"),
              by = "speciesfull")
          genesToHumanGenes <- merge(genesToHumanGenes,
              self$getData("humangenes")[
                  ,.(geneid,gene)], by= "geneid")
          genesToHumanGenes <- unique(genesToHumanGenes[,.(speciescommon, gene, homolog_gene, geneid)])
          setnames(genesToHumanGenes, "speciescommon", "species")
          
          # Keep all gene names that are already matching human names,
          # and try to match remaining ones via mouse/rat orthologs.
          # If multiple genes are given separated by "|", the first one is used
          onlyFirstGene <- copy(inputTable)[, gene := gsub("^([^\\|]*)\\s*\\|.*","\\1",gene)]
          genesMappedFromOtherSpecies <- merge(
              onlyFirstGene[!gene %in% self$getData("humangenes")[,gene]],
              genesToHumanGenes,
              by.x = c("species", "gene"),
              by.y = c("species", "homolog_gene"))
          genesMappedFromOtherSpecies[, gene := NULL]
          setnames(genesMappedFromOtherSpecies, "gene.y", "gene")
          genesAlreadyHuman <- merge(onlyFirstGene,
              self$getData("humangenes")[,.(geneid,gene)],
              by = "gene")
          result <- rbindlist(list(genesAlreadyHuman,genesMappedFromOtherSpecies), use.name = TRUE, fill = TRUE)
          return(result)
        }
    ))

#' auxiliary function for pathway ranking
#' @param nrowProfile number of genes associated with observed phenotypes 
#' @param nrowTargetGenes number of genes in target pathway 
#' @return rankMax value 
getRankMax <- function(nrowProfile, nrowTargetGenes){
  if(nrowProfile < nrowTargetGenes) {
    return(sum(cumsum(c(rep(1, nrowProfile)))))
  } else {
    # if profile = 4 and target = 3, the best hit would be 1,1,1,0, the max sumcum = 1,2,3,3 = sum 9
    return(sum(cumsum(c(rep(1, nrowTargetGenes), rep(0, nrowProfile-nrowTargetGenes)))))
  }
}

