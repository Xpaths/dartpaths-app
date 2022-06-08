# R6 class for dealing with substance and constituent (compound) data
# 
# Author: Monique van der Voet, Marvin Steijaert
###############################################################################

SUBSTANCEALLOWEDVALUES <- list(
    substancetype = c("uvcb","mono","multi", NA_character_),
    idtype = c("CAS","EC","name")
)

DARTHAZARDCODES <- c("H360", "H360F","H360D","H360FD","H360Fd","H360Df","H361","H361f","H361d","H361fd","H362")

#' Class for storing substance data
#' 
#' @export
#' @import R6
#' @useDynLib dartTools
Substances <- R6Class("Substances",
    inherit = Data,
    public = list(
        #' @field classname Name of the class
        classname = "Substances",
        #' @field metadata Named list with metadata
        metadata = list(allowedValues = SUBSTANCEALLOWEDVALUES),
        #' @description method to add external substance identifiers
        #' @param names character vector with new substance identifiers
        #' @return NULL 
        addIdTypes = function(names){
          # remove duplicate names (with different case and/or dots, underscores, hyphens)
          namesFiltered <- unique(names)
          existingIdsAndColumnNames <- c(self$metadata$allowedValues$idtype, unlist(self$tableColumns,recursive = TRUE, use.names = FALSE))
          namesFiltered <- names[! gsub("[._[:space:]-]","",tolower(names))  %in% gsub("[._[:space:]-]","",
                  tolower(existingIdsAndColumnNames))]
          cat("Adding idtypes:", paste(namesFiltered,collapse =","), "\n")
          namesRemoved <- paste(setdiff(names, namesFiltered))
          if(length(namesRemoved)>0) cat("the following idtypes will not be added (due to match with existing names):", paste(namesRemoved, collapse =","),"\n")
          self$metadata$allowedValues$idtype <- c(self$metadata$allowedValues$idtype, namesFiltered)
        },
        
        #' @description method to add substance category data
        #' @param categoryInfo NULL or data.table with columns category, description, url
        #' @param categoryMembers NULL or data.table with column category (name/identifier of category) and EC/CAS (and optionally name)
        #' @param categoryProfiles NULL or data.table with columns category, name,typicalconcentration and concentrationrange and EC/CAS
        #' @param verbose Toggle verbosity (TRUE/FALSE)
        #' @return NULL 
        addSubstanceCategory = function(categoryInfo = NULL,
            categoryMembers = NULL, categoryProfiles = NULL, verbose = FALSE){
          allowedIdTypes <- setdiff(self$metadata$allowedValues$idtype, "name")
          if(!is.null(categoryInfo)){
            requiredColumns <- c("category", "description", "url")
            if(!all(requiredColumns%in% names(categoryInfo))){
              stop("Argument categoryInfo should contain at least the following columns: ", paste(requiredColumns, collapse = ", "))
            }
            self$addData("substancecategoryinfo",
                categoryInfo[,requiredColumns ,with = FALSE])
          }
          if(!is.null(categoryMembers)){
            requiredColumns <- c("category")
            if(!all(requiredColumns%in% names(categoryMembers))){
              stop("Argument categoryMembers should contain at least the following columns: ", paste(requiredColumns, collapse = ", "))
            }
            if(!any(allowedIdTypes%in% names(categoryMembers))){
              stop("Argument categoryMembers should contain at least one of the following columns: ", paste(allowedIdTypes, collapse = ", "))
            }
            categoryMembers <- categoryMembers[!is.na(category)]
            if(! "smiles" %in% names(categoryMembers)) categoryMembers[, smiles := NA_character_]
            ids <- self$addSubstances(categoryMembers, verbose = verbose)
            self$addData("substancecategorysubstances",
                data.table(substanceid = ids, category = categoryMembers[,category]))
          }
          
          if(!is.null(categoryProfiles)){
            requiredColumns <- c("category", "name","typicalconcentration", "concentrationrange")
            if(!all(requiredColumns%in% names(categoryProfiles))){
              stop("Argument categoryProfiles should contain at least the following columns: ", paste(requiredColumns, collapse = ", "))
            }
            if(!any(allowedIdTypes%in% names(categoryProfiles))){
              stop("Argument categoryProfiles should contain at least one of the following columns: ", paste(allowedIdTypes, collapse = ", "))
            }
            if(! "smiles" %in% names(categoryProfiles)) categoryProfiles[, smiles := NA_character_]
            #if(! "substancetype" %in% names(categoryProfiles)) categoryProfiles[, substancetype := NA_character_]
            ids <- self$addSubstances(categoryProfiles, verbose = verbose)
            self$addData("substancecategoryprofiles",
                cbind(
                    categoryProfiles[, .(category, name, typicalconcentration, concentrationrange)],
                    substanceid = ids)
            )
          }
          return(invisible())
        },
        
        #' @description method to add constituents
        #' @param smiles Character vector with molecules in SMILES notation
        #' @param returnTable Logical, if TRUE, a table is returned; if FALSE, a vector of constituentid values is returned
        #' @return data.table object or integer vector
        addConstituents = function(smiles, returnTable = FALSE){
          # standardize smiles
          # if smiles are invalid or standardization fails, smiles entry will be set to NA
          stdSmiles <- standardizeSmilesRdkit(smiles)
          
          # Ignore smiles standardized to "O" (H2O). This is typically a side effect of "pharma-style standardization"
          # in which the unchanged version of the largest fragment is kept. 
          # keeping those smiles will yield an unwanted aggregation of anorganic salts.
          stdSmiles[smiles=="O", smiles:= NA_character_]
          
          # add unique new constituents to constituents table
          newSmiles <- stdSmiles[!is.na(smiles) , setdiff(smiles, self$getData("constituents", copy = FALSE)[,smiles])]
          if(length(newSmiles)){
            newConstIds <- seq_along(newSmiles) + self$nrows("constituents")
            self$addData("constituents",
                data.table(constituentid = newConstIds, smiles = newSmiles))
          }
          # return constituentids
          foundConstituents <- self$getData("constituents", copy = FALSE)
          if(nrow(foundConstituents)){
            foundConstituents <- foundConstituents[.(stdSmiles[,smiles]), on = "smiles"]
          }
          if(returnTable){
            invisible(foundConstituents)
          } else invisible(foundConstituents[,constituentid])
        },
        #' @description method to add substances to database
        #' @param inputTable data.table or data.frame with new substances, see details
        #' @param structureColumn should be "smiles" (if a column exist with that name) or None (otherwise)
        #' @param verbose Toggle verbosity
        #' @param returnTable Logical, if TRUE, a table is returned; if FALSE, a vector of substanceid values is returned
        #' @return data.table object or integer vector
        #' @details table should contain the following columns 
        #' \itemize{
        #' \item{"smiles"}{Molecular structures in SMILES notation (Optional)}
        #' \item{"substancetype"}{(Optional) type of substances (allowed values are "uvcb","mono","multi", NA). If not specified, NA is used.}
        #' \item{"url"}{(Optional) URL of webpage with original data}
        #' }
        #' In addition, additional columns can be included with external substance identifiers.
        #' By default the following identifiers are supported: "CAS","EC","name",
        #' Additional identifiers can be registered with the \code{addIdTypes} method.
        addSubstances = function(inputTable, structureColumn = "smiles",
            verbose = FALSE, returnTable = FALSE){
          
          # hard-coded column names
          # (these are deliberately not exposed a parameters)
          urlColumn = "url"
          altIds = private$listAltIdColumns(inputTable)
          structureType = "smiles"
          typeColumn = "substancetype"
          
          if(structureType != "smiles") stop("Provided structureType is not (yet) supported")
          
          ## Prepare inputTable
          # inputTable is copied. For current data size this is not a problem
          # advantage is that column names can be set to defaults (simplifies column selection in downstream code)
          # e.g. inputTable[,smiles] instead of inputTable[,get(structureColumn)]
          # do not use unique(inputTable) as we will miss some substanceids in the output
          inputTable <- copy(inputTable)
          setDT(inputTable) # change to data.table if data.frame is provided
          if(!all(c(structureColumn) %in% names(inputTable))){
            stop("inputTable should at least contain the column names specified by structureColumn and typeColumn")
          }
          if(!is.null(structureColumn)) setnames(inputTable, structureColumn, "smiles")
          if(is.null(typeColumn) || ! typeColumn %in% names(inputTable)){
            inputTable[, substancetype := NA_character_]
          } else {
            setnames(inputTable, typeColumn, "substancetype")
            inputTable[substancetype=="", substancetype := NA]
            nUnknownSubstanceTypes <- inputTable[!substancetype %in% self$metadata$allowedValues$substancetype,.N]
            if(nUnknownSubstanceTypes) stop("There are ",nUnknownSubstanceTypes," records with unsupported substance types. Please remove those records before calling this function.")
          }
          
          if(! is.null(urlColumn) && urlColumn %in% names(inputTable)){
            setnames(inputTable, urlColumn, "url")
            inputTable[, url := filterInvalid(url, type = "other", mode = "replace")]
          } else inputTable[,url := NA_character_]
          
          registeredIdTypes <- self$metadata$allowedValues$idtype
          altIdsFound <- intersect(altIds, registeredIdTypes)
          altIdsUnknown <- setdiff(altIds, registeredIdTypes)
          if(length(altIdsUnknown)) warning("The following altIds were not recognized and will be ignored:\n",
                paste(altIdsUnknown, collapse=","),"\nAdditional altIds can be added using ")
          
          ## Filter to remove commonly encountered invalid data entries
          if("smiles" %in% names(inputTable)){
            inputTable[, smiles := filterInvalid(smiles, type = "smiles", mode = "replace")]
          }
          for(thisAltId in altIdsFound){
            set(inputTable, i = NULL, j = thisAltId,
                value = filterInvalid(inputTable[, get(thisAltId)],
                    type = switch(thisAltId, name = "substancenames", EC = "EC", CAS = "CAS", "other"),
                    mode = "replace"))
          }
          if("name" %in% altIdsFound){
            # lower case where allowed (e.g., not O,S,S-triethyl phosphorodithioate or D-glucose)
            inputTable[, name:= gsub("([A-Z][a-z])","\\L\\1",name, perl = TRUE)]
            if("smiles" %in% names(inputTable)) inputTable[name == smiles, name:= NA_character_]
          } 
          
          # standardize smiles and add constituents to table
          if("smiles" %in% names(inputTable) && !all(is.na(inputTable[,smiles]))){
            constituentIds <- self$addConstituents(inputTable[,smiles], returnTable = TRUE)
            inputTable[, constituentid := constituentIds[,constituentid]]
            inputTable[, smilesOrig := constituentIds[,smiles]]
            inputTable[, smiles := constituentIds[,smiles]]
          } else {
            inputTable[, constituentid := NA_integer_]
          }
          
          # get substanceid, generate one if not yet existing
          inputTable[, substanceid := NA_integer_]
          if("smiles" %in% names(inputTable) && "mono" %in% inputTable[,substancetype]){
            inputTable[substancetype == "mono", monoConstituentSmiles := smiles]
            clusterColumns = c(altIdsFound, "monoConstituentSmiles")
          } else{
            clusterColumns = altIdsFound
          }
          
          # do not cluster by name. E.g. 2-hydroxypropanoic acid is used for monoconstituent, multiconstituent and UVCB
          if(verbose) cat("The following altIds were found and will be used for matching new records with existing ones and each other:",paste(setdiff(clusterColumns, "name"), collapse=",") ,"\n")
          if(verbose && "name" %in% names(clusterColumns)) cat("Also a 'name' column was found. The names in this column will be added to the database, but are not used for substance matching as names can be ambiguous.\n")
          clusterColumns = setdiff(clusterColumns, "name")
          
          # remove unformative rows (removed row numbers are stored as attr(inputTable, "nonInformative") )
          inputTable <- removeNonInformativeEntries(inputTable, c(clusterColumns, "substancetype"))
          informativeEntries <- inputTable$informative
          inputTable <- inputTable$inputTable
          if(verbose) cat(sprintf("Removed %s non-informative entries\n",sum(attr(inputTable, "nonInformative"))))
          clusterByMatchingEntries(inputTable,
              clusterColumns = clusterColumns ,
              uniqueColumns = c(),
              verbose = verbose)
          
          # Look up existing substanceids
          # Consider as same substance if substancetype == "mono" and other "mono" substance exists with same smiles.
          if("smiles" %in% names(inputTable) && length(self$getMonoSubstanceSmiles())){
            inputTable[is.na(substanceid) &substancetype %in% "mono" & !smiles %in% c("", NA_character_),
                substanceid := self$mapSmilesToSubstance(smiles)]
          }
          # Consider as same substance if one of the external ids match with entry in externalsubstanceids
          # (exclude name as names can be ambiguous)
          for(thisAltId in setdiff(altIdsFound, "name")){
            # in case multiple substances have same value, take first one (firstMatch = TRUE)
            thisAltIdTable <- self$getSubstanceExtIds(thisAltId, firstMatch = TRUE)
            if(nrow(thisAltIdTable)){
              altIdLookup <- as.character(inputTable[is.na(substanceid), get(thisAltId)])
              inputTable[is.na(substanceid), substanceid := thisAltIdTable[altIdLookup, substanceid , on = thisAltId]]
            }
          }
          
          # assign minimum matching substanceid for the current cluster
          # (or a new substanceid if no matches were found)
          nextSubstanceid = self$maxSubstanceid() + 1L
          determineSubstanceid <- function(vec){
            vec2 = na.omit(vec)
            if(length(vec2) == 0){
              res <- nextSubstanceid
              nextSubstanceid <<- nextSubstanceid + 1L
            } else {
              res <- min(vec2)
            }
            res
          }
          inputTable[, substanceid := determineSubstanceid(substanceid), by = cluster]
          
          ## update substance tables in database and flag conflicts in input data
          # update table: substancetypes
          substanceidConflicts <- inputTable[ , .(conflict = length(unique(na.omit(substancetype)))>1) , by = substanceid]
          if(any(substanceidConflicts[,conflict])){
            warning("Found ",sum(substanceidConflicts[,conflict])," substances with conflicting substance types in new data. Using the first occuring non-NA type")
          }
          
          updateCandidate <- unique(rbind(
                  self$getData("substancetypes", copy = FALSE),
                  inputTable[, .(substanceid, substancetype)]
              ))
          nTypeConflicts <- 0L
          determineSubstanceType <- function(vec){
            vec <- unique(vec)
            if(length(vec) == 1){
              return(vec)
            } else if (length(na.omit(vec)) > 1){
              nTypeConflicts <<- nTypeConflicts + 1L
            }
            # first non-NA item if existing, otherwise NA
            return(c(na.omit(vec),NA_character_)[1])
          }
          updateCandidate <- updateCandidate[,.(substancetype = determineSubstanceType(substancetype)), by = substanceid]
          if(nTypeConflicts) warning("Found ",nTypeConflicts," substances with conflicting substance types between existing and new data. Keeping existing type.")
          nNewSubstances <-  updateCandidate[substanceid>self$maxSubstanceid(),.N]
          if(verbose) cat("Adding",nNewSubstances ,"new substanceids to the database\n")
          if(self$nrows("substancetypes") > 0){
            naSubstanceRecords <- self$getData("substancetypes", copy = FALSE)[is.na(substancetype), substanceid]
            nChangedSubstanceTypes <- updateCandidate[substanceid %in% naSubstanceRecords, sum(!is.na(substancetype))]
            if(verbose && nChangedSubstanceTypes) cat("For",nChangedSubstanceTypes ,"existing substances, type changed from NA to something else\n")
          }
          
          if(updateCandidate[is.na(substanceid),.N]){
            warning("updateCandidate contains substanceid with invalid NA value. This entry will be skipped")
            updateCandidate <- updateCandidate[!is.na(substanceid)]
          }
          
          self$addData("substancetypes", updateCandidate, overwrite = TRUE)
          rm(updateCandidate)
          
          # update table: substanceconstituents
          if("constituentid" %in% names(inputTable)){
            updateCandidate <- unique(rbind(
                    self$getData("substanceconstituents", copy = FALSE),
                    inputTable[!is.na(constituentid),.(substanceid, constituentid)]))
            updateCheck <- updateCandidate[.(self$getSubstanceIds("mono")), on="substanceid"]
            if(updateCheck[,.N,by = substanceid][,any(N>1)]) warning("Some monoconstituent substances have multiple constituents")
            nNewItems <- updateCandidate[,.N] - self$nrows("substanceconstituents")
            self$addData("substanceconstituents", updateCandidate, overwrite = TRUE)
            rm(updateCandidate)
            if(verbose) cat("Adding",nNewItems ,"new constituentids to the database\n")
            if(verbose) cat("Note that existing substance identifiers have not been updated.\nRun the mergeSubstance method to merge already existing substances in all database tables\n.")            
          }
          
          # update table: externalsubstanceids
          if(length(altIdsFound)){
            
            getAltIdTable <- function(altId, inpTable){
              cat("External substance ids: processing", altId, "\n")
              inpTable <- inpTable[!is.na(substanceid)]
              setnames(inpTable, altId, "idvalue") 
              inpTable <- inpTable[!is.na(idvalue),
                  .(substanceid, idtype = altId, idvalue, url)]
              return(inpTable)
            } 
            updateCandidate <- lapply(altIdsFound, getAltIdTable, inpTable = inputTable)
            updateCandidate <- c(list(self$getData("externalsubstanceids", copy = FALSE)), updateCandidate)
            updateCandidate <- unique(rbindlist(updateCandidate, fill = TRUE))
            # if multiple url values are given, only keep the first non-NA, non-empty one
            updateCandidate[, url2 := c(url[!is.na(url)],NA_character_)[1],
                by = .(substanceid, idtype, idvalue)]
            updateCandidate[,url := url2]
            updateCandidate[,url2 := NULL]
            updateCandidate <- unique(updateCandidate)
            nNewItems <- updateCandidate[,.N] - self$nrows("externalsubstanceids")
            self$addData("externalsubstanceids", updateCandidate, overwrite = TRUE)
            if(verbose) cat("Adding",nNewItems ,"new externalsubstanceids to the database\n")
          }
          
          ## return substanceids
          if(returnTable){
            inputTable
          } else {
            # add NAs for rows removed by removeNonInformativeEntries()
            ids <- rep(NA_integer_, length(informativeEntries))
            ids[informativeEntries] <- inputTable[, substanceid]
            return(invisible(ids))
          } 
        },
        #' @description add substance activity data
        #' @param inputTable Data.table object
        #' @param substanceIdentifier Character string indicating which column should be used as a substance identifier ("substanceid" indicates non-persistent internal identifiers)
        #' @param type Character string indicating type of activity data. Currently allowed: "invivo","clp","invitro"
        #' @return NULL
        addSubstanceActivity = function(inputTable, substanceIdentifier, type = c("invivo", "clp", "invitro", "phenotypes")){
          type <- match.arg(gsub("\\s","",tolower(type)), type)
          
          dbSubstanceTableName <- switch(type,
              invivo = "substanceinvivo", 
              clp = "substanceclp",
              invitro = "substanceinvitro",
              phenotypes = "substancephenotypes")
          
          if(!substanceIdentifier %in% names(inputTable)) stop("substanceIdentifier is not a column name in inputTable")
          if(!substanceIdentifier %in% c("substanceid", self$metadata$allowedValues$idtype)){
            stop("substanceIdentifier should be 'substanceid' or one of the other allowed 'external' id types:",
                paste(self$metadata$allowedValues$idtype ,collapse = " "))
          } 
          
          foundTableColumns <- setdiff(intersect(names(inputTable), self$tableColumns[[dbSubstanceTableName]]), "substanceid")
          missingTableColumns <- setdiff(self$tableColumns[[dbSubstanceTableName]], c("substanceid", "externalsubstanceidtype", "externalsubstanceid", names(inputTable)))
          ignoredTableColumn <- setdiff(names(inputTable), c(self$tableColumns[[dbSubstanceTableName]], substanceIdentifier))
          if(!length(foundTableColumns)) stop("inputTable does not contain any of the required columns")
          if(length(missingTableColumns)) stop("inputTable does not contain the following REQUIRED columns (please add and set to NA if not available): ", paste(missingTableColumns,collaps = ","))
          if(length(ignoredTableColumn)) warning("The following columns in inputTable are ignored: ", paste(ignoredTableColumn,collapse = ", "))
          
          inputTable <- unique(inputTable[, c(substanceIdentifier, foundTableColumns) , with = FALSE])[!is.na(get(substanceIdentifier))]
          # set certain species to lowercase
          if("species" %in% names(inputTable)) inputTable[, species:= standardizeSpecies(species)]
          
          if(substanceIdentifier == "substanceid"){
            inputTable[, externalsubstanceidtype := NA_character_]
            inputTable[, externalsubstanceid := NA]
          } else {
            setnames(inputTable, substanceIdentifier, "externalsubstanceid")
            # all external identifiers should be character (for now)
            inputTable[, externalsubstanceid := as.character(externalsubstanceid)]
            inputTable[, externalsubstanceidtype := substanceIdentifier]
          }
          
          if(type=="clp"){
            # split hazardcode column in individual codes:
            inputTable <- inputTable[,.(externalsubstanceidtype,externalsubstanceid,hazardcode,.I)][,
                .(  externalsubstanceidtype,
                    externalsubstanceid,
                    hazardcode = strsplit(hazardcode,"[[:punct:][:space:]]+")[[1]]
                ),by = I][,I:=NULL]
          }
          
          if(substanceIdentifier == "substanceid"){
            if(!all(inputTable[,na.omit(unique(substanceid))] %in% self$getData("substancetypes", copy = FALSE)[,
                    unique(substanceid)])){
              stop("Column substanceid contains one or more ids that are not defined for the current database.")
            }
          } else {
            # map external identifier to substanceid
            inputTable <- merge(inputTable, self$getSubstanceExtIds(substanceIdentifier, firstMatch = TRUE), by.x = "externalsubstanceid", by.y = substanceIdentifier) 
            inputTable <- inputTable[, setdiff(names(inputTable), substanceIdentifier), with = FALSE]
          }
          nBefore <- self$nrows(dbSubstanceTableName)
          self$addData(dbSubstanceTableName, inputTable, fill = TRUE)
          nAfter <- self$nrows(dbSubstanceTableName)
          cat((sprintf("Added %d substance %s activity records (new total = %d)\n", nAfter-nBefore, type, nAfter)))
          invisible()
        },
        #' @description get smiles for constituents of mono-constituent substances
        #' @param substanceids Vector with integer substance identifiers
        #' @param uniqueBy Character string. If NULL, all matches are returned. If "substanceid" or "smiles", the first match per substanceid or smiles is returned. 
        #' @return NULL
        getMonoSubstanceSmiles = function(substanceids = NULL, uniqueBy = NULL){
          if(self$nrows("substancetypes") && self$nrows("substanceconstituents")){
            if(is.null(substanceids)){
              monoSubstances <- self$getData("substancetypes", copy = FALSE)[substancetype == "mono", substanceid]
            } else {
              monoSubstances <- self$getData("substancetypes", copy = FALSE)[substanceid %in% substanceids & substancetype == "mono", substanceid]
            }
            res <- merge(
                self$getData("substanceconstituents", copy = FALSE)[substanceid %in% monoSubstances],
                self$getData("constituents", copy = FALSE),
                by = "constituentid")
            if(!is.null(uniqueBy)){
              if(uniqueBy == "substanceid"){
                res = res[,.SD[1], by = substanceid]
              } else if(uniqueBy == "smiles"){
                res = res[,.SD[1], by = smiles]
              } else stop("unsupported uniqueBy argument")
            }
            res
          } else data.table(constituentid = list(), substanceid = list(), smiles = list())
        },
        #' @description lookup first mono-constituent substance for each smiles
        #' @param smilesVector Vector with smiles (character strings)
        #' @return vector with substanceids
        mapSmilesToSubstance = function(smilesVector){
          # maps to first monoconstituent substance with this smiles
          smilesLookup <- self$getMonoSubstanceSmiles(uniqueBy = "smiles")
          if (nrow(smilesLookup) == 0 ){
            return(rep(NA_integer_, length(smilesVector)))
          } else {
            smilesLookup[smilesVector, substanceid, on = "smiles"]
          }
        },
        #' @description create a list of tables with substance activity information
        #' @param substanceids Vector with integer substance identifiers
        #' @param summarize If TRUE, a summary table is added to the returned list
        #' @param skipDartNA If TRUE, only DART-active and DART-inactive records are shown
        #' @param web If TRUE, some elements are added for usage in a web application (hyperlinks, embedded images)
        #' @return Named list of data.table objects for each of the types of activity data (invitro, invivo, clp, ...) and optionally a summary
        getSubstanceActivity = function(substanceids = NULL, summarize = FALSE, skipDartNA = FALSE, web = TRUE){
          
          newline <- if(web) "<br>" else if (.Platform$OS.type=="windows") "\r\n" else "\n"
          
          if(is.numeric(substanceids)){
            NULL
          } else if(inherits(substanceids, "data.frame") &&  "substanceid" %in% colnames(substanceids)){
            setDT(substanceids)
            # TODO: do we really need to support data.frames/ data.tables here?
            # If so, also add other columns from substanceids to output table and adapt roxygen documentation
            substanceids <- substanceids[,substanceid]
          } else if(!is.null(substanceids)) stop("substanceids should be an integer vector, a data.frame or a data.table")
          
          results <- list()
          activitySummary <- list()
          for(type in c("invivo","clp", "invitro", "phenotypes")){
            dbSubstanceTableName <- switch(type,
                invivo = "substanceinvivo", 
                clp = "substanceclp",
                invitro = "substanceinvitro",
                phenotypes = "substancephenotypes")
            
            activityDetail <- if(self$nrows(dbSubstanceTableName) && !is.null(substanceids)){
                  self$getData(dbSubstanceTableName, copy = TRUE)[substanceid %in% substanceids]
                } else {
                  self$getData(dbSubstanceTableName, copy = TRUE)
                }
            
            if(type == "invivo"){
              # filtering of records
              activityDetail <- activityDetail[!is.na(guideline) & !guideline %in% "Not Reported"]
              # selection of active/inactive records
              dartTrueDescriptors <- c("LOEL", "LOAEL", "LOAEC", "LOEC")
              dartFalseDescriptors <- c("NOAEL", "NOEL", "NOAEC", "NOEC", "NOAEDD", "NOEDD", "NOEL calculated")
              activityDetail[effectdescriptor%in%dartFalseDescriptors, dart := FALSE]
              activityDetail[effectdescriptor%in%dartTrueDescriptors, dart := TRUE]
              # Only show one record per report (maximum reported effect)
              # (similar to below, each unique combination of
              # substanceid, species and url is interpreted as a unique report)
              normalized <- function(vec){if(any(!is.na(vec))) vec/max(vec,na.rm = TRUE) else vec}
              activityDetail <- activityDetail[,
                  .SD[order(effectdescriptor%in%dartTrueDescriptors + normalized(effectvalue),
                          decreasing = TRUE)[1]],
                  by = .(substanceid,species,url,effectunit)]
              if(web){
                activityDetail[, sourcedb:= createLink(url,sourcedb)]
                activityDetail[, effectvalue:= round(effectvalue,3)]
              }
            } else if(type == "invitro"){
              # filtering of records
              activityDetail <- activityDetail[!is.na(gene)]
              if(web){
                activityDetail[, sourcedb:= createLink(url,sourcedb)]
              }
              
              # map to human genes, add geneid and drop genes that cannot be mapped to human genes
              activityDetail <- self$mapGenesToHumanGenes(activityDetail)
              
              # selection of active/inactive records
              # for now, we show both active and inactive records
              # aggregation of records -- combine all externalsubstanceid
              # (consider hit if there is a hit for any externalsubstanceid)
              if (nrow(activityDetail)){
                activityDetail <- activityDetail[, .(
                        hitcall = as.logical(max(hitcall)),
                        ac50 = maxWithNA(ac50),
                        sourcedb = paste(sourcedb, collapse = ","),
                        url = paste(url, collapse = " ")),
                    by = .(substanceid, assay, gene, geneid, species)]
              }
            } else if(type == "phenotypes"){
              # filtering of records
              activityDetail <- activityDetail[phenotypeid!="" & !is.na(phenotypeid)]
              # selection of active/inactive records
              # -
              if(web){
                activityDetail[, sourcedb:= createLink(url,sourcedb)]
              }             
            } else if(type =="clp"){
              # only keep H360*, H361* and H362
              # note that case differences of H360* and H361* suffixes have a meaning (capitals indicate stronger evidence)
              activityDetail[hazardcode %in% DARTHAZARDCODES, dart := TRUE]
            }
            if(skipDartNA && "dart" %in% names(activityDetail)) activityDetail <- activityDetail[dart %in% c(TRUE, FALSE)]
            
            if(summarize){
              # from tox detail to tox summary
              # detailrows gives row numbers from results[[type]]
              if(type == "invivo"){
                # each unique combination of substanceid, species, url is interpreted as a unique report
                activityBySpecies <- activityDetail[,.(dart = any(dart, na.rm = TRUE)), by = .(substanceid, species, url)]
                activityBySpecies <- activityBySpecies[,.(
                        nActive = sum(dart, na.rm = TRUE),
                        nInactive = sum(!dart, na.rm = TRUE),
                        nRecords = sum(!is.na(dart))
                    ), by = .(substanceid, species)]
                
                activityBySpecies <- activityBySpecies[, .(nActive, nInactive, nRecords,
                        `DART records` = if(web){
                              paste0('<div class = "invivo-sum-wrapper">', species, " ", '<div class = "invivo-sum">', nActive, "/", nRecords, " ", '</div></div>', 
                                  createBarImg(nActive, nInactive))
                            } else paste0(species, " ", '<div class = "invivo-sum">', nActive, "/", nRecords, '</div>')
                    ), by = .(substanceid, species)]
                if(web){
                  activitySummary[[type]] <- activityBySpecies[, .(
                          `DART in vivo` = paste0('<div class = "invivo-sum-wrapper"><div class = "invivo-total">', "total ", '</div>', '<div class = "invivo-sum">', sum(nActive),"/",sum(nRecords), '</div></div>', 
                              createBarImg(sum(nActive), sum(nInactive)), 
                              newline,
                              paste(`DART records`, collapse = newline))
                      ), by = substanceid]
                } else {
                  activitySummary[[type]] <- activityBySpecies[, .(
                          `DART in vivo` = 
                              paste0('<div class = "invivo-sum-wrapper"><div class = "invivo-sum">', sum(nActive), "/", sum(nRecords), '</div>', 
                                  '<div class = "invivo-total">', " total; ", '</div>',
                                  paste(`DART records`,collapse = "; "))
                      ), by = substanceid]                      
                }
              } else if(type == "clp"){
                activitySummary[[type]] <- activityDetail[hazardcode %in% DARTHAZARDCODES,][,.(
                        `DART CLP` = paste(
                            #'<div class="tooltip">',
                            sort(unique(hazardcode)),
                            #'<span class="tooltiptext">hazardcode</span></div>',
                            collapse = newline) 
                    ), by = substanceid]
              } else if(type == "invitro"){
                activitySummary[[type]] <- activityDetail[,
                    .(
                        invitrohitgenecount = .SD[hitcall == TRUE,length(unique(na.omit(gene)))],
                        invitrohitassaycount = .SD[hitcall == TRUE,length(gene)], # length(gene) instead of .N allows to get 0 values
                        invitrogenecount = .SD[,length(unique(na.omit(gene)))],
                        invitroassaycount = .SD[,length(gene)],
                        invitrotargetnames = .SD[hitcall == TRUE, paste(
                                unique(na.omit(sort(gene))),
                                collapse = newline)],
                        invitrotargetoccurrences = .SD[hitcall == TRUE,paste(as.numeric(table(na.omit(sort(gene)))), collapse = newline)]
                    ), by = substanceid]
              } else if(type == "phenotypes"){
                activitySummary[[type]] <- activityDetail[!is.na(phenotypename),
                    .(
                        speciesname = paste(
                            unique(species), collapse = newline),
                        phenotypesnamescount = length(unique(phenotypename)),
                        phenotypesnames = paste(
                            sort(unique(phenotypename)),
                            collapse = newline),
                        `phenotypes` = paste(
                            sort(unique(paste(species,":",phenotypename))),
                            collapse = newline),
                        phenosummary = {summaryTable <- rowSums(table(species, phenotypename) >=1);
                          speciesNames <- names(summaryTable);
                          speciesCounts <- as.numeric(summaryTable);
                          paste(speciesNames,":", speciesCounts, collapse = newline)
                        }                            
                    ), by = substanceid]
              } 	
              
            }
            results[[type]] <- activityDetail
          }
          if(summarize){
            activitySummaryCombined <- data.table(substanceid = substanceids)
            
            for(item in activitySummary){
              if(!is.null(item)){
                activitySummaryCombined <- merge(activitySummaryCombined, item , by = "substanceid", all.x = TRUE, all.y = TRUE)
              }
            }
            
            
            results[["activitySummary"]] <- activitySummaryCombined
          }
          return(results)
        },
        #' @description get all substance ids
        #' @param substancetype Optional vector with types of substances. E.g., c("uvcb","mono","multi")
        #' @return NULL
        getSubstanceIds = function(substancetype = self$metadata$allowedValues$substancetype){
          if(self$nrows("substancetypes")){
            sort(unique(self$getData("substancetypes", copy = FALSE)[
                        substancetype %in%self$metadata$allowedValues$substancetype, substanceid]))
          } else c()
        },
        #' @description get all external substance ids
        #' @param idType Character string with name of external id. E.g. "CAS", "EC", "name"
        #' @param firstMatch If TRUE, the first substanceid is returned if multiple external ids map to the same internal substanceid
        #' @param returnURL If TRUE, also a column url will be returned
        #' @param requireURL If TRUE, rows with url==NA will be left out of the returned table
        #' @return a data.table with column "substanceid" and column matching idType
        getSubstanceExtIds = function(idType, firstMatch = FALSE, returnURL = FALSE, requireURL = FALSE){
          if(self$nrows("externalsubstanceids")){
            res <- setnames(self$getData("externalsubstanceids", copy = FALSE)[
                    idtype %in% idType,.(substanceid, idvalue, url)],c("substanceid", idType, "url"))
            if(requireURL){
              res <- res[!is.na(url)]
            }
            if(firstMatch){
              res <- res[,.SD[1], by = idType]
            }
            if(!returnURL) res[, url:= NULL]
          } else {
            res <- emptyDT(c("substanceid", idType))
          }
          return(res)
        },
        #' @description create a table with substance information
        #' @param substanceids Vector with integer substance identifiers
        #' @param web If TRUE, some elements are added for usage in a web application (hyperlinks, embedded images)
        #' @param limitOutput If TRUE, some less informative information is left out
        #' @param priorityField Field ("name", "CAS" or "EC") used for sorting results
        #' @param priorityPattern Value in priorityField for which matches will be listed before non-matching items.
        #' @return a data.table object
        tabulateSubstanceInfo = function(substanceids, web = FALSE, limitOutput = TRUE,
            priorityField = NULL, priorityPattern = NULL){
          if(is.numeric(substanceids)){
            substanceids = data.table(substanceid = substanceids)
          } else if (!inherits(substanceids, "data.frame") ||
              !("substanceid" %in% colnames(substanceids))){
            stop("substanceids should be an integer vector, a data.frame or a data.table")
          }
          setDT(substanceids)
          res1 <- self$getData("substancetypes", copy = FALSE)[substanceids, on = "substanceid"]
          res2 <- self$getMonoSubstanceSmiles(substanceids = substanceids[, substanceid], uniqueBy = "substanceid")
          res2[ , constituentid:= NULL]
          altIdsLong <- self$getData("externalsubstanceids", copy = FALSE)[substanceid%in% substanceids[, substanceid]]
          
          if(!is.null(priorityField) && !is.null(priorityPattern) && priorityField %in% c("name", "CAS", "EC")){
            prio <- if (priorityField =="name"){
                  altIdsLong[, ifelse(idtype==priorityField,-is.na(url)+grepl(paste0("^",priorityPattern,"$"),idvalue),-1)]
                } else {
                  altIdsLong[, ifelse(idtype==priorityField,-is.na(url)+1/nchar(idvalue)+grepl(paste0("^",priorityPattern,"$"),idvalue),-1)]
                }
            # sort altIds according to prio
            altIdsLong <- altIdsLong[order(prio,decreasing = TRUE)]
          }
          
          if(web){
            # abbreviate substance names with more than 100 characters
            maxChars <- 100
            if(altIdsLong[, any(idtype == "name" & nchar(idvalue) > maxChars)]){
              altIdsLong[idtype == "name" & nchar(idvalue) > maxChars, idvalue := paste(substring(idvalue, 1, maxChars), "...")]
            }
            # create links  
            altIdsLong[, idvalue:= createLink(url,idvalue)]
          }
          
          if(limitOutput){
            # limit the amount of output
            # only show altIds with a url (or first one if no URL is available)
            limitFun <- function(x){
              if(x[!is.na(url),.N] == 0){
                return(x[1,])
              } else {
                return(x[!is.na(url)])
              }
            }
            altIdsLong <- altIdsLong[, limitFun(.SD) , by = .(idtype, substanceid)]
          }
          if(web){
            res3 <- dcast(altIdsLong, substanceid ~ idtype, value.var = "idvalue",
                fun.aggregate = function(x) paste(x, collapse = "</br>"))
          } else {
            res3 <- dcast(altIdsLong, substanceid ~ idtype, value.var = "idvalue",
                fun.aggregate = function(x) paste(x, collapse = "\n"))
          }
          
          res <- merge(
              merge(res1, res2 , by = "substanceid", all.x = TRUE),
              res3, by = "substanceid")
          
          # add png images with structures
          if(web && res[!is.na(smiles),.N] > 0 ){
            res[!is.na(smiles), standardized_structure := smilesToPng(smiles)]
          } else res[, standardized_structure := NA_character_] 
          
          return(res[substanceids, on = "substanceid"])
        },
        #' @description create list of data.tables with substance/UVCB category information
        #' @param categories Name of category
        #' @return list of data.tables with substance category information
        tabulateSubstanceCategory = function(categories){
          list(
              info = self$getData("substancecategoryinfo")[category %in% categories],
              profile = self$getData("substancecategoryprofiles")[category %in% categories],
              members = self$getData("substancecategorysubstances")[category %in% categories]
          )         
        },
        #' @description get maximum integer substanceid value
        #' @return integer value
        maxSubstanceid = function(){
          if(self$getData("substancetypes", copy = FALSE)[!is.na(substanceid),.N] > 0){
            self$getData("substancetypes", copy = FALSE)[!is.na(substanceid),max(substanceid)] # allows missing substanceids
          } else 0L
        },
        #' @description Update stored constituent descriptors (e.g. fingerprints) for all constituents in database
        #' @param type descriptor type used for similarity search. Allowed values: "morgan" (Morgan/ECFP fingerprints)
        #' @return NULL
        updateDescriptors = function(type = "morgan"){
          
          if(self$nrows("constituents") == 0) return(invisible())
          constituents <- self$getData("constituents", copy = FALSE)
          tablename <- paste0("constituentdescriptor", type)
          
          if(self$nrows(tablename)){
            missingConstituentIds = setdiff(constituents[,constituentid], self$getData(tablename, copy = FALSE)[,constituentid])
            if(length(missingConstituentIds)){
              self$addData(tablename,
                  private$calculateDescriptors(constituents[.(missingConstituentIds),on="constituentid"], type),
                  fill = FALSE)}
          } else {
            self$addData(tablename, private$calculateDescriptors(constituents, type))
          }
        },
        #' @description Get constituent descriptors (e.g. fingerprints)
        #' @param constituentids vector integer ids of query constituents
        #' @param type descriptor type used for similarity search. Allowed values: "morgan" (Morgan/ECFP fingerprints)
        #' @return data.table with descriptors
        getDescriptors = function(constituentids = NULL, type = "morgan"){
          # update stored descriptors
          self$updateDescriptors(type)
          # lookup descriptors for requested constituents
          tablename <- paste0("constituentdescriptor", type)
          if(is.null(constituentids)){
            return(self$getData(tablename, copy = FALSE))
          } else {
            return(self$getData(tablename, copy = FALSE)[.(constituentids), on = "constituentid"])
          }
        },
        #' @description Find similar constituents
        #' @param constituentids vector integer ids of query constituents
        #' @param type descriptor type used for similarity search (currently, only 'morgan' is supported)
        #' @return data.table with columns constituentid and similarity
        #' @details if the query vector constituentids has length >1, each returned row contains the maximum similarity against these query constituents  
        getSimilarConstituents = function(constituentids, type = "morgan"){
          if(type != "morgan"){
            stop("Only type='morgan' is currently supported")
            # 'morgan' gives Tanimoto similarity for Morgan fingerprints
            # in the future we may add different descriptor types and different similarity functions
          }
          descriptorTable <- self$getDescriptors(type = type)
          descriptorIds <- descriptorTable[[1]]
          queryTable <- descriptorTable[constituentid %in% constituentids]
          similarities <- createAsymmetricTanimotoMatrix(as.matrix(descriptorTable[,-1]),
              as.matrix(queryTable[,-1]))
          return(data.table(constituentid = descriptorIds,
                  similarity = apply(similarities, 1, max)))
        },
        #' @description Find similar constituents
        #' @param substanceids vector integer ids of query substances
        #' @param type descriptor type used for similarity search (currently, only 'morgan' is supported)
        #' @param digits Number of digits kept when rounding similarity values
        #' @return data.table with columns substanceid and similarity
        #' @details each returned row contains the maximum similarity between constituents of query substances and returned substances  
        getSimilarSubstances = function(substanceids, type = "morgan", digits = 3){
          constituentQuery <- self$getData("substanceconstituents", copy = FALSE)[substanceid %in% substanceids, unique(constituentid)]
          if(length(constituentQuery) == 0) return(NULL)
          constituentSimilarity <- self$getSimilarConstituents(constituentQuery, type)
          substanceSimilarity <- self$getData("substanceconstituents", copy = FALSE)[constituentSimilarity, , on = "constituentid"]
          return(substanceSimilarity[,
                  .(similarity = round(max(similarity), digits)),
                  by = substanceid][order(-similarity)])
        }, 
        #' @description get substances (e.g. UVCBs) that are part of the same category as the provided substances
        #' @param substanceids vector with substanceid values (integers)
        getSameCategorySubstances = function(substanceids){
          categories <- self$getData("substancecategorysubstances")[substanceid %in% substanceids, unique(category)]
          if(length(categories)>0){
            unique(c(substanceids,
                    self$getData("substancecategorysubstances")[category %in% categories, substanceid]))
          } else unique(substanceids)
          
        },
        #' @description print summary of database contents and warnings about inconsistencies etc.
        inspect = function(){
          info <- c()
          addInfo <- function(msg){info <<- c(info, msg)}
          warningMessages <- c()
          addWarning <- function(msg){warningMessages <<- c(warningMessages, msg)}
          
          # substances
          addInfo(sprintf("Database contains %d substances with types:\n    %s",
                  self$nrows("substancetypes"),
                  self$getData("substancetypes", copy = FALSE)[,.N, by = substancetype][, paste(substancetype, N, sep =": ", collapse = ", ")]
              ))
          
          # external substance ids
          addInfo(sprintf("Database contains the following external substance identifiers:\n    %s",
                  self$getData("externalsubstanceids", copy = FALSE)[,
                      .N, by=idtype][, paste(idtype, N, sep =": ", collapse = ", ")]
              ))
          n <- self$getData("externalsubstanceids", copy = FALSE)[, sum(is.na(idvalue))]
          if(n>0) addWarning(sprintf("Table externalsubstanceids contains %d records with NA idvalue: %s",n,
                    self$getData("externalsubstanceids", copy = FALSE)[,
                        sum(is.na(idvalue)), by = idtype][, paste(idtype, V1, sep =": ", collapse = ", ")]
                ))
          
          replicatedNames <- self$getSubstanceExtIds("name")[,.(n=.N, substanceids = paste(sort(unique(substanceid)), collapse = ",")), by = name][n>1]
          if(replicatedNames[,.N]){
            addWarning(sprintf("Table externalsubstanceids contains %d names that are shared by multiple substances", replicatedNames[,.N]))
          }
          
          
          # constituents
          nConstRecords <- self$nrows("constituents") 
          nConstIds <- self$getData("constituents", copy = FALSE)[, length(unique(constituentid)) ]
          nSmiles <- self$getData("constituents", copy = FALSE)[, length(unique(smiles)) ]
          addInfo(sprintf("Database contains %s unique substance constituents",nConstIds))
          if(nSmiles < nConstRecords) addWarning(sprintf("Table constituents contains %d records, while the number of unique smiles is %d",nConstRecords, nSmiles))
          if(nConstIds < nConstRecords) addWarning(sprintf("Table constituents contains %d records, while the number of constituentids is %d",nConstRecords, nConstIds))
          monoConst <- merge(self$getData("substanceconstituents", copy = FALSE),
              self$getData("substancetypes", copy = FALSE), by = "substanceid")[substancetype == "mono"]
          monoWithMultipleConst <- monoConst[,.N > 1, by = "substanceid"][, sum(V1)]
          if(monoWithMultipleConst>0) addWarning(sprintf("Database contains %d mono-constituent substances with multiple constituents", monoWithMultipleConst))
          
          # Activity data
          ## In vivo regulatory studies (mammalian)
          speciesCounts <- self$getData("substanceinvivo", copy = FALSE)[,.N, by = species][order(-N), paste(species, N, sep =": ")]
          speciesCounts <- paste0(speciesCounts,ifelse(seq_along(speciesCounts)%%5 ==0,"\n    "," "), collapse = "")
          addInfo(sprintf("Database contains %d in vivo substance activity records (%s unique urls) for the following species:\n    %s",
                  self$nrows("substanceinvivo"),
                  self$getData("substanceinvivo", copy = FALSE)[!is.na(url),length(unique(url))],
                  speciesCounts))
          ## In vitro (mammalian)
          speciesCounts <- self$getData("substanceinvitro", copy = FALSE)[,.N, by = species][order(-N), paste(species, N, sep =": ")]
          speciesCounts <- paste0(speciesCounts,ifelse(seq_along(speciesCounts)%%5 ==0,"\n    "," "), collapse = "")
          addInfo(sprintf("Database contains %d in vitro substance activity records for the following species:\n    %s",
                  self$nrows("substanceinvitro"), speciesCounts))
          ## Phenotypes (mammalian + NAM)
          phenotypes <- self$getData("substancephenotypes")[, ontology:=gsub("^([A-Za-z]+):.*$","\\1", phenotypeid)][ontology != ""]
          ontologyCounts <- phenotypes[,.N, by = ontology][order(-N), paste(ontology, N, sep =": ")]
          ontologyCounts <- paste0(ontologyCounts,ifelse(seq_along(ontologyCounts)%%5 ==0,"\n    "," "), collapse = "")
          substanceCountsByOntology <- phenotypes[,length(unique(substanceid)), by = ontology][order(-V1), paste(ontology, V1, sep =": ")]
          substanceCountsByOntology <-  paste0(substanceCountsByOntology,ifelse(seq_along(substanceCountsByOntology)%%5 ==0,"\n    "," "), collapse = "")
          addInfo(sprintf("Database contains %d phenotype records:\n   * Unique records per ontology:\n    %s \n   * Unique substances per ontology:\n    %s",
                  nrow(phenotypes),
                  ontologyCounts, substanceCountsByOntology))
          
          ## CLP
          # note: counts may be confusing as the same hazardcode can appear multiple times for the same substanceid
          # (due to aggregration of multiple source-database substances into one substanceid)
          addInfo(sprintf("Database contains %d CLP records for %d unique substances",
                  self$nrows("substanceclp"),self$getData("substanceclp", copy = FALSE)[,length(unique(substanceid))]))
          addInfo(sprintf("Database contains %d unique substances with a DART-related CLP hazard code",
                  self$getData("substanceclp", copy = FALSE)[hazardcode %in% DARTHAZARDCODES,length(unique(substanceid))]))
          
          cat("INFORMATION ABOUT CURRENT DATABASE\n")
          cat(paste(" *", info,collapse = "\n"),"\n")
          if(length(warningMessages) > 0){
            cat("\nWARNINGS ABOUT CURRENT DATABASE\n")
            cat(paste(" *", warningMessages,collapse = "\n"),"\n")
          }
          
        },
        #' @description merge substances that have matching identifiers and mono-constituent substances with identical standardized smiles
        mergeSubstances = function(){
          nIdentifiersWithOverlap <- Inf
          while(nIdentifiersWithOverlap>0){
            nIdentifiersWithOverlap <- self$.mergeSubstancesOnce()
          }
        },
        #' @description loops once over external identifiers and smiles to merge substances with overlapping identifiers  
        .mergeSubstancesOnce = function(){
          nRecordsTot <- self$getData("externalsubstanceids", copy = FALSE)[,.N]
          nRecordsUnique <- self$getData("externalsubstanceids", copy = FALSE)[,unique(.(substanceid,idtype,idvalue))][,.N]
          if (nRecordsTot != nRecordsUnique) stop("Database corruption: externalsubstanceids should contain only unique records")
          
          # merge substance on external identifiers
          idTypes <- setdiff(self$metadata$allowedValues$idtype, "name")
          nIdentifiersWithOverlap <- 0
          for (thisIdType in idTypes){
            externalsubstanceids <- self$getData("externalsubstanceids", copy = TRUE)
            # map to the lowest substanceid
            thisMap <- externalsubstanceids[idtype == thisIdType][,
                .(substanceid, newsubstanceid = min(substanceid)), by = idvalue]
            thisMapDiff <- unique(thisMap[substanceid!=newsubstanceid,.(substanceid, newsubstanceid)])
            # map again to the lowest substanceid to deal with multiple external ids for the same substanceid
            if(thisMapDiff[,.N]){
              thisMapDiff <- thisMapDiff[,.(newsubstanceid = min(newsubstanceid)),by = substanceid]
            }
            cat("Found", thisMapDiff[,.N], "substances that can be merged with another substance, based on identifier type", thisIdType, ".\n")
            if(thisMapDiff[,.N]){
              nIdentifiersWithOverlap <- nIdentifiersWithOverlap + 1
              self$.updateSubstanceids(thisMapDiff)
            }
          }
          
          # merge mono constituent substances with identical smiles
          monoSubstanceSmiles <- self$getMonoSubstanceSmiles()
          # map to the lowest substanceid
          thisMap <- monoSubstanceSmiles[, .(substanceid, newsubstanceid = min(substanceid)), by = smiles]
          thisMapDiff <- unique(thisMap[substanceid!=newsubstanceid,.(substanceid, newsubstanceid)])
          # map again to the lowest substanceid to deal with multiple external ids for the same substanceid
          if(thisMapDiff[,.N]){
            thisMapDiff <- thisMapDiff[,.(newsubstanceid = min(newsubstanceid)),by = substanceid]
          }
          cat("Found", thisMapDiff[,.N], "mono-constituent substances that can be merged with another substance, based on identical standardized smiles.\n")
          if(thisMapDiff[,.N]){
            nIdentifiersWithOverlap <- nIdentifiersWithOverlap + 1
            self$.updateSubstanceids(thisMapDiff)
          }
          return(nIdentifiersWithOverlap)
        },
        #' @description utility function for updating substance ids (used for merging of substances). Handle with care to avoid database corruption.
        #' @param mapping data.table object with columns substanceid newsubstanceid
        .updateSubstanceids = function(mapping){
          if(!inherits(mapping, "data.table")) stop("Argument 'mapping' should be a data.table object")
          if(! all(c("substanceid","newsubstanceid") %in% names(mapping))) stop("Argument 'mapping' should have column names 'substanceid' and 'newsubstanceid'")
          
          tablesWithSubstanceids <- names(which(sapply(self$tableColumns,function(x) "substanceid" %in% x)))
          expectedTables <- c("substancetypes", "substanceconstituents", "externalsubstanceids", "substancecategorysubstances",
              "substancecategoryprofiles", "substanceinvivo", "substancephenotypes", "substanceinvitro", "substanceclp")
          if (!all(sort(expectedTables) == sort(expectedTables))){
            warning("updateSubstanceids has not been tested for all tables in the current database. Please check and update the class method.")
          }
          
          mapping <- unique(mapping) # avoid merging errors
          
          # take into account multi-step merges
          mappingPrevious <- NULL
          while(all.equal(mapping, mappingPrevious) != TRUE){
            mappingPrevious <- mapping
            mapping <- merge(mapping, mapping[,.(newsubstanceid = substanceid, newersubstanceid = newsubstanceid)], by = "newsubstanceid", all.x= TRUE)
            mapping <- unique(mapping[,.(substanceid, newsubstanceid = ifelse(is.na(newersubstanceid),newsubstanceid, newersubstanceid))])
          }
          
          # update substanceid column in all tables
          nUpdatedTables <- 0
          
          for (tableName in tablesWithSubstanceids){
            
            copiedTable <- self$getData(tableName, copy = TRUE)
            
            nNA <- copiedTable[is.na(substanceid),.N]
            if(nNA) warning("Table ",tableName," contains ", nNA," rows with invalid identifier substanceid=NA.")
            
            # skip table if nothing needs to change
            if(copiedTable[mapping, .N, on="substanceid"] == 0) next
            
            nRecordsPrevious <- copiedTable[,.N]
            copiedTable <- merge(copiedTable, mapping, by = "substanceid", all.x = TRUE)
            copiedTable[!is.na(newsubstanceid), substanceid := newsubstanceid]
            copiedTable[, newsubstanceid := NULL]
            
            # additional, table-specific update rules
            if (tableName %in% c("substancetypes")){
              # one record per substanceid
              nInconclusiveTypes <- copiedTable[, .(inconcl = length(unique(na.omit(substancetype)))), by = substanceid][inconcl>1,.N]
              if (nInconclusiveTypes > 0){
                warning("Found ", nInconclusiveTypes, " merged substances with conflicting substance types. Keeping the first occurring non-NA substance type.")
              }
              # update strategy: take first non-NA if existing (similar to determineSubstanceType)
              copiedTable <- copiedTable[, .(substancetype = c(na.omit(substancetype),NA_character_)[1]), by = substanceid]
              
            } else {
              # one record per substanceid
              copiedTable <- unique(copiedTable)
            }
            
            # assert changes in number of records match expectations
            # number of records per substance id should stay the same, go to 0 or increase, but not decrease to a value >0
            oldTableCounts <- unique(self$getData(tableName, copy = FALSE))[,.N,by=substanceid]
            newTableCounts <- copiedTable[,.N,by=substanceid]
            combinedCounts <- merge(oldTableCounts,newTableCounts,by="substanceid", all.x=TRUE, all.y=TRUE)
            unexpectedChanges <- combinedCounts[!is.na(N.y) & N.x>N.y,] # N.y=NA indicates zero records in copiedTable 
            if(unexpectedChanges[,.N]){
              warning("Unexpected changes in number of records per substanceid occurred in table ",tableName)
            }
            
            # add updated table to database
            self$addData(tableName, copiedTable, ignoreNewColumns = FALSE, overwrite = TRUE)
            cat("Updated table ",tableName," (",nRecordsPrevious ," --> ", copiedTable[,.N]," records)\n", sep = "")
            nUpdatedTables <- nUpdatedTables + 1
          }
          cat("Updated substanceids in", nUpdatedTables, "tables.\n")
          
        }
    ),
    private = list(
        listAltIdColumns = function(inputTable) intersect(names(inputTable),self$metadata$allowedValues$idtype),
        
        calculateDescriptors = function(constituentsTable, type){
          # to access outside class: <objectname>$.__enclos_env__$private$calculateDescriptors()
          descriptorFunc = switch(type,
              morgan = fingerprintsRdkit,
              stop("unsupported descriptor type")
          )
          res = merge(constituentsTable, descriptorFunc(constituentsTable[,smiles]), on = "smiles")[order(constituentid)]
          res[,smiles:=NULL]
          return(res)
        }
    )
)

