# "integration tests" using actual data
# These will only be executed if the data is available on the location
# specified by the DARTPATHS_DUMPPATH environment variable or options("dartpaths_default_dumppath")

skipIfDataUnavailable <- function() {
  
  dataAvailable <- dir.exists(getOption("dartpaths_default_dumppath","")) &&
      length(list.files(getOption("dartpaths_default_dumppath",""))) > 10
  
  if (dataAvailable) return(invisible(TRUE))
  
  skip("Real data is not available")
}

skipIfInsufficientMemory <- function() {
  
  files <- list.files(getOption("dartpaths_default_dumppath"),full.names = TRUE)
  dirSizeInBytes <- sum(sapply(files, file.size))
  
  # heuristic: assume 3.5* size of data directory is sufficient
  requiredMemInBytes <- dirSizeInBytes * 3.5
  
  availableMemInKB <- as.numeric(gsub("^.* ([0-9]+) .*$","\\1",system('grep MemTotal /proc/meminfo', intern = TRUE)))
  availableMemInBytes <- availableMemInKB * 1e3
  memoryLimitInBytes <- as.numeric(readLines("/sys/fs/cgroup/memory/memory.limit_in_bytes"))
  actualMemoryLimitInBytes <- min(availableMemInBytes, memoryLimitInBytes)
  
  if (requiredMemInBytes > actualMemoryLimitInBytes){
    skip(paste0("Insufficient memory for tests on real data. Required: ",
            round(requiredMemInBytes/1e9,2) ,
            " GB (",round(requiredMemInBytes/1024^3,2),
            " GiB), available: ", round(actualMemoryLimitInBytes/1e9,1), " GB."))
  }
}

# testthat documentation discorages test to modify global state
# however, in this case we do use a global cache to avoid reloading
# exactly the same data
# a copy is created to avoid further modifications to the global
# DATABASE object after loading
USE_DATABASE_CACHE = TRUE
DATABASE <- NULL

loadRealData <- function(useCache = USE_DATABASE_CACHE){
  skipIfDataUnavailable()
  skipIfInsufficientMemory()
  
  if(is.null(DATABASE) || !useCache){
    database <- DartDB$new()
    database$loadDump()
    if (useCache) DATABASE <<- copy(database)
    return(database) 
  } else {
    return(copy(DATABASE))
  }
}

test_that("load real data in database", {
      expect_message(database <- loadRealData(),
          "Successfully loaded database dump from")
    })


test_that("pathway ranking", {
      
      database <- loadRealData()
      
      # use substance with most mammalian phenotypes as a test case
      querySubstanceid <- database$tables$substancephenotypes[,
          .(nMP = sum(grepl("^MP.*",phenotypeid))), by = substanceid][
          order(nMP, decreasing = TRUE)[1]]
      
      pathwayRankingObject <- PathwayRanking$new(database = database, substanceid = querySubstanceid,
          pathwayLevels = 3:database$getHumanPathwayLevels()[,max(level)],
          sizePathwayMin = 4, sizePathwayMax = 200, phenotypeRankingLowestLevel = FALSE)
      ranking1 <- pathwayRankingObject$rankPathways()
      
      expect_s3_class(ranking1$summary, "data.table")
      expect_gte(nrow(ranking1$summary), 1)
      expect_gt(ncol(ranking1$summary), 1)
      expect_type(ranking1$summary$min_p_adjusted_all, "double")
      expect_true(any(is.finite(ranking1$summary$min_p_adjusted_all)))
    })

test_that("substance searches", {
      
      database <- loadRealData()
      
      #  search by name, CAS, EC
      for (exampleType in c("name", "EC", "CAS")){
        exampleItem <- database$tables$externalsubstanceids[idtype == exampleType & !is.na(url)][1]
        expect_true(exampleItem[,substanceid] %in% matchSubstanceQuery(
                database, exampleItem[,idvalue], type = exampleType, partialMatch = FALSE))
      }
      
      # search by smiles
      mySearch <- "CCCCC"
      constituentId <- database$addConstituents(mySearch)
      expect_type(constituentId, "integer")
      constituentSimilarity <- database$getSimilarConstituents(constituentId)
      expect_s3_class(constituentSimilarity, "data.table")
      expect_gte(nrow(constituentSimilarity), 1)
      expect_equal(ncol(constituentSimilarity), 2)
    })


test_that("substance activity", {
      
      database <- loadRealData()
      
      exampleSubstance <- try({
            intersect(
                intersect( database$getData("substanceinvivo")[,substanceid],
                    database$getData("substanceinvitro")[,substanceid]),
                database$getData("substancephenotypes")[,substanceid])[1]
          }, silent = TRUE)
      if(inherits(exampleSubstance, "try-error") || !is.integer(exampleSubstance)){
        skip("no substance found with all types of activity data")
      }
      res <- database$getSubstanceActivity(exampleSubstance, summarize = TRUE)
      
      expect_equal(names(res), c("invivo", "clp", "invitro", "phenotypes", "activitySummary"))
    })


test_that("pathways and orthology", {
      
      database <- loadRealData()
      
      humanPathwayLevels <- database$getHumanPathwayLevels()
      expect_s3_class(humanPathwayLevels, "data.table")
      expect_gte(nrow(humanPathwayLevels), 1)
      expect_gte(ncol(humanPathwayLevels), 3)
      
      orthology <- database$getPathwayOrthology(humanPathwayLevels[1,reactome_pathway_stable_identifier], renameEvent = TRUE)
      expect_s3_class(orthology, "data.table")
      expect_gte(nrow(orthology), 1)
      expect_gte(ncol(orthology), 3)
      
      interactiveOrthology <- interactiveGeneTable(orthology)
      expect_s3_class(interactiveOrthology, c("datatables", "htmlwidget"))
      expect_equal(nrow(interactiveOrthology$x$data), nrow(orthology))
    })

