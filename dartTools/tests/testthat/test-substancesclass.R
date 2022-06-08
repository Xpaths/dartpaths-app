options("dartpaths_smilescache" = tempfile(fileext = ".txt"))

test_that("only add new constituents", {
      substances <- DartDB$new()
      substances$addConstituents(c("CC","CC",NA,""))
      substances$addConstituents(c("CCCCCC","CC"))
      expect_equal(nrow(substances$tables$constituents), 2)
    })

test_that("add new substance data", {
      inputTable <- getExampleData(dataType = "substance")
      substances <- DartDB$new()
      substances$addSubstances(inputTable)
      expect_equal(nrow(substances$tables$substanceconstituents), 2)
      expect_equal(nrow(substances$tables$constituents), 2)
      expect_equal(nrow(substances$tables$substancetypes), 3) # 2 unique mono + 1 unique uvcb
      expect_equal(nrow(substances$tables$externalsubstanceids), 4) # 3 unique substances + 1 substance with two EC numbers
      # called twice to check if now duplicates are created
      substances$addSubstances(inputTable)
      expect_equal(nrow(substances$tables$substanceconstituents), 2)
      expect_equal(nrow(substances$tables$constituents), 2)
      expect_equal(nrow(substances$tables$substancetypes), 3) # 2 unique mono + 1 unique uvcb
      expect_equal(nrow(substances$tables$externalsubstanceids), 4) # 3 unique substances + 1 substance with two EC numbers
    })

test_that("add descriptors", {
      inputTable <- getExampleData(dataType = "substance")
      substances <- DartDB$new()
      substances$addSubstances(inputTable)
      morganTable <- substances$getDescriptors(type ="morgan")
      expect_equal(dim(morganTable),c(2,1024 + 1))
    })


test_that("dump substance database", {
      inputTable <- getExampleData(dataType = "substance")
      substances <- DartDB$new()
      substances$addSubstances(inputTable)
      dumpPath <- file.path(tempdir(),"testDump1")
      unlink(dumpPath, recursive = TRUE)
      expect_message(substances$dump(dumpPath = dumpPath, dumpType = "txt", gzip = FALSE, overwrite = FALSE), "Successfully created")
    })

test_that("load dumped substance database", {
      substances <- DartDB$new()
      dumpPath <- file.path(tempdir(),"testDump1")
      expect_message(substances$loadDump(dumpPath), "Successfully loaded")
    })

test_that("Tanimoto matrix", {
      substances <- DartDB$new()
      dumpPath <- file.path(tempdir(),"testDump1")
      substances$loadDump(dumpPath)
      descriptorTable <- substances$getDescriptors(type = "morgan")
      tan1 <- createTanimotoMatrix(as.matrix(descriptorTable[,-1]))
      tan2 <- createAsymmetricTanimotoMatrix(as.matrix(descriptorTable[,-1]),as.matrix(descriptorTable[,-1]))
      expect_equal(tan1,tan2)
    })
