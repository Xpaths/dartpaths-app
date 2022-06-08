test_that("harmonic_mean_column_wise", {
      vec <- runif(10)
      expect_equal(harmonicMeanColumnWise(vec, vec, vec), vec)
      set.seed(1)
      dt <- data.table(x=runif(10), y = runif(10), z = runif(10))
      expect_equal(harmonicMeanColumnWise(vec, vec, vec), vec)
      expect_equal(dt[order(x), harmonicMeanColumnWise(x, y, z)], 
          harmonicMeanColumnWise(dt[order(x),x], dt[order(x),y], dt[order(x),z]))
      setkey(dt, y)      
      expect_equal(dt[, harmonicMeanColumnWise(x, y, z)],
          harmonicMeanColumnWise(dt[["x"]], dt[["y"]], dt[["does_not_exist"]], dt[["z"]]))
      
      expect_equal(harmonicMeanColumnWise(c(0.1,0.2,0.3), c(0.3, 0.6, NA)), c(0.15, 0.3, NA))
      expect_equal(harmonicMeanColumnWise(c(0.1,0.2,0.3), c(0.3, 0.6, NA), na.rm = TRUE), c(0.15, 0.3, 0.3))
      
    })
