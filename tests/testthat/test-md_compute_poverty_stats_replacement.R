
test_that("md_compute_headcount works", {

  #_______________________________________________________________________
  # Download test data
  #________________________________________________________________________
  benchmark <- readRDS(test_path("testdata", "synthetic-microdata.RDS"))

  benchmark <- md_clean_data(benchmark[[1]]$data,
                             welfare = 'welfare',
                             weight = 'weight',
                             quiet = TRUE)$data
  # ______________________________________________________________________
  # Get intermediate for benchmark
  # ______________________________________________________________________
  povline_lcu       <- mean(benchmark$welfare)
  pov_status        <- (benchmark$welfare < povline_lcu)
  #relative_distance <- (1 - (benchmark$welfare[pov_status] / povline_lcu))
  weight_pov        <- benchmark$weight[pov_status]
  weight_total      <- sum(benchmark$weight)

  out <- md_compute_headcount(
    welfare      = benchmark$welfare,
    weight       = benchmark$weight,
    povline      = povline_lcu,
    weight_pov   = weight_pov,
    weight_total = weight_total
  )

  expect_equal(out, 0.7333513, tolerance = 1e-6) #match compute_poverty_stats in povcalnet

})

test_that("md_compute_headcount works with Null for weight_pov and weight_total", {

  #_______________________________________________________________________
  # Download test data
  #________________________________________________________________________
  benchmark <- readRDS(test_path("testdata", "synthetic-microdata.RDS"))

  benchmark <- md_clean_data(benchmark[[1]]$data,
                             welfare = 'welfare',
                             weight = 'weight',
                             quiet = TRUE)$data
  # ______________________________________________________________________
  # Get intermediate for benchmark
  # ______________________________________________________________________
  povline_lcu       <- mean(benchmark$welfare)
  #pov_status        <- (benchmark$welfare < povline_lcu)
  #relative_distance <- (1 - (benchmark$welfare[pov_status] / povline_lcu))
  #weight_pov        <- benchmark$weight[pov_status]
  #weight_total      <- sum(benchmark$weight)

  out <- md_compute_headcount(
    welfare      = benchmark$welfare,
    weight       = benchmark$weight,
    povline      = povline_lcu
  )

  expect_equal(out, 0.7333513, tolerance = 1e-6) #match compute_poverty_stats in povcalnet

})

test_that("md_compute_pov_gap works", {

  #_______________________________________________________________________
  # Download test data
  #________________________________________________________________________
  benchmark <- readRDS(test_path("testdata", "synthetic-microdata.RDS"))

  benchmark <- md_clean_data(benchmark[[1]]$data,
                             welfare = 'welfare',
                             weight = 'weight',
                             quiet = TRUE)$data
  # ______________________________________________________________________
  # Get intermediate for benchmark
  # ______________________________________________________________________
  povline_lcu       <- mean(benchmark$welfare)
  pov_status        <- (benchmark$welfare < povline_lcu)
  relative_distance <- (1 - (benchmark$welfare[pov_status] / povline_lcu))
  weight_pov        <- benchmark$weight[pov_status]
  weight_total      <- sum(benchmark$weight)

  out <- md_compute_pov_gap(
    welfare           = benchmark$welfare,
    weight            = benchmark$weight,
    povline           = povline_lcu,
    weight_pov        = weight_pov,
    weight_total      = weight_total,
    relative_distance = relative_distance
  )

  expect_equal(out, 0.3957584, tolerance = 1e-6) #match compute_poverty_stats in povcalnet

})

test_that("md_compute_pov_gap works with Null for weight_pov, weight_total and relative_distance", {

  #_______________________________________________________________________
  # Download test data
  #________________________________________________________________________
  benchmark <- readRDS(test_path("testdata", "synthetic-microdata.RDS"))

  benchmark <- md_clean_data(benchmark[[1]]$data,
                             welfare = 'welfare',
                             weight = 'weight',
                             quiet = TRUE)$data
  # ______________________________________________________________________
  # Get intermediate for benchmark
  # ______________________________________________________________________
  povline_lcu       <- mean(benchmark$welfare)
  #pov_status        <- (benchmark$welfare < povline_lcu)
  #relative_distance <- (1 - (benchmark$welfare[pov_status] / povline_lcu))
  #weight_pov        <- benchmark$weight[pov_status]
  #weight_total      <- sum(benchmark$weight)

  out <- md_compute_pov_gap(
    welfare           = benchmark$welfare,
    weight            = benchmark$weight,
    povline           = povline_lcu
  )

  expect_equal(out, 0.3957584, tolerance = 1e-6) #match compute_poverty_stats in povcalnet

})

test_that("md_compute_pov_severity works", {

  #_______________________________________________________________________
  # Download test data
  #________________________________________________________________________
  benchmark <- readRDS(test_path("testdata", "synthetic-microdata.RDS"))

  benchmark <- md_clean_data(benchmark[[1]]$data,
                             welfare = 'welfare',
                             weight = 'weight',
                             quiet = TRUE)$data
  # ______________________________________________________________________
  # Get intermediate for benchmark
  # ______________________________________________________________________
  povline_lcu       <- mean(benchmark$welfare)
  pov_status        <- (benchmark$welfare < povline_lcu)
  relative_distance <- (1 - (benchmark$welfare[pov_status] / povline_lcu))
  weight_pov        <- benchmark$weight[pov_status]
  weight_total      <- sum(benchmark$weight)

  out <- md_compute_pov_severity(
    welfare           = benchmark$welfare,
    weight            = benchmark$weight,
    povline           = povline_lcu,
    weight_pov        = weight_pov,
    weight_total      = weight_total,
    relative_distance = relative_distance
  )

  expect_equal(out, 0.2534849, tolerance = 1e-6) #match compute_poverty_stats in povcalnet

})

test_that("md_compute_pov_severity works with Null for weight_pov, weight_total and relative_distance", {

  #_______________________________________________________________________
  # Download test data
  #________________________________________________________________________
  benchmark <- readRDS(test_path("testdata", "synthetic-microdata.RDS"))

  benchmark <- md_clean_data(benchmark[[1]]$data,
                             welfare = 'welfare',
                             weight = 'weight',
                             quiet = TRUE)$data
  # ______________________________________________________________________
  # Get intermediate for benchmark
  # ______________________________________________________________________
  povline_lcu       <- mean(benchmark$welfare)
  pov_status        <- (benchmark$welfare < povline_lcu)
  relative_distance <- (1 - (benchmark$welfare[pov_status] / povline_lcu))
  weight_pov        <- benchmark$weight[pov_status]
  weight_total      <- sum(benchmark$weight)

  out <- md_compute_pov_severity(
    welfare           = benchmark$welfare,
    weight            = benchmark$weight,
    povline           = povline_lcu
  )

  expect_equal(out, 0.2534849, tolerance = 1e-6) #match compute_poverty_stats in povcalnet

})

test_that("md_compute_watss works", {

  #_______________________________________________________________________
  # Download test data
  #________________________________________________________________________
  benchmark <- readRDS(test_path("testdata", "synthetic-microdata.RDS"))

  benchmark <- md_clean_data(benchmark[[1]]$data,
                             welfare = 'welfare',
                             weight = 'weight',
                             quiet = TRUE)$data
  # ______________________________________________________________________
  # Get intermediate for benchmark
  # ______________________________________________________________________
  povline_lcu       <- mean(benchmark$welfare)
  # pov_status        <- (benchmark$welfare < povline_lcu)
  # relative_distance <- (1 - (benchmark$welfare[pov_status] / povline_lcu))
  # weight_pov        <- benchmark$weight[pov_status]
  # weight_total      <- sum(benchmark$weight)

  out <- md_compute_watts(
    welfare           = benchmark$welfare,
    weight            = benchmark$weight,
    povline           = povline_lcu
  )

  expect_equal(out, 0.6899868, tolerance = 1e-4) #match compute_poverty_stats in povcalnet

})
