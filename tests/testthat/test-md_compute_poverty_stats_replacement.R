
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

test_that("md_compute_poverty_stats_replacement works", {

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

  out <- md_compute_poverty_stats_replacement(
    welfare     = benchmark$welfare,
    weight      = benchmark$weight,
    povline_lcu = povline_lcu
  )

  expect_equal(out[["headcount"]], 0.7333513, tolerance = 1e-6)
  expect_equal(out[["poverty_gap"]], 0.3957584, tolerance = 1e-6)
  expect_equal(out[["poverty_severity"]], 0.2534849, tolerance = 1e-6)
  expect_equal(out[["watts"]], 0.6899868, tolerance = 1e-4)
})

test_that("md_compute_poverty_stats_replacement matches previous function", {

  #_______________________________________________________________________
  # Download test data
  #________________________________________________________________________
  benchmark <- readRDS(test_path("testdata", "synthetic-microdata.RDS"))

  benchmark <- md_clean_data(benchmark[[1]]$data,
                             welfare = 'welfare',
                             weight = 'weight',
                             quiet = TRUE)$data

  out_old <- md_compute_poverty_stats(
    welfare     = benchmark$welfare,
    weight      = benchmark$weight,
    povline_lcu = mean(benchmark$welfare)
  )

  out <- md_compute_poverty_stats_replacement(
    welfare     = benchmark$welfare,
    weight      = benchmark$weight,
    povline_lcu = mean(benchmark$welfare)
  )

  expect_equal(out[["headcount"]], out_old[["headcount"]], tolerance = 1e-6)
  expect_equal(out[["poverty_gap"]], out_old[["poverty_gap"]], tolerance = 1e-6)
  expect_equal(out[["poverty_severity"]], out_old[["poverty_severity"]], tolerance = 1e-6)
  expect_equal(out[["watts"]], out_old[["watts"]], tolerance = 1e-4)
})

test_that("md_compute_poverty_stats_replacement calculates with weight = 1 when is null", {

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

  #____________________________________________________________________
  # Run old function with weight one to find  true values
  #____________________________________________________________________
  # benchmark$weight_1 <- 1
  # out_old <- md_compute_poverty_stats(
  #   welfare     = benchmark$welfare,
  #   weight      = benchmark$weight_1,
  #   povline_lcu = mean(benchmark$welfare)
  # )
  #
  # out_old # $headcount = 0.705 $poverty_gap = 0.3557192
  #         # $poverty_severity = 0.2193438 $watts = 0.598579

  out <- suppressMessages(md_compute_poverty_stats_replacement(
    welfare     = benchmark$welfare,
    weight      = NULL,
    povline_lcu = povline_lcu
  ))

  expect_equal(out[["headcount"]], 0.705, tolerance = 1e-6)
  expect_equal(out[["poverty_gap"]], 0.3557192, tolerance = 1e-6)
  expect_equal(out[["poverty_severity"]], 0.2193438, tolerance = 1e-6)
  expect_equal(out[["watts"]], 0.598579, tolerance = 1e-4)
})

test_that("md_compute_poverty_stats_replacement prints error when welfare and/or povline is null", {

  #_______________________________________________________________________
  # Download test data
  #________________________________________________________________________
  benchmark <- readRDS(test_path("testdata", "synthetic-microdata.RDS"))

  benchmark <- md_clean_data(benchmark[[1]]$data,
                             welfare = 'welfare',
                             weight = 'weight',
                             quiet = TRUE)$data

  expect_error(md_compute_poverty_stats_replacement(
    welfare     = benchmark$welfare,
    weight      = benchmark$weight,
    povline_lcu = NULL
  ),"`welfare` and `povline` arguments must be non-NULL")

  expect_error(md_compute_poverty_stats_replacement(
    welfare     = NULL,
    weight      = benchmark$weight,
    povline_lcu = mean(benchmark$welfare)
  ),"`welfare` and `povline` arguments must be non-NULL")

  expect_error(md_compute_poverty_stats_replacement(
    welfare     = NULL,
    weight      = benchmark$weight,
    povline_lcu = NULL
  ),"`welfare` and `povline` arguments must be non-NULL")
})

test_that("md_compute_poverty_stats_replacement creates warning when weight is null", {

  #_______________________________________________________________________
  # Download test data
  #________________________________________________________________________
  benchmark <- readRDS(test_path("testdata", "synthetic-microdata.RDS"))

  benchmark <- md_clean_data(benchmark[[1]]$data,
                             welfare = 'welfare',
                             weight = 'weight',
                             quiet = TRUE)$data

  expect_message(md_compute_poverty_stats_replacement(
    welfare     = benchmark$welfare,
    weight      = NULL,
    povline_lcu = mean(benchmark$welfare)
  ),"The `weight` argument is NULL, thus each observation is given equal weight by default. ")
})

test_that("md_compute_headcount prints error when welfare and/or povline is null", {

  #_______________________________________________________________________
  # Download test data
  #________________________________________________________________________
  benchmark <- readRDS(test_path("testdata", "synthetic-microdata.RDS"))

  benchmark <- md_clean_data(benchmark[[1]]$data,
                             welfare = 'welfare',
                             weight = 'weight',
                             quiet = TRUE)$data

  expect_error(md_compute_headcount(
    welfare     = benchmark$welfare,
    weight      = benchmark$weight,
    povline = NULL
  ),"`welfare` and `povline` arguments must be non-NULL")

  expect_error(md_compute_headcount(
    welfare     = NULL,
    weight      = benchmark$weight,
    povline = mean(benchmark$welfare)
  ),"`welfare` and `povline` arguments must be non-NULL")

  expect_error(md_compute_headcount(
    welfare     = NULL,
    weight      = benchmark$weight,
    povline = NULL
  ),"`welfare` and `povline` arguments must be non-NULL")
})

test_that("md_compute_headcount creates warning when weight is null", {

  #_______________________________________________________________________
  # Download test data
  #________________________________________________________________________
  benchmark <- readRDS(test_path("testdata", "synthetic-microdata.RDS"))

  benchmark <- md_clean_data(benchmark[[1]]$data,
                             welfare = 'welfare',
                             weight = 'weight',
                             quiet = TRUE)$data

  expect_message(md_compute_headcount(
    welfare     = benchmark$welfare,
    weight      = NULL,
    povline = mean(benchmark$welfare)
  ),"The `weight` argument is NULL, thus each observation is given equal weight by default. ")
})

test_that("md_compute_pov_gap prints error when welfare and/or povline is null", {

  #_______________________________________________________________________
  # Download test data
  #________________________________________________________________________
  benchmark <- readRDS(test_path("testdata", "synthetic-microdata.RDS"))

  benchmark <- md_clean_data(benchmark[[1]]$data,
                             welfare = 'welfare',
                             weight = 'weight',
                             quiet = TRUE)$data

  expect_error(md_compute_pov_gap(
    welfare     = benchmark$welfare,
    weight      = benchmark$weight,
    povline = NULL
  ),"`welfare` and `povline` arguments must be non-NULL")

  expect_error(md_compute_pov_gap(
    welfare     = NULL,
    weight      = benchmark$weight,
    povline = mean(benchmark$welfare)
  ),"`welfare` and `povline` arguments must be non-NULL")

  expect_error(md_compute_pov_gap(
    welfare     = NULL,
    weight      = benchmark$weight,
    povline = NULL
  ),"`welfare` and `povline` arguments must be non-NULL")
})

test_that("md_compute_pov_gap creates warning when weight is null", {

  #_______________________________________________________________________
  # Download test data
  #________________________________________________________________________
  benchmark <- readRDS(test_path("testdata", "synthetic-microdata.RDS"))

  benchmark <- md_clean_data(benchmark[[1]]$data,
                             welfare = 'welfare',
                             weight = 'weight',
                             quiet = TRUE)$data

  expect_message(md_compute_pov_gap(
    welfare     = benchmark$welfare,
    weight      = NULL,
    povline = mean(benchmark$welfare)
  ),"The `weight` argument is NULL, thus each observation is given equal weight by default. ")
})

test_that("md_compute_pov_severity prints error when welfare and/or povline is null", {

  #_______________________________________________________________________
  # Download test data
  #________________________________________________________________________
  benchmark <- readRDS(test_path("testdata", "synthetic-microdata.RDS"))

  benchmark <- md_clean_data(benchmark[[1]]$data,
                             welfare = 'welfare',
                             weight = 'weight',
                             quiet = TRUE)$data

  expect_error(md_compute_pov_severity(
    welfare     = benchmark$welfare,
    weight      = benchmark$weight,
    povline = NULL
  ),"`welfare` and `povline` arguments must be non-NULL")

  expect_error(md_compute_pov_severity(
    welfare     = NULL,
    weight      = benchmark$weight,
    povline = mean(benchmark$welfare)
  ),"`welfare` and `povline` arguments must be non-NULL")

  expect_error(md_compute_pov_severity(
    welfare     = NULL,
    weight      = benchmark$weight,
    povline = NULL
  ),"`welfare` and `povline` arguments must be non-NULL")
})

test_that("md_compute_pov_severity creates warning when weight is null", {

  #_______________________________________________________________________
  # Download test data
  #________________________________________________________________________
  benchmark <- readRDS(test_path("testdata", "synthetic-microdata.RDS"))

  benchmark <- md_clean_data(benchmark[[1]]$data,
                             welfare = 'welfare',
                             weight = 'weight',
                             quiet = TRUE)$data

  expect_message(md_compute_pov_severity(
    welfare     = benchmark$welfare,
    weight      = NULL,
    povline = mean(benchmark$welfare)
  ),"The `weight` argument is NULL, thus each observation is given equal weight by default. ")
})

test_that("md_compute_watts prints error when welfare and/or povline is null", {

  #_______________________________________________________________________
  # Download test data
  #________________________________________________________________________
  benchmark <- readRDS(test_path("testdata", "synthetic-microdata.RDS"))

  benchmark <- md_clean_data(benchmark[[1]]$data,
                             welfare = 'welfare',
                             weight = 'weight',
                             quiet = TRUE)$data

  expect_error(md_compute_watts(
    welfare     = benchmark$welfare,
    weight      = benchmark$weight,
    povline = NULL
  ),"`welfare` and `povline` arguments must be non-NULL")

  expect_error(md_compute_watts(
    welfare     = NULL,
    weight      = benchmark$weight,
    povline = mean(benchmark$welfare)
  ),"`welfare` and `povline` arguments must be non-NULL")

  expect_error(md_compute_watts(
    welfare     = NULL,
    weight      = benchmark$weight,
    povline = NULL
  ),"`welfare` and `povline` arguments must be non-NULL")
})

test_that("md_compute_watts creates warning when weight is null", {

  #_______________________________________________________________________
  # Download test data
  #________________________________________________________________________
  benchmark <- readRDS(test_path("testdata", "synthetic-microdata.RDS"))

  benchmark <- md_clean_data(benchmark[[1]]$data,
                             welfare = 'welfare',
                             weight = 'weight',
                             quiet = TRUE)$data

  expect_message(md_compute_watts(
    welfare     = benchmark$welfare,
    weight      = NULL,
    povline = mean(benchmark$welfare)
  ),"The `weight` argument is NULL, thus each observation is given equal weight by default. ")
})

test_that("When watts is numeric(0) then watts equals 0" , {

  # # ______________________________________________________________________
  # # Example of when watts can be numeric(0)
  # # ______________________________________________________________________
  # welfare <- benchmark$welfare
  # weight <- benchmark$weight
  # povline_lcu       <- min(welfare)-1
  # pov_status        <- (welfare < povline_lcu)
  # weight_total      <- sum(weight)
  # keep               <- welfare > 0 & pov_status
  # w_gt_zero          <- welfare[keep] # Makes it numeric(0)
  # sensitive_distance <- log(povline_lcu / w_gt_zero) # Makes it numeric(0)
  # watts              <- sum(sensitive_distance * weight[keep])/weight_total #The sum makes it just 0
  #
  #
  #_______________________________________________________________________
  # Download test data
  #________________________________________________________________________
  benchmark <- readRDS(test_path("testdata", "synthetic-microdata.RDS"))

  benchmark <- md_clean_data(benchmark[[1]]$data,
                             welfare = 'welfare',
                             weight = 'weight',
                             quiet = TRUE)$data

  out <- md_compute_watts(
    welfare = benchmark$welfare,
    povline = min(welfare)-1,
    weight = benchmark$weight)

  expect_equal(
    out,
    0
  )
})

test_that("Does headcount function return 0 headcount when all welfare is above poverty line?", {

  out <- md_compute_headcount(
    welfare = 10:100,
    povline = 9,
    weight = 10:100)

  expect_equal(
    out,
    0
  )
})

test_that("Does headcount function return all as poor when all welfare values are below poverty line?", {

  pop <- 1:100
  out <- md_compute_headcount(
    welfare = pop,
    povline = 101,
    weight = rep(1, 100))

  expect_equal(
    out,
    1
  )
})

test_that("Does poverty gap = 1 in when welfare values are 0?", {

  out <- md_compute_pov_gap(
    welfare = rep(0, 10),
    povline = 12,
    weight = rep(1, 10)
  )

  expect_equal(
    out,
    1
  )
})

test_that("Does poverty gap = 0 when welfare values are at least the povline?", {

  out <- md_compute_pov_gap(
    welfare = 21:30,
    povline = 15,
    weight = rep(1, 10)
  )

  expect_equal(
    out,
    0
  )
})

test_that("md_compute_poverty_stats_replacement does not fail when poverty == 0", {

  #_______________________________________________________________________
  # Download test data
  #________________________________________________________________________
  benchmark <- readRDS(test_path("testdata", "synthetic-microdata.RDS"))

  benchmark <- md_clean_data(benchmark[[1]]$data,
                             welfare = 'welfare',
                             weight = 'weight',
                             quiet = TRUE)$data

  out <- md_compute_poverty_stats_replacement(
    welfare = benchmark$welfare,
    povline_lcu = min(benchmark$welfare) - 1,
    weight = benchmark$weight
  )

  expect_equal(out[["headcount"]], 0)
  expect_equal(out[["poverty_gap"]], 0)
  expect_equal(out[["poverty_severity"]], 0)
  expect_equal(out[["watts"]], 0)
})
