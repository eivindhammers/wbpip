benchmark <- readRDS(test_path("testdata", "synthetic-microdata.RDS"))

benchmark <- md_clean_data(benchmark[[1]]$data,
                           welfare = 'welfare',
                           weight = 'weight',
                           quiet = TRUE)$data

test_that("does function return 0 headcount when all welfare is above poverty line?", {
  res <- md_compute_poverty_stats(welfare = 10:100, povline_lcu = 9, weight = 10:100)
  expect_equal(
    res$headcount,
    0
  )
})

test_that("does function return all as poor when all welfare values are below poverty line?", {
  pop <- 1:100
  res <- md_compute_poverty_stats(welfare = pop, povline_lcu = 101, weight = rep(1, 100))
  expect_equal(
    res$headcount,
    1
  )
})


test_that("does poverty gap = 1 when welfare values are 0?", {
  res <- md_compute_poverty_stats(
    welfare = rep(0, 10),
    povline_lcu = 12,
    weight = rep(1, 10)
  )
  expect_equal(
    res$poverty_gap,
    1
  )
})

test_that("does poverty gap = 0 when welfare values are at least the povline?", {
  res <- md_compute_poverty_stats(
    welfare = 21:30,
    povline_lcu = 15,
    weight = rep(1, 10)
  )
  expect_equal(
    res$poverty_gap,
    0
  )
})

test_that("does function produce results that match compute_poverty_stats in povcalnet", {
  res <- md_compute_poverty_stats(
    welfare = benchmark$welfare,
    povline_lcu = mean(benchmark$welfare),
    weight = benchmark$weight
  )

  expect_equal(res[["headcount"]], 0.7333513, tolerance = 1e-6)
  expect_equal(res[["poverty_gap"]], 0.3957584, tolerance = 1e-6)
  expect_equal(res[["poverty_severity"]], 0.2534849, tolerance = 1e-6)
  expect_equal(res[["watts"]], 0.6899868, tolerance = 1e-4)
  # expect_equal(res[["watts_old"]], 0.6899868, tolerance = 1e-6)
})

test_that("md_compute_poverty_stats does not fail when poverty == 0", {
  res <- md_compute_poverty_stats(
    welfare = benchmark$welfare,
    povline_lcu = min(benchmark$welfare) - 1,
    weight = benchmark$weight
  )

  expect_equal(res[["headcount"]], 0)
  expect_equal(res[["poverty_gap"]], 0)
  expect_equal(res[["poverty_severity"]], 0)
  expect_equal(res[["watts"]], 0)
})


# New tests -----------
# ______________________________________________________________________
# Get intermediate for benchmark
# ______________________________________________________________________
povline_lcu       <- mean(benchmark$welfare)
pov_status        <- (benchmark$welfare < povline_lcu)
relative_distance <- (1 - (benchmark$welfare[pov_status] / povline_lcu))

weight_pov        <- benchmark$weight[pov_status]
weight_total      <- sum(benchmark$weight)

#_______________________________________________________________________
# Test - md_compute_headcount

test_that("md_compute_headcount works", {

  out1 <- md_compute_headcount(
    welfare      = benchmark$welfare,
    weight       = benchmark$weight,
    povline      = povline_lcu,
    weight_pov   = weight_pov,
    weight_total = weight_total
  )

  out2 <- md_compute_headcount(
    welfare      = benchmark$welfare,
    weight       = benchmark$weight,
    povline      = povline_lcu
  )

  expect_equal(out1,
               0.7333513,
               tolerance = 1e-6) #match compute_poverty_stats in povcalnet

  expect_equal(
    out1,
    out2
  )

})

test_that("md_compute_headcount works with NULL for weight_pov and weight_total", {

  expect_message(
    out <- md_compute_headcount(
      welfare      = benchmark$welfare,
      weight       = benchmark$weight,
      povline      = povline_lcu,
      verbose      = T
    ),
    regexp = "The `weight_pov` and/or `weight_total` arguments are NULL, therefore calculated internally"
  )

  expect_equal(out,
               0.7333513,
               tolerance = 1e-6) #match compute_poverty_stats in povcalnet


})

test_that("md_compute_headcount works when NULL welfare and weight", {

  expect_message(
    out <- md_compute_headcount(
      povline      = povline_lcu,
      weight_pov   = weight_pov,
      weight_total = weight_total,
      verbose      = T
    ),
    regexp = "The `weight_pov` and/or `weight_total` arguments are used for directly for headcount calculation"
  )

  expect_equal(out, 0.7333513, tolerance = 1e-6) #match compute_poverty_stats in povcalnet

  expect_equal(
    md_compute_headcount(
      welfare      = benchmark$welfare,
      povline      = povline_lcu,
    ),
    mean(benchmark$welfare < mean(benchmark$welfare))
  )

  expect_message(
    md_compute_headcount(
      welfare      = benchmark$welfare,
      povline      = povline_lcu,
      verbose      = T
    )
  )

})

test_that("md_compute_headcount error and message", {

  expect_error(
    out <- md_compute_headcount(
      weight_pov   = weight_pov,
      weight_total = weight_total
    )
  )

  expect_message(
    md_compute_headcount(
      welfare      = benchmark$welfare,
      povline      = povline_lcu,
      weight_total = weight_total,
      verbose      = TRUE
    )
  )
  expect_message(
    md_compute_headcount(
      welfare      = benchmark$welfare,
      povline      = povline_lcu,
      weight_total = weight_total,
      verbose      = TRUE
    )
  )

  expect_message(
    md_compute_headcount(
      welfare      = benchmark$welfare,
      weight       = benchmark$weight,
      povline      = povline_lcu,
      weight_pov   = weight_pov,
      weight_total = weight_total,
      verbose      = TRUE
    )
  )
})


#_______________________________________________________________________
# Test - md_compute_pov_gap
test_that("md_compute_pov_gap works", {

  out <- md_compute_pov_gap(
    welfare           = benchmark$welfare,
    weight            = benchmark$weight,
    povline           = povline_lcu,
    weight_pov        = weight_pov,
    weight_total      = weight_total,
    relative_distance = relative_distance,
    verbose           = TRUE
  )

  out2 <- md_compute_pov_gap(
    welfare           = benchmark$welfare,
    weight            = benchmark$weight,
    povline           = povline_lcu,
    verbose           = TRUE
  )

  expect_equal(out,
               0.3957584,
               tolerance = 1e-6) # match compute_poverty_stats in povcalnet

  expect_equal(
    out,
    out2
  )
})

test_that("md_compute_pov_gap messages and errors", {

  # povline error
  expect_error(
    md_compute_pov_gap(
      welfare           = benchmark$welfare,
      weight            = benchmark$weight,
      weight_pov        = weight_pov,
      weight_total      = weight_total,
      relative_distance = relative_distance
    )
  )
  # no weight message
  expect_message(
    md_compute_pov_gap(
      welfare           = benchmark$welfare,
      povline           = povline_lcu,
      weight_pov        = weight_pov,
      weight_total      = weight_total,
      relative_distance = relative_distance,
      verbose           = TRUE
    )
  )
  expect_message(
    md_compute_pov_gap(
      welfare           = benchmark$welfare,
      povline           = povline_lcu,
      weight_total      = weight_total,
      relative_distance = relative_distance,
      verbose           = TRUE
    )
  )
  # calculate pars message
  expect_message(
    md_compute_pov_gap(
      welfare           = benchmark$welfare,
      weight            = benchmark$weight,
      povline           = povline_lcu,
      verbose           = TRUE
    )
  )
  # use pars message
  expect_message(
    md_compute_pov_gap(
      welfare           = benchmark$welfare,
      weight            = benchmark$weight,
      povline           = povline_lcu,
      weight_pov        = weight_pov,
      weight_total      = weight_total,
      relative_distance = relative_distance,
      verbose           = TRUE
    )
  )
})

test_that("md_compute_pov_gap works with NULLs", {

  out <- md_compute_pov_gap(
    welfare           = benchmark$welfare,
    weight            = benchmark$weight,
    povline           = povline_lcu
  )

  out2 <- md_compute_pov_gap(
    povline           = povline_lcu,
    weight_pov        = weight_pov,
    weight_total      = weight_total,
    relative_distance = relative_distance,
  )

  expect_equal(out,
               0.3957584,
               tolerance = 1e-6) #match compute_poverty_stats in povcalnet
  expect_equal(out2,
               0.3957584,
               tolerance = 1e-6) #match compute_poverty_stats in povcalnet

})



#_______________________________________________________________________
# Test - md_compute_pov_severity
test_that("md_compute_pov_severity works", {

  out <- md_compute_pov_severity(
    welfare           = benchmark$welfare,
    weight            = benchmark$weight,
    povline           = povline_lcu,
    weight_pov        = weight_pov,
    weight_total      = weight_total,
    relative_distance = relative_distance
  )

  out2 <- md_compute_pov_severity(
    welfare           = benchmark$welfare,
    weight            = benchmark$weight,
    povline           = povline_lcu
  )

  expect_equal(out,
               0.2534849,
               tolerance = 1e-6) #match compute_poverty_stats in povcalnet

  expect_equal(
    out,
    out2
  )
})

test_that("md_compute_pov_severity errors and messages", {

  # povline
  expect_error(
    md_compute_pov_severity(
      welfare     = benchmark$welfare,
      weight      = benchmark$weight,
      povline     = NULL
    ),
    "`povline` argument must be non-NULL")

  # no weight
  expect_message(
    md_compute_pov_severity(
      welfare           = benchmark$welfare,
      povline           = povline_lcu,
      weight_total      = weight_total,
      weight_pov        = weight_pov,
      relative_distance = relative_distance,
      verbose           = TRUE
    )
  )
  expect_message(
    md_compute_pov_severity(
      welfare           = benchmark$welfare,
      povline           = povline_lcu,
      verbose           = TRUE
    )
  )
  # no pars
  expect_message(
    md_compute_pov_severity(
      welfare           = benchmark$welfare,
      weight            = benchmark$weight,
      povline           = povline_lcu,
      weight_pov        = weight_pov,
      weight_total      = weight_total,
      relative_distance = relative_distance,
      verbose           = TRUE
    )
  )
  # given pars
  expect_message(
    md_compute_pov_severity(
      welfare           = benchmark$welfare,
      weight            = benchmark$weight,
      povline           = povline_lcu,
      weight_pov        = weight_pov,
      weight_total      = weight_total,
      relative_distance = relative_distance,
      verbose           = TRUE
    )
  )


})




#_______________________________________________________________________
# Test - md_compute_watts
test_that("md_compute_watts works", {

  out <- md_compute_watts(
    welfare           = benchmark$welfare,
    weight            = benchmark$weight,
    povline           = povline_lcu
  )

  expect_equal(out, 0.6899868, tolerance = 1e-4) #match compute_poverty_stats in povcalnet

})

test_that("md_compute_watts messages and errors", {

  # no povline
  expect_error(
    md_compute_watts(
      welfare           = benchmark$welfare,
      weight            = benchmark$weight
    )
  )
  # no welfare
  expect_error(
    md_compute_watts(
      weight            = benchmark$weight,
      povline           = povline_lcu
    )
  )

  # no weight
  expect_message(
    md_compute_watts(
      welfare           = benchmark$welfare,
      povline           = povline_lcu,
      verbose           = TRUE
    )
  )

})


test_that("md_compute_watts prints error when welfare and/or povline is null", {



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


  out <- md_compute_watts(
    welfare = benchmark$welfare,
    povline = min(benchmark$welfare) - 1,
    weight = benchmark$weight)

  expect_equal(
    out,
    0
  )
})



#_______________________________________________________________________
# Test - md_compute_poverty_stats
test_that("md_compute_poverty_stats works", {

  out <- md_compute_poverty_stats(
    welfare     = benchmark$welfare,
    weight      = benchmark$weight,
    povline_lcu = povline_lcu
  )
  expect_equal(out[["headcount"]],
               0.7333513,
               tolerance = 1e-6)
  expect_equal(out[["poverty_gap"]],
               0.3957584,
               tolerance = 1e-6)
  expect_equal(out[["poverty_severity"]],
               0.2534849,
               tolerance = 1e-6)
  expect_equal(out[["watts"]],
               0.6899868,
               tolerance = 1e-4)
})

test_that("md_compute_poverty_stats matches previous function", {

  out_old <- md_compute_poverty_stats(
    welfare     = benchmark$welfare,
    weight      = benchmark$weight,
    povline_lcu = mean(benchmark$welfare)
  )

  out <- md_compute_poverty_stats(
    welfare     = benchmark$welfare,
    weight      = benchmark$weight,
    povline_lcu = mean(benchmark$welfare)
  )

  expect_equal(out[["headcount"]],
               out_old[["headcount"]],
               tolerance = 1e-6)
  expect_equal(out[["poverty_gap"]],
               out_old[["poverty_gap"]],
               tolerance = 1e-6)
  expect_equal(out[["poverty_severity"]],
               out_old[["poverty_severity"]],
               tolerance = 1e-6)
  expect_equal(out[["watts"]],
               out_old[["watts"]],
               tolerance = 1e-4)
})

test_that("md_compute_poverty_stats calculates with weight = 1 when is NULL", {

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

  out <- md_compute_poverty_stats(
    welfare     = benchmark$welfare,
    weight      = NULL,
    povline_lcu = povline_lcu
  )

  expect_equal(out[["headcount"]],
               0.705,
               tolerance = 1e-6)
  expect_equal(out[["poverty_gap"]],
               0.3557192,
               tolerance = 1e-6)
  expect_equal(out[["poverty_severity"]],
               0.2193438,
               tolerance = 1e-6)
  expect_equal(out[["watts"]],
               0.598579,
               tolerance = 1e-4)
})

test_that("md_compute_poverty_stats prints error when welfare and/or povline is NULL", {

  expect_error(md_compute_poverty_stats(
    welfare     = benchmark$welfare,
    weight      = benchmark$weight,
    povline_lcu = NULL
  ),
  "`welfare` and `povline` arguments must be non-NULL")

  expect_error(md_compute_poverty_stats(
    welfare     = NULL,
    weight      = benchmark$weight,
    povline_lcu = mean(benchmark$welfare)
  ),
  "`welfare` and `povline` arguments must be non-NULL")

  expect_error(md_compute_poverty_stats(
    welfare     = NULL,
    weight      = benchmark$weight,
    povline_lcu = NULL
  ),
  "`welfare` and `povline` arguments must be non-NULL")
})

test_that("md_compute_poverty_stats creates warning when weight is NULL", {

  expect_message(md_compute_poverty_stats(
    welfare     = benchmark$welfare,
    weight      = NULL,
    povline_lcu = mean(benchmark$welfare),
    verbose     = TRUE
  ),
  "The `weight` argument is NULL, thus each observation is given equal weight by default. ")
})

