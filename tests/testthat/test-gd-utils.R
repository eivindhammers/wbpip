test_that("BETAICF() gives correct results", {

  expect_equal(BETAICF(1, 2, 3),  -0.125)
  expect_equal(BETAICF(0.1, 0.2, 0.3),  1.1014918, tolerance = 1e-7)
  expect_equal(BETAICF(-0.1, -0.2, -0.3),  1.0905613, tolerance = 1e-7)
  expect_equal(BETAICF(0.9352649, 2.060105, 0.3706313), 2.106916911)
  expect_equal(BETAICF(1.9440658, 0.9624106, 0.48681904), 1.92934409)
  expect_equal(BETAICF(0.97744306, 1.9339481, 0.25566411), 1.559899929)

})

test_that("gd_compute_watts_lb_test() gives correct results", {

  res <- gd_compute_watts_lb(
    headcount = 0.4,
    mean = 20,
    povline = 1.9,
    dd = 0.005,
    A = 0.2,
    B = 0.3,
    C = 0.4
  )
  expect_true(is.na(res))

  res <- gd_compute_watts_lb(
    headcount = 0.513180957,
    mean = 78.962,
    povline = 57.79166667,
    dd = 0.005,
    A = 0.7688156902,
    B = 0.9812052979,
    C = 0.4720329161
  )
  expect_equal(res, 0.2883967218)

})
