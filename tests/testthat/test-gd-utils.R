test_that("BETAICF() gives correct results", {

  expect_equal(BETAICF(1, 2, 3),  -0.125)
  expect_equal(BETAICF(0.1, 0.2, 0.3),  1.1014918, tolerance = 1e-7)
  expect_equal(BETAICF(-0.1, -0.2, -0.3),  1.0905613, tolerance = 1e-7)
  expect_equal(BETAICF(0.9352649, 2.060105, 0.3706313), 2.106916911)
  expect_equal(BETAICF(1.9440658, 0.9624106, 0.48681904), 1.92934409)
  expect_equal(BETAICF(0.97744306, 1.9339481, 0.25566411), 1.559899929)

})
