test_that("compute_spl() returns expected results", {
  weighted_median_ppp <- 100
  ppp_year <- 2017

  out <- compute_spl(weighted_median_ppp = weighted_median_ppp,
                     ppp_year = ppp_year)

  expect_equal(out, 51.15)

  weighted_median_ppp <- 100
  ppp_year <- 2011

  out <- compute_spl(weighted_median_ppp = weighted_median_ppp,
                     ppp_year = ppp_year)

  expect_equal(out, 51.0)

})

test_that("compute_spl() has a lower bound", {
  weighted_median_ppp <- -100
  ppp_year <- 2017

  out <- compute_spl(weighted_median_ppp = weighted_median_ppp,
                     ppp_year = ppp_year)

  expect_equal(out, 2.15)

  weighted_median_ppp <- -100
  ppp_year <- 2011

  out <- compute_spl(weighted_median_ppp = weighted_median_ppp,
                     ppp_year = ppp_year)

  expect_equal(out, 1.9)

})

test_that("compute_spl() only accepts valid PPP years", {
  weighted_median_ppp <- -100
  ppp_year <- 2012

  expect_error(compute_spl(weighted_median_ppp = weighted_median_ppp,
                           ppp_year = ppp_year))
})
