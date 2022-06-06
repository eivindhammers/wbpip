gd_ex2 <- readRDS('tests/testdata/gd_ex2.RDS')

testthat::test_that("test sd_create_synth_vector", {
  res <- wbpip:::sd_create_synth_vector(
    welfare = gd_ex2$welfare,
    population = gd_ex2$weight,
    mean = 8,
    pop = NULL,
    p0 = 0.5,
    nobs = 1e5)

  testthat::expect_equal(dim(res), c(100000, 2))
  testthat::expect_equal(tail(res$weight, 1), 1)
})

