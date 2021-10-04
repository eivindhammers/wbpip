# Lo_ad datasets
data("md_ABC_2000_income")
data("md_ABC_2010_income")
data("md_DEF_2000_consumption")
data("md_GHI_2000_income")
data("gd_GHI_2009_income")
data("md_GHI_2000_consumption")

# Clean datasets
md_ABC_2000_income <-
  md_clean_data(md_ABC_2000_income,
                welfare = "welfare",
                weight = "weight",
                quiet = TRUE
  )$data
md_ABC_2010_income <-
  md_clean_data(md_ABC_2010_income,
                welfare = "welfare",
                weight = "weight",
                quiet = TRUE
  )$data
md_DEF_2000_consumption <-
  md_clean_data(md_DEF_2000_consumption,
                welfare = "welfare",
                weight = "weight",
                quiet = TRUE
  )$data
md_GHI_2000_income <-
  md_clean_data(md_GHI_2000_income,
                welfare = "welfare",
                weight = "weight",
                quiet = TRUE
  )$data
md_GHI_2000_income <-
  md_clean_data(md_GHI_2000_consumption,
                welfare = "welfare",
                weight = "weight",
                quiet = TRUE
  )$data

gd_GHI_2009_income <-
  gd_clean_data(gd_GHI_2009_income,
                welfare = "welfare",
                population = "weight",
                gd_type = 5,
                quiet = TRUE
  )



# Output format (named list)
test_that("prod_fg_compute_pip_stats() returns the correct output format", {
  # Test that prod_fg_compute_pip_stats() returns a list with all poverty stats
  df <- data.frame(welfare = 1:1000, weight = rep(1, 1000))
  res <- prod_fg_compute_pip_stats(
    request_year = 2005,
    survey_year = 2000,
    data = list(df0 = df),
    default_ppp = 1,
    predicted_request_mean = 5,
    distribution_type = "micro",
    poverty_line = 1.9,
    svy_mean_lcu = 500.5,
    ppp = NULL,
    popshare = NULL
  )
  expect_identical(
    names(res),
    c(
      "poverty_line", "mean", # "median",
      "headcount", "poverty_gap", "poverty_severity", "watts"
    )
  )
})

# Extrapolation
test_that("prod_fg_compute_pip_stats() extrapolates correctly for microdata", {
  res <- prod_fg_compute_pip_stats(
    request_year = 2005,
    survey_year = c(2000),
    data = list(df0 = md_DEF_2000_consumption),
    predicted_request_mean = 6,
    default_ppp = 1,
    distribution_type = "micro",
    poverty_line = 1.9,
    svy_mean_lcu = 17322.98,
    ppp = NULL,
    popshare = NULL
  )
  expect_equal(res$poverty_line, 1.9)
  expect_equal(res$mean, 6, tolerance = 1.5e-7)
  # expect_equal(res$median, 4.726458, tolerance = 1.5e-7)
  expect_equal(res$headcount, 0.005424768, tolerance = 1.5e-7)
  expect_equal(res$poverty_gap, 0.0010347565, tolerance = 1.5e-7)
  expect_equal(res$poverty_severity, 0.0002663465, tolerance = 1.5e-7)
  expect_equal(res$watts, 0.001201002, tolerance = 1.5e-7)
})

# Extrapolation
test_that("prod_fg_compute_pip_stats() extrapolates correctly for grouped data", {
  res <- prod_fg_compute_pip_stats(
    request_year = 2005,
    survey_year = 2009,
    data = list(df0 = gd_GHI_2009_income),
    predicted_request_mean = 6,
    default_ppp = 1,
    distribution_type = "group",
    poverty_line = 1.9,
    svy_mean_lcu = 5,
    ppp = NULL,
    popshare = NULL
  )
  expect_equal(res$poverty_line, 1.9)
  expect_equal(res$mean, 6, tolerance = 1.5e-7)
  # expect_equal(res$median, 4.231318, tolerance = 1.5e-7)
  expect_equal(res$headcount, 0.12776, tolerance = 1.5e-7)
  expect_equal(res$poverty_gap, 0.02657251, tolerance = 1.5e-7)
  expect_equal(res$poverty_severity, 0.007863721, tolerance = 1.5e-7)
  expect_equal(res$watts, 0.0318211, tolerance = 1.5e-7)
})

# Monotonic interpolation
test_that("prod_fg_compute_pip_stats() interpolates correctly (monotonic) for microdata", {
  res <- prod_fg_compute_pip_stats(
    request_year = 2005,
    survey_year = c(2000, 2010),
    data = list(df0 = md_ABC_2000_income, df1 = md_ABC_2010_income),
    predicted_request_mean = c(13, 13),
    default_ppp = c(1, 1),
    distribution_type = "micro",
    poverty_line = 1.9,
    svy_mean_lcu = c(3436146, 7186782),
    ppp = NULL,
    popshare = NULL
  )
  expect_equal(res$poverty_line, 1.9)
  expect_equal(res$mean, 13, tolerance = 1.5e-7)
  # expect_identical(res$median, NA)
  expect_equal(res$headcount, 0.0459062, tolerance = 1.5e-7)
  expect_equal(res$poverty_gap, 0.0161475, tolerance = 1.5e-7)
  expect_equal(res$poverty_severity, 0.008425631, tolerance = 1.5e-7)
  expect_equal(res$watts, 0.02101141, tolerance = 1.5e-7)
})

# Monotonic interpolation
test_that("prod_fg_compute_pip_stats() interpolates correctly (monotonic) for micro vs grouped data", {
  res <- prod_fg_compute_pip_stats(
    request_year = 2005,
    survey_year = c(2000, 2009),
    data = list(df0 = md_GHI_2000_consumption, df1 = gd_GHI_2009_income),
    predicted_request_mean = c(6, 6),
    default_ppp = 1,
    distribution_type = c("micro", "group"),
    poverty_line = 1.9,
    svy_mean_lcu = c(7769.661, 7769.661),
    ppp = NULL,
    popshare = NULL
  )
  expect_equal(res$poverty_line, 1.9)
  expect_equal(res$mean, 6, tolerance = 1.5e-7)
  # expect_identical(res$median, NA)
  expect_equal(res$headcount, 0.09451118, tolerance = 1.5e-7)
  expect_equal(res$poverty_gap, 0.018744685, tolerance = 1.5e-7)
  expect_equal(res$poverty_severity, 0.005512714, tolerance = 1.5e-7)
  expect_equal(res$watts, 0.022445304, tolerance = 1.5e-7)
})

# Non-monotonic interpolation
test_that("prod_fg_compute_pip_stats() interpolates correctly (non-monotonic) for microdata", {
  res <- prod_fg_compute_pip_stats(
    request_year = 2005,
    survey_year = c(2000, 2010),
    data = list(df0 = md_ABC_2000_income, df1 = md_ABC_2010_income),
    predicted_request_mean = c(14, 17),
    default_ppp = c(1, 1),
    distribution_type = "micro",
    poverty_line = 1.9,
    svy_mean_lcu = c(3436146, 7186782),
    ppp = NULL,
    popshare = NULL
  )
  expect_equal(res$poverty_line, 1.9)
  expect_equal(res$mean, 15.5, tolerance = 1.5e-7)
  # expect_identical(res$median, NA)
  expect_equal(res$headcount, 0.03680496, tolerance = 1.5e-7)
  expect_equal(res$poverty_gap, 0.01232436, tolerance = 1.5e-7)
  expect_equal(res$poverty_severity, 0.006587024, tolerance = 1.5e-7)
  expect_equal(res$watts, 0.01543411, tolerance = 1.5e-7)
})

# Non-monotonic interpolation
test_that("prod_fg_compute_pip_stats() interpolates correctly (non-monotonic) for micro vs grouped data", {
  res <- prod_fg_compute_pip_stats(
    request_year = 2005,
    survey_year = c(2000, 2009),
    data = list(df0 = md_GHI_2000_consumption, df1 = gd_GHI_2009_income),
    predicted_request_mean = c(4, 6),
    default_ppp = c(1, 1),
    distribution_type = c("micro", "group"),
    poverty_line = 1.9,
    svy_mean_lcu = c(7769.661, 9000),
    ppp = NULL,
    popshare = NULL
  )
  expect_equal(res$poverty_line, 1.9)
  expect_equal(res$mean, 5.111111, tolerance = 1.5e-7)
  # expect_identical(res$median, NA)
  expect_equal(res$headcount, 0.1480512, tolerance = 1.5e-7)
  expect_equal(res$poverty_gap, 0.03320061, tolerance = 1.5e-7)
  expect_equal(res$poverty_severity, 0.010884384, tolerance = 1.5e-7)
  expect_equal(res$watts, 0.04089535, tolerance = 1.5e-7)
})
