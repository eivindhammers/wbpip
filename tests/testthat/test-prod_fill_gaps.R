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
    svy_median_lcu = 450.2,
    svy_median_ppp = 4.49,
    ppp = NULL,
    popshare = NULL
  )
  expect_identical(
    names(res),
    c(
      "poverty_line", "mean", "median",
      "headcount", "poverty_gap", "poverty_severity", "watts"
    )
  )
})

# Extrapolation
test_that("prod_fg_compute_pip_stats() extrapolates correctly for microdata", {

  predicted_request_mean <- 6

  dist_stats <- md_compute_dist_stats(
    welfare = md_DEF_2000_consumption$welfare,
    weight = md_DEF_2000_consumption$weight
  )

  median_ppp <- dist_stats$median / (dist_stats$mean / predicted_request_mean)

  res <- prod_fg_compute_pip_stats(
    request_year = 2005,
    survey_year = c(2000),
    data = list(df0 = md_DEF_2000_consumption),
    predicted_request_mean = predicted_request_mean,
    default_ppp = 1,
    distribution_type = "micro",
    poverty_line = 1.9,
    svy_mean_lcu = dist_stats$mean,
    svy_median_lcu = dist_stats$median,
    svy_median_ppp = median_ppp,
    ppp = NULL,
    popshare = NULL
  )
  expect_equal(res$poverty_line, 1.9)
  expect_equal(res$mean, 6, tolerance = 1.5e-7)
  expect_equal(res$median, 4.726458, tolerance = 1.5e-7)
  expect_equal(res$headcount, 0.005424768, tolerance = 1.5e-7)
  expect_equal(res$poverty_gap, 0.0010347565, tolerance = 1.5e-7)
  expect_equal(res$poverty_severity, 0.0002663465, tolerance = 1.5e-7)
  expect_equal(res$watts, 0.001201002, tolerance = 1.5e-7)
})

# Extrapolation
test_that("prod_fg_compute_pip_stats() extrapolates correctly for grouped data", {

  predicted_request_mean <- 6

  dist_stats <- gd_compute_dist_stats(
    welfare    = gd_GHI_2009_income$welfare,
    population = gd_GHI_2009_income$weight,
    mean = predicted_request_mean
  )

  median_ppp <- dist_stats$median / (dist_stats$mean / predicted_request_mean)

  res <- prod_fg_compute_pip_stats(
    request_year = 2005,
    survey_year = 2009,
    data = list(df0 = gd_GHI_2009_income),
    predicted_request_mean = predicted_request_mean,
    default_ppp = 1,
    distribution_type = "group",
    poverty_line = 1.9,
    svy_mean_lcu = 5,
    svy_median_lcu = dist_stats$median,
    svy_median_ppp = median_ppp,
    ppp = NULL,
    popshare = NULL
  )
  expect_equal(res$poverty_line, 1.9)
  expect_equal(res$mean, 6, tolerance = 1.5e-7)
  expect_equal(res$median, 4.231318, tolerance = 1.5e-7)
  expect_equal(res$headcount, 0.12776, tolerance = 1.5e-7)
  expect_equal(res$poverty_gap, 0.02657251, tolerance = 1.5e-7)
  expect_equal(res$poverty_severity, 0.007863721, tolerance = 1.5e-7)
  expect_equal(res$watts, 0.0318211, tolerance = 1.5e-7)
})

# Monotonic interpolation
test_that("prod_fg_compute_pip_stats() interpolates correctly (monotonic) for microdata", {

  predicted_request_means <- c(13, 13)

  dist_stats0 <- md_compute_dist_stats(
    welfare    = md_ABC_2000_income$welfare,
    weight     = md_ABC_2000_income$weight
  )

  dist_stats1 <- md_compute_dist_stats(
    welfare    = md_ABC_2010_income$welfare,
    weight     = md_ABC_2010_income$weight
  )

  median_ppp0 <- dist_stats0$median / (dist_stats0$mean / predicted_request_means[1])
  median_ppp1 <- dist_stats1$median / (dist_stats1$mean / predicted_request_means[2])



  res <- prod_fg_compute_pip_stats(
    request_year = 2005,
    survey_year = c(2000, 2010),
    data = list(df0 = md_ABC_2000_income, df1 = md_ABC_2010_income),
    predicted_request_mean = predicted_request_means,
    default_ppp = c(1, 1),
    distribution_type = "micro",
    poverty_line = 1.9,
    svy_mean_lcu = c(dist_stats0$mean, dist_stats1$mean),
    svy_median_lcu = c(dist_stats0$median, dist_stats1$median),
    svy_median_ppp = c(median_ppp0, median_ppp1),
    ppp = NULL,
    popshare = NULL
  )
  expect_equal(res$poverty_line, 1.9)
  expect_equal(res$mean, 13, tolerance = 1.5e-7)
  expect_identical(res$median, NA_real_)
  expect_equal(res$headcount, 0.0459062, tolerance = 1.5e-7)
  expect_equal(res$poverty_gap, 0.0161475, tolerance = 1.5e-7)
  expect_equal(res$poverty_severity, 0.008425631, tolerance = 1.5e-7)
  expect_equal(res$watts, 0.02101141, tolerance = 1.5e-7)
})

# Monotonic interpolation
test_that("prod_fg_compute_pip_stats() interpolates correctly (monotonic) for micro vs grouped data", {

  predicted_request_means <- c(6, 6)

  dist_stats0 <- md_compute_dist_stats(
    welfare    = md_GHI_2000_consumption$welfare,
    weight     = md_GHI_2000_consumption$weight,
    mean       = predicted_request_means[1]
  )

  dist_stats1 <- gd_compute_dist_stats(
    welfare    = gd_GHI_2009_income$welfare,
    population = gd_GHI_2009_income$weight,
    mean       = predicted_request_means[2]
  )

  median_ppp0 <- dist_stats0$median / (dist_stats0$mean / predicted_request_means[1])
  median_ppp1 <- dist_stats1$median / (dist_stats1$mean / predicted_request_means[2])



  res <- prod_fg_compute_pip_stats(
    request_year = 2005,
    survey_year = c(2000, 2009),
    data = list(df0 = md_GHI_2000_consumption, df1 = gd_GHI_2009_income),
    predicted_request_mean = predicted_request_means,
    default_ppp = 1,
    distribution_type = c("micro", "group"),
    poverty_line = 1.9,
    svy_mean_lcu = c(7769.661, 7769.661),
    svy_median_lcu = c(dist_stats0$median, dist_stats1$median),
    svy_median_ppp = c(median_ppp0, median_ppp1),
    ppp = NULL,
    popshare = NULL
  )
  expect_equal(res$poverty_line, 1.9)
  expect_equal(res$mean, 6, tolerance = 1.5e-7)
  expect_identical(res$median, NA_real_)
  expect_equal(res$headcount, 0.09451118, tolerance = 1.5e-7)
  expect_equal(res$poverty_gap, 0.018744685, tolerance = 1.5e-7)
  expect_equal(res$poverty_severity, 0.005512714, tolerance = 1.5e-7)
  expect_equal(res$watts, 0.022445304, tolerance = 1.5e-7)
})

# Non-monotonic interpolation
test_that("prod_fg_compute_pip_stats() interpolates correctly (non-monotonic) for microdata", {

  predicted_request_means <- c(14, 17)

  dist_stats0 <- md_compute_dist_stats(
    welfare    = md_ABC_2000_income$welfare,
    weight     = md_ABC_2000_income$weight,
    mean       = predicted_request_means[1]
  )

  dist_stats1 <- md_compute_dist_stats(
    welfare    = md_ABC_2010_income$welfare,
    weight     = md_ABC_2010_income$weight,
    mean       = predicted_request_means[2]
  )

  median_ppp0 <- dist_stats0$median / (dist_stats0$mean / predicted_request_means[1])
  median_ppp1 <- dist_stats1$median / (dist_stats1$mean / predicted_request_means[2])



  res <- prod_fg_compute_pip_stats(
    request_year = 2005,
    survey_year = c(2000, 2010),
    data = list(df0 = md_ABC_2000_income, df1 = md_ABC_2010_income),
    predicted_request_mean = predicted_request_means,
    default_ppp = c(1, 1),
    distribution_type = "micro",
    poverty_line = 1.9,
    svy_mean_lcu = c(3436146, 7186782),
    svy_median_lcu = c(dist_stats0$median, dist_stats1$median),
    svy_median_ppp = c(median_ppp0, median_ppp1),
    ppp = NULL,
    popshare = NULL
  )
  expect_equal(res$poverty_line, 1.9)
  expect_equal(res$mean, 15.5, tolerance = 1.5e-7)
  expect_identical(res$median, NA_real_)
  expect_equal(res$headcount, 0.03680496, tolerance = 1.5e-7)
  expect_equal(res$poverty_gap, 0.01232436, tolerance = 1.5e-7)
  expect_equal(res$poverty_severity, 0.006587024, tolerance = 1.5e-7)
  expect_equal(res$watts, 0.01543411, tolerance = 1.5e-6)
})

# Non-monotonic interpolation
test_that("prod_fg_compute_pip_stats() interpolates correctly (non-monotonic) for micro vs grouped data", {

  predicted_request_means <- c(4, 6)

  dist_stats0 <- md_compute_dist_stats(
    welfare    = md_GHI_2000_consumption$welfare,
    weight     = md_GHI_2000_consumption$weight,
    mean       = predicted_request_means[1]
  )

  dist_stats1 <- gd_compute_dist_stats(
    welfare    = gd_GHI_2009_income$welfare,
    population = gd_GHI_2009_income$weight,
    mean       = predicted_request_means[2]
  )

  median_ppp0 <- dist_stats0$median / (dist_stats0$mean / predicted_request_means[1])
  median_ppp1 <- dist_stats1$median / (dist_stats1$mean / predicted_request_means[2])


  res <- prod_fg_compute_pip_stats(
    request_year = 2005,
    survey_year = c(2000, 2009),
    data = list(df0 = md_GHI_2000_consumption, df1 = gd_GHI_2009_income),
    predicted_request_mean = predicted_request_means,
    default_ppp = c(1, 1),
    distribution_type = c("micro", "group"),
    poverty_line = 1.9,
    svy_mean_lcu = c(7769.661, 9000),
    svy_median_lcu = c(dist_stats0$median, dist_stats1$median),
    svy_median_ppp = c(median_ppp0, median_ppp1),
    ppp = NULL,
    popshare = NULL
  )

  expect_equal(res$poverty_line, 1.9)
  expect_equal(res$mean, 5.111111, tolerance = 1.5e-7)
  expect_identical(res$median, NA_real_)
  expect_equal(res$headcount, 0.1480512, tolerance = 1.5e-7)
  expect_equal(res$poverty_gap, 0.03320061, tolerance = 1.5e-7)
  expect_equal(res$poverty_severity, 0.010884384, tolerance = 1.5e-7)
  expect_equal(res$watts, 0.04089535, tolerance = 1.5e-7)
})
