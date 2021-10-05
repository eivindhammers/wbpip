skip_if(Sys.getenv('WBPIP_RUN_LOCAL_TESTS') != "TRUE")

# Load microdatasets
ago2008 <- readRDS("../testdata/local/ago2008.RDS")
ago2018 <- readRDS("../testdata/local/ago2018.RDS")
bdi2013 <- readRDS("../testdata/local/bdi2013.RDS")
nga1996 <- readRDS("../testdata/local/nga1996.RDS")
nga2003 <- readRDS("../testdata/local/nga2003.RDS")
zwe2011 <- readRDS("../testdata/local/zwe2011.RDS")
zwe2017 <- readRDS("../testdata/local/zwe2017.RDS")
ukr1995 <- readRDS("../testdata/local/ukr1995.RDS")
# ukr1999 <- readRDS('../testdata/local/ukr1999.RDS')
ukr2002 <- readRDS("../testdata/local/ukr2002.RDS")

# Clean datasets
ago2008 <- md_clean_data(
  ago2008,
  welfare = "welfare",
  weight = "weight", quiet = TRUE
)$data
ago2018 <- md_clean_data(
  ago2018,
  welfare = "welfare",
  weight = "weight", quiet = TRUE
)$data
bdi2013 <- md_clean_data(
  bdi2013,
  welfare = "welfare",
  weight = "weight", quiet = TRUE
)$data
nga1996 <- md_clean_data(
  nga1996,
  welfare = "welfare",
  weight = "weight", quiet = TRUE
)$data
nga2003 <- md_clean_data(
  nga2003,
  welfare = "welfare",
  weight = "weight", quiet = TRUE
)$data
ukr1995 <- gd_clean_data(
  ukr1995,
  welfare = "welfare", population = "weight",
  gd_type = 5, quiet = TRUE
)
ukr2002 <- md_clean_data(
  ukr2002,
  welfare = "welfare",
  weight = "weight", quiet = TRUE
)$data
zwe2011 <- gd_clean_data(
  zwe2011,
  welfare = "welfare", population = "weight",
  gd_type = 5, quiet = TRUE
)
zwe2017 <- md_clean_data(
  zwe2017,
  welfare = "welfare",
  weight = "weight", quiet = TRUE
)$data

# ukr1999$welfare <- ukr1999$welfare * 365
# ukr1999 <- gd_clean_data(ukr1999, welfare = 'welfare', population = 'weight', data_type = 5)
# names(ukr1999)[1] <- 'weight'

# Tests
test_that("prod_fg_compute_pip_stats() works correctly on production microdata examples", {

  # predicted_request_mean <- 49.9499928133758
  predicted_request_mean <- 1.642192

  dist_stats <- md_compute_dist_stats(
    welfare = bdi2013$welfare,
    weight = bdi2013$weight
  )

  median_ppp <- dist_stats$median / (dist_stats$mean / predicted_request_mean)

  # Calculate poverty stats (one-point adjusted)

  # Extrapolate Burundi survey 2013.5 to 2015
  # True values retrieved with:
  # povcalnetR::povcalnet('BDI',  year = 2015, fill_gaps = TRUE)
  res <- prod_fg_compute_pip_stats(
    request_year = 2015,
    data = list(df0 = bdi2013),
    predicted_request_mean = predicted_request_mean,
    svy_mean_lcu = dist_stats$mean,
    svy_median_lcu = dist_stats$median,
    svy_median_ppp = median_ppp,
    survey_year = 2013.5,
    default_ppp = 496.1166,
    distribution_type = "micro",
    poverty_line = 1.9,
    ppp = NULL,
    popshare = NULL
  )
  expect_equal(res$mean, 49.9499928133757 * 12 / 365, tolerance = 1.5e-6)
  expect_equal(res$median, 37.09282 * 12 / 365, tolerance = 1.5e-6)
  expect_equal(res$headcount, 0.754573, tolerance = 1.5e-6)
  expect_equal(res$poverty_gap, 0.3385979, tolerance = 1.5e-6)
  expect_equal(res$poverty_severity, 0.18495, tolerance = 2e-6)
  expect_equal(res$watts, 0.5118873, tolerance = 1.5e-6)

  # Calculate poverty stats (interpolation, monotonic)
  # Interpolate AGO 2009 from AGO 2008.5 and 2018.17
  # True values retrieved with:
  # povcalnetR::povcalnet('AGO', year = 2009, fill_gaps = TRUE)
  # On 2021-10-05

  predicted_request_means <- c(3.54874439355, 3.54874439355)

  dist_stats0 <- md_compute_dist_stats(
    welfare    = ago2008$welfare,
    weight     = ago2008$weight
  )

  dist_stats1 <- md_compute_dist_stats(
    welfare    = ago2018$welfare,
    weight     = ago2018$weight
  )

  median_ppp0 <- dist_stats0$median / (dist_stats0$mean / predicted_request_means[1])
  median_ppp1 <- dist_stats1$median / (dist_stats1$mean / predicted_request_means[2])


  res <- prod_fg_compute_pip_stats(
    request_year = 2009,
    data = list(df0 = ago2008, df1 = ago2018),
    predicted_request_mean = predicted_request_means,
    svy_mean_lcu = c(dist_stats0$mean, dist_stats1$mean),
    svy_median_lcu = c(dist_stats0$median, dist_stats1$median),
    svy_median_ppp = c(median_ppp0, median_ppp1),
    survey_year = c(2008.5, 2018.17),
    default_ppp = c(80.931801, 80.931801), # c(1, 1)
    distribution_type = "micro",
    poverty_line = 1.9,
    ppp = NULL,
    popshare = NULL
  )
  expect_equal(res$mean, 107.940932826744 * 12 / 365, tolerance = 1.5e-6)
  expect_identical(res$median, NA_real_)
  expect_equal(res$headcount, 0.356819, tolerance = 1.5e-7)
  expect_equal(res$poverty_gap, 0.1246207, tolerance = 1.5e-6)
  expect_equal(res$poverty_severity, 0.0595037, tolerance = 2e-6)
  expect_equal(res$watts, 0.1771409, tolerance = 1.5e-6)

  # Calculate poverty stats (interpolation, non-monotonic)
  # Interpolate NGA 2003 from NGA 1996 and 2003.67
  # True values retrieved with:
  # povcalnetR::povcalnet('NGA', year = 2003, fill_gaps = TRUE)
  # On 2021-10-05

  predicted_request_means <- c(2.74332376482, 2.13976696463)

  dist_stats0 <- md_compute_dist_stats(
    welfare    = nga1996$welfare,
    weight     = nga1996$weight
  )

  dist_stats1 <- md_compute_dist_stats(
    welfare    = nga2003$welfare,
    weight     = nga2003$weight
  )

  median_ppp0 <- dist_stats0$median / (dist_stats0$mean / predicted_request_means[1])
  median_ppp1 <- dist_stats1$median / (dist_stats1$mean / predicted_request_means[2])

  res <- prod_fg_compute_pip_stats(
    request_year = 2003,
    data = list(df0 = nga1996, df1 = nga2003),
    predicted_request_mean = predicted_request_means,
    svy_mean_lcu = c(dist_stats0$mean, dist_stats1$mean),
    svy_median_lcu = c(dist_stats0$median, dist_stats1$median),
    svy_median_ppp = c(median_ppp0, median_ppp1),
    survey_year = c(1996.25, 2003.67),
    default_ppp = c(83.583336, 83.583336),
    distribution_type = "micro",
    poverty_line = 1.9,
    ppp = NULL,
    popshare = NULL
  )

  expect_equal(res$mean, 66.7347015760592 * 12 / 365, tolerance = 2e-4)
  expect_identical(res$median, NA_real_)
  expect_equal(res$headcount, 0.5779128, tolerance = 2e-4)
  expect_equal(res$poverty_gap, 0.2476595, tolerance = 2e-4)
  expect_equal(res$poverty_severity, 0.1372906, tolerance = 2e-4)
  expect_equal(res$watts, 0.3835661, tolerance = 2e-4)
})

test_that("prod_fg_compute_pip_stats() works correctly on production grouped data examples", {
  # Calculate poverty stats (one-point adjusted)

  # Extrapolate ZWE 2005 from ZWE 2011
  # True values retrieved with:
  # povcalnetR::povcalnet('ZWE', year = 2005, fill_gaps = TRUE)
  # On 2021-10-05

  predicted_request_mean <- 4.41750285232

  dist_stats <- gd_compute_dist_stats(
    welfare    = zwe2011$welfare,
    population = zwe2011$weight,
    mean       = predicted_request_mean
  )

  median_ppp <- dist_stats$median / (dist_stats$mean / predicted_request_mean)

  res <- prod_fg_compute_pip_stats(
    request_year = 2005,
    data = list(df0 = zwe2011),
    predicted_request_mean = predicted_request_mean,
    svy_mean_lcu = dist_stats$mean,
    svy_median_lcu = dist_stats$median,
    svy_median_ppp = median_ppp,
    survey_year = 2011,
    default_ppp = 0.535976308,
    distribution_type = "group",
    poverty_line = 1.9,
    ppp = NULL,
    popshare = NULL
  )
  expect_equal(res$mean, 134.365617305919 * 12 / 365, tolerance = 2e-6)
  expect_equal(res$median, 93.48877 * 12 / 365, tolerance = 2e-6)
  expect_equal(res$headcount, 0.2549557, tolerance = 2e-6)
  expect_equal(res$poverty_gap, 0.0678412, tolerance = 2e-6)
  expect_equal(res$poverty_severity, 0.02385209, tolerance = 5e-6)
  expect_equal(res$watts, 0.08848638, tolerance = 2e-5)

  # Calculate poverty stats (interpolation, monotonic)

  # Interpolate UKR 1997 from UKR 1996 (grouped) and 2002 (micro)
  # True values retrieved with:
  # povcalnetR::povcalnet('UKR', year = 1997, fill_gaps = TRUE)
  # res <- prod_fg_compute_pip_stats(
  #   request_year = 1997,
  #   data = list(df0 = ukr1995, df1 = ukr2002),
  #   predicted_request_mean = c(194.171236258708, 194.171236258708),
  #   survey_year = c(1995, 2002),
  #   default_ppp = 1,
  #   distribution_type = c('group', 'micro'),
  #   poverty_line = 1.9 * 365/12)
  # expect_equal(res$mean, 194.1712, tolerance = 1.5e-7)
  # expect_identical(res$median, NA)
  # expect_equal(res$poverty_gap, 0.01156786, tolerance = 1.5e-7)
  # expect_equal(res$poverty_severity, 0.004754342, tolerance = 1.5e-7)
  # expect_equal(res$watts, 0.0151301, tolerance = 1.5e-7)

  # Calculate poverty stats (interpolation, non-monotonic)

  # Interpolate ZWE 2013 from ZWE 2011 (grouped) and 2017 (micro)
  # True values retrieved with:
  # povcalnetR::povcalnet('ZWE', year = 2013, fill_gaps = TRUE)
  # On 2021-10-05

  predicted_request_means <- c(5.57405757622, 3.80027614070)

  dist_stats0 <- gd_compute_dist_stats(
    welfare    = zwe2011$welfare,
    population = zwe2011$weight,
    mean       = predicted_request_means[1]
  )

  dist_stats1 <- md_compute_dist_stats(
    welfare    = zwe2017$welfare,
    weight     = zwe2017$weight
  )

  median_ppp0 <- dist_stats0$median / (dist_stats0$mean / predicted_request_means[1])
  median_ppp1 <- dist_stats1$median / (dist_stats1$mean / predicted_request_means[2])

  res <- prod_fg_compute_pip_stats(
    request_year = 2013,
    data = list(df0 = zwe2011, df1 = zwe2017),
    predicted_request_mean = predicted_request_means,
    svy_mean_lcu = c(dist_stats0$mean, dist_stats1$mean),
    svy_median_lcu = c(dist_stats0$median, dist_stats1$median),
    svy_median_ppp = c(median_ppp0, median_ppp1),
    survey_year = c(2011, 2017),
    default_ppp = c(0.53541636, 0.53541636),
    distribution_type = c("group", "micro"),
    poverty_line = 1.9,
    ppp = NULL,
    popshare = NULL
  )
  expect_equal(res$mean, 151.558957176556 * 12 / 365, tolerance = 2e-5)
  expect_identical(res$median, NA_real_)
  expect_equal(res$headcount, 0.2247359, tolerance = 2e-4)
  expect_equal(res$poverty_gap, 0.05612637, tolerance = 2e-4)
  expect_equal(res$poverty_severity, 0.01933052, tolerance = 2e-4)
  expect_equal(res$watts, 0.06982316, tolerance = 2e-4)
})
