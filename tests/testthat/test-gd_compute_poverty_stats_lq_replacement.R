test_that("gd_compute_headcount works as expected", {
  mean <- 51.5660557757944
  povline <- 57.791666666666664
  A <- 0.795981535745657
  B <- -1.4445933880119242
  C <- 0.14728191995919815
  e <- -0.498670067692931
  m <- -1.0970760862948583
  n <- 0.851623285340541
  r <- 1.3477796260474386
  s1 <- -0.22612667749534146
  s2 <- 1.002393060455814


  benchmark <- 0.76005810499191284

  out <- gd_compute_headcount_lq(
    mean    = mean,
    povline = povline,
    B       = B,
    m       = m,
    n       = n,
    r       = r
  )

  expect_equal(round(out, 7), round(benchmark, 7))

})


test_that("gd_compute_pov_gap_lq works as expected", {
  mean <- 51.5660557757944
  povline <- 57.791666666666664
  A <- 0.795981535745657
  B <- -1.4445933880119242
  C <- 0.14728191995919815
  e <- -0.498670067692931
  m <- -1.0970760862948583
  n <- 0.851623285340541
  r <- 1.3477796260474386
  s1 <- -0.22612667749534146
  s2 <- 1.002393060455814
  headcount <- 0.76005810499191284

  benchmark <- 0.27617606019159308

  out <- gd_compute_pov_gap_lq(
    mean      = mean,
    povline   = povline,
    headcount = headcount,
    A         = A,
    B         = B,
    C         = C
  )

  expect_equal(out, benchmark)

})

test_that("gd_compute_pov_severity_lq works as expected", {
  mean <- 51.5660557757944
  povline <- 57.791666666666664
  A <- 0.795981535745657
  B <- -1.4445933880119242
  C <- 0.14728191995919815
  e <- -0.498670067692931
  m <- -1.0970760862948583
  n <- 0.851623285340541
  r <- 1.3477796260474386
  s1 <- -0.22612667749534146
  s2 <- 1.002393060455814

  headcount <- 0.76005810499191284

  pov_gap <- 0.27617606019159308

  benchmark <- 0.12832887439632906

  out <- gd_compute_pov_severity_lq(
    mean      = mean,
    povline   = povline,
    headcount = headcount,
    pov_gap   = pov_gap,
    A         = A,
    B         = B,
    C         = C,
    e         = e,
    m         = m,
    n         = n,
    r         = r,
    s1        = s1,
    s2        = s2
  )

  expect_equal(out, benchmark)

})

