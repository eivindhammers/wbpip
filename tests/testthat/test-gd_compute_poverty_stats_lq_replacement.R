#_____________________________________________________
# define parameters ----------------------------------
mean      <- 51.5660557757944
povline   <- 57.791666666666664
A         <- 0.795981535745657
B         <- -1.4445933880119242
C         <- 0.14728191995919815
e         <- -0.498670067692931
m         <- -1.0970760862948583
n         <- 0.851623285340541
r         <- 1.3477796260474386
s1        <- -0.22612667749534146
s2        <- 1.002393060455814
headcount <- 0.76005810499191284
pov_gap   <- 0.27617606019159308

#______________________________________________________
# Tests -----------------------------------------------

test_that("gd_compute_headcount works as expected", {

  # expected headcount ----
  benchmark <- 0.76005810499191284

  # headcount function ----
  out <- gd_compute_headcount_lq(
    mean    = mean,
    povline = povline,
    B       = B,
    m       = m,
    n       = n,
    r       = r
  )

  expect_equal(round(out, 7),
               round(benchmark, 7))

})


test_that("gd_compute_pov_gap_lq works as expected", {

  # expected pov gap
  benchmark <- 0.27617606019159308

  # output
  out <- gd_compute_pov_gap_lq(
    mean      = mean,
    povline   = povline,
    headcount = headcount,
    A         = A,
    B         = B,
    C         = C
  )

  expect_equal(out,
               benchmark)

})

test_that("gd_compute_pov_gap_lq works as expected when headcount negative", {

  headcount_neg <- -0.76005810499191284
  benchmark     <- 0

  out <- gd_compute_pov_gap_lq(
    mean      = mean,
    povline   = povline,
    headcount = headcount_neg,
    A         = A,
    B         = B,
    C         = C
  )

  expect_equal(out,
               benchmark)

})

test_that("gd_compute_pov_severity_lq works as expected", {

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

  expect_equal(out,
               benchmark)

})

test_that("gd_compute_poverty_stats_lq_replacement works as expected", {

  benchmark <- list(
    headcount = 0.76005810499191284,
    pg        = 0.27617606019159308,
    p2        = 0.12832887439632906,
    eh        = -0.87181309219603054,
    epg       = -1.7520781651553494,
    ep        = -2.3041920454886071,
    gh        = -0.093916119652440649,
    gpg       = 0.70353220826204077,
    gp        = 1.5363306438390838,
    watts     = 0.39088363448720104,
    dl        = 1.1207307654300092,
    ddl       = 1.691340795153677
  )

  out <- gd_compute_poverty_stats_lq_replacement(
    mean    = mean,
    povline = povline,
    A       = A,
    B       = B,
    C       = C,
    e       = e,
    m       = m,
    n       = n,
    r       = r,
    s1      = s1,
    s2      = s2
  )

  # Expectations----
  expect_equal(length(out),
               length(benchmark))
  expect_equal(names(out),
               c("headcount", "pg", "p2", "eh", "epg",
                 "ep", "gh", "gpg", "gp", "watts", "dl", "ddl"))
  expect_equal(round(out$headcount, 7),
               round(benchmark$headcount, 7))
  expect_equal(out$pg,
               benchmark$pg)
  expect_equal(out$p2,
               benchmark$p2)
  expect_equal(round(out$eh, 6),
               round(benchmark$eh, 6)) # Due to headcount difference
  expect_equal(round(out$epg, 7),
               round(benchmark$epg, 7)) # Due to headcount difference
  expect_equal(out$ep,
               benchmark$ep)
  expect_equal(out$gh,
               benchmark$gh, tolerance = 1.1e-07)
  expect_equal(out$gpg,
               benchmark$gpg)
  expect_equal(out$gp,
               benchmark$gp)
  expect_equal(out$watts,
               benchmark$watts)
  expect_equal(round(out$dl, 7),
               round(benchmark$dl, 7))
  expect_equal(round(out$ddl, 6),
               round(benchmark$ddl, 6))
  expect_equal(out,
               benchmark,
               tolerance = 0.000001)
})

test_that("gd_compute_poverty_stats_lq_replacement works as gd_compute_poverty_stats_lq", {

  benchmark <- gd_compute_poverty_stats_lq(
    mean    = mean,
    povline = povline,
    A       = A,
    B       = B,
    C       = C,
    e       = e,
    m       = m,
    n       = n,
    r       = r,
    s1      = s1,
    s2      = s2)

  out <- gd_compute_poverty_stats_lq_replacement(
    mean    = mean,
    povline = povline,
    A       = A,
    B       = B,
    C       = C,
    e       = e,
    m       = m,
    n       = n,
    r       = r,
    s1      = s1,
    s2      = s2
  )

  expect_equal(length(out), length(benchmark))
  expect_equal(names(out),
               c("headcount", "pg", "p2", "eh", "epg",
                 "ep", "gh", "gpg", "gp", "watts", "dl", "ddl"))
  expect_equal(round(out$headcount, 7),
               round(benchmark$headcount, 7))
  expect_equal(out$pg,
               benchmark$pg)
  expect_equal(out$p2,
               benchmark$p2)
  expect_equal(round(out$eh, 6),
               round(benchmark$eh, 6)) # Due to headcount difference
  expect_equal(round(out$epg, 7),
               round(benchmark$epg, 7)) # Due to headcount difference
  expect_equal(out$ep,
               benchmark$ep)
  expect_equal(out$gh,
               benchmark$gh,
               tolerance = 1.1e-07)
  expect_equal(out$gpg,
               benchmark$gpg)
  expect_equal(out$gp,
               benchmark$gp)
  expect_equal(out$watts,
               benchmark$watts)
  expect_equal(round(out$dl, 7),
               round(benchmark$dl, 7))
  expect_equal(round(out$ddl, 6),
               round(benchmark$ddl, 6))
  expect_equal(out,
               benchmark,
               tolerance = 0.000001)
})

test_that("gd_compute_poverty_stats_lq_replacement works with negative headcount", {

  povline_neg <- -57.791666666666664 # Negative to test negative headcount

  benchmark <- list(
    headcount = 0,
    pg        = 0,
    p2        = 0,
    eh        = 0,
    epg       = 0,
    ep        = 0,
    gh        = 0,
    gpg       = 0,
    gp        = 0,
    watts     = 0,
    dl        = -1.120731,
    ddl       = 41.74948
  )

  out <- gd_compute_poverty_stats_lq_replacement(
    mean    = mean,
    povline = povline_neg,
    A       = A,
    B       = B,
    C       = C,
    e       = e,
    m       = m,
    n       = n,
    r       = r,
    s1      = s1,
    s2      = s2
  )

  out_original <- gd_compute_poverty_stats_lq(
    mean    = mean,
    povline = povline_neg,
    A       = A,
    B       = B,
    C       = C,
    e       = e,
    m       = m,
    n       = n,
    r       = r,
    s1      = s1,
    s2      = s2
  )


  expect_equal(length(out), length(benchmark))
  expect_equal(names(out),
               c("headcount", "pg", "p2", "eh", "epg",
                 "ep", "gh", "gpg", "gp", "watts", "dl", "ddl"))
  expect_equal(round(out$headcount, 7),
               round(benchmark$headcount, 7))
  expect_equal(out$pg,
               benchmark$pg)
  expect_equal(out$p2,
               benchmark$p2)
  expect_equal(round(out$eh, 6),
               round(benchmark$eh, 6)) # Due to headcount difference
  expect_equal(round(out$epg, 7),
               round(benchmark$epg, 7)) # Due to headcount difference
  expect_equal(out$ep,
               benchmark$ep)
  expect_equal(out$gh,
               benchmark$gh, tolerance = 1.1e-07)
  expect_equal(out$gpg,
               benchmark$gpg)
  expect_equal(out$gp,
               benchmark$gp)
  expect_equal(out$watts,
               benchmark$watts)
  expect_equal(round(out$dl, 5),
               round(benchmark$dl, 5))
  expect_equal(round(out$ddl, 5),
               round(benchmark$ddl, 5))
  expect_equal(out,
               benchmark,
               tolerance = 0.000001)
  expect_equal(out,
               out_original)
})

