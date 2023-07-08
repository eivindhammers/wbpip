# get_number_poor()

## Test 1
test_that("get_number_poor() returns expected results", {


  # check NA ------
  hc <- rep(c(0.1, 0.2), 5)
  pop <- sample(c(1:100), size = 10, replace = T)

  hc_na  <- sample(length(pop), 2)
  pop_na <- sample(length(pop), 2)

  hc[hc_na]   <- NA
  pop[pop_na] <- NA

  output <- get_number_poor(headcount = hc, pop = pop)

  expect_equal(is.na(output) |>
                 which() |>
                 sort(),
               c(hc_na, pop_na) |>
                 unique() |>
                 sort()
               )

  expect_equal(output,hc*pop)

  # Check negative ----
  hc <- rep(c(0.1, 0.2), 5)
  pop <- sample(c(1:100), size = 10, replace = T)

  hc_na  <- sample(length(pop), 2)
  pop_na <- sample(length(pop), 2)

  hc[hc_na]   <- -.5
  pop[pop_na] <- -2

  output <- get_number_poor(headcount = hc, pop = pop)

  expect_equal(is.na(output) |>
                 which() |>
                 sort(),
               c(hc_na, pop_na) |>
                 unique() |>
                 sort()
               )

  # errors ------

  hc <- "character"
  pop <- 100
  expect_error(get_number_poor(headcount = hc, pop = pop))

  # Check not ratio ------
  hc <- c(-0.1, 0.1, 0.2, 0.3, 0.4, 1, 4)
  hc_check <- c(NA, 0.1, 0.2, 0.3, 0.4, 1, NA)
  pop <- c(1:7)

  expect_identical(
    get_number_poor(headcount = hc, pop = pop),
    hc_check*pop
  )


})


## Test 1
test_that("get_average_shortfall() returns expected results", {


  # right values -----
  hc <- sample(c(1:100)/100, 20)
  pl <- sample(c(1:100), size = 20, replace = T)
  pg <- sample(c(1:100)/100, 20)
  output <- get_average_shortfall(headcount = hc, povgap = pg, povline = pl)
  expect_equal(output, pl*pg/hc)


  # negative values -----
  hc_na  <- sample(length(hc), 2)
  pl_na  <- sample(length(hc), 2)
  pg_na  <- sample(length(hc), 2)


  hc[hc_na]   <- -.5
  pl[pl_na]   <- -2
  pg[pg_na]   <- -8

  output <- get_average_shortfall(headcount = hc, povgap = pg, povline = pl)
  expect_equal(is.na(output) |>
                 which() |>
                 sort(),
               c(hc_na, pl_na, pg_na) |>
                 unique() |>
                 sort())

  # Check not ratio ------
  hc <- c(-0.1, 0.1, 0.2, 0.3, 0.4, 1, 4)
  hc_check <- c(NA, 0.1, 0.2, 0.3, 0.4, 1, NA)
  hc_correct <- rep(0.2, 7)
  pg_correct <- rep(0.1, 7)
  pg <- c(-0.1, 0.1, 0.2, 0.3, 0.4, 1, 4)
  pg_check <- c(NA, 0.1, 0.2, 0.3, 0.4, 1, NA)
  pl_1 <- rep(10, 7)
  pl_2 <- rep(-10, 7)
  pl_2_check <- rep(NA, 7)

  expect_identical( # check headcount
  get_average_shortfall(headcount = hc, povgap = pg_correct, pl_1),
   pl_1*pg_correct/hc_check
  )
  expect_identical( # check poverty gap
    get_average_shortfall(headcount = hc_correct, povgap = pg, pl_1),
    pl_1*pg_check/hc_correct
  )
  expect_identical( # check poverty line
    get_average_shortfall(headcount = hc_correct, povgap = pg_correct, pl_2),
    pl_2_check*pg_correct/hc_correct
  )





})

## Test 1
test_that("get_total_shortfall() returns expected results", {

  # right value -----
  hc <- 0.2
  pl <- 100
  pg <- 0.3
  pop <- 10
  output <- get_total_shortfall(headcount = hc, pop = pop, povgap = pg, povline = pl)
  expect_equal(output, (hc*pop)*pl*pg/hc)

  # negative values ----------
  hc  <- sample(c(1:100)/100, 20)
  pl  <- sample(c(1:100), size = 20, replace = T)
  pg  <- sample(c(1:100)/100, 20)
  pop <- sample(c(1:100), 20)



  hc_na  <- sample(length(hc), 2)
  pl_na  <- sample(length(hc), 2)
  pg_na  <- sample(length(hc), 2)
  pop_na <- sample(length(pop), 2)


  hc[hc_na]   <- -.5
  pl[pl_na]   <- -2
  pg[pg_na]   <- -8
  pop[pop_na] <- -2

  output <- get_total_shortfall(headcount = hc, pop = pop, povgap = pg, povline = pl)
  expect_equal(is.na(output) |>
                 which() |>
                 sort(),
               c(hc_na, pl_na, pg_na, pop_na) |>
                 unique() |>
                 sort())

  # errors ---------
  hc <- "0.2"
  pl <- 100
  pg <- 0.3
  pop <- 10
  expect_error(get_total_shortfall(headcount = hc, pop = pop, povgap = pg, povline = pl))


})


## Test 1
test_that("get_income_gap_ratio() returns expected results", {

  # expectation 1
  hc <- 0.2
  pg <- 0.3
  output <- get_income_gap_ratio(headcount = hc, povgap = pg)
  expect_equal(output, pg/hc)

  # expectation 2
  hc <- rep(c(0.1, 0.2), 5)
  pg <- rep(c(0.2), 10)
  output <- get_income_gap_ratio(headcount = hc, povgap = pg)
  expect_equal(output, pg/hc)


  # right values -----
  hc <- sample(c(1:100)/100, 20)
  pg <- sample(c(1:100)/100, 20)


  # negative values -----
  hc_na  <- sample(length(hc), 2)
  pg_na  <- sample(length(hc), 2)


  hc[hc_na]   <- -.5
  pg[pg_na]   <- -8

  output <- get_income_gap_ratio(headcount = hc, povgap = pg)
  expect_equal(is.na(output) |>
                 which() |>
                 sort(),
               c(hc_na, pg_na) |>
                 unique() |>
                 sort())

  # Check not ratio ------
  hc <- c(-0.1, 0.1, 0.2, 0.3, 0.4, 1, 4)
  hc_check <- c(NA, 0.1, 0.2, 0.3, 0.4, 1, NA)
  hc_correct <- rep(0.2, 7)
  pg_correct <- rep(0.1, 7)
  pg <- c(-0.1, 0.1, 0.2, 0.3, 0.4, 1, 4)
  pg_check <- c(NA, 0.1, 0.2, 0.3, 0.4, 1, NA)

  expect_identical( # check headcount
    get_income_gap_ratio(headcount = hc, povgap = pg_correct),
    pg_correct/hc_check
  )
  expect_identical( # check poverty gap
    get_income_gap_ratio(headcount = hc_correct, povgap = pg),
    pg_check/hc_correct
  )




})



# get_palma_ratio()

## Test 1
test_that("get_palma_ratio() returns expected results", {

  # expectation 1
  top10 <- 10
  bottom40 <- 4
  output <- get_palma_ratio(top10 = top10, bottom40 = bottom40)
  expect_equal(output, top10/bottom40)

  # expectation 2
  top10 <- rep(c(10, 20), 5)
  bottom40 <- rep(c(3), 10)
  output <- get_palma_ratio(top10 = top10, bottom40 = bottom40)
  expect_equal(output, top10/bottom40)

  # expectation 3
  top10 <- 10
  decile1 <- 1
  decile2 <- 1.5
  decile3 <- 2
  decile4 <- 3
  output <- get_palma_ratio(top10 = top10, decile1 = decile1,  decile2 = decile2,  decile3 = decile3,  decile4 = decile4)
  expect_equal(output, top10/(decile1 + decile2 + decile3 + decile4))

  # expectation 4
  top10 <- rep(c(10, 20), 5)
  decile1 <- rep(1, 10)
  decile2 <- rep(1.5, 10)
  decile3 <- rep(2, 10)
  decile4 <- rep(3, 10)
  output <- get_palma_ratio(top10 = top10, decile1 = decile1,  decile2 = decile2,  decile3 = decile3,  decile4 = decile4)
  expect_equal(output, top10/(decile1 + decile2 + decile3 + decile4))

  # Negatives ----
  d1 <- sample(c(1:100), 20)
  d2 <- sample(c(1:100), 20)
  d3 <- sample(c(1:100), 20)
  d4 <- sample(c(1:100), 20)
  b40 <- sample(c(1:100), 20)
  t10 <- sample(c(1:100), 20)

  d1_na  <- sample(length(t10), 2)
  d2_na  <- sample(length(t10), 2)
  d3_na  <- sample(length(t10), 2)
  d4_na  <- sample(length(t10), 2)
  b40_na  <- sample(length(t10), 2)
  t10_na  <- sample(length(t10), 2)

  d1[d1_na]    <- -5
  d2[d2_na]    <- -10
  d3[d3_na]    <- -15
  d4[d4_na]    <- -20
  b40[b40_na]  <- -30
  t10[t10_na]  <- -40

  output <- get_palma_ratio(top10 = t10, bottom40 = b40)
  expect_equal(is.na(output) |>
                 which() |>
                 sort(),
               c(t10_na, b40_na) |>
                 unique() |>
                 sort())

  output <- get_palma_ratio(top10 = t10, decile1 = d1,  decile2 = d2,  decile3 = d3,  decile4 = d4)
  expect_equal(is.na(output) |>
                 which() |>
                 sort(),
               c(t10_na, d1_na, d2_na, d3_na, d4_na) |>
                 unique() |>
                 sort())

})



# get_9010_ratio()

## Test 1
test_that("get_9010_ratio() returns expected results", {

  # expectation 1
  top10 <- 10
  bottom10 <- 4
  output <- get_9010_ratio(top10 = top10, bottom10 = bottom10)
  expect_equal(output, top10/bottom10)

  # expectation 2
  top10 <- rep(c(10, 20), 5)
  bottom10 <- rep(c(3), 10)
  output <- get_9010_ratio(top10 = top10, bottom10 = bottom10)
  expect_equal(output, top10/bottom10)

  # Negatives ----
  d1 <- sample(c(1:100), 20)
  t10 <- sample(c(1:100), 20)

  d1_na  <- sample(length(t10), 2)
  t10_na  <- sample(length(t10), 2)

  d1[d1_na]    <- -5
  t10[t10_na]  <- -40

  output <- get_9010_ratio(top10 = t10, bottom10 = d1)
  expect_equal(is.na(output) |>
                 which() |>
                 sort(),
               c(t10_na, d1_na) |>
                 unique() |>
                 sort())


})








