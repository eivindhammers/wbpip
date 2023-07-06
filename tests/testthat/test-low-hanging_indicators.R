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


})

## Test 2
test_that("get_income_gap_ratio() gives correct errors", {

  # expectation 1
  hc <- "0.2"
  pg <- 0.3
  expect_error(get_income_gap_ratio(headcount = hc, povgap = pg))

  # expectation 2
  hc <- c(0.1, 0.2)
  expect_error(get_income_gap_ratio(headcount = hc, povgap = pg))


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

})

## Test 2
test_that("get_income_gap_ratio() gives correct errors", {

  # expectation 1
  top10 <- "10"
  bottom40 <- 4
  expect_error(get_palma_ratio(top10 = top10, bottom40 = bottom40))

  # expectation 2
  top10 <- rep(c(10, 20), 1)
  bottom40 <- rep(3, 10)
  expect_error(get_palma_ratio(top10 = top10, bottom40 = bottom40))

  # expectation 3
  top10 <- "10"
  decile1 <- 1
  decile2 <- 1.5
  decile3 <- 2
  decile4 <- 3
  expect_error(get_palma_ratio(top10 = top10, decile1 = decile1,  decile2 = decile2,  decile3 = decile3,  decile4 = decile4))

  # expectation 4
  top10 <- rep(c(10, 20), 1)
  decile1 <- rep(1, 10)
  decile2 <- rep(1.5, 10)
  decile3 <- rep(2, 10)
  decile4 <- rep(3, 10)
  expect_error(get_palma_ratio(top10 = top10, decile1 = decile1,  decile2 = decile2,  decile3 = decile3,  decile4 = decile4))


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


})

## Test 2
test_that("get_9010_ratio() gives correct errors", {

  # expectation 1
  top10 <- "10"
  bottom10 <- 4
  expect_error(get_9010_ratio(top10 = top10, bottom10 = bottom10))

  # expectation 2
  top10 <- rep(c(10, 20), 1)
  bottom10 <- rep(3, 10)
  expect_error(get_9010_ratio(top10 = top10, bottom10 = bottom10))



})







