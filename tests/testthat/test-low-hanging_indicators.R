# get_number_poor()

## Test 1
test_that("get_number_poor() returns expected results", {

  # expectation 1
  hc <- 0.2
  pop <- 100
  output <- get_number_poor(headcount = hc, pop = pop)
  expect_equal(output, hc*pop)

  # expectation 2
  hc <- rep(c(0.1, 0.2), 5)
  pop <- sample(c(1:100), size = 10, replace = T)
  output <- get_number_poor(headcount = hc, pop = pop)
  expect_equal(output, hc*pop)


})

## Test 2
test_that("get_number_poor() gives correct errors", {

  # expectation 1
  hc <- "character"
  pop <- 100
  expect_error(get_number_poor(headcount = hc, pop = pop))

  # expectation 2
  hc <- rep(c(0.1, 0.2), 5)
  pop <- c(10)
  expect_error(get_number_poor(headcount = hc, pop = pop))

})

# get_average_shortfall()

## Test 1
test_that("get_average_shortfall() returns expected results", {

  # expectation 1
  hc <- 0.2
  pl <- 100
  pg <- 0.3
  output <- get_average_shortfall(headcount = hc, povgap = pg, povline = pl)
  expect_equal(output, pl*pg/hc)

  # expectation 2
  hc <- rep(c(0.1, 0.2), 5)
  pl <- sample(c(1:100), size = 10, replace = T)
  pg <- rep(c(0.2), 10)
  output <- get_average_shortfall(headcount = hc, povgap = pg, povline = pl)
  expect_equal(output, pl*pg/hc)


})

## Test 2
test_that("get_average_shortfall() gives correct errors", {

  # expectation 1
  hc <- "0.2"
  pl <- 100
  pg <- 0.3

  expect_error(get_average_shortfall(headcount = hc, povgap = pg, povline = pl))

  # expectation 2
  hc <- rep(c(0.1, 0.2), 5)
  expect_error(get_average_shortfall(headcount = hc, povgap = pg, povline = pl))

})

# get_total_shortfall()

## Test 1
test_that("get_total_shortfall() returns expected results", {

  # expectation 1
  hc <- 0.2
  pl <- 100
  pg <- 0.3
  pop <- 10
  output <- get_total_shortfall(headcount = hc, pop = pop, povgap = pg, povline = pl)
  expect_equal(output, (hc*pop)*pl*pg/hc)

  # expectation 2
  hc <- rep(c(0.1, 0.2), 5)
  pl <- sample(c(1:100), size = 10, replace = T)
  pg <- rep(c(0.2), 10)
  pop <- c(10:19)
  output <- get_total_shortfall(headcount = hc, pop = pop, povgap = pg, povline = pl)
  expect_equal(output, (hc*pop)*pl*pg/hc)


})

## Test 2
test_that("get_total_shortfall() gives correct errors", {

  # expectation 1
  hc <- "0.2"
  pl <- 100
  pg <- 0.3
  pop <- 10
  expect_error(get_total_shortfall(headcount = hc, pop = pop, povgap = pg, povline = pl))

  # expectation 2
  hc <- c(0.1, 0.2)
  expect_error(get_total_shortfall(headcount = hc, pop = pop, povgap = pg, povline = pl))


})




# get_income_gap_ratio()

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







