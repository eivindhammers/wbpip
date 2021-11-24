
test_that("md_compute_pip_stats fails when required arguments are not specified", {
  expect_error(md_compute_pip_stats(welfare = 10:100, povline = 1.9))
  expect_error(md_compute_pip_stats(povline = 1.9, population = 10:100))
  expect_error(md_compute_pip_stats(welfare = 10:100, population = 10:100))
})
