
test_that("get_gd_lorenz_params works", {

  params <- get_gd_lorenz_params(
    welfare = grouped_data_ex2$welfare,
    population = grouped_data_ex2$weight)

  # List objects ----------
  expect_equal(names(params), c("lq", "lb", "data"))
  expect_equal(names(params$lq), names(params$lb))
  expect_equal(names(params$lq$reg_results) |>
                 sort(),
               names(params$lb$reg_results) |>
                 sort())

  expect_length(params$lq$reg_results$coef, 3)
  expect_length(params$lb$reg_results$coef, 3)



})

test_that("get_gd_lorenz_validity works", {
  # Using Lorenz parameters from get_gd_lorenz_params
  params <- get_gd_lorenz_params(
    welfare = grouped_data_ex2$welfare,
    population = grouped_data_ex2$weight)

  with_pars <- get_gd_lorenz_validity(params = params)

  # Using welfare and population vecotrs
  with_vecs <- get_gd_lorenz_validity(
    welfare = grouped_data_ex2$welfare,
    population = grouped_data_ex2$weight)

  expect_equal(with_pars, with_vecs)

  # complete results

  with_pars_comp <- get_gd_lorenz_validity(params = params,
                                           complete = TRUE)

  # Using welfare and population vecotrs
  with_vecs_comp <- get_gd_lorenz_validity(
    welfare = grouped_data_ex2$welfare,
    population = grouped_data_ex2$weight,
    complete = TRUE)

  expect_equal(with_pars_comp, with_vecs_comp)

})

test_that("get_gd_select_lorenz works", {

  # regular -----------

  # With parameters
  params <- get_gd_lorenz_validity(
    welfare = grouped_data_ex2$welfare,
    population = grouped_data_ex2$weight,
    complete = TRUE)

  wp <- get_gd_select_lorenz(params = params)
  expect_equal(names(wp), "selected_lorenz")
  expect_equal(names(wp$selected_lorenz),
               c("for_dist", "for_pov", "use_lq_for_dist", "use_lq_for_pov"))

  # With vector
  wv <- get_gd_select_lorenz( welfare = grouped_data_ex2$welfare,
                                     population = grouped_data_ex2$weight)

  expect_equal(wp, wv)

  # Complete -----
  wpc <- get_gd_select_lorenz(params = params,
                                    complete = TRUE)

  wpc_wo_l4 <- wpc
  wpc_wo_l4$selected_lorenz <- NULL

  expect_equal(wpc_wo_l4, params)

  wpc_l4 <- wpc["selected_lorenz"]
  expect_equal(wpc_l4, wp)


})

