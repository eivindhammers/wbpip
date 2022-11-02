#' Get Lorenz Parameters
#'
#' Get Parameters and key values derived from the quadratic and Beta Lorenz
#' parametrization. `welfare` and `population` must be vectors of a group data
#' dataset
#'
#' @param welfare numeric: cumulative sahre of welfare (income/consumption)
#' @param population numeric: cumulative share of the population
#'
#' @return list with parameters
#' @export
#'
#' @examples
#' # Get Lorenz parameters
#' res <- get_gd_lorenz_params(
#'   welfare = grouped_data_ex2$welfare,
#'   population = grouped_data_ex2$weight)
#' str(res)
get_gd_lorenz_params <- function(welfare,
                                 population) {

#   ____________________________________________________________________________
#   on.exit                                                                 ####
  on.exit({

  })

#   ____________________________________________________________________________
#   Defenses                                                                ####
  stopifnot( exprs = {

    }
  )

#   ____________________________________________________________________________
#   Early returns                                                           ####
  if (FALSE) {
    return()
  }

#   ____________________________________________________________________________
#   Computations                                                            ####

  # create results list
  l_res <- vector(mode = "list", length = 2)
  names(l_res) <- c("lq", "lb")


  # Apply Lorenz quadratic fit ----------------------------------------------

  ## STEP 1: Prep data to fit functional form-------------
  functional_form_lq <-
    create_functional_form_lq(welfare    = welfare,
                              population = population)

  ## STEP 2: Estimate regression coefficients using LQ parametrization------
  reg_results_lq <- regres(functional_form_lq, is_lq = TRUE)
  names(reg_results_lq$coef) <- c("A", "B", "C")

  # add to results list
  l_res$lq$reg_results <- reg_results_lq


  ## STEP 3: get key values
  # Compute key numbers from Lorenz quadratic form
  kv <- gd_lq_key_values(reg_results_lq$coef[["A"]],
                         reg_results_lq$coef[["B"]],
                         reg_results_lq$coef[["C"]])

  l_res$lq$key_values <- kv


  # Apply Lorenz beta fit ---------------------------------------------------

  ## STEP 1: Prep data to fit functional form --------------
  functional_form_lb <-
    create_functional_form_lb(welfare    = welfare,
                              population = population)

  ## STEP 2: Estimate regression coefficients using LB parameterization
  reg_results_lb <- regres(functional_form_lb, is_lq = FALSE)
  names(reg_results_lb$coef) <- c("A", "B", "C")

  # add to results list
  l_res$lb$reg_results <- reg_results_lb

  l_res$lb$key_values <- NULL

  # Check validity
  validity_lb <- check_curve_validity_dist_lb(reg_results_lb$coef[["A"]],
                                              reg_results_lb$coef[["B"]],
                                              reg_results_lb$coef[["C"]])
  l_res$lb$validity   <- validity_lb


#   ____________________________________________________________________________
#   Return                                                                  ####
  return(l_res)

}


#' Check validity of Lorenz Curve for distributional Stats
#'
#' @inheritParams get_gd_lorenz_params
#' @param params list of parameters from `get_gd_lorenz_params()`
#' @param complete logical: If TRUE, returns a list a cumulative returns from
#'   previously used `get_gd` functions. Default is `FALSE`
#'
#' @return list of distributional validity of each Lorenz model
#' @export
#'
#' @examples
get_gd_lorenz_validity_dist <- function(welfare    = NULL,
                                        population = NULL,
                                        params     = NULL,
                                        complete   = FALSE) {

#   ____________________________________________________________________________
#   on.exit                                                                 ####
  on.exit({

  })

#   ____________________________________________________________________________
#   Defenses                                                                ####
  stopifnot( exprs = {
    "Either `params` or `welfare` and `population` should be spefied" =
      (is.null(params) && !is.null(welfare) && !is.null(population)) ||
      (!is.null(params) && is.null(welfare) && is.null(population))

    "`params` should be a list from `get_gd_lorenz_params()`" =
      is.list(params) || is.null(params)

    "`complete` must be logical" =
      is.logical(complete)
  }
  )

#   ____________________________________________________________________________
#   Early returns                                                           ####
  if (FALSE) {
    return()
  }

#   ____________________________________________________________________________
#   Computations                                                            ####
  if (!is.null(welfare)) {
    params <- get_gd_lorenz_params(welfare, population)
  }

  # Validity or LQ
  validity_lq <- check_curve_validity_lq(params$lq$reg_results$coef[["A"]],
                                         params$lq$reg_results$coef[["B"]],
                                         params$lq$reg_results$coef[["C"]],
                                         params$lq$key_values$e,
                                         params$lq$key_values$m,
                                         params$lq$key_values$n,
                                         params$lq$key_values$r^2)

  # Validity of LB
  validity_lb <-
    check_curve_validity_dist_lb(params$lb$reg_results$coef[["A"]],
                                 params$lb$reg_results$coef[["B"]],
                                 params$lb$reg_results$coef[["C"]])

#   ____________________________________________________________________________
#   Return                                                                  ####

  if (isFALSE(complete)) {
    params <- vector("list")
  }

  params$lq$validity <- validity_lq
  params$lb$validity <- validity_lb
  return(params)

}


#' Check validity of Lorenz Curve for poverty Stats
#'
#' @inheritParams get_gd_lorenz_validity_dist
#' @param mean numeric: welfare mean of distribution.
#' @param povline numeric: value of poverty line. Default is the `mean` value
#'
#'
#' @return
#' @export
#'
#' @examples
get_gd_lorenz_validity_pov <- function(welfare    = NULL,
                                       population = NULL,
                                       params     = NULL,
                                       mean       = 1,
                                       povline    = mean,
                                       complete   = FALSE) {

#   ____________________________________________________________________________
#   on.exit                                                                 ####
  on.exit({

  })

#   ____________________________________________________________________________
#   Defenses                                                                ####
  stopifnot( exprs = {

    }
  )

#   ____________________________________________________________________________
#   Early returns                                                           ####
  if (FALSE) {
    return()
  }

#   ____________________________________________________________________________
#   Computations                                                            ####
  if (!is.null(welfare)) {
    params <- get_gd_lorenz_params(welfare, population)
  }

  # Validity or LQ
  validity_lq <- check_curve_validity_lq(params$lq$reg_results$coef[["A"]],
                                         params$lq$reg_results$coef[["B"]],
                                         params$lq$reg_results$coef[["C"]],
                                         params$lq$key_values$e,
                                         params$lq$key_values$m,
                                         params$lq$key_values$n,
                                         params$lq$key_values$r^2)

  # Validity of LB
  # Compute poverty stats
  headcount <- gd_compute_headcount_lb(mean,
                                       povline,
                                       params$lb$reg_results$coef[["A"]],
                                       params$lb$reg_results$coef[["B"]],
                                       params$lb$reg_results$coef[["C"]])

  # Check validity
  validity_lb <-
    check_curve_validity_lb(headcount = headcount,
                            params$lb$reg_results$coef[["A"]],
                            params$lb$reg_results$coef[["B"]],
                            params$lb$reg_results$coef[["C"]])

  #   __________________________________________________________________
  #   Return                                                          ####

  if (isFALSE(complete)) {
    params <- vector("list")
  }

  params$lq$validity <- validity_lq
  params$lb$validity <- validity_lb
  return(params)


#   ____________________________________________________________________________
#   Return                                                                  ####
  return(TRUE)

}




#' Get selected Lorenz curve for distributional stats
#'
#' @inheritParams get_gd_lorenz_params
#' @inheritParams get_gd_lorenz_validity_dist
#' @param params list of parameters from `get_gd_lorenz_params()`
#'
#' @return list of values with best lorenz fit for distributional Stats
#' @export
#'
#' @examples
#' # Using Lorenz parameters from get_gd_lorenz_params
#' params <- get_gd_lorenz_params(
#'   welfare = grouped_data_ex2$welfare,
#'   population = grouped_data_ex2$weight)
#'
#' params <- get_gd_lorenz_validity_dist(
#'   params = params,
#'   complete = TRUE)
#' get_gd_selected_lorenz_dist(params = params)
#'
#' # Using Lorenz parameters from get_gd_lorenz_validity_dist
#' params <- get_gd_lorenz_validity_dist(
#'   welfare = grouped_data_ex2$welfare,
#'   population = grouped_data_ex2$weight,
#'   complete = TRUE)
#' get_gd_selected_lorenz_dist(params = params)
#'
#' # Using original vectors
#'
#' get_gd_selected_lorenz_dist(
#'   welfare = grouped_data_ex2$welfare,
#'   population = grouped_data_ex2$weight)
get_gd_selected_lorenz_dist <-
  function(welfare    = NULL,
           population = NULL,
           params     = NULL,
           complete   = FALSE) {

#   ____________________________________________________________________________
#   on.exit                                                                 ####
  on.exit({

  })

#   ____________________________________________________________________________
#   Defenses                                                                ####
  stopifnot( exprs = {
    "Either `params` or `welfare` and `population` should be spefied" =
      (is.null(params) && !is.null(welfare) && !is.null(population)) ||
      (!is.null(params) && is.null(welfare) && is.null(population))

    "`params` should be a list from `get_gd_lorenz_params()`" =
      is.list(params) || is.null(params)

    "`complete` must be logical" =
      is.logical(complete)
    }
  )

#   ____________________________________________________________________________
#   Early returns                                                           ####
  if (FALSE) {
    return()
  }

#   ____________________________________________________________________________
#   Computations                                                            ####
  if (!is.null(welfare)) {
    params <- get_gd_lorenz_validity_dist(welfare,
                                          population,
                                          complete = TRUE)
  }

  # prepare parameters
  lq <- append(params$lq$validity,
               params$lq$reg_results["sse"])
  lb <- append(params$lb$validity,
               params$lb$reg_results["sse"])

  use_lq_for_dist <-
    use_lq_for_distributional(lq,lb)

  l_res <- list(selected_lorenz = ifelse(use_lq_for_dist, "lq", "lb"),
                use_lq_for_dist = use_lq_for_dist)

#   ____________________________________________________________________________
#   Return                                                                  ####
  if (isFALSE(complete)) {
    params <- vector("list")
  }

  params$l4dist <- l_res
  attr(params$l4dist, "label") <- "l4dist: Lorenz for distributional stats"
  return(params)

}


#' Get selected Lorenz curve for distributional stats
#'
#' @inheritParams get_gd_lorenz_validity_pov
#'
#' @return list with information of selected lorenz for poverty
#' @export
#'
#' @examples
get_gd_selected_lorenz_pov <- function(welfare    = NULL,
                                       population = NULL,
                                       params     = NULL,
                                       mean       = 1,
                                       povline    = mean,
                                       complete   = FALSE) {

#   ____________________________________________________________________________
#   on.exit                                                                 ####
  on.exit({

  })

#   ____________________________________________________________________________
#   Defenses                                                                ####
  pl <- as.list(environment())
  check_get_gd_fun_params(pl)

#   ____________________________________________________________________________
#   Early returns                                                           ####
  if (FALSE) {
    return()
  }

#   ____________________________________________________________________________
#   Computations                                                            ####


#   ____________________________________________________________________________
#   Return                                                                  ####
  return(TRUE)

}
