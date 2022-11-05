#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Selection of Lorenz   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


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
#   Defenses                                                                ####
  pl <- as.list(environment())
  check_get_gd_fun_params(pl)

#   ____________________________________________________________________________
#   Computations                                                            ####

  # create results list
  l_res <- vector(mode = "list", length = 3)
  names(l_res) <- c("lq", "lb", "data")


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

  l_res$lb$key_values <- NA

#   ____________________________________________________________________________
#   Return                                                                  ####
  l_res$data$welfare    <- welfare
  l_res$data$population <- population

  return(l_res)

}

#' Check validity of Lorenz Curve
#'
#' @inheritParams get_gd_lorenz_params
#' @param params list of parameters from `get_gd_lorenz_params()`
#' @param complete logical: If TRUE, returns a list a cumulative returns from
#'   previously used `get_gd` functions. Default is `FALSE`
#' @param mean numeric: welfare mean of distribution.
#' @param povline numeric: value of poverty line. Default is the `mean` value
#' @param popshare numeric: range (0,1). Share of population. Provide share of
#'   population instead of poverty line
#'
#'
#' @return list of distributional validity of each Lorenz model
#' @export
#'
#' @examples
#' # Using Lorenz parameters from get_gd_lorenz_params
#' params <- get_gd_lorenz_params(
#'   welfare = grouped_data_ex2$welfare,
#'   population = grouped_data_ex2$weight)
#'
#' get_gd_lorenz_validity(params = params)
#'
#' # Using welfare and population vecotrs
#' get_gd_lorenz_validity(
#'   welfare = grouped_data_ex2$welfare,
#'   population = grouped_data_ex2$weight)
get_gd_lorenz_validity <- function(welfare    = NULL,
                                   population = NULL,
                                   params     = NULL,
                                   mean       = 1,
                                   times_mean = 1,
                                   popshare   = NULL,
                                   povline    = ifelse(is.null(popshare),
                                                   mean*times_mean,
                                                   NA_real_),
                                   complete   = FALSE) {


#   ____________________________________________________________________________
#   Defenses                                                                ####
  pl <- as.list(environment())
  check_get_gd_fun_params(pl)


#   ____________________________________________________________________________
#   Computations                                                            ####
  if (!is.null(welfare)) {
    params <- get_gd_lorenz_params(welfare, population)
  }

  if (!is.null(popshare)) {
    povline_lq <- mean * derive_lq(popshare,
                                   params$lq$reg_results$coef[["A"]],
                                   params$lq$reg_results$coef[["B"]],
                                   params$lq$reg_results$coef[["C"]])

    povline_lb <- mean * derive_lb(popshare,
                                   params$lb$reg_results$coef[["A"]],
                                   params$lb$reg_results$coef[["B"]],
                                   params$lb$reg_results$coef[["C"]])

  } else {
    povline_lb <- povline_lq <- povline

  }

  # Validity or LQ
  validity_lq <- check_curve_validity_lq(params$lq$reg_results$coef[["A"]],
                                         params$lq$reg_results$coef[["B"]],
                                         params$lq$reg_results$coef[["C"]],
                                         params$lq$key_values$e,
                                         params$lq$key_values$m,
                                         params$lq$key_values$n,
                                         params$lq$key_values$r^2)

  headcount_lq <- gd_compute_headcount_lq(mean,
                                          povline_lq,
                                          params$lq$reg_results$coef[["B"]],
                                          params$lq$key_values$m,
                                          params$lq$key_values$n,
                                          params$lq$key_values$r)

  validity_lq$headcount <- headcount_lq

  # Validity of LB
  # Compute poverty stats
  headcount_lb <- gd_compute_headcount_lb(mean,
                                          povline_lb,
                                          params$lb$reg_results$coef[["A"]],
                                          params$lb$reg_results$coef[["B"]],
                                          params$lb$reg_results$coef[["C"]])

  # Check validity
  validity_lb <-
    check_curve_validity_lb(headcount = headcount_lb,
                            params$lb$reg_results$coef[["A"]],
                            params$lb$reg_results$coef[["B"]],
                            params$lb$reg_results$coef[["C"]])

  validity_lb$headcount <- headcount_lb

  if ( povline_lb != mean*times_mean) {
    times_mean <- povline_lb/mean
  }

  norm_lb_label <- paste0("Normality with a mean of ", mean,
                          " and a poverty line of ", povline_lb,
                          ";", times_mean, " times the mean.")

  attr(validity_lb$is_normal, "label") <- norm_lb_label


  #   __________________________________________________________________
  #   Return                                                          ####

  if (isFALSE(complete)) {
    params <- vector("list")
  }

  params$lq$validity <- validity_lq
  params$lb$validity <- validity_lb
  return(params)


}




#' Get selected Lorenz curve for distributional stats
#'
#' @inheritParams get_gd_lorenz_params
#' @inheritParams get_gd_lorenz_validity
#' @param params list of parameters from `get_gd_lorenz_validity()`
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
#' params <- get_gd_lorenz_validity(
#'   params = params,
#'   complete = TRUE)
#' get_gd_select_lorenz(params = params)
#'
#' # Using Lorenz parameters from get_gd_lorenz_validity
#' params <- get_gd_lorenz_validity(
#'   welfare = grouped_data_ex2$welfare,
#'   population = grouped_data_ex2$weight,
#'   complete = TRUE)
#' get_gd_select_lorenz(params = params)
#'
#' # Using original vectors
#'
#' get_gd_select_lorenz(
#'   welfare = grouped_data_ex2$welfare,
#'   population = grouped_data_ex2$weight)
get_gd_select_lorenz <- function(welfare    = NULL,
                                   population = NULL,
                                   params     = NULL,
                                   mean       = 1,
                                   times_mean = 1,
                                   popshare   = NULL,
                                   povline    = ifelse(is.null(popshare),
                                                     mean*times_mean,
                                                     NA_real_),
                                   complete   = FALSE) {

#   ____________________________________________________________________________
#   Defenses                                                                ####
  pl <- as.list(environment())
  check_get_gd_fun_params(pl)
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
#   Computations                                                            ####
  if (!is.null(welfare)) {
    params <- get_gd_lorenz_validity(welfare,
                                     population,
                                     complete   = TRUE,
                                     mean       = mean,
                                     times_mean = times_mean,
                                     povline    = povline,
                                     popshare   = popshare)
  }

  ## Selected Lorenz for  Distribution-------
  lq <- append(params$lq$validity,
               params$lq$reg_results["sse"])
  lb <- append(params$lb$validity,
               params$lb$reg_results["sse"])

  use_lq_for_dist <-
    use_lq_for_distributional(lq,lb)

  ## Selected Lorenz for Poverty -----------

  fit_lb <- gd_compute_fit_lb(params$data$population,
                              params$data$population,
                              params$lb$validity$headcount,
                              params$lb$reg_results$coef[["A"]],
                              params$lb$reg_results$coef[["B"]],
                              params$lb$reg_results$coef[["C"]])

  fit_lq <- gd_compute_fit_lq(params$data$population,
                              params$data$population,
                              params$lq$validity$headcount,
                              params$lb$reg_results$coef[["A"]],
                              params$lb$reg_results$coef[["B"]],
                              params$lb$reg_results$coef[["C"]])

  lq <- append(lq,
               fit_lq["ssez"])
  lb <- append(lb,
               fit_lb["ssez"])


  use_lq_for_pov <- use_lq_for_poverty(lq, lb)

  l_res <- list(for_dist = ifelse(use_lq_for_dist, "lq", "lb"),
                for_pov  = ifelse(use_lq_for_pov, "lq", "lb"),
                use_lq_for_dist = use_lq_for_dist,
                use_lq_for_pov  = use_lq_for_pov)

#   ____________________________________________________________________________
#   Return                                                                  ####
  if (isFALSE(complete)) {
    params <- vector("list")
  }

  params$selected_lorenz <- l_res
  return(params)

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Calculate Stats   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Welfare share by quantile in group data
#'
#' @inheritParams get_gd_select_lorenz
#' @param lorenz character or NULL. Lorenz curve selected. It could be "lq" for
#'   Lorenz Quadratic or "lb" for Lorenz Beta
#' @param  n numeric: Number of quantiles
#'
#' @return list with vector of share of welfare by quantiles
#' @export
#'
#' @examples
#' # Using params
#' params <- get_gd_select_lorenz(
#'   welfare = grouped_data_ex2$welfare,
#'   population = grouped_data_ex2$weight)
#' qt <- get_gd_wlf_sahre_by_qtl(params = params)
#' qt$dist_stats$quantiles
#'
#' # Using orignal vectors
#' qt <- get_gd_wlf_sahre_by_qtl(
#' welfare = grouped_data_ex2$welfare,
#'   population = grouped_data_ex2$weight)
#' qt$dist_stats$quantiles
get_gd_wlf_sahre_by_qtl <- function(welfare    = NULL,
                                    population = NULL,
                                    params     = NULL,
                                    complete   = FALSE,
                                    lorenz     = NULL,
                                    n          = 10) {

  #   ________________________________________________________
  #   on.exit                                     ####
  on.exit({

  })

  #   _________________________________________________________
  #   Defenses                                                ####
  pl <- as.list(environment())
  check_get_gd_fun_params(pl)

  #   ________________________________________________________
  #   Early returns                                ####
  if (FALSE) {
    return()
  }

  #   ____________________________________________________
  #   Computations                              ####
  if (!is.null(welfare)) {
    params <- get_gd_select_lorenz(welfare,
                                   population,
                                   complete   = TRUE)
  }

  if (is.null(lorenz)) {
    lorenz <- params$selected_lorenz$for_dist
  }

  qfun <- paste0("gd_compute_quantile_", lorenz)

  welfare_share <-  match.fun(qfun)(params[[lorenz]]$reg_results$coef[["A"]],
                                params[[lorenz]]$reg_results$coef[["B"]],
                                params[[lorenz]]$reg_results$coef[["C"]],
                                n_quantile = n)
  #   ____________________________________________________________
  #   Return                                                ####
  if (isFALSE(complete)) {
    params <- vector("list")
  }

  params$dist_stats$welfare_share <- welfare_share
  return(params)

}


#' Estimate quantiles based population share or number of bins
#'
#' @inheritParams get_gd_wlf_sahre_by_qtl
#' @param popshare numeric: vector of share of population. Default is `seq(from
#'   = 1/n, to = 1, by = 1/n)`
#'
#' @return
#' @export
#'
#' @examples
get_gd_quantiles <- function(welfare    = NULL,
                             population = NULL,
                             complete   = FALSE,
                             mean       = 1,
                             lorenz     = NULL,
                             n          = 10,
                             popshare   = seq(from = 1/n, to = 1, by = 1/n)
                             ) {

#   ____________________________________________________
#   on.exit                                         ####
  on.exit({

  })

#   ____________________________________________________
#   Defenses                                        ####
  pl <- as.list(environment())
  check_get_gd_fun_params(pl)

  #   ________________________________________________________
  #   Early returns                                ####
  if (FALSE) {
    return()
  }

  #   ____________________________________________________
  #   Computations                              ####


  # |>
  #   sapply( \(.) {
  #     .$selected_lorenz$for_pov
  #   })


  #   Computations                              ####
  if (!is.null(welfare)) {
    params <- get_gd_select_lorenz(welfare,
                                   population,
                                   complete   = TRUE)
  }

  if (is.null(lorenz)) {
    lorenz <- params$selected_lorenz$for_dist
  }

  # Vectorize function
  qfun <- paste0("derive_", lorenz)
  vc_derive <- Vectorize(match.fun(qfun),
                         vectorize.args = "x",
                         SIMPLIFY = TRUE)

  # quantiles <- mean * match.fun(qfun)(popshare,
  #                               params[[lorenz]]$reg_results$coef[["A"]],
  #                               params[[lorenz]]$reg_results$coef[["B"]],
  #                               params[[lorenz]]$reg_results$coef[["C"]])


  quantiles <- mean * vc_derive(popshare,
                                params[[lorenz]]$reg_results$coef[["A"]],
                                params[[lorenz]]$reg_results$coef[["B"]],
                                params[[lorenz]]$reg_results$coef[["C"]])

  #   ____________________________________________________________
  #   Return                                                ####
  if (isFALSE(complete)) {
    params <- vector("list")
  }

  params$dist_stats$quantiles <- quantiles
  return(params)
}


#' Get vectors related to the Lorenz Curve
#'
#' @inheritParams get_gd_quantiles
#'
#' @return data.table or list with Lorenz stats
#' @export
#'
#' @examples
get_gd_lorenz_stats <- function(welfare    = NULL,
                                population = NULL,
                                params     = NULL,
                                mean       = 1,
                                times_mean = 1,
                                povline    = mean*times_mean,
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


#   ____________________________________________________________________________
#   Return                                                                  ####
  return(TRUE)

}


get_gd_gini <- function() {

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


#   ____________________________________________________________________________
#   Return                                                                  ####
  return(TRUE)

}

