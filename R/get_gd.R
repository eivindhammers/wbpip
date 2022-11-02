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

  ## STEP 4: check viability
  validity_lq <- check_curve_validity_lq(reg_results_lq$coef[["A"]],
                                         reg_results_lq$coef[["B"]],
                                         reg_results_lq$coef[["C"]],
                                         kv$e,
                                         kv$m,
                                         kv$n,
                                         kv$r^2)
  l_res$lq$key_values <- kv
  l_res$lq$validity   <- validity_lq

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

  return(l_res)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # create template for synthetic vector   ---------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## welfare --------
  # Create equally spaced distribution points that will be used to compute
  # the vector to welfare values
  first <- 1 / (2 * nobs)
  last <- 1 - (1 / (2 * nobs))
  n <- c(1:nobs)
  weight_range <- (n - 1) / (nobs - 1) * ((last) - (first)) + (first)


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Apply selection rules   ---------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # 3 Selection of Lorenz fit for distributional statistics ----
  if (is.null(selected_model)) {

    use_lq_for_dist <- use_lq_for_distributional(
      lq = results_lq,
      lb = results_lb
    )

  } else {
    if (selected_model == "q") {
      use_lq_for_dist <- TRUE
    } else {
      use_lq_for_dist <- FALSE
    }
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## apply correct lorenz --------
  if (use_lq_for_dist) {
    A <- reg_coef_lq[1]
    B <- reg_coef_lq[2]
    C <- reg_coef_lq[3]
    # Compute welfare values

    # Vectorize is faster than purrr
    vderive_lq <- Vectorize(derive_lq, vectorize.args = "x")
    welfare_s <- vderive_lq(weight_range, A, B, C) * mean

    model_used <- "quadratic Lorenz"

  } else {
    A <- reg_coef_lb[1]
    B <- reg_coef_lb[2]
    C <- reg_coef_lb[3]

    # Compute welfare values

    vderive_lb <- Vectorize(derive_lb, vectorize.args = "x")
    welfare_s <- vderive_lb(weight_range, A, B, C) * mean

    model_used <- "Beta Lorenz"
  }

  if (verbose) {
    cli::cli_alert("Parameters used in {.field {model_used}} model")
    cli::cli_dl(c(A = A, B = B, C = C, mean = mean))
  }

  # manage population
  if (is.null(pop)) {
    weight <- 1
  } else {
    weight <- pop / nobs
  }

  df <- data.table::data.table(
    welfare = welfare_s,
    weight  = weight
  )

  return(df)


#   ____________________________________________________________________________
#   Return                                                                  ####
  return(TRUE)

}






