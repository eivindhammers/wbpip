get_gd_lorenz_params <- function(welfare,
                                 population,
                                 mean,
                                 pop            = NULL,
                                 p0             = 0.5,
                                 nobs           = 1e5,
                                 selected_model = NULL,
                                 verbose        = FALSE) {
  # Check arguments
  if (!is.null(selected_model) && !selected_model  %in% c("q", "b") ) {
    cli::cli_abort(c("Incorrect selected model",
                     "must be either {.field 'q'} or {.field 'b'},
                       not {.val {selected_model}}"))
  }


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
  prepped_data <- create_functional_form_lq(welfare    = welfare,
                                            population = population)

  ## STEP 2: Estimate regression coefficients using LQ parametrization------
  reg_results_lq <- regres(prepped_data, is_lq = TRUE)
  names(reg_results_lq$coef) <- c("A", "B", "C")

  # add to results list
  l_res$lq$reg_results <- reg_results_lq


  ## STEP 3: get key values
  # Compute key numbers from Lorenz quadratic form
  kv <- gd_lq_key_values(A, B, C)

  ## STEP 4: check viability
  validity <- check_curve_validity_lq(A,
                                      B,
                                      C,
                                      kv$e,
                                      kv$m,
                                      kv$n,
                                      kv$r^2)
  reg_coef_lq    <- reg_results_lq$coef
  results_lq     <- gd_estimate_dist_stats_lq(
    mean = mean,
    p0   = p0,
    A    = reg_coef_lq[1],
    B    = reg_coef_lq[2],
    C    = reg_coef_lq[3]
  )

  results_lq <- append(results_lq, reg_results_lq)

  # Apply Lorenz beta fit ---------------------------------------------------

  ## STEP 1: Prep data to fit functional form --------------
  prepped_data <- create_functional_form_lb(
    welfare = welfare, population = population
  )

  ## STEP 2: Estimate regression coefficients using LB parameterization
  reg_results_lb <- regres(prepped_data, is_lq = FALSE)
  reg_coef_lb <- reg_results_lb$coef

  ## STEP 3: Calculate distributional stats ---------------
  results_lb <- gd_estimate_dist_stats_lb(
    mean = mean, p0 = p0, A = reg_coef_lb[1],
    B = reg_coef_lb[2], C = reg_coef_lb[3]
  )

  results_lb <- append(results_lb, reg_results_lb)


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



