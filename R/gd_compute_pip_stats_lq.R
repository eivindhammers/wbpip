#' Computes poverty statistics (Lorenz quadratic)
#'
#' Compute poverty statistics for grouped data using the quadratic functional
#' form of the Lorenz qurve.
#'
#' @inheritParams gd_compute_pip_stats
#'
#' @examples
#' # Set initial parameters
#' L <- c(
#'   0.00208, 0.01013, 0.03122, 0.07083, 0.12808, 0.23498, 0.34887,
#'   0.51994, 0.6427, 0.79201, 0.86966, 0.91277, 1
#' )
#' P <- c(
#'   0.0092, 0.0339, 0.085, 0.164, 0.2609, 0.4133, 0.5497, 0.7196,
#'   0.8196, 0.9174, 0.957, 0.9751, 1
#' )
#' mu <- 109.9 # mean
#' z <- 89 # poverty line
#'
#' res <- wbpip:::gd_compute_pip_stats_lq(
#'   welfare = L,
#'   population = P,
#'   requested_mean = mu,
#'   povline = z,
#'   default_ppp = 1
#' )
#' res$headcount
#'
#' res2 <- wbpip:::gd_compute_pip_stats_lq(
#'   welfare = L,
#'   population = P,
#'   requested_mean = mu,
#'   popshare = res$headcount,
#'   default_ppp = 1
#' )
#' res2$povline
#' @return list
#' @keywords internal
gd_compute_pip_stats_lq <- function(welfare,
                                    povline,
                                    population,
                                    requested_mean,
                                    popshare = NULL,
                                    default_ppp,
                                    ppp = NULL,
                                    p0 = 0.5) {

  # Adjust mean if different PPP value is provided
  if (!is.null(ppp)) {
    requested_mean <- requested_mean * default_ppp / ppp
  } else {
    ppp <- default_ppp
  }
  # STEP 1: Prep data to fit functional form
  prepped_data <- create_functional_form_lq(
    welfare = welfare,
    population = population
  )

  # STEP 2: Estimate regression coefficients using LQ parameterization
  reg_results <- regres(prepped_data, is_lq = TRUE)
  reg_coef <- reg_results$coef

  A <- reg_coef[1]
  B <- reg_coef[2]
  C <- reg_coef[3]

  # OPTIONAL: Only when popshare is supplied
  # return poverty line if share of population living in poverty is supplied
  # intead of a poverty line
  if (!is.null(popshare)) {
    povline <- derive_lq(popshare, A, B, C) * requested_mean
  }

  # Boundary conditions (Why 4?)
  z_min <- requested_mean * derive_lq(0.001, A, B, C) + 4
  z_max <- requested_mean * derive_lq(0.980, A, B, C) - 4
  z_min <- if (z_min < 0) 0L else z_min

  results1 <- list(requested_mean, povline, z_min, z_max, ppp)
  names(results1) <- list("mean", "poverty_line", "z_min", "z_max", "ppp")

  # STEP 3: Estimate poverty measures based on identified parameters
  results2 <- gd_estimate_lq(requested_mean, povline, p0, A, B, C)

  # STEP 4: Compute measure of regression fit
  results_fit <- gd_compute_fit_lq(welfare, population, results2$headcount, A, B, C)

  res <- c(results1, results2, results_fit, reg_results)

  return(res)
}

#' Prepares data for Lorenz Quadratic regression
#'
#' Prepares data for regression of L(1-L) on (P^2-L), L(P-1) and (P-L). The last
#' observation of (p,l), which by construction has the value (1, 1), is excluded
#' since the functional form for the Lorenz curve already forces it to pass
#' through the point (1, 1). Equation 15 in Lorenz Quadratic original paper.
#'
#' @param welfare numeric: Welfare vector from empirical Lorenz curve.
#' @param population numeric: Population vector from empirical Lorenz curve.
#'
#' @references
#' Krause, M. 2013. "[Corrigendum to Elliptical Lorenz
#' curves](https://doi.org/10.1016/j.jeconom.2013.01.001)". *Journal of
#' Econometrics 174* (1): 44.
#'
#' Villasenor, J., B. C. Arnold. 1989. "[Elliptical Lorenz
#' curves](https://EconPapers.repec.org/RePEc:eee:econom:v:40:y:1989:i:2:p:327-338)".
#' *Journal of Econometrics 40* (2): 327-338.
#'
#' @return data.frame
#' @export
create_functional_form_lq <- function(welfare,
                                      population) {
  # CHECK inputs
  # assertthat::assert_that(is.numeric(population))
  # assertthat::assert_that(is.numeric(welfare))
  # assertthat::assert_that(length(population) == length(welfare))
  # assertthat::assert_that(length(population) > 1)

  # Remove last observation (the functional form for the Lorenz curve already forces
  # it to pass through the point (1, 1)
  nobs <- length(population) - 1
  population <- population[1:nobs]
  welfare <- welfare[1:nobs]

  # L(1-L)
  y <- welfare * (1 - welfare)
  # (P^2-L)
  x1 <- population^2 - welfare
  # L(P-1)
  x2 <- welfare * (population - 1)
  # P-L
  x3 <- population - welfare

  return(list(y = y, X = cbind(x1, x2, x3)))

}


#' Returns the first derivative of the quadratic Lorenz
#'
#' `derive_lq()` returns the first derivative of the quadratic Lorenz curves
#' with c = 1. General quadratic form: ax^2 + bxy + cy^2 + dx + ey + f = 0. This
#' function implements computes the derivative of equation (6b) in the original
#' Lorenz Quadratic paper: \deqn{-(B / 2) - (\beta + 2 \alpha x) / (4
#' \sqrt(\alpha x^2 + \beta x + e^2)}
#'
#' @param x numeric: Point on curve.
#' @inheritParams gd_estimate_lq
#'
#' @references
#' Villasenor, J., B. C. Arnold. 1989.
#' "[Elliptical Lorenz curves](https://EconPapers.repec.org/RePEc:eee:econom:v:40:y:1989:i:2:p:327-338)".
#' *Journal of Econometrics 40* (2): 327-338.
#'
#' @return numeric
#' @export
derive_lq <- function(x, A, B, C) {
  e <- -(A + B + C + 1)
  alpha <- (B^2) - (4 * A)
  beta <- (2 * B * e) - (4 * C) # C is called D in original paper, but C in Datt paper
  tmp <- (alpha * x^2) + (beta * x) + (e^2)
  tmp[!is.na(tmp) & tmp < 0]  <- 0L

  # Formula for first derivative of GQ Lorenz Curve
  val <- -(B / 2) - ((2 * alpha * x + beta) / (4 * sqrt(tmp)))

  return(val)
}

#' Check validity of Lorenz Quadratic fit
#'
#' `check_curve_validity_lq()` checks the validity of the Lorenz Quadratic fit
#'
#' @inheritParams gd_estimate_lq
#' @param e numeric: e = -(A + B + C + 1): condition for the curve to go through
#' (1, 1).
#' @param m numeric: m = (B^2) - (4 * A). m < 0: condition for the curve to be
#' an ellipse (m is called alpha in paper).
#' @param n numeric: n = (2 * B * e) - (4 * C). n is called Beta in paper
#' @param r r = (n^2) - (4 * m * e^2). r is called K in paper.
#'
#' @references
#' Datt, G. 1998. "[Computational Tools For Poverty Measurement And
#' Analysis](https://www.ifpri.org/cdmref/p15738coll2/id/125673)". FCND
#' Discussion Paper 50. World Bank, Washington, DC.
#'
#' Krause, M. 2013. "[Corrigendum to Elliptical Lorenz
#' curves](https://doi.org/10.1016/j.jeconom.2013.01.001)". *Journal of
#' Econometrics 174* (1): 44.
#'
#' Villasenor, J., B. C. Arnold. 1989. "[Elliptical Lorenz
#' curves](https://EconPapers.repec.org/RePEc:eee:econom:v:40:y:1989:i:2:p:327-338)".
#' *Journal of Econometrics 40* (2): 327-338.
#'
#' @return list
#' @export
check_curve_validity_lq <- function(A, B, C, e, m, n, r) {
  is_normal <- FALSE
  is_valid <- FALSE

  # r needs to be > 0 because need to extract sq root
  if (r < 0) {
    return(list(
      is_normal = is_normal,
      is_valid = is_valid
    ))
  }

  if (e > 0 || C < 0) {
    return(list(
      is_normal = is_normal,
      is_valid = is_valid
    ))
  }

  # Failure conditions for checking theoretically valid Lorenz curve
  # Found in section 4 of Datt computational tools paper
  cn1 <- n^2
  cn3 <- cn1 / (4 * e^2)

  if (!((m < 0) |
    ((m > 0) & (m < cn3) & (n >= 0)) |
    ((m > 0) & (m < -n / 2) & (m < cn3)))) {
    return(list(
      is_normal = is_normal,
      is_valid = is_valid
    ))
  }

  is_normal <- TRUE
  is_valid <- (A + C) >= 0.9

  return(list(
    is_normal = is_normal,
    is_valid = is_valid
  ))
}

#' Compute gini index from Lorenz Quadratic fit
#'
#' `gd_compute_gini_lq()` computes the gini index from a Lorenz Quadratic fit
#'
#' @inheritParams gd_estimate_lq
#' @inheritParams check_curve_validity_lq
#'
#' @references
#' Datt, G. 1998. "[Computational Tools For Poverty Measurement And
#' Analysis](https://www.ifpri.org/cdmref/p15738coll2/id/125673)". FCND
#' Discussion Paper 50. World Bank, Washington, DC.
#'
#' @return numeric
#' @export
gd_compute_gini_lq <- function(A, B, C, e, m, n, r) {

  # For the GQ Lorenz curve, the Gini formula are valid under the condition A+C>=1
  # P.isValid <- (A + C) >= 0.9
  # P.isNormal <- TRUE

  e1 <- abs(A + C - 1)
  e2 <- 1 + (B / 2) + e

  tmp1 <- n * (B + 2) / (4 * m)
  tmp2 <- (r^2) / (8 * m)
  tmp3 <- (2 * m) + n

  if (m > 0) {
    # tmpnum <- tmp3 + 2 * sqrt(m) * abs(e)
    # tmpden <- n - 2 * abs(e) * sqrt(m)

    # Formula from Datt paper
    # CHECK that code matches formulas in paper
    gini <- e2 + (tmp3 / (4 * m)) * e1 - (n * abs(e) / (4 * m)) - ((r^2) / (8 * sqrt(m)^3)) *
      log(abs(((tmp3 + (2 * sqrt(m) * e1))) / (n + (2 * sqrt(m) * abs(e)))))
    # P.gi <- (e/2) - tmp1 - (tmp2 * log(abs(tmpnum/tmpden)) / sqrt(m))
  } else {
    tmp4 <- ((2 * m) + n) / r
    tmp4 <- if (tmp4 < -1) -1 else tmp4
    tmp4 <- if (tmp4 > 1) 1 else tmp4

    # Formula does not match with paper
    gini <- e2 + (tmp3 / (4 * m)) * e1 - (n * abs(e) / (4 * m)) + (tmp2 * (asin(tmp4) - asin(n / r)) / sqrt(-m))
    # P.gi <- (e/2) - tmp1 + ((tmp2 * (asin(tmp4) - asin(n/r))) / sqrt(-m))
  }

  return(gini)
}

#' Solves for quadratic Lorenz curves
#'
#' `value_at_lq()`solves for quadratic Lorenz curves with c = 1
#' General quadratic form: ax^2 + bxy + cy^2 + dx + ey + f = 0
#'
#' @param x numeric: Point on curve.
#' @inheritParams gd_estimate_lq
#'
#' @return numeric
#' @export
value_at_lq <- function(x, A, B, C) {
  e <- -(A + B + C + 1)
  m <- (B^2) - (4 * A)
  n <- (2 * B * e) - (4 * C)
  temp <- (m * x^2) + (n * x) + (e^2)
  temp <- if (temp < 0) 0L else temp

  # Solving the equation of the Lorenz curve
  estle <- -0.5 * ((B * x) + e + sqrt(temp))

  return(estle)
}

#' Computes MLD from Lorenz Quadratic fit
#'
#' `gd_compute_mld_lq()` computes the Mean Log deviation (MLD) from a Lorenz
#' Quadratic fit
#'
#' @param dd numeric: **TO BE DOCUMENTED**.
#' @inheritParams gd_estimate_lq
#'
#' @return numeric
#' @export
gd_compute_mld_lq <- function(dd, A, B, C) {
  x1 <- derive_lq(0.0005, A, B, C)
  gap <- 0L
  mld <- 0L
  if (x1 == 0) {
    gap <- 0.0005
  } else {
    mld <- suppressWarnings(log(x1) * 0.001)
  }
  x1 <- derive_lq(0, A, B, C)
  for (xstep in seq(0, 0.998, 0.001)) {
    x2 <- derive_lq(xstep + 0.001, A, B, C)
    if ((x1 <= 0) || (x2 <= 0)) {
      gap <- gap + 0.001
      if (gap > 0.5) {
        return(-1)
      }
    } else {
      gap <- 0L
      mld <- mld + (log(x1) + log(x2)) * 0.0005
    }
    x1 <- x2
  }
  return(-mld)
}

#' Compute quantiles from Lorenz Quandratic fit
#'
#' `gd_compute_quantile_lq()` computes quantiles from a Lorenz Quadratic fit.
#'
#' @inheritParams gd_estimate_lq
#' @param n_quantile numeric: Number of quantiles to return.
#'
#' @return numeric
#' @keywords internal
gd_compute_quantile_lq <- function(A, B, C, n_quantile = 10) {
  vec <- vector(mode = "numeric", length = n_quantile)
  x1 <- 1 / n_quantile
  q <- 0L
  lastq <- 0L
  for (i in seq_len(n_quantile - 1)) {
    q <- value_at_lq(x1, A, B, C)
    v <- q - lastq
    vec[i] <- v
    lastq <- q
    x1 <- x1 + 1 / n_quantile
  }
  vec[n_quantile] <- 1 - lastq

  return(vec)
}

#'  Computes Watts Index from Quadratic Lorenz fit
#'
#' `gd_compute_watts_lq()` computes Watts Index from Quadratic Lorenz fit
#' The first distribution-sensitive poverty measure was proposed in 1968 by Watts
#' It is defined as the mean across the population of the proportionate poverty
#' gaps, as measured by the log of the ratio of the poverty line to income,
#' where the mean is formed over the whole population, counting the nonpoor as
#' having a zero poverty gap.
#'
#' @inheritParams gd_compute_fit_lq
#' @param mu numeric: **TO BE DOCUMENTED**.
#' @param dd numeric: **TO BE DOCUMENTED**.
#' @inheritParams gd_estimate_lq
#'
#' @return numeric
#' @export
gd_compute_watts_lq <- function(headcount, mu, povline, dd, A, B, C) {
  if (headcount <= 0 | is.na(headcount)) {
    return(0)
  }

  snw <- headcount * dd
  x1 <- derive_lq(snw / 2, A, B, C)
  if (x1 <= 0) {
    gap <- snw / 2
    watts <- 0L
  } else {
    gap <- 0L
    watts <- log(x1) * snw
  }

  xend <- headcount - snw
  xstep_snw <- seq(0, xend, by = snw) + snw
  x2 <- vapply(xstep_snw, function(x)
    derive_lq(x, A, B, C), FUN.VALUE = numeric(1))
  x1 <- c(derive_lq(0, A, B, C), x2[1:(length(x2) - 1)])

  check <- (x1 <= 0 ) | (x2 <= 0)
  if (any(check)) {
    gap <- gap + sum(check) * snw
    if (gap > 0.05) {
      return(NA_real_) # Return watts as NA
    }
  }
  watts <- sum((log(x1[!check]) + log(x2[!check])) * snw * 0.5) + watts

  if ((mu != 0) && (watts != 0)) {
    x1 <- povline / mu
    if (x1 > 0) {
      watts <- log(x1) * headcount - watts
      if (watts > 0) {
        return(watts)
      }
    }
    return(NA_real_) # Return watts as NA
  } else {
    return(watts)
  }
}

#' Computes polarization index from parametric Lorenz fit
#'
#' Used for grouped data computations
#'
#' @param dcm numeric: Distribution corrected mean
#' @inheritParams gd_estimate_lq
#'
#' @return numeric
#' @keywords internal
gd_compute_polarization_lq <- function(mean,
                                       p0,
                                       dcm,
                                       A, B, C) {
  pol <- 2 - (1 / p0) +
    (dcm - (2 * value_at_lq(p0, A, B, C) * mean)) /
      (p0 * mean * derive_lq(p0, A, B, C))

  return(pol)
}


#' Computes distributional stats from Lorenz Quadratic fit
#'
#' @inheritParams gd_estimate_lq
#' @inheritParams check_curve_validity_lq
#'
#' @return list
#' @keywords internal
gd_compute_dist_stats_lq <- function(mean, p0, A, B, C, e, m, n, r) {
  gini <- gd_compute_gini_lq(A, B, C, e, m, n, r)
  median <- mean * derive_lq(0.5, A, B, C)
  rmhalf <- value_at_lq(p0, A, B, C) * mean / p0 # What is this??
  dcm <- (1 - gini) * mean
  pol <- gd_compute_polarization_lq(mean, p0, dcm, A, B, C)
  ris <- value_at_lq(0.5, A, B, C)
  mld <- gd_compute_mld_lq(0.01, A, B, C)
  deciles <- gd_compute_quantile_lq(A, B, C)

  return(list(
    gini         = gini,
    median       = median,
    rmhalf       = rmhalf,
    dcm          = dcm,
    polarization = pol,
    ris          = ris,
    mld          = mld,
    deciles      = deciles
  ))
}

#' Computes poverty stats from Lorenz Quadratic fit
#'
#' @inheritParams gd_estimate_lq
#' @inheritParams check_curve_validity_lq
#' @param s1 numeric: **TO BE DOCUMENTED**.
#' @param s2 numeric: **TO BE DOCUMENTED**.
#'
#' @return list
#' @keywords internal
gd_compute_poverty_stats_lq <- function(
    mean,
    povline,
    A,
    B,
    C,
    e,
    m,
    n,
    r,
    s1,
    s2
) {
  # ____________________________________________________________________________
  # Compute headcount
  # ____________________________________________________________________________
  headcount <- gd_compute_headcount_lq(
    mean    = mean,
    povline = povline,
    B       = B,
    m       = m,
    n       = n,
    r       = r
  )

  # ____________________________________________________________________________
  # Compute intermediate terms
  # ____________________________________________________________________________
  u    <- mean / povline
  tmp0 <- (m * headcount^2) + (n * headcount) + (e^2)
  tmp0 <- if (tmp0 < 0) 0L else tmp0
  tmp0 <- sqrt(tmp0)

  # ____________________________________________________________________________
  # Compute dl - first derivative of Lorenz curve
  # ____________________________________________________________________________
  dl <- -(0.5 * B) - (0.25 * ((2 * m * headcount) + n) / tmp0)

  # ____________________________________________________________________________
  # Compute ddl - second derivative of Lorenz curve
  # ____________________________________________________________________________
  ddl <- r^2 / (tmp0^3 * 8)

  # if negative headcount, set all to 0
  if (headcount < 0) {
    headcount <- pov_gap <- pov_gap_sq <- watts <- 0L
    eh <- epg <- ep <- gh <- gpg <- gp <- 0L
  } else {

    # __________________________________________________________________________
    # Compute Poverty gap
    # __________________________________________________________________________
    pov_gap <- gd_compute_pov_gap_lq(
      mean      = mean,
      povline   = povline,
      headcount = headcount,
      A         = A,
      B         = B,
      C         = C
    )

    # __________________________________________________________________________
    # Compute poverty severity
    # __________________________________________________________________________

    pov_gap_sq <- gd_compute_pov_severity_lq(
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
    # __________________________________________________________________________
    # Compute elasticity of headcount index wrt mean (P.eh)
    # __________________________________________________________________________
    eh <- -povline / (mean * headcount * ddl)

    # __________________________________________________________________________
    # Compute Elasticity of poverty gap index w.r.t mean (P.epg)
    # __________________________________________________________________________
    epg <- 1 - (headcount / pov_gap)

    # __________________________________________________________________________
    # Compute Elasticity of distributionally sensitive
    #            FGT poverty measure w.r.t mean (P.ep)
    # __________________________________________________________________________
    ep <- 2 * (1 - pov_gap / pov_gap_sq)

    # __________________________________________________________________________
    # Compute Elasticity of headcount index w.r.t gini index (P.gh)
    # __________________________________________________________________________
    gh <- (1 - povline / mean) / (headcount * ddl)

    # __________________________________________________________________________
    # Compute Elasticity of poverty gap index w.r.t gini index (P.gpg)
    # __________________________________________________________________________
    gpg <- 1 + (((mean / povline) - 1) * headcount / pov_gap)

    # __________________________________________________________________________
    # Compute Elasticity of distributionally sensitive
    #             FGT poverty measure w.r.t gini index (P.gp)
    # __________________________________________________________________________
    gp <- 2 * (1 + (((mean / povline) - 1) * pov_gap / pov_gap_sq))

    # ____________________________________________________________________________
    # Compute Watts
    # ____________________________________________________________________________
    watts <- gd_compute_watts_lq(headcount, mean, povline, 0.01, A, B, C)
  }

  return(
    list(
      headcount = headcount,
      pg        = pov_gap,
      p2        = pov_gap_sq,
      eh        = eh,
      epg       = epg,
      ep        = ep,
      gh        = gh,
      gpg       = gpg,
      gp        = gp,
      watts     = watts,
      dl        = dl,
      ddl       = ddl
    )
  )
}







#' Compute poverty for Quadratic Lorenz
#'
#' @inheritParams gd_compute_poverty_stats_lq
#'
#' @return poverty headcount
#' @export
gd_compute_headcount_lq <- function(
    mean,
    povline,
    B,
    m,
    n,
    r
) {

  #   _____________________________________________________________________
  #   Compute headcount
  #   _____________________________________________________________________
  bu <- B + (2 * povline / mean)
  headcount <- -(n + ((r * bu) / sqrt(bu^2 - m))) / (2 * m)


  #   _____________________________________________________________________
  #   Return
  #   _____________________________________________________________________
  return(headcount)

}



#' Compute poverty gap using Lorenz quadratic fit
#'
#' @inheritParams gd_compute_poverty_stats_lq
#' @inheritParams gd_compute_fit_lq
#' @inheritParams value_at_lq
#'
#' @return numeric
#' @export
gd_compute_pov_gap_lq <- function(mean,
                                  povline,
                                  headcount,
                                  A,
                                  B,
                                  C) {

  #   _____________________________________________________________________
  #   Computations
  #   _____________________________________________________________________


  if (headcount < 0 ) {
    pov_gap <- 0L
  } else {

    u    <- mean / povline

    hc_lq <- value_at_lq(headcount, A, B, C)

    # Poverty gap index (P.pg)
    pov_gap <- headcount - (u * hc_lq)
  }


  #   _____________________________________________________________________
  #   Return
  #   _____________________________________________________________________
  return(pov_gap)

}





#' Compute poverty severity for Lorenz Quadratic fit
#'
#' @param pov_gap numeric: Poverty gap.
#' @inheritParams gd_compute_fit_lq
#' @inheritParams gd_compute_poverty_stats_lq
#'
#' @return numeric
#' @export
gd_compute_pov_severity_lq <- function(
    mean,
    povline,
    headcount,
    pov_gap,
    A,
    B,
    C,
    e,
    m,
    n,
    r,
    s1,
    s2
) {

  # ________________________________________________________________________
  # Define objects
  # ________________________________________________________________________
  u <-  mean/povline
  hc_lq <- value_at_lq(headcount, A, B, C)

  # ________________________________________________________________________
  # Calculations
  # ________________________________________________________________________
  pov_gap_sq <-
    (2 * pov_gap) -
    headcount -
    (u ^ 2 * (A * headcount +
                B * hc_lq -
                ((r / 16) * log(
                  (1 - headcount / s1) / (1 - headcount / s2)
                ))))
  # ________________________________________________________________________
  # Return
  # ________________________________________________________________________
  return(pov_gap_sq)
}







#' Estimates poverty and inequality stats from Quadratic Lorenz fit
#'
#' @param mean numeric: Welfare mean.
#' @param povline numeric: Poverty line.
#' @param p0 numeric: **TO BE DOCUMENTED**.
#' @inheritParams gd_compute_fit_lq
#' @return list
#' @keywords internal
gd_estimate_lq <- function(mean, povline, p0, A, B, C) {

  # Compute key numbers from Lorenz quadratic form
  # Theorem 3 from original Lorenz quadratic paper
  e <- -(A + B + C + 1) # e = -(A + B + C + 1): condition for the curve to go through (1, 1)
  m <- (B^2) - (4 * A) # m < 0: condition for the curve to be an ellipse (m is called alpha in paper)
  n <- (2 * B * e) - (4 * C) # n is called Beta in paper
  r <- (n^2) - (4 * m * e^2) # r is called K in paper

  validity <- check_curve_validity_lq(A, B, C, e, m, n, r)
  if (validity$is_valid == FALSE & validity$is_normal == FALSE) {
    return(empty_gd_compute_pip_stats_response)
  }

  r <- sqrt(r)
  s1 <- (r - n) / (2 * m)
  s2 <- -(r + n) / (2 * m)

  # Compute distributional measures -----------------------------------------

  dist_stats <- gd_compute_dist_stats_lq(mean, p0, A, B, C, e, m, n, r)


  # Compute poverty stats ---------------------------------------------------

  pov_stats <- gd_compute_poverty_stats_lq(mean, povline, A, B, C, e, m, n, r, s1, s2)

  out <- list(
    gini = dist_stats$gini,
    median = dist_stats$median,
    rmhalf = dist_stats$rmhalf,
    polarization = dist_stats$polarization,
    ris = dist_stats$ris,
    mld = dist_stats$mld,
    dcm = dist_stats$dcm,
    deciles = dist_stats$deciles,
    headcount = pov_stats$headcount,
    poverty_gap = pov_stats$pg,
    poverty_severity = pov_stats$p2,
    eh = pov_stats$eh,
    epg = pov_stats$epg,
    ep = pov_stats$ep,
    gh = pov_stats$gh,
    gpg = pov_stats$gpg,
    gp = pov_stats$gp,
    watts = pov_stats$watts,
    dl = pov_stats$dl,
    ddl = pov_stats$ddl,
    is_normal = validity$is_normal,
    is_valid = validity$is_valid
  )

  return(out)
}

#' Computes the sum of squares of error
#'
#' Measures the fit of the model to the data.
#'
#' @param welfare numeric: Welfare vector (grouped).
#' @param population numeric: Population vector (grouped).
#' @param headcount numeric: Headcount index.
#' @param A numeric: Lorenz curve coefficient. Output of
#'   `regres_lq()$coef[1]`.
#' @param B numeric: Lorenz curve coefficient. Output of
#'   `regres_lq()$coef[2]`.
#' @param C numeric: Lorenz curve coefficient. Output of
#'   `regres_lq()$coef[3]`.
#'
#' @return list
#' @export
gd_compute_fit_lq <- function(welfare,
                              population,
                              headcount,
                              A,
                              B,
                              C) {
  if (is.na(headcount)) {
    return(list(
      sse  = NA_real_,
      ssez = NA_real_
    ))
  }

  lasti <- 0L
  sse <- 0L # Sum of square error
  ssez <- 0L

  for (i in seq_len(length(welfare) - 1)) {
    residual <- welfare[i] - value_at_lq(population[i], A, B, C)
    residual_sq <- residual^2
    sse <- sse + residual_sq
    if (population[i] < headcount) {
      ssez <- ssez + residual_sq
      lasti <- i
    }
  }
  lasti <- lasti + 1
  residual <- welfare[lasti] - value_at_lq(population[lasti], A, B, C)
  ssez <- ssez + residual^2

  out <- list(sse, ssez)
  names(out) <- list("sse", "ssez")

  return(out)
}


