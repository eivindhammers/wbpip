
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
    (
      u^2 * (
        A * headcount +
          B * hc_lq -
          (
            (r / 16) * log(
              (1 - headcount / s1) / (1 - headcount / s2)
            )
          )
      )
    )
  # ________________________________________________________________________
  # Return
  # ________________________________________________________________________
  return(pov_gap_sq)
}


