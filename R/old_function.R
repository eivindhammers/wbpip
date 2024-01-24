#' Computes poverty stats from Lorenz Quadratic fit
#'
#' @inheritParams gd_estimate_lq
#' @inheritParams check_curve_validity_lq
#' @param s1 numeric: **TO BE DOCUMENTED**.
#' @param s2 numeric: **TO BE DOCUMENTED**.
#'
#' @return list
#' @keywords internal
old_gd_compute_poverty_stats_lq <- function(mean,
                                        povline,
                                        A,
                                        B,
                                        C,
                                        e,
                                        m,
                                        n,
                                        r,
                                        s1,
                                        s2) {
  # Compute headcount
  bu <- B + (2 * povline / mean)
  u <- mean / povline

  headcount <- -(n + ((r * bu) / sqrt(bu^2 - m))) / (2 * m)

  tmp0 <- (m * headcount^2) + (n * headcount) + (e^2)
  tmp0 <- if (tmp0 < 0) 0L else tmp0
  tmp0 <- sqrt(tmp0)

  # First derivative of the Lorenz curve
  dl <- -(0.5 * B) - (0.25 * ((2 * m * headcount) + n) / tmp0)

  # Second derivative of the Lorenz curve
  ddl <- r^2 / (tmp0^3 * 8)

  if (headcount < 0) {
    headcount <- pov_gap <- pov_gap_sq <- watts <- 0L
    eh <- epg <- ep <- gh <- gpg <- gp <- 0L
  } else {

    # HC value at LQ
    hc_lq <- value_at_lq(headcount, A, B, C)

    # Poverty gap index (P.pg)
    pov_gap <- headcount - (u * hc_lq)

    # P.p2 - Distributionally sensitive FGT poverty measure
    # P.p2 <- (2*P.pg) - P.h - u^2 * (A*P.h + B*value_at_lq(P.h, A, B, C) - (r/16 *log((1 - P.h/s1))/(1 - P.h/s2)))
    # Poverty severity
    pov_gap_sq <- (2 * pov_gap) - headcount -
      (u^2 * (A * headcount + B * hc_lq -
                ((r / 16) * log((1 - headcount / s1) / (1 - headcount / s2)))))

    # Elasticity of headcount index w.r.t mean (P.eh)
    eh <- -povline / (mean * headcount * ddl)

    # Elasticity of poverty gap index w.r.t mean (P.epg)
    epg <- 1 - (headcount / pov_gap)

    # Elasticity of distributionally sensitive FGT poverty measure w.r.t mean (P.ep)
    ep <- 2 * (1 - pov_gap / pov_gap_sq)

    # PElasticity of headcount index w.r.t gini index (P.gh)
    gh <- (1 - povline / mean) / (headcount * ddl)

    # Elasticity of poverty gap index w.r.t gini index (P.gpg)
    gpg <- 1 + (((mean / povline) - 1) * headcount / pov_gap)

    # Elasticity of distributionally sensitive FGT poverty measure w.r.t gini index (P.gp)
    gp <- 2 * (1 + (((mean / povline) - 1) * pov_gap / pov_gap_sq))

    watts <- gd_compute_watts_lq(headcount, mean, povline, 0.01, A, B, C)
  }

  return(
    list(
      headcount = headcount,
      pg = pov_gap,
      p2 = pov_gap_sq,
      eh = eh,
      epg = epg,
      ep = ep,
      gh = gh,
      gpg = gpg,
      gp = gp,
      watts = watts,
      dl = dl,
      ddl = ddl
    )
  )
}
