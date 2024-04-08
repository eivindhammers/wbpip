#' Compute default PIP statistics (microdata)
#'
#' Compute poverty and distributional statistics for microdata.
#'
#' @param welfare numeric: A vector of income or consumption values
#' @param povline numeric: Poverty line in international dollars
#' @param population numeric: A vector of population weights, optional, a vector
#' of 1s if not specified.
#' @param requested_mean numeric: Welfare mean in international dollars
#' @param popshare numeric: Share of population for which the corresponding
#' quantile is desired. Default .5 (i.e., weighted median)
#' @param default_ppp numeric: Default purchasing power parity
#' @param ppp numeric: PPP requested by user
#' @param p0 numeric: TO be documented
#' @param distribution_type character: Type of distribution, either micro,
#'   group, aggregate or imputed.
#'
#' @return list
#' @export
compute_pip_stats <- function(welfare,
                              povline,
                              population = NULL,
                              requested_mean = NULL,
                              popshare = NULL,
                              default_ppp = 1,
                              ppp = NULL,
                              p0 = 0.5,
                              distribution_type = c(
                                "micro",
                                "group",
                                "aggregate",
                                "imputed"
                              ),
                              force_lorenz_form = NULL) {
  distribution_type <- match.arg(distribution_type)

  if (distribution_type == "micro") {
    out <- md_compute_pip_stats(
      welfare = welfare,
      povline = povline,
      population = population,
      requested_mean = requested_mean,
      popshare = popshare,
      default_ppp = default_ppp,
      ppp = ppp
    )

    return(out)
  } else if (distribution_type %in% c("group", "aggregate")) {
    stopifnot("requested_mean must be specified if distribution_type is 'group' or 'aggregate'" = !is.null(requested_mean))

    out <- gd_compute_pip_stats(
      welfare = welfare,
      povline = povline,
      population = population,
      requested_mean = requested_mean,
      popshare = popshare,
      default_ppp = default_ppp,
      ppp = ppp,
      p0 = p0,
      force_lorenz_form = force_lorenz_form
    )

    return(out)
  } else if (distribution_type == "imputed") {
    return(NA)
  } else {
    return(NA)
  }
}
