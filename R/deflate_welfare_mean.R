#' Deflate welfare mean
#'
#' Adjust the welfare mean of a survey to constant PPP-adjusted dollars to make
#' surveys comparable over time and across countries.
#'
#' Note that the CPI value to be used must be pre-adjusted so that welfare
#' aggregates are comparable across surveys within a given country. This implies
#' that adjustments to account for temporal and spatial factors within the
#' survey as well as currency changes between surveys must already have been
#' made.
#'
#' @param welfare_mean numeric: The weighted welfare mean from a survey.
#' @param ppp numeric: PPP value
#' @param cpi numeric: CPI value.
#'
#' @return numeric
#' @references
#' Azevedo, J. P., P. A. Corral, D. Jolliffe, C. Lakner, D. G. Mahler,
#' J. Montes, M. C. Nguyen, E. B. Prydz. 2018.
#' "[Prices used in Global Poverty Measurement](http://documents1.worldbank.org/curated/en/651541537208471889/pdf/129963-WP-PUBLIC-Disclosed-9-19-2018.pdf)".
#' Global Poverty Monitoring Technical Note 3.
#' World Bank, Washington, DC.
#'
#' @examples
#' # Simple examples
#' deflate_welfare_mean(100, ppp = 5, cpi = 1.5)
#' deflate_welfare_mean(10.3, ppp = 1.2, cpi = 0.1)
#'
#' # Works on vectors as well
#' deflate_welfare_mean(c(100, 10.3), ppp = c(5, 1.2), cpi = c(1.5, 0.1))#'
#' @export
deflate_welfare_mean <- function(welfare_mean, ppp, cpi) {
  deflated_welfare_mean <- welfare_mean / ppp / cpi
  return(deflated_welfare_mean)
}
