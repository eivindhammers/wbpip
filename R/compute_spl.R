#' Compute SPL
#'
#' Compute the societal poverty line based on the median welfare in PPP-adjusted
#' dollars.
#'
#' @param weighted_median_ppp numeric: Median. Weighted median in PPP terms.
#' @param threshold_rate numeric: A value between 0 and 1 for the proportion of
#'   the median welfare for SPL calculation.
#' @param min_level numeric: Minimum level of the SPL. Defaults to the current
#'   IPL ($1.90).
#'
#' @references D. Jolliffe, E. B. Prydz. 2017. "[Societal Poverty: A Relative
#' and Relevant
#' Measure](https://documents1.worldbank.org/curated/en/133671495562984832/pdf/Societal-poverty-a-relative-and-relevant-measure.pdf)".
#' Policy Research Working Paper 8073. World Bank, Washington, DC.
#'
#' @return numeric
#' @keywords internal
compute_spl <- function(weighted_median_ppp,
                        threshold_rate = 0.5,
                        min_level = 1.9) {

  assertthat::assert_that(
    !(threshold_rate >= 1 | threshold_rate <= 0),
    msg = "`threshold_rate` must between 0 and 1.")

  # Calculate SPL according to threshold rate
  spl <- 1 + threshold_rate * weighted_median_ppp

  # Set minimum level if needed
  if (spl < min_level) spl <- min_level

  return(spl)

}
