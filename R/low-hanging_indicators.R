#' Calculate the number of poor using headcount and population
#'
#' @param headcount A numeric vector giving the headcount ratio
#' @param pop A numeric vector giving population size
#'
#' @return A numeric vector giving number of poor for each observation
#' @export
#'
#' @examples
#' if (suppressWarnings(require(pipr))) {
#'   pip <- get_stats()
#'   get_lh_number_poor(
#'     pip$headcount,
#'     pip$pop) |>
#'     head()
#' }
get_lh_number_poor <- function(headcount, pop){

  # Input checks
  stopifnot(is.numeric(headcount) & is.vector(headcount)) # numeric vector
  stopifnot(is.numeric(pop) & is.vector(pop)) # numeric vector
  stopifnot(length(headcount) == length(pop)) # vectors of same length
  if (any(headcount < 0) | any(pop < 0))
    warning("both headcount and pop should be positive")

  # Number of poor
  np <- headcount*pop

  # Return
  return(np)

}


#' Calculate average shortfall for the poor using headcount, poverty gap ratio, and poverty line
#'
#' @param headcount Numeric vector giving headcount ratio, i.e. P0 FGT measure
#' @param povgap Numeric vector giving poverty gap ratio, i.e. P1 FGT measure
#' @param povline Numeric vector giving the poverty line
#'
#' @return Numeric vector giving average shortfall for the poor in the units of the poverty line
#' @export
#'
#' @examples
#' if (suppressWarnings(require(pipr))) {
#'   pip <- get_stats()
#'   get_lh_average_shortfall(
#'     pip$headcount,
#'     pip$poverty_gap,
#'     pip$poverty_line) |>
#'     head()
#' }
get_lh_average_shortfall <- function(headcount, povgap, povline){

  # Input checks
  stopifnot(is.numeric(headcount) & is.vector(headcount)) # numeric vector
  stopifnot(is.numeric(povgap) & is.vector(povgap)) # numeric vector
  stopifnot(is.numeric(povline) & is.vector(povline)) # numeric vector
  stopifnot(length(headcount) == length(povline)) # vectors of same length
  stopifnot(length(headcount) == length(povgap)) # vectors of same length

  # Average Shortfall
  av_sf <- povline*povgap/headcount # z times P1/P0

  # Return
  return(av_sf)

}




#' Calculate total shortfall from headcount, poverty gap, and poverty line
#'
#' @param headcount Numeric vector giving headcount ratio, i.e. P0 FGT measure
#' @param pop Numeric vector giving population size
#' @param povgap Numeric vector giving poverty gap ratio, i.e. P1 FGT measure
#' @param povline Numeric vector giving the poverty line
#'
#' @return Numeric vector giving total shortfall in the same units as the poverty line
#' @export
#'
#' @examples
#' if (suppressWarnings(require(pipr))) {
#'   pip <- get_stats()
#'   get_lh_total_shortfall(
#'     pip$headcount,
#'     pip$pop,
#'     pip$poverty_gap,
#'     pip$poverty_line) |>
#'     head()
#' }
get_lh_total_shortfall <- function(headcount, pop, povgap, povline){

  # Input checks
  stopifnot(is.numeric(headcount) & is.vector(headcount)) # numeric vector
  stopifnot(is.numeric(pop) & is.vector(pop)) # numeric vector
  stopifnot(is.numeric(povgap) & is.vector(povgap)) # numeric vector
  stopifnot(is.numeric(povline) & is.vector(povline)) # numeric vector
  stopifnot(length(headcount) == length(povline)) # vectors of same length
  stopifnot(length(headcount) == length(pop)) # vectors of same length
  stopifnot(length(headcount) == length(povgap)) # vectors of same length

  # Total Shortfall
  tot_sf <- get_lh_number_poor(
    headcount = headcount, pop = pop
  )*get_lh_average_shortfall(
    headcount, povgap, povline
  )

  # Return
  return(tot_sf)
}



#' Calculate income gap ratio from headcount and poverty gap
#'
#' @param headcount Numeric vector giving headcount ratio, i.e. P0 FGT measure
#' @param povgap Numeric vector giving poverty gap ratio, i.e. P1 FGT measure
#'
#' @return Numeric vector giving the income gap ratio
#' @export
#'
#' @examples
#' if (suppressWarnings(require(pipr))) {
#'   pip <- get_stats()
#'   get_lh_income_gap_ratio(
#'     pip$headcount,
#'     pip$poverty_gap) |>
#'     head()
#' }
get_lh_income_gap_ratio <- function(headcount, povgap){

  # Input checks
  stopifnot(is.numeric(headcount) & is.vector(headcount)) # numeric vector
  stopifnot(is.numeric(povgap) & is.vector(povgap)) # numeric vector
  stopifnot(length(headcount) == length(povgap)) # vectors of same length

  # income gap
  income_gap <- povgap/headcount

  # return
  return(income_gap)

}



#' Calculate Palma ratio using decile information
#'
#' @param decile1 Numeric vector giving either welfare or welfare share for first decile
#' @param decile2 Numeric vector giving either welfare or welfare share for second decile
#' @param decile3 Numeric vector giving either welfare or welfare share for third decile
#' @param decile4 Numeric vector giving either welfare or welfare share for fourth decile
#' @param top10 Numeric vector giving either welfare or welfare share for tenth decile
#' @param bottom40 numeric: sum of deciles 1 to 4.
#'
#' @return Numeric vector giving the Palma ratio, the ratio of top 10% to bottom 40% welfare
#' @export
#'
#' @examples
#' if (suppressWarnings(require(pipr))) {
#'   pip <- get_stats()
#'
#'   get_lh_palma_ratio(
#'     top10   = pip$decile10,
#'     decile1 = pip$decile1,
#'     decile2 = pip$decile2,
#'     decile3 = pip$decile3,
#'     decile4 = pip$decile4,
#'     ) |>
#'     head()
#'
#' # Build the bottom 40 first
#'
#' pip <- pip |>
#' transform(bottom40 = decile1 + decile2 + decile3 + decile4)
#'
#'   get_lh_palma_ratio(
#'     top10    = pip$decile10,
#'     bottom40 = pip$bottom40
#'     ) |>
#'     head()
#' }
get_lh_palma_ratio <- function(top10,
                               bottom40 = NULL,
                               decile1  = NULL,
                               decile2  = NULL,
                               decile3  = NULL,
                               decile4  = NULL){

  # Input checks
  stopifnot( is.numeric(top10) & is.vector(top10)) # numeric vector)
  if (is.null(bottom40)) {
    stopifnot(
      exprs = {
        is.numeric(decile1) & is.vector(decile1) # numeric vecto
        is.numeric(decile2) & is.vector(decile2) # numeric vector
        is.numeric(decile3) & is.vector(decile3) # numeric vector
        is.numeric(decile4) & is.vector(decile4) # numeric vector
        length(decile1) == length(decile2) # vectors of same length
        length(decile1) == length(decile3) # vectors of same length
        length(decile1) == length(decile4) # vectors of same length
        length(decile1) == length(top10)
      }
    )

    # create bottom 40
    bottom40 <- decile1 + decile2 + decile3 + decile4
  } else {
    stopifnot(
      exprs = {
        is.numeric(bottom40) & is.vector(bottom40) # numeric vecto
        length(bottom40) == length(top10)
      }
    )
  }

  # Palma ratio
  palma <- top10/bottom40

  # return
  return(palma)


}


#' Calculate ratio of the top 10% to the bottom 10% of the welfare distribution
#'
#' @param top10 Numeric vector giving the welfare of the 10th decile - i.e. the top 10%
#' @param bottom10 Numeric vector giving the welfare of the 1st decile - i.e. the bottom 10%
#'
#' @return Numeric vector giving the ratio of the top 10% to the bottom 10% of the welfare distribution
#' @export
#'
#' @examples
#' if (suppressWarnings(require(pipr))) {
#'   pip <- get_stats()
#'
#'   get_lh_9010_ratio(
#'     top10    = pip$decile10,
#'     bottom10 = pip$decile1
#'     ) |>
#'     head()
#' }
get_lh_9010_ratio <- function(
    top10,
    bottom10){

  # Input Checks
  stopifnot(
    exprs = {
      is.numeric(top10) # numeric
      is.numeric(bottom10) # numeric
      is.vector(top10) # vector
      is.vector(bottom10) # vector
      length(top10) == length(bottom10) # two inputs are vectors of same length
    }
  )

  # Decile ratio
  ratio <- top10/bottom10

  # return
  return(ratio)


}


