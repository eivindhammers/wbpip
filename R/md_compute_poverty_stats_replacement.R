#' Compute Poverty Statistics
#'
#' Compute poverty statictics for microdata - replace old [md_compute_poverty_stats]
#'
#' Given a vector of consumption or income values and their respective weights
#' `md_compute_poverty_stats_replacement()` computes poverty headcount, poverty gap,
#' poverty severity and the watts index.
#'
#' @inheritParams compute_pip_stats
#' @param povline_lcu numeric: Poverty line in Local Currency Unit (LCU).
#'
#' @examples
#' wbpip:::md_compute_poverty_stats_replacement(
#'   welfare = 1:2000,
#'   weight = rep(1, 2000),
#'   povline_lcu = 10
#' )
#' @return list
#' @export
md_compute_poverty_stats_replacement <- function(
    welfare     = NULL,
    weight      = NULL,
    povline_lcu = NULL
  ) {
  # ______________________________________________________________________
  # Arguments
  # ______________________________________________________________________
  if (is.null(welfare) | is.null(povline_lcu)) {
    cli::cli_abort(
      "`welfare` and `povline` arguments must be non-NULL"
    )
  }
  if (is.null(weight)) {
    weight <- rep(1, length(welfare))
    cli::cli_alert_info(
      "The `weight` argument is NULL, thus each observation is given equal weight by default. "
    )
  }

  # ______________________________________________________________________
  # Get intermediate
  # ______________________________________________________________________

  pov_status        <- (welfare < povline_lcu)
  relative_distance <- (1 - (welfare[pov_status] / povline_lcu))
  weight_pov        <- weight[pov_status]
  weight_total      <- sum(weight)


  # ______________________________________________________________________
  # Computations
  # ______________________________________________________________________

  hc    <- md_compute_headcount(
    welfare      = welfare,
    weight       = weight,
    povline      = povline_lcu,
    weight_pov   = weight_pov,
    weight_total = weight_total
  )
  pg    <- md_compute_pov_gap(
    welfare           = welfare,
    weight            = weight,
    povline           = povline_lcu,
    weight_pov        = weight_pov,
    weight_total      = weight_total,
    relative_distance = relative_distance
  )
  ps    <- md_compute_pov_severity(
    welfare           = welfare,
    weight            = weight,
    povline           = povline_lcu,
    weight_pov        = weight_pov,
    weight_total      = weight_total,
    relative_distance = relative_distance
  )
  watts <- md_compute_watts(
    welfare           = welfare,
    weight            = weight,
    povline           = povline_lcu
  )

  # ______________________________________________________________________
  # Return list
  # ______________________________________________________________________
  return(
    list(
      headcount        = hc,
      poverty_gap      = pg,
      poverty_severity = ps,
      watts            = watts
    )
  )
}



#' Compute headcount from microdata
#'
#' @inheritParams compute_pip_stats
#' @param weight numeric: A vector of population weights, optional, a vector of 1s if not specified.
#' @param weight_pov numeric: A vector of population weights for the population
#' in poverty. Default is NULL, primary purpose is internal
#' @param weight_total numeric: sum of population weights
#'
#' @return numeric
#' @export
md_compute_headcount <- function(
    welfare      = NULL,
    weight       = NULL,
    povline      = NULL,
    weight_pov   = NULL,
    weight_total = NULL
){

  # ______________________________________________________________________
  # Arguments
  # ______________________________________________________________________
  if (is.null(welfare) | is.null(povline)) {
    cli::cli_abort(
      "`welfare` and `povline` arguments must be non-NULL"
    )
  }
  if (is.null(weight)) {
    weight <- rep(1, length(welfare))
    cli::cli_alert_info(
      "The `weight` argument is NULL, thus each observation is given equal weight by default. "
    )
  }
  if (is.null(weight_pov) | is.null(weight_total)) {
    pov_status        <- (welfare < povline)
    weight_pov        <- weight[pov_status]
    weight_total      <- sum(weight)
  }
  # ______________________________________________________________________
  # Computations
  # ______________________________________________________________________
  hc <- sum(weight_pov) / weight_total

  # ______________________________________________________________________
  # Return
  # ______________________________________________________________________
  return(hc)

}



#' Compute poverty gap from microdata
#'
#' @inheritParams md_compute_headcount
#' @param relative_distance numeric: vector of relative, standardized distances
#' to poverty line for the poor. Default is NULL, which means it will be computed.
#'
#' @return numeric: poverty gap
#' @export
md_compute_pov_gap <- function(
    welfare           = NULL,
    weight            = NULL,
    povline           = NULL,
    weight_pov        = NULL,
    weight_total      = NULL,
    relative_distance = NULL
) {
  # ______________________________________________________________________
  # Arguments
  # ______________________________________________________________________
  if (is.null(welfare) | is.null(povline)) {
    cli::cli_abort(
      "`welfare` and `povline` arguments must be non-NULL"
    )
  }
  if (is.null(weight)) {
    weight <- rep(1, length(welfare))
    cli::cli_alert_info(
      "The `weight` argument is NULL, thus each observation is given equal weight by default. "
    )
  }
  if (is.null(weight_pov) | is.null(weight_total) | is.null(relative_distance)) {
    pov_status        <- (welfare < povline)
    weight_pov        <- weight[pov_status]
    weight_total      <- sum(weight)
    relative_distance <- (1 - (welfare[pov_status] / povline))
  }
  # ______________________________________________________________________
  # Computations
  # ______________________________________________________________________
  povgap <- sum(relative_distance * weight_pov) / weight_total

  # ______________________________________________________________________
  # Return
  # ______________________________________________________________________
  return(povgap)
}

#' Compute poverty severity
#'
#' @inheritParams md_compute_pov_gap
#'
#' @return numeric: poverty severity
#' @export
md_compute_pov_severity <- function(
    welfare           = NULL,
    weight            = NULL,
    povline           = NULL,
    weight_pov        = NULL,
    weight_total      = NULL,
    relative_distance =  NULL
) {
  # ______________________________________________________________________
  # Arguments
  # ______________________________________________________________________
  if (is.null(welfare) | is.null(povline)) {
    cli::cli_abort(
      "`welfare` and `povline` arguments must be non-NULL"
    )
  }
  if (is.null(weight)) {
    weight <- rep(1, length(welfare))
    cli::cli_alert_info(
      "The `weight` argument is NULL, thus each observation is given equal weight by default. "
    )
  }
  if (is.null(weight_pov) | is.null(weight_total) | is.null(relative_distance)) {
    pov_status        <- (welfare < povline)
    weight_pov        <- weight[pov_status]
    weight_total      <- sum(weight)
    relative_distance <- (1 - (welfare[pov_status] / povline))
  }
  # ______________________________________________________________________
  # Computations
  # ______________________________________________________________________
  povsev <- sum(relative_distance^2 * weight_pov) / weight_total

  # ______________________________________________________________________
  # Return
  # ______________________________________________________________________
  return(povsev)
}

#' Compute Watts Index on micro data
#'
#' @inheritParams md_compute_headcount
#'
#' @return numeric: watts index
#' @export
md_compute_watts <- function(
    welfare           = NULL,
    weight            = NULL,
    povline           = NULL
) {
  # ______________________________________________________________________
  # Arguments
  # ______________________________________________________________________
  if (is.null(welfare) | is.null(povline)) {
    cli::cli_abort(
      "`welfare` and `povline` arguments must be non-NULL"
    )
  }
  if (is.null(weight)) {
    weight <- rep(1, length(welfare))
    cli::cli_alert_info(
      "The `weight` argument is NULL, thus each observation is given equal weight by default. "
    )
  }


  # ______________________________________________________________________
  # Computations
  # ______________________________________________________________________
  pov_status         <- (welfare < povline)
  weight_total       <- sum(weight)
  keep               <- welfare > 0 & pov_status
  w_gt_zero          <- welfare[keep]
  sensitive_distance <- log(povline / w_gt_zero)
  watts              <- sum(sensitive_distance * weight[keep])/weight_total

  # Handle cases where Watts is numeric(0)
  if (identical(watts, numeric(0))) {
    watts <- 0
  }

  # ______________________________________________________________________
  # Return
  # ______________________________________________________________________
  return(watts)
}

