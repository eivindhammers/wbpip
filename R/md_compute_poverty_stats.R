#' Compute Poverty Statistics
#'
#' Compute poverty statictics for microdata - replace old [md_compute_poverty_stats]
#'
#' Given a vector of consumption or income values and their respective weights
#' `md_compute_poverty_stats()` computes poverty headcount, poverty gap,
#' poverty severity and the watts index.
#'
#' @inheritParams compute_pip_stats
#' @inheritParams md_compute_headcount
#' @param povline_lcu numeric: Poverty line in Local Currency Unit (LCU).
#'
#' @examples
#' wbpip:::md_compute_poverty_stats(
#'   welfare = 1:2000,
#'   weight = rep(1, 2000),
#'   povline_lcu = 10
#' )
#' @return list
#' @export
md_compute_poverty_stats <- function(
    welfare     = NULL,
    weight      = NULL,
    povline_lcu = NULL,
    verbose     = FALSE
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
    if (verbose) {
      cli::cli_alert_info(
        "The `weight` argument is NULL, thus each observation is given equal weight by default. "
      )
    }
  }

  # ______________________________________________________________________
  # Get intermediate
  # ______________________________________________________________________

  pov_status        <- (welfare < povline_lcu)
  relative_distance <- (1 - (welfare[pov_status] / povline_lcu))
  weight_pov        <- weight[pov_status]
  weight_total      <- fsum(weight)

  # ______________________________________________________________________
  # Computations
  # ______________________________________________________________________

  hc    <- md_compute_headcount(
    welfare      = welfare,
    weight       = weight,
    povline      = povline_lcu,
    weight_pov   = weight_pov,
    weight_total = weight_total,
    verbose      = verbose
  )
  pg    <- md_compute_pov_gap(
    welfare           = welfare,
    weight            = weight,
    povline           = povline_lcu,
    weight_pov        = weight_pov,
    weight_total      = weight_total,
    relative_distance = relative_distance,
    verbose           = verbose
  )
  ps    <- md_compute_pov_severity(
    welfare           = welfare,
    weight            = weight,
    povline           = povline_lcu,
    weight_pov        = weight_pov,
    weight_total      = weight_total,
    relative_distance = relative_distance,
    verbose           = verbose
  )
  watts <- md_compute_watts(
    welfare           = welfare,
    weight            = weight,
    povline           = povline_lcu,
    verbose           = verbose
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
#' @param verbose logical: display messages. Default is FALSE
#'
#' @return numeric
#' @export
md_compute_headcount <- function(
    welfare      = NULL,
    weight       = NULL,
    povline      = NULL,
    weight_pov   = NULL,
    weight_total = NULL,
    verbose      = FALSE
){

  # ______________________________________________________________________
  # Arguments
  # ______________________________________________________________________
  if (is.null(povline)) {
    cli::cli_abort(
      "povline` argument must be non-NULL"
    )
  }
  if (is.null(weight) & (is.null(weight_pov) | is.null(weight_total))) {
    weight <- rep(1, length(welfare))
    if (verbose) {
      cli::cli_alert_info(
        "The `weight` argument is NULL, thus each observation is given equal weight by default. "
      )
    }
  }
  if (is.null(weight_pov) | is.null(weight_total)) {
    pov_status        <- (welfare < povline)
    weight_pov        <- weight[pov_status]
    weight_total      <- sum(weight)
    if (verbose) {
      cli::cli_alert_info(
        "The `weight_pov` and/or `weight_total` arguments are NULL, therefore calculated internally"
      )
    }
  } else if (verbose) {
    cli::cli_alert_info(
      "The `weight_pov` and/or `weight_total` arguments are used for directly for headcount calculation"
    )
  }
  # ______________________________________________________________________
  # Computations
  # ______________________________________________________________________
  hc <- fsum(weight_pov) / weight_total

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
    relative_distance = NULL,
    verbose           = FALSE
) {
  # ______________________________________________________________________
  # Arguments
  # ______________________________________________________________________
  if (is.null(povline)) {
    cli::cli_abort(
      "`povline` argument must be non-NULL"
    )
  }
  if (is.null(weight) & (is.null(weight_pov) |
                         is.null(weight_total) |
                         is.null(relative_distance))) {
    weight <- rep(1, length(welfare))
    if (verbose) {
      cli::cli_alert_info(
        "The `weight` argument is NULL, thus each observation is
        given equal weight by default."
      )
    }
  }
  if (is.null(weight_pov) | is.null(weight_total) | is.null(relative_distance)) {
    pov_status        <- (welfare < povline)
    weight_pov        <- weight[pov_status]
    weight_total      <- fsum(weight)
    relative_distance <- (1 - (welfare[pov_status] / povline))

    if (verbose) {
      cli::cli_alert_info(
        "The `weight_pov`, `weight_total`, and `relative_distance` arguments calculated internally "
      )
    }
  } else if (verbose) {
    cli::cli_alert_info(
      "The supplied `weight_pov`, `weight_total`, and `relative_distance` arguments are used"
    )
  }

  # ______________________________________________________________________
  # Computations
  # ______________________________________________________________________
  povgap <- fsum(relative_distance * weight_pov) / weight_total

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
    relative_distance = NULL,
    verbose           = FALSE
) {
  # ______________________________________________________________________
  # Arguments
  # ______________________________________________________________________
  if (is.null(povline)) {
    cli::cli_abort(
      "`povline` argument must be non-NULL"
    )
  }
  if (is.null(weight) & (is.null(weight_pov) | is.null(weight_total) | is.null(relative_distance))) {
    weight <- rep(1, length(welfare))
    if (verbose) {
      cli::cli_alert_info(
        "The `weight` argument is NULL, thus each observation is given equal weight by default. "
      )
    }
  }
  if (is.null(weight_pov) | is.null(weight_total) | is.null(relative_distance)) {
    pov_status        <- (welfare < povline)
    weight_pov        <- weight[pov_status]
    weight_total      <- fsum(weight)
    relative_distance <- (1 - (welfare[pov_status] / povline))
    if (verbose) {
      cli::cli_alert_info(
        "The `weight_pov`, `weight_total`, and `relative_distance` arguments calculated internally."
      )
    }
  } else if (verbose) {
    cli::cli_alert_info(
      "The supplied `weight_pov`, `weight_total`, and `relative_distance` arguments are used."
    )
  }
  # ______________________________________________________________________
  # Computations
  # ______________________________________________________________________
  povsev <- fsum(relative_distance^2 * weight_pov) / weight_total

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
    povline           = NULL,
    verbose           = FALSE
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
    if (verbose) {
      cli::cli_alert_info(
        "The `weight` argument is NULL, thus each observation is given equal weight by default. "
      )
    }
  }


  # ______________________________________________________________________
  # Computations
  # ______________________________________________________________________
  pov_status         <- (welfare < povline)
  weight_total       <- fsum(weight)
  keep               <- welfare > 0 & pov_status
  w_gt_zero          <- welfare[keep]
  sensitive_distance <- log(povline / w_gt_zero)
  watts              <- fsum(sensitive_distance * weight[keep])/weight_total

  # # Handle cases where Watts is numeric(0)
  # if (identical(watts, numeric(0))) {
  #   watts <- 0
  # }

  # ______________________________________________________________________
  # Return
  # ______________________________________________________________________
  return(watts)
}

