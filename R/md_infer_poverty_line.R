#' Infer poverty line
#'
#' Infer poverty line for microdata.
#'
#' This function calculate percentiles corresponding to the specified share of
#' population (in percentages).
#'
#' @inheritParams compute_pip_stats
#' @param include logical: **TO BE DOCUMENTED**.
#'
#' @examples
#' wbpip:::md_infer_poverty_line(1:2000, weight = rep(1, 2000))
#' wbpip:::md_infer_poverty_line(1:2000,
#'   weight = rep(1, 2000),
#'   popshare = .2
#' )
#' wbpip:::md_infer_poverty_line(1:2000,
#'   weight = rep(1, 2000),
#'   popshare = .6
#' )
#' @return numeric
#' @keywords internal
md_infer_poverty_line <- function(welfare,
                                  weight,
                                  popshare = .5,
                                  include = FALSE) {


  prob <- cumsum(weight) / sum(weight)
  ps <- lapply(popshare, \(.) {
    abs(prob - .) |>
      which.min()
  })

  # Weighted mean with the next available value in order to
  # guarantee inclusion in poverty calculation

  if (include) {

    pctile <-
      sapply(ps, \(.) {
        stats::weighted.mean(
          x = c(welfare[.], welfare[. + 1]),
          w = c(weight[.], weight[. + 1])
        )
      })


  } else {
    pctile <-
      sapply(ps, \(.) {
        mean(welfare[.])
      })
  }


  return(pctile)
}
