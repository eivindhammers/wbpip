#' Compute poverty line in Local Currency Unit (LCU)
#'
#' Compute the LCU poverty line for microdata.
#'
#' @param welfare numeric: A vector of income or consumption values.
#' @param povline numeric: Poverty line in international dollars (PPP).
#' @param weight numeric: A vector of weights.
#' @param popshare numeric: Share of population for which the corresponding.
#'   quantile is desired. Default .5 (i.e., weighted median).
#' @param requested_mean numeric: requested mean (adjusted with PPPs).
#' @param data_mean numeric: Data mean.
#'
#' @return list
#' @keywords internal
#' @examples
#'
#' # Load and clean example data
#' data("md_ABC_2000_income")
#' df <- wbpip:::md_clean_data(
#'   md_ABC_2000_income,
#'   welfare = 'welfare',
#'   weight = 'weight')$data
#'
#' # Compute LCU poverty line
#' res <-  wbpip:::md_compute_povline_lcu(
#'   df$welfare, df$weight,
#'   popshare = NULL,
#'   requested_mean = 5000,
#'   data_mean = 4000)
#' str(res)
md_compute_povline_lcu <- function(welfare,
                                   povline,
                                   weight,
                                   popshare,
                                   requested_mean,
                                   data_mean) {
  if (!is.null(popshare)) {
    # Infer poverty line from share of population living in poverty
    pl_lcu <- md_infer_poverty_line(
      welfare = welfare,
      weight = weight,
      popshare = popshare
    )

    povline <- pl_lcu * requested_mean / data_mean
  } else {
    # Convert user defined international poverty line in Local Currency Units
    pl_lcu <- povline * data_mean / requested_mean
  }

  return(list(
    povline_lcu = pl_lcu,
    povline     = povline
  ))
}
