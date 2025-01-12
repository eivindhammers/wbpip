% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/md_compute_poverty_stats.R
\name{md_compute_fgt}
\alias{md_compute_fgt}
\alias{md_compute_headcount}
\alias{md_compute_pov_gap}
\alias{md_compute_pov_severity}
\alias{md_compute_watts}
\title{Compute FGT poverty family measures and Watts index for Microdata}
\usage{
md_compute_fgt(
  fgt_data = NULL,
  welfare = NULL,
  weight = rep(1, length(welfare)),
  povline = fmedian(welfare, w = weight)/2,
  alpha = 0,
  return_data = FALSE,
  include_povline = FALSE
)

md_compute_headcount(
  welfare,
  weight = rep(1, length(welfare)),
  povline = fmedian(welfare, w = weight)/2,
  return_data = FALSE,
  include_povline = FALSE
)

md_compute_pov_gap(
  welfare,
  weight = rep(1, length(welfare)),
  povline = fmedian(welfare, w = weight)/2,
  return_data = FALSE,
  include_povline = FALSE
)

md_compute_pov_severity(
  welfare,
  weight = rep(1, length(welfare)),
  povline = fmedian(welfare, w = weight)/2,
  return_data = FALSE,
  include_povline = FALSE
)

md_compute_watts(welfare, weight = rep(1, length(welfare)), povline)
}
\arguments{
\item{fgt_data}{list of previously computed fgt calculations}

\item{welfare}{numeric vector with either income or consumption}

\item{weight}{numeric vector with sample weights. Default is 1.}

\item{povline}{poverty line. Default is the half the weighted median of
\code{welfare}}

\item{alpha}{numeric. Alpha parameter of FGT measures. if \code{0}, the default,
it estimates the poverty headcount. If \code{1}, the poverty gap, and if \code{2},
the poverty severity. In practice, you can use higher levels of \code{alpha},
but their theoretical interpretation usually goes up to a value of \code{2}.}

\item{return_data}{logical: whether to return a list to be used in subsequent
calls of \link{md_compute_fgt} in the parameter \code{fgt_data}.}

\item{include_povline}{logical: Whether to include the poverty line as
threshold for poverty measure. The default is \code{FALSE}, as absolute poverty
is defined as those household \emph{below} the poverty line. Yet, it might be
useful to include the value of the poverty line for a very limited set of
analysis (\emph{seldom used}).}
}
\value{
either a vector with the fgt measure selected in argument \code{alpha} or
a list of dgt estimations if \code{return_data} is \code{TRUE}
}
\description{
Compute FGT poverty family measures and Watts index for Microdata
}
\details{
\link{md_compute_fgt} works in two ways. It could either receive a list
of previously computed calculations in argument \code{fgt_data} or receive the
standard poverty calculation inputs such as \code{welfare}, \code{weights} and
\code{povline}. The first modality ensures efficiency in computations as the
poverty status of each observation and their relative distance to the
poverty line is calculated only once.
}
\section{wrappers}{


There are a few functions that are basically wrappers of \link{md_compute_fgt}.
They do not serve any purpose beyond ease to the user to identify the right
measure.

\link{md_compute_headcount} Computes poverty headcount, which is equivalent to
\code{md_compute_fgt(alpha = 0)}

\link{md_compute_pov_gap}   Computes poverty gap, which is equivalent to
\code{md_compute_fgt(alpha = 1)}

\link{md_compute_pov_severity} Computes poverty severity, which is equivalent to
\code{md_compute_fgt(alpha = 2)}

\link{md_compute_watts} is not a wrapper of \link{md_compute_fgt} but it is part of
the poverty measures, so it is included in this documentation. Notice that
the arguments are the same as of the functions above.
}

\section{inclusion of poverty line}{
 when \code{include_povline} is \code{TRUE}, the
value of the \code{povline} is artificially modify to \code{povline + e} where \code{e} is
a very small number (\code{1e-10}), ensure the inclusion of the line.
}

\examples{
welfare <- md_ABC_2010_income$welfare/1e6
weight  <- md_ABC_2010_income$weight

wna     <- !is.na(welfare)
welfare <- welfare[wna]
weight  <- weight[wna]

md_compute_fgt(welfare = welfare,
               weight  = weight,
               povline = 5)

fgt <- md_compute_fgt(welfare     = welfare,
                      weight      = weight,
                      povline     = 5,
                      return_data =  TRUE) |>
  md_compute_fgt(alpha = 1,
                 return_data =  TRUE) |>
  md_compute_fgt(alpha = 2,
                 return_data =  TRUE)

c(fgt$FGT0, fgt$FGT1, fgt$FGT2)
}
