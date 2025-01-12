% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gd_compute_pip_stats_lb.R
\name{gd_compute_pov_severity_lb}
\alias{gd_compute_pov_severity_lb}
\title{Compute poverty severity for Lorenz Beta fit}
\usage{
gd_compute_pov_severity_lb(
  mean,
  povline,
  headcount,
  pov_gap,
  A,
  B,
  C,
  u = NULL
)
}
\arguments{
\item{mean}{numeric: Welfare mean.}

\item{povline}{numeric: Poverty line.}

\item{headcount}{numeric: Headcount index.}

\item{pov_gap}{numeric: Poverty gap.}

\item{A}{numeric: Lorenz curve coefficient. Output of
\code{regres()$coef[1]}.}

\item{B}{numeric: Lorenz curve coefficient. Output of
\code{regres()$coef[2]}.}

\item{C}{numeric: Lorenz curve coefficient. Output of
\code{regres()$coef[3]}.}

\item{u}{numeric: Mean? \strong{TO BE DOCUMENTED}.}
}
\value{
numeric
}
\description{
Compute poverty severity for Lorenz Beta fit
}
