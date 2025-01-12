% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gd_compute_pip_stats_lq.R
\name{gd_compute_pov_gap_lq}
\alias{gd_compute_pov_gap_lq}
\title{Compute poverty gap using Lorenz quadratic fit}
\usage{
gd_compute_pov_gap_lq(mean, povline, headcount, A, B, C)
}
\arguments{
\item{mean}{numeric: Welfare mean.}

\item{povline}{numeric: Poverty line.}

\item{headcount}{numeric: Headcount index.}

\item{A}{numeric: Lorenz curve coefficient. Output of
\code{regres_lq()$coef[1]}.}

\item{B}{numeric: Lorenz curve coefficient. Output of
\code{regres_lq()$coef[2]}.}

\item{C}{numeric: Lorenz curve coefficient. Output of
\code{regres_lq()$coef[3]}.}
}
\value{
numeric
}
\description{
Compute poverty gap using Lorenz quadratic fit
}
