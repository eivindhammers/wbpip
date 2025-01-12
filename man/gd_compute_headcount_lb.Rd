% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gd_compute_pip_stats_lb.R
\name{gd_compute_headcount_lb}
\alias{gd_compute_headcount_lb}
\title{Compute the headcount statistic from Lorenz Beta fit}
\usage{
gd_compute_headcount_lb(mean, povline, A, B, C)
}
\arguments{
\item{mean}{numeric: Welfare mean.}

\item{povline}{numeric: Poverty line.}

\item{A}{numeric: Lorenz curve coefficient. Output of
\code{regres()$coef[1]}.}

\item{B}{numeric: Lorenz curve coefficient. Output of
\code{regres()$coef[2]}.}

\item{C}{numeric: Lorenz curve coefficient. Output of
\code{regres()$coef[3]}.}
}
\value{
numeric
}
\description{
Compute the headcount statistic from Lorenz Beta fit
}
