% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gd_compute_pip_stats_lb.R
\name{gd_compute_dist_stats_lb}
\alias{gd_compute_dist_stats_lb}
\title{Computes distributional stats from Lorenz beta fit}
\usage{
gd_compute_dist_stats_lb(mean, p0, A, B, C)
}
\arguments{
\item{mean}{numeric: Welfare mean.}

\item{p0}{numeric: \strong{TO BE DOCUMENTED}.}

\item{A}{numeric: Lorenz curve coefficient. Output of
\code{regres()$coef[1]}.}

\item{B}{numeric: Lorenz curve coefficient. Output of
\code{regres()$coef[2]}.}

\item{C}{numeric: Lorenz curve coefficient. Output of
\code{regres()$coef[3]}.}
}
\value{
list
}
\description{
Computes distributional stats from Lorenz beta fit
}
\keyword{internal}
