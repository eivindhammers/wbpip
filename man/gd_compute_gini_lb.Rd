% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gd_compute_pip_stats_lb.R
\name{gd_compute_gini_lb}
\alias{gd_compute_gini_lb}
\title{Compute gini index from Lorenz beta fit}
\usage{
gd_compute_gini_lb(A, B, C, nbins = 499)
}
\arguments{
\item{A}{numeric: Lorenz curve coefficient. Output of
\code{regres()$coef[1]}.}

\item{B}{numeric: Lorenz curve coefficient. Output of
\code{regres()$coef[2]}.}

\item{C}{numeric: Lorenz curve coefficient. Output of
\code{regres()$coef[3]}.}

\item{nbins}{numeric: Number of bins used to compute Gini.}
}
\value{
numeric
}
\description{
\code{gd_compute_gini_lb()} computes the gini index from a Lorenz beta fit.
}
\references{
Datt, G. 1998. "\href{https://www.ifpri.org/cdmref/p15738coll2/id/125673}{Computational Tools For Poverty Measurement And Analysis}". FCND
Discussion Paper 50. World Bank, Washington, DC.
}
