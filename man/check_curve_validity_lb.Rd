% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gd_compute_pip_stats_lb.R
\name{check_curve_validity_lb}
\alias{check_curve_validity_lb}
\title{Check validity of Lorenz beta fit}
\usage{
check_curve_validity_lb(headcount, A, B, C)
}
\arguments{
\item{headcount}{numeric: Headcount index.}

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
\code{check_curve_validity_lb()} checks the validity of the Lorenz beta fit
}
\references{
Datt, G. 1998. "\href{https://ageconsearch.umn.edu/record/94862/}{Computational Tools For Poverty Measurement And Analysis}". FCND Discussion
Paper 50. World Bank, Washington, DC.

Kakwani, N. 1980. "\href{https://EconPapers.repec.org/RePEc:ecm:emetrp:v:48:y:1980:i:2:p:437-46}{On a Class of Poverty Measures}".
\emph{Econometrica 48} (2): 437-46.
}
