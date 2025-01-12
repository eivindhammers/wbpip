% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prod_compute_pip_stats.R
\name{prod_compute_pip_stats}
\alias{prod_compute_pip_stats}
\title{Compute default PIP statistics (microdata, prod)}
\usage{
prod_compute_pip_stats(
  welfare,
  povline,
  population = NULL,
  requested_mean = NULL,
  svy_mean_lcu = NULL,
  svy_median_lcu,
  svy_median_ppp,
  popshare = NULL,
  default_ppp,
  ppp = NULL,
  p0 = 0.5,
  distribution_type = c("micro", "group", "aggregate", "imputed")
)
}
\arguments{
\item{welfare}{numeric: A vector of income or consumption values}

\item{povline}{numeric: Poverty line in international dollars}

\item{population}{numeric: A vector of population weights, optional, a vector
of 1s if not specified.}

\item{requested_mean}{numeric: Welfare mean in international dollars}

\item{svy_mean_lcu}{numeric: Welfare mean in Local Currency Unit}

\item{svy_median_lcu}{numeric: Welfare median in Local Currency Unit}

\item{svy_median_ppp}{numeric: Welfare median in PPP}

\item{popshare}{numeric: Share of population for which the corresponding
quantile is desired. Default .5 (i.e., weighted median)}

\item{default_ppp}{numeric: Default purchasing power parity}

\item{ppp}{numeric: PPP requested by user}

\item{p0}{numeric: TO be documented}

\item{distribution_type}{character: Type of distribution, either micro,
group, aggregate or imputed.}
}
\value{
list
}
\description{
Compute poverty and distributional statistics for microdata.
}
\details{
Version used in production. Does not re-compute all distributional stats.
Distributional stats are pre-computed as they are not sensitive to the poverty line.
}
