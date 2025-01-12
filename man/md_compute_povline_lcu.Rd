% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/md_compute_povline_lcu.R
\name{md_compute_povline_lcu}
\alias{md_compute_povline_lcu}
\title{Compute poverty line in Local Currency Unit (LCU)}
\usage{
md_compute_povline_lcu(
  welfare,
  povline,
  weight,
  popshare,
  requested_mean,
  data_mean
)
}
\arguments{
\item{welfare}{numeric: A vector of income or consumption values}

\item{povline}{numeric: Poverty line in international dollars}

\item{popshare}{numeric: Share of population for which the corresponding
quantile is desired. Default .5 (i.e., weighted median)}

\item{requested_mean}{numeric: Welfare mean in international dollars}

\item{data_mean}{numeric: Data mean.}
}
\value{
list
}
\description{
Compute the LCU poverty line for microdata.
}
\examples{

# Load and clean example data
data("md_ABC_2000_income")
df <- wbpip:::md_clean_data(
  md_ABC_2000_income,
  welfare = 'welfare',
  weight = 'weight')$data

# Compute LCU poverty line
res <-  wbpip:::md_compute_povline_lcu(
  df$welfare, df$weight,
  popshare = NULL,
  requested_mean = 5000,
  data_mean = 4000)
str(res)
}
\keyword{internal}
