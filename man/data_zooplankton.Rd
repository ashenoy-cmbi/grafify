% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{data_zooplankton}
\alias{data_zooplankton}
\title{Time-series data on zooplankton in lake Menon.}
\format{
tibble:	1127 obs. of  8 variables:

\describe{
\item{day}{Numeric integer variable.}
\item{year}{Numeric integer variable of years during which data were collected.}
\item{lake}{This data is for lake Menona; data for other others not included in this subset.}
\item{taxon}{Names of zooplankton taxa as factor of 8 levels.}
\item{density}{Numeric values of density of measurements.}
\item{density_adj}{Numeric values of adjusted density .}
\item{min_density}{Numeric values of minimum densities.}
\item{desnsity_scaled}{Numeric value of scaled density.}
}
}
\usage{
data_zooplankton
}
\description{
A subset of data from (Lathro RC, 2000) (\doi{10.6073/pasta/ec3d0186753985147d4f283252388e05}) provided by the Wisconsin Department of Natural Resources
}
\keyword{datasets}
