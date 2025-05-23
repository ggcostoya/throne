% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datasets_doc.R
\docType{data}
\name{flights_data}
\alias{flights_data}
\title{Example flights data}
\format{
A \code{tibble} with 263008 rows and 7 columns, all numeric:
\describe{
 \item{longitude}{Longitude of a given tile in decimal degrees}
 \item{latitude}{Latitude of a given tile in decimal degrees}
 \item{ir_temp}{Surface temperature measured via an IR camera}
 \item{year}{Year when the flight took place}
 \item{doy}{Day of the year when the flight took place}
 \item{mod_start}{Minute of the day when the flight started}
 \item{mod_end}{Minute of the day when the flight ended}
}
}
\usage{
flights_data
}
\description{
A data set of 33 flights processed through the \code{rnp_flights_data} function
Each row in the data represents the surface temperature measured recorded in a
given tile (i.e., unique latitude and longitude combination) at the moment in
time when a flight took place (specified by a unique year, day of the year
and minute of the day).
}
\examples{
flights_data
}
\keyword{datasets}
