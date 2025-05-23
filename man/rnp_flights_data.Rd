% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rnp_flights_data.R
\name{rnp_flights_data}
\alias{rnp_flights_data}
\title{Reads and processes data from multiple flights in \code{.tif} format and adds metadata}
\usage{
rnp_flights_data(path, metadata, resolution)
}
\arguments{
\item{path}{A character string indicating the directory where all \code{.tif}`
files for flights are stored. The name of each flight \code{.tif} file must
have associated metadata.}

\item{metadata}{A metadata \code{tibble} for the flights with information on
the \code{flight_id} (character), \code{date} (MM/DD/YYYY), \code{time_start} (HH:MM)
and \code{time_end} (HH:MM) of each flight.}

\item{resolution}{A numeric > 0.5 indicating the spatial resolution of the
the final processed output in squared meters. This lower boundary is chosen
to exceed the spatial resolution of most drone-mounted thermal imaging cameras.}
}
\value{
A \code{data.frame} where each row represents a unique tile (a unique
   combination of \code{x} and \code{y} UTM coordinates), with an associated
   surface temperature measurement (\code{surf_temp}) for a unique flight.
   It also provides columns with the \code{year}, month \code{month},
   day of the year \code{doy} the minute of the day when the flight started
   (\code{mod_start}) and ended (\code{mod_end})
}
\description{
Reads and processes data from multiple flights in \code{.tif} format and adds metadata
}
