% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gen_otm_splines.R
\name{gen_otm_splines}
\alias{gen_otm_splines}
\title{Generate OTM splines}
\usage{
gen_otm_splines(otm_data, knot_p)
}
\arguments{
\item{otm_data}{An OTM data frame that can (and should) include metadata}

\item{knot_p}{The number of knots the spline should have taken as a percentage of all observations made in a given day.}
}
\value{
An OTM splines complex tibble with otm_id, date, {metadata} and a nested spline model objects column.
}
\description{
Generate a unique spline model for each OTM on each date (Julian) while the OTM was measuring
}