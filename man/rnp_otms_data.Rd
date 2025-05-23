% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rnp_otms_data.R
\name{rnp_otms_data}
\alias{rnp_otms_data}
\title{Read and process OTM data}
\usage{
rnp_otms_data(path, rows_skip, date_col, time_col, op_temp_col, metadata)
}
\arguments{
\item{path}{A character specifying either the path for a single or the folder
for multiple OTM files in \code{.csv} format. The file needs to have at least a
column with information on the moment (i.e., date and time of the day) when
each measurement was made and a column with an operative temperature value
for each measurement}

\item{rows_skip}{An integer >= 0 indicating the rows to skip when reading
OTM \code{.csv} files. If omitted, it defaults to \code{0}.}

\item{date_col}{An integer >= 1 indicating the column in the OTM \code{.csv} file
that contains date of the measurement. Dates need to be in either MM/DD/YYYY
HH:MM:SS, MM/DD/YYYY HH:MM or MM/DD/YYYY.}

\item{time_col}{An integer >= 1 indicating the column in the OTM \code{.csv} file
that contains the time of the measurement. Times need to be in HH:MM:SS or HH:MM
format. If not specified, it will try to extract the date information
from \code{date_col}.}

\item{op_temp_col}{An integer >= 1 indicating the column column in the
OTM \code{.csv} file that contains the operative temperature measurement}

\item{metadata}{A \code{tibble} or \code{data.frame} containing metadata related to each
OTM. It must include an \code{otm_id} character column matching the names of each
of the OTM files and numeric columns for \code{latitude} and \code{longitude}}
}
\value{
A \code{tibble} where each row represents a unique operative temperature
   measurement (\code{op_temp}) taken by a given OTM (specified by \code{otm_id}),
   that occurred in a given \code{year}, date of the year (\code{doy}) and minute of
   the day (\code{mod}). The \code{tibble} will have as many additional columns as
   metadata characteristics included in \code{metadata}.
}
\description{
Read and processes data from multiple operative temperature model (OTM)
\code{.csv} files and adds metadata
}
