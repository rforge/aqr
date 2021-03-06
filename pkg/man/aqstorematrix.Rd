% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hist_data.R
\name{aqStoreMatrix}
\alias{aqStoreMatrix}
\title{stores a matrix onto an AQ Master Server}
\usage{
aqStoreMatrix(seriesId, freq, data, con = aqInit(), silent = FALSE,
  writeKey = "unset")
}
\arguments{
\item{seriesId}{a series ID to store}

\item{freq}{the frequency, must be one of AQ's enums}

\item{data}{the data as XTS object}

\item{con}{a connection object, will be initialized by aqInit by default}

\item{silent}{whether  it should print storage diagnostics.}

\item{writeKey}{the write key, unset by default}
}
\description{
stores a matrix onto an AQ Master Server
}

