% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/appCompound.R
\name{interactiveTable}
\alias{interactiveTable}
\title{Create interactive table for UI}
\usage{
interactiveTable(myData, columnNames = NULL, opts)
}
\arguments{
\item{myData}{data.table}

\item{columnNames}{character vector, column names for the interactive table}

\item{opts}{additional options to pass to \code{datatable} e.g. for row callbacks}
}
\value{
datatable, interactive version of \code{myData}
}
\description{
Create interactive table for UI
}
\author{
mvarewyck
}
