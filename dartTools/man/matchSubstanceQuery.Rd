% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/appSubstance.R
\name{matchSubstanceQuery}
\alias{matchSubstanceQuery}
\title{Helper function for substance query in Shiny app}
\usage{
matchSubstanceQuery(substancesObject, mySearch, type, partialMatch = TRUE)
}
\arguments{
\item{substancesObject}{Object that inherits from Substances (R6 class)}

\item{mySearch}{Character string with query}

\item{type}{Type of (external) substance identifier}

\item{partialMatch}{Boolean indicating if a partial match is allowed}
}
\value{
vector with (integer) substanceids
}
\description{
Helper function for substance query in Shiny app
}
\author{
Marvin Steijaert
}
