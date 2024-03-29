% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/connectRdkit.R
\name{standardizeSmilesList}
\alias{standardizeSmilesList}
\title{Auxiliary function for standardizeSmilesRdkit}
\usage{
standardizeSmilesList(smiles, pythonBinding = "reticulate")
}
\arguments{
\item{smiles}{List of smiles strings}

\item{pythonBinding}{Character string indicating if system call ("system2") or reticulate ("reticulate") is used to call python module}
}
\value{
data.table object
}
\description{
Auxiliary function for standardizeSmilesRdkit
}
\author{
Marvin Steijaert
}
