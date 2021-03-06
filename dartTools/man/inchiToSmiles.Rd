% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/connectRdkit.R
\name{inchiToSmiles}
\alias{inchiToSmiles}
\title{inchiToSmiles}
\usage{
inchiToSmiles(inchis, pythonPath = getOption("dartpaths_python"))
}
\arguments{
\item{inchis}{single inchi string or a vector with multiple inchis}

\item{pythonPath}{full path of python executable}
}
\value{
vector with character strings
}
\description{
use rdkit to convert a vector of inchi to a vector of smiles
}
\examples{
\dontrun{
smiles <- c("N[C@H](C)C(=O)O","O1CCCC[C@H]1C")
result <- smilesToPng(smiles)
}
}
\author{
Marvin Steijaert
}
