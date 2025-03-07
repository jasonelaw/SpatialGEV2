% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/interface.R
\name{regression_matrix}
\alias{regression_matrix}
\title{Extract design matrix from all RHS random effect terms}
\usage{
regression_matrix(x, data)
}
\description{
Extract the design matrix from the data.frame for all RHS random effect terms.
For the spatial GEV model here, any RHS terms should consist solely of a single
RE term, optionally with regression terms.
}
