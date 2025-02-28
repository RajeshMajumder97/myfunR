% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/My_functions.R
\name{mytab_two_two_Contingency}
\alias{mytab_two_two_Contingency}
\title{mytab_two_two_Contingency}
\usage{
mytab_two_two_Contingency(x, y, xlev = deparse(substitute(x)))
}
\arguments{
\item{x}{indep. character vector.}

\item{y}{dep. character vector.}

\item{xlev}{levels of x.}
}
\value{
Returns a data frame containing the probs., percentages along with the Chi-square Test P-Value & Fisher's Exact Test P-Value for Non-Parametric.
}
\description{
It gives an output table for testing the significant difference between the probabilities of two independent populations,i.e., for 2x2 contingency table.
}
