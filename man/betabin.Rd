% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{betabin}
\alias{betabin}
\title{Create a beta-binomial model}
\usage{
betabin(data, alpha, beta, as_obs = TRUE)
}
\arguments{
\item{data}{Numeric vector of 1s or 0s if as_obs is TRUE or a vector with
length 2 if the summed successes and failures are reported.}

\item{alpha}{The alpha value of the prior}

\item{beta}{The beta value of the prior}

\item{as_obs}{A boolean indicating whether the data is summed or not}
}
\value{
The model object
}
\description{
Create a beta-binomial model
}
