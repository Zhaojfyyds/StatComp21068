% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StatCompR.R
\name{SGDR}
\alias{SGDR}
\title{A function to compute Coefficients of the unary linear regression equation by Stochastic Gradient Descent approach with fixed step}
\usage{
SGDR(x, y, theta, step, maxiter = 10000, epsilon = 1e-04)
}
\arguments{
\item{x}{the covariate}

\item{y}{the response variable}

\item{theta}{the initial value of beta}

\item{step}{the learning rate}

\item{maxiter}{the max number of iteration}

\item{epsilon}{the end condition}
}
\value{
the estimation
}
\description{
A function to compute Coefficients of the unary linear regression equation by Stochastic Gradient Descent approach with fixed step
}
\examples{
\dontrun{
SGD(x, y, theta, 0.1)
}
}
