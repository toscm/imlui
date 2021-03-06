% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc.R
\name{predict.numeric}
\alias{predict.numeric}
\title{Predict S3 Method for numeric Vectors}
\usage{
\method{predict}{numeric}(object, ...)
}
\arguments{
\item{object}{Named numeric vector}

\item{...}{Numeric matrix with \code{names(b) \%in\% c("Intercept", colnames(X))}}
}
\value{
Vector of predicted scores
}
\description{
Predict method for linear models provided as plain named vectors
}
