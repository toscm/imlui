% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/php.R
\name{plot_predictions_histogram}
\alias{plot_predictions_histogram}
\title{Plot Histogram of Predictions}
\usage{
plot_predictions_histogram(
  predictions,
  density_lines = TRUE,
  rug = TRUE,
  binwidth = NULL
)
}
\arguments{
\item{predictions}{as the name says (named list of named numeric vectors)}

\item{density_lines}{add density lines to the plot (logical)? (logical)}

\item{rug}{add rugs to the plot? (logical)}

\item{binwidth}{binwidth for histogram}
}
\value{
ggplot2 object
}
\description{
Return ggplot2 object of histogram of Predictions
}
