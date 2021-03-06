% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_hist.R
\name{plot_hist}
\alias{plot_hist}
\title{Historgram of a numerical column in a dataframe}
\usage{
plot_hist(.data, .num, .bins = ".get_bins")
}
\arguments{
\item{.data}{a dataframe}

\item{.num}{a numeric vector}

\item{.bins}{bins argument for \code{geom_histogram}}
}
\value{
a ggplot object
}
\description{
Historgram of a numerical column in a dataframe
}
\examples{
plot_hist(iris, Sepal.Length)
}
