% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PDP_Hist.R
\name{PDP_Hist}
\alias{PDP_Hist}
\title{Creates PDP, Histogram, and PLSMO plots}
\usage{
PDP_Hist(dataset, response, model, best.iter.model, gg = FALSE, ...)
}
\arguments{
\item{dataset}{\code{data.frame} model object was built from}

\item{response}{name of dependent variable}

\item{model}{model object}

\item{best.iter.model}{numeric number of trees used for gbm object}
}
\value{
three plots (PDP, Histogram, and PLSMO) for the dependent
}
\description{
\code{PDP_Hist} creates PDP, Histogram, and PLSMO plots for all variables
   in a model object
}

