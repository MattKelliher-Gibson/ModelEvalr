% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Bin.R
\name{Bin}
\alias{Bin}
\alias{Bin.data.frame}
\alias{Bin.data.table}
\title{Bins continuous and categorical variables}
\usage{
Bin(data, var = "Bin", threshold = 0.1)

\method{Bin}{data.table}(data, var = "Bin", threshold = 0.1)

\method{Bin}{data.frame}(data, var = "Bin", threshold = 0.1)
}
\arguments{
\item{data}{a sorted \code{data.frame} or \code{data.table}}

\item{var}{name to be used for new bin variable. Default is "Bin"}

\item{threshold}{number from 0-1 that give the break from one bin to another.
Default is .1}
}
\value{
a \code{data.table} (invisibly) or \code{data.frame}
}
\description{
\code{Bin} seperates continuos variables into bin by a maximum
   threshold.
}
\section{Methods (by class)}{
\itemize{
\item \code{data.table}: Method for data.table

\item \code{data.frame}: Method for data.frame
}}

