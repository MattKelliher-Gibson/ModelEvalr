#' Bins continuous and categorical variables
#'
#' \code{Bin} seperates continuos variables into bin by a maximum
#'    threshold.
#'
#' @param data a sorted \code{data.frame} or \code{data.table}
#' @param var name to be used for new bin variable. Default is "Bin"
#' @param threshold number from 0-1 that give the break from one bin to another.
#'    Default is .1
#' @return a \code{data.table} (invisibly) or \code{data.frame}
#'
#' @export

Bin <- function(data, var = "Bin", threshold = .1){
  UseMethod("Bin", data)
}

#' @export
#' @import data.table
#' @describeIn Bin

Bin.data.table <- function(data, var = "Bin", threshold = .1){
  bin <- 0
  pct <- 0

  for (i in seq(nrow(data))) {
    if(i == 1) {
      bin <- 1

      data[i, (var) := bin]

      pct <- data[i, Pct_Total]
    } else if (is.na(data[i, Variable]) == TRUE){
      bin <- bin + 1
      data[i, (var) := eval(bin)]
      pct <- 0
    } else {
      pct <- pct + data[i, Pct_Total]

      if (pct > threshold ) {
        bin <- bin + 1
        data[i, (var) := eval(bin)]
        pct <- 0
      } else {
        data[i, (var) := eval(bin)]
      }
    }
  }
}

#' @export
#' @describeIn Bin

Bin.data.frame <- function(data, var = "Bin", threshold = .1){
  bin <- 0
  pct <- 0

  for (i in seq(nrow(data))) {
    if(i == 1) {
      bin <- 1

      data[i, var] <- bin

      pct <- data[i, "Pct_Total"]
    } else {
      pct <- pct + data[i, "Pct_Total"]

      if (pct > threshold ) {
        bin <- bin + 1
        data[i, var] <- bin
        pct <- 0
      } else {
        data[i, var] <- bin
      }
    }
  }

  data
}
