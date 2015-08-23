#' Plots Partial Dependency Plots (PDP)
#'
#' \code{PDP} plots the Partial Dependency Plots of varaibles in a model object
#'
#' @param model Model Object (curently only gbm object tested)
#' @param iter numberic number of tree to be used from gbm object
#' @param var.name independent variable to be plotted
#' @param xlim numberic X-axis limit
#' @param pts numberic number of points used in graph. Default is \code{4000}
#' @param ylab character Y-axis title. Default is \code{"P(Conversion)"}
#' @param leg character for legend location. Default \code{"right"}
#' @param title chatacer title for plot
#' @return plot of predicted partial dependecy
#'
#' @export

PDP <- function(model, iter, var.name, xlim = NULL, pts = 4000, ylab = "P(Conversion)", leg = "right", title = NULL){

  t1 <- match(var.name, model[["var.names"]])

  t2 <- gbm::plot.gbm(model, t1, iter, return.grid = T, cont = pts)

  t2$y <- 1/(1+exp(-t2$y))

  t3 <- round(max(t2$y)-min(t2$y), 3)*100

  names(t2)[2] <- ylab

  plot(t2, type = "l", xlim = NULL, main = "", las = 2, xlab = "")

  legend(x=leg, legend = bquote(delta == .(t3)*"%"))
}
