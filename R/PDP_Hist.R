#' Creates PDP, Histogram, and PLSMO plots
#'
#' \code{PDP_Hist} creates PDP, Histogram, and PLSMO plots for all variables
#'    in a model object
#'
#' @param dataset \code{data.frame} model object was built from
#' @param response name of dependent variable
#' @param model model object
#' @param best.iter.model numeric number of trees used for gbm object
#' @return three plots (PDP, Histogram, and PLSMO) for the dependent
#'
#' @export

PDP_Hist <- function(dataset, response, model, best.iter.model, gg = FALSE, ...){

  dataset$response <- dataset[[response]]

  frame <- as.data.frame(summary(model, n.trees = best.iter.model))
  if (gg != FALSE){
    for (i in 1:length(frame[[1]])) {
      par(mfrow = c(1, 3))

      PDP(mod = model,
          iter = best.iter.model,
          var.name = as.character(frame[[1]][[i]]),
          xlim = NULL,
          pts = 400,
          ylab = "P(Conversion)",
          leg = "topleft",
          title = "PDP")

      Histogram(mod = model,
                data = dataset,
                target = dataset$response,
                var.name = as.character(frame[[1]][[i]]))

      plsmo_plot(mod = model,
                 data = dataset,
                 var.name = as.character(frame[[1]][[i]]),
                 target = dataset$response)
    }
    par(mfrow = c(1,1))
  } else {
    for (i in 1:length(frame[[1]])){
      PDP_gg(dataset, model, i, best.iter.model, ...)
    }
  }
}
