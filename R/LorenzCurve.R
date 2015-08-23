#' Plot Lorenz Curve
#'
#' \code{Lorenz} plots the Lorenz curve along with C-Statistic, KS-Statistic,
#'    and Gini-Coefficient.
#'
#' @param data data.frame consisting of a vector of predicted probabilities and
#'    a vector of actaul (1,0)
#' @param target name of response variable
#' @param pred name of variable with predicted probabilities
#' @param title title of the Lorenz plot. Default is "Lorenz Curve"
#' @return plot of Lorenz curve with C-Stat, KS-Stat, and Gini-Coef
#'
#' @export

Lorenz <- function(data, target, pred, title = "Lorenz Curve") {
      temp <- as.data.frame(cbind(a = c(1:nrow(data))/nrow(data),
                                  b = data[[target]][order(data[[pred]], decreasing = F)],
                                  c = data[[target]][order(data[[target]], decreasing = T)]))

      temp$cumt <- cumsum(temp$b)/sum(temp$b)

      temp$perf <- cumsum(temp$c)/sum(temp$c)

      temp$cpart <- abs((temp$cumt - temp$a)/nrow(data))

      temp$ppart <- abs((temp$perf - temp$a)/nrow(data))

      plot(c(1:nrow(data))/nrow(data)*100,
           cumsum(data[order(data[[pred]], decreasing=T), c(target)])
              / sum(data[[target]])*100,
           xlab = "% of Population",
           ylab = paste("Conversion Rate"),
           type = "l",
           main = paste(title), col = "blue")

      lines(temp$a*100, cumsum(data[order(data[[target]], decreasing = T),
                                    c(target)])/sum(data[[target]])*100,
            type = "l", col = "red")

      lines(c(0,100), c(0,100), col = "orange")

      #text(80, 26, labels = paste("p-stat =", round(sum(temp$ppart), 3)))
      text(80, 19, labels = paste("c-stat =", round(sum(temp$cpart), 3)))
      text(80, 12, labels = paste("KS-stat =", round(max(temp$cpart*nrow(data)), 3)))
      text(80, 5, labels = paste("Gini-coef =", round(sum(temp$cpart)/sum(temp$ppart), 3)))
}

#' @export
#' @rdname Lorenz Prep data for Lorenz Curve

Lorenz_data <- function(data, model, response.var = "Response", ...){
  #1. Score Validation Data

    pred1 = predict(model, newdata = data, type = "response", ...)

  #2. Combine Predicted with Actual

    dataLor1 = cbind(data[response.var], pred1)
}
