#' Creates a Histogram Plot
#'
#' \code{Historgram} creates a basic histogram plot or bar plot (for categorical varaibles
#'    and variables with less than 10 values)
#'
#' @param mod model object
#' @param data \code{data.frame} model object was build from
#' @param target name dependent variable
#' @param var.name name of independent variable
#' @return Histogram or Bar plot
#'
#' @export

Histogram<- function(mod, data, target, var.name) {
  t1 <- match(var.name, mod[["var.names"]])

  if(is.numeric(data[,mod$var.names[t1]]) & (as.numeric(Hmisc::describe(data[,mod$var.names[t1]])$counts[[3]]) > 10)) {
    hist(data[,mod$var.names[t1]],
         main = paste("Histogram of ", var.name),
         xlab = "")
  } else {
    barplot(table(target, data[,mod$var.names[t1]]),
            col = c("darkblue", "red"),
            main = paste("Bar Plot of ", var.name),
            ylab = mod$response.name,
            las=2,
            xlab = "")

    legend("topright",
           title="Loan Status",
           c("Conversion", "Non Conversion"),
           fill = c("red", "darkblue"))
  }
}
