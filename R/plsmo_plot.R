#' Creates a PLSMO Plot
#'
#' \code{plsmo_plot} creates a PLSMO plot
#'
#' @param mod model object
#' @param data \code{data.frame} model object was build from
#' @param var.name name of independent variable
#' @param target name of dependent variable
#' @return PLSMO plot
#'
#' @export

plsmo_plot <- function(mod, data, var.name, target) {
  t1 <- match(var.name,mod[["var.names"]])

  if(is.numeric(data[,mod$var.names[t1]]))
    {
      Hmisc::plsmo(data[,mod$var.names[[t1]]],
            target,
            xlab = "",
            ylab = as.character(mod$response.name))
    }
  else
    {
      plot(x = c(0,1), y = c(0,1), type = "n", axes = FALSE, xlab = "", ylab = "")
    }
}
