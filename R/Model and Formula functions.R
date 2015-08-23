#' Function to help with model building
#'
#' \code{CreateFormula} creates a formula object for predictive models
#' \code{Mono} replaces the value in a list used for \code{var.monotone}
#' \code{Mono_List} creates a list that can be reduced and used for \code{var.monotone}
#' @param variables a list or character vector of variables
#' @param target the dependent variable to be put before \code{~}
#' @return object of class formula
#'
#' @name Model Functions
NULL

#' @export
#' @rdname Model Functions
CreateFormula <- function(variables, target){
  form <- paste(target, "~")
    for (i in 1:length(variables)) {
      if (i == length(variables)){
        form <- paste(form, variables[i])
      } else {
        form <- paste(form, paste(variables[i], "+"))
      }
    }
  as.formula(form)
}

#' @param list a list of [1,0,-1] with each slot named after a variable to be used
#' @param variable name of variable slot to be changed
#' @param value a of [1,0,-1]
#' @export
#' @rdname Model Functions
"Mono<-" <- function(list, variable, value){
  list[[variable]] <- value
}

#' @param variables list of variables used in a model
#' @export
#' @rdname Model Functions
Mono_List <- function(variables){
  .list <- list()

  for (i in Variable){
    .list[[i]] <- 0
  }
  .list
}

Mono_Reduce <- function(list){
  Reduce(c, list)
}
