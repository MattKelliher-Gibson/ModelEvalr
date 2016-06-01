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

#' @export
#' @rdname Model Functions
CreateFormula <- function(variables, target, model_type = NULL, ...){
  if (is.null(model_type)){
    form <- paste(target, "~")
      for (i in 1:length(variables)) {
        if (i == length(variables)){
          form <- paste(form, variables[i])
        } else {
          form <- paste(form, paste(variables[i], "+"))
        }
      }
  as.formula(form)
  } else if (toupper(model_type) == "GAM" ){
    CreateForumlaGAM(variables, target, ...)
  }
}

#' @export
#' @rdname Model Functions
CreateForumlaGAM <- function(variables, target, s = NULL, data = NULL){
  if (is.null(data)){
    stop("data.frame must be provided")
  }
  if (is.null(s) | missing(s)){
    for (i in 1:length(variables)){
      if (i == 1){
        if (is.binary(data[[variables[i]]]) | is.character(data[[variables[i]]]) | length(unique(data[[variables[i]]])) < 4){
          form <- paste(target, "~", variables[i])
        } else {
          form <- paste0(target, " ~ s(", variables[i], ", bs = 'ps'", ")")
        }
      } else {
        if (is.binary(data[[variables[i]]]) | is.character(data[[variables[i]]]) | length(unique(data[[variables[i]]])) < 4){
          form <- paste(form, "+", variables[i])
        } else {
          form <- paste0(form, " + s(", variables[i], ", bs = 'ps'", ")")
        }
      }
    }
  } else {
    for (i in 1:length(variables)){
      if (i == 1){
        if (is.binary(data[[variables[i]]]) | is.character(data[[variables[i]]]) | length(unique(data[[variables[i]]])) < 4){
          form <- paste(target, "~", variables[i])
        } else {
          form <- paste0(target, " ~ s(", variables[i], ", bs = 'ps'", ", sp = ", s, ")")
        }
      } else {
        if (is.binary(data[[variables[i]]]) | is.character(data[[variables[i]]]) | length(unique(data[[variables[i]]])) < 4){
          form <- paste(form, "+", variables[i])
        } else {
          form <- paste0(form, " + s(", variables[i], ", bs = 'ps'", ", sp = ", s, ")")
        }
      }
    }
  }
  as.formula(form)
}

#' @param list a list of [1,0,-1] with each slot named after a variable to be used
#' @param variable name of variable slot to be changed
#' @param value a of [1,0,-1]
#' @export
#' @rdname Model Functions
"Mono<-" <- function(list.var, value){
  list.var <- value

  list
}

#' @param variables list of variables used in a model
#' @export
#' @rdname Model Functions
Mono_List <- function(variables){
  .list <- list()

  for (i in variables){
    .list[[i]] <- 0
  }
  .list
}

#' @export
#' @rdname Model Functions
Mono_Reduce <- function(list){
  Reduce(c, list)
}

#' @export
#' @rdname Model Functions
is.binary <- function(x){
  unique = base::unique(x)
  if (!is.numeric(x) | any(is.na(x))){
    return(FALSE)
  } else {
    return(!(any(as.integer(unique) != unique) || length(unique) > 2 || min(x) != 0 || max(x) != 1))
  }
}
