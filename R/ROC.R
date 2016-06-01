#' Plots ROC Curve
#'
#' \code{ROC} plots a ROC curve for models with a binary dependent variable
#'
#' @param data \code{data.frame} of validation data
#' @param model a model object (e.g. gbm, gam)
#' @param response depenedent variable name
#' @param ... all other arugements
#' @return ROC plot
#'
#' @import methods
#' @importClassesFrom ROCR prediction
#' @export

ROC <- function(model, data, response, ...) {
  UseMethod("ROC")
}

#' @export
#' @importClassesFrom ROCR prediction
#' @describeIn ROC default method

ROC.default <- function(model, data, response = NULL, ...){

  if (is.character(response)){
    data$Response <- data[[response]]
  } else {
    data$Response <- response
  }

  pred1 <- predict(model, type = "response", newdata = data, ...)

  pred2 <- ROCR::prediction(pred1, data$Response)

  perf <- ROCR::performance(pred2, "tpr", "fpr")

  auc <- ROCR::performance(pred2, "auc")
  # print(auc)

  plot(perf, colorize = TRUE)

  mtext(paste("AUC:", round(as.numeric(auc@y.values), 3)), side = 1, line = -1)
}

#' @importClassesFrom ROCR prediction
#' @export
#' @describeIn ROC gam objects from mgcv package
ROC.gam <- function(model, data, response, ...){
  if (is.character(response)){
    data$Response <- data[[response]]
  } else {
    data$Response <- response
  }

  pred1 <- as.numeric(mgcv::predict.gam(model, newdata = data, type = "response", ...))

  pred2 <- ROCR::prediction(pred1, data$Response)

  perf <- ROCR::performance(pred2, "tpr", "fpr")

  auc <- ROCR::performance(pred2, "auc")
  # print(auc)

  plot(perf, colorize = TRUE)
  mtext(paste("AUC:", round(as.numeric(auc@y.values), 3)), side = 1, line = -1)
}

#' @importClassesFrom ROCR prediction
#' @export
#' @describeIn ROC gbm model objects from gbm package
ROC.gbm <- function(model, data, response = NULL, trees, ... ){

  if (is.character(response)){
    data$Response <- data[[response]]
  } else {
    data$Response <- response
  }

  pred1 <- gbm::predict.gbm(model, type = "response", newdata = data, n.trees = trees, ...)

  pred2 <- ROCR::prediction(pred1, data$Response)

  perf <- ROCR::performance(pred2, "tpr", "fpr")

  auc <- ROCR::performance(pred2, "auc")
  # print(auc)

  plot(perf, colorize = TRUE)

  mtext(paste("AUC:", round(as.numeric(auc@y.values), 3)), side = 1, line = -1)
}
