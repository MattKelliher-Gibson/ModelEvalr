#' Calculate WOE and IV
#'
#' \code{WOE_IV} caluclates the Weight of Evidence (WOE) and
#'    Information Value (IV) of a list of variable for a \code{data.frame}
#'
#' @param data a \code{data.frame} or \code{data.table} with idependent and
#'    dependent variables
#' @param variables a characeter vector or list of variables to calculate WOE and IV
#' @param convert name of the independent variable
#' @param .threshold Bin functino threshold
#' @return a list of length 2 containing the summarized WOE and IV values and
#'    a list of unsummarized WOE and IV values
#'
#' @export
#' @importFrom magrittr "%>%"

WOE_IV <- function(data, variables, convert = NULL, .threshold){
  UseMethod("WOE_IV")
}

#' @export
#' @import data.table
#' @describeIn WOE_IV Method for data.table

WOE_IV.data.table <- function(data, variables, convert = NULL, .threshold = .1){
  var.list <- list()

  datatable <- copy(data)

  setnames(datatable, convert, "Response_Var")

  for (i in variables) {
    table <- woe_table(datatable, i)

    bins_total <- table[, sum(Total)]
    bins_convert <- table[, sum(Converted)]

    table[, Pct_Total := Total/ bins_total]

    woe_bin(datatable, table, i, threshold = .threshold)

    table_2 <- table[,c(-1,-4), with = FALSE] %>% dplyr::group_by(Bin) %>% dplyr::summarise_each(funs(sum))

    table_2[, ':=' (Pct_Good = Converted / eval(bins_convert), Pct_Bad = (Total - Converted) / (eval(bins_total) - eval(bins_convert)), ConversionRate = Converted / Total)]

    table_2[, WOE := ifelse(Pct_Good == 0 | Pct_Bad == 0, 0, log(Pct_Good/Pct_Bad))]

    table_2[, IV := (Pct_Good - Pct_Bad)*WOE]

    if (class(datatable[[i]]) == "character") {
      table[, ':=' (Min = Variable, Max = Variable)]

      table_3 <- table[, .(Bin, Min, Max)] %>% dplyr::left_join(table_2, by = "Bin")
    } else {
      table_3 <- table %>% dplyr::group_by(Bin) %>% dplyr::summarise(Min = min(Variable), Max = max(Variable)) %>% dplyr::left_join(table_2, by = "Bin")
    }

    var.list[[i]] <- table_3

    rm(table, bins_total, bins_convert, table_2, table_3)
  }

  var.list

  IV <- plyr::ldply(var.list, function(x){sum(x$IV)})

  names(IV) <- c("Variable", "IV")

  final <- WOE(var.list, IV)
}

#' @export
#' @describeIn WOE_IV defult method

WOE_IV.default <- function(data, variables, convert = NULL, .threshold = .1){
  var.list <- list()

  names(data)[match(convert, names(data))] <- "Response_Var"

  bins_total <- nrow(data)

  for (i in variables) {
    table <- woe_table(data, i)

    bins_convert <- sum(data$Converted)

    table$Pct_Total <- table$Total/ bins_total

    table <- woe_bin(data, table, i, threshold = .threshold)

    table_2 <- woe_table_2(table, bins_convert, bons_total)

    if (class(datatable[[i]]) == "character") {
      table$Min <- Variable
      table$Max <- Variable

      table_3 <- table[, c("Bin", "Min", "Max")] %>% dplyr::left_join(table_2, by = "Bin")
    } else {
      table_3 <- table %>% dplyr::group_by_(Bin) %>% dplyr::summarise_(Min = min(Variable), Max = max(Variable)) %>% dplyr::left_join(table_2, by = "Bin")
    }

    var.list[[i]] <- table_3

    rm(table, bins_convert, table_2, table_3)
  }

  # var.list

  IV <- plyr::ldply(var.list, function(x){sum(x$IV)})

  names(IV) <- c("Variable", "IV")

  final <- WOE(var.list, IV)
}

#' @export
#' @importFrom foreach "%dopar%"
#' @rdname WOE_IV

WOE_IV_multi <- function(data, variables, convert = NULL, convert_loc = length(data), .threshold = .1){
  # var.list <- list()

  names(data)[convert_loc] <- "Response_Var"

  bins_total <- nrow(data)

  var.list <- foreach::foreach(j = 1:length(variables), .final = function(x) setNames(x, variables)) %dopar% {
    i <- variables[j]

    table <- data %>% dplyr::group_by_(i) %>% dplyr::summarise_(Total = dplyr::n(), Converted = sum(Response_Var))

    table <- dplyr::arrange_(table, i)

    names(table)[[i]] <- "Variable"

    # bins_total <- sum(data$Total)
    bins_convert <- sum(data$Converted)

    table$Pct_Total <- table$Total/ bins_total

    if (class(data[[i]]) == "character" || nrow(table) <=5) {
      .row <- c(1:nrow(data))
      table$Bin <- .row
    } else {
      Bin(table, "Bin")
    }

    table_2 <- table[,c(-1,-4)] %>% dplyr::group_by_(Bin) %>% dplyr::summarise_each_(funs(sum))

    table_2$Pct_Good <- table_2$Converted / bins_convert

    table_2$Pct_Bad <- (table_2$Total - table_2$Converted) / bins_total

    table_2$ConversionRate <- table_2$Converted / table_2$Total

    table_2$WOE <- ifelse(table_2$Pct_Good == 0 | table_2$Pct_Bad == 0, 0, log(table2$Pct_Good/table2$Pct_Bad))

    table_2$IV <- (table_2$Pct_Good - table_2$Pct_Bad)*table_2$WOE

    if (class(datatable[[i]]) == "character") {
      table$Min <- Variable
      table$Max <- Variable

      table_3 <- table[, c("Bin", "Min", "Max")] %>% dplyr::left_join(table_2, by = "Bin")
    } else {
      table_3 <- table %>% dplyr::group_by_(Bin) %>% dplyr::summarise_(Min = min(Variable), Max = max(Variable)) %>% dplyr::left_join(table_2, by = "Bin")
    }

    var.list[[i]] <- table_3

    rm(table, bins_total, bins_convert, table_2, table_3)
  }

  # var.list

  IV <- plyr::ldply(var.list, function(x){sum(x$IV)})

  names(IV) <- c("Variable", "IV")

  final <- WOE(var.list, IV)
}

#' @export
#' @rdname WOE_IV
WOE <- function(woe_tables, IV_table){
  object <- list(woe_table = woe_tables, IV = IV_table)
  class(object) <- c("WOE", "list")
  object
}

#' @export
woe_plot <- function(woe_table, title) {
  plot1 <- ggplot2::ggplot(woe_table, ggplot2::aes(Bin, ConversionRate)) + ggplot2::geom_line(color = "green4") + ggplot2::geom_hline(yintercept = (sum(woe_table$Converted) / sum(woe_table$Total)))

  if (sum(woe_table$IV) > .05){
    plot2 <- ggplot2::ggplot(woe_table, ggplot2::aes(Bin, IV)) + ggplot2::geom_line(color = "blue")
  } else {
    plot2 <- ggplot2::ggplot(woe_table, ggplot2::aes(Bin, IV)) + ggplot2::geom_line(color = "red")
  }

  if (missing(title)){
    grid.arrange(plot1, plot2, ncol = 2)
  } else {
    gridExtra::grid.arrange(plot1 + ggplot2::ggtitle(title), plot2 + ggplot2::ggtitle(title), ncol = 2)
  }
}

#' @importFrom magrittr "%>%"
woe_table <- function(datatable, var, response = "Response_Var"){
  UseMethod("woe_table")
}

woe_table.default <- function(datatable, var, response = "Response_Var"){
  table <- dataframe %>% dplyr::group_by_(var) %>% dplyr::summarise_(Total = dplyr::n(), Converted = sum(response))

  table <- dplyr::arrange_(table, var)

  names(table)[[var]] <- "Variable"

  table
}

#' @import data.table
#'
woe_table.data.table <- function(datatable, var, response = "Response_Var"){
  table <- datatable[, .(Total = .N, Converted = sum(get(response))), by = eval(noquote(var))]

  table <- dplyr::arrange_(table, var)

  setnames(table, var, "Variable")

  table
}

woe_bin <- function(dataset, table, var, threshold){
  UseMethod("woe_bin")
}

woe_bin.default <- function(dataset, table, var, threshold){
 if (class(dataset[[var]]) == "character" || nrow(table) <=5) {
    .row <- c(1:nrow(table))
    table$Bin <- .row
    table
  } else {
    Bin(table, "Bin", threshold = threshold)
  }
}

woe_bin.data.table <- function(dataset, table, var, threshold){
 if (class(dataset[[var]]) == "character") {
    table[, Bin := .I]
  } else if (nrow(table) < 5) {
    table[, Bin := .I]
  } else {
    Bin(table, "Bin", threshold = threshold)
  }
}

woe_table_2 <- function(){
  UseMethod("woe_table_2")
}

woe_table_2.default <- function(table, bins_convert, bins_total){
  table_2 <- table[,c(-1,-4)] %>% dplyr::group_by_(Bin) %>% dplyr::summarise_each_(funs(sum))

  table_2$Pct_Good <- table_2$Converted / bins_convert

  table_2$Pct_Bad <- (table_2$Total - table_2$Converted) / bins_total

  table_2$ConversionRate <- table_2$Converted / table_2$Total

  table_2$Pct_Total <- (table_2$Total/ bins_total)

  table_2$WOE <- ifelse(table_2$Pct_Good == 0 | table_2$Pct_Bad == 0, 0, log(table2$Pct_Good/table2$Pct_Bad))

  table_2$IV <- (table_2$Pct_Good - table_2$Pct_Bad)*table_2$WOE
}
