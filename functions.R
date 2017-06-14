
#'
#' This function transforms a data frame to a list that can be used in hc_add_series_list
#'
#' @param x a data frame or data table with the data. One column by serie.
#' @param by a characte vector or a list containing the names of the columns corresponding to the series.
#'
#' @return a nested list of the same length as by. Each serie is a list with two elements, name and data.
#'
toHighchartList <- function(x, by){
  lapply(by, function(name){
    list(name = name, data = x[[name]])
  })
}

#'
#' This function adds a named element to a nested list. It is used to add an argument to a series list.
#'
#' It is not the fastest solution but since the number of series should not be large it is not an issue for now.
#'
#' @param list the list with the series data
#' @param arg a list or vector with the values of the argument to add
#' @param name the name of the argument used in highchart, default to the arg name.
#'
#' @return the list with the extra arguments added to each serie.
#'
addArgument <- function(list, arg, name = deparse(substitute(arg))){
  if(length(list) != length(arg)) stop("'list' and 'arg' must have the same length.")
  for (i in 1:length(list)){
    list[[i]] <- c(list[[i]], arg[[i]])
    names(list[[i]])[length(list[[i]])] <- name
  }
  return(list)
}