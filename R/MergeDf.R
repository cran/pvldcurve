#' Merges data to data frames
#'
#' merges data frames containing all neccessary informations for plotting with PlotOutput()
#'
#' @param x vector containing the x values
#' @param y vector containing the y values
#' @param y2 optional vector containing the values for the second y coordinates
#' @param y3 optional vector containing the values for the third y coordinates
#' @param legend name of the y values in the legend
#' @param legend.y2 optional name of the second y values in the legend
#' @param legend.y3 optional name of the third y values in the legend
#' @return data frame with columns containing all above information in equalized length as requested by gglot
#'
#' @import ggplot2
#' @importFrom graphics legend
#' @importFrom stats approx coef confint lm na.omit nls





MergeDf <- function(x,
                    y,
                    y2 = FALSE,
                    y3 = FALSE,
                    legend,
                    legend.y2 = FALSE,
                    legend.y3 = FALSE) {
  # merge data and its labeling
  merged_df <- data.frame(y = y,
                          x = x,
                          legend = rep(legend, times = length(y)))   # helping variable for the appropriate display of the legend for the raw data


  if (y2[1] != FALSE) {
    merged_df <- cbind(merged_df,
                       y2 = y2,
                       legend.y2 = rep(legend.y2, times = length(y2)))
  }

  if (y3[1] != FALSE) {
    merged_df <- cbind(merged_df,
                       y3 = y3,
                       legend.y3 = rep(legend.y3, times = length(y3)))
  }

  return(data.frame(merged_df))
}
