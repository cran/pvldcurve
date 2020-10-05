#' Plot Output
#'
#' plots the data as specified
#'
#' @param sub.sample sample ID
#' @param x vector containing the x coordinates of the data to be plotted as points
#' @param y vector containing the y coodinates of the data to be plotted as points
#' @param y2 optional vector containing the second y coordinates of the data to be plotted as points
#' @param y3 optional vector containing the third y coordinates of the data to be plotted as points
#' @param legend.y string, name of data points to be printed in the legend
#' @param legend.y2 string, optional name of second set of data points to be printed in the legend
#' @param legend.y3 string, optional name of third set of data points to be printed in the legend
#' @param x.axis string, label of x axis
#' @param y.axis sring, label of y axis
#' @param x.intercept vector containg the x coordinate of the intercept
#' @param y.intercept, optional vector containg the y coordinate of the intercept
#' @param legend.x.intercept string, name of x.intercept to be printed in the legend
#' @param line.x vector containing the x coordinate for the lines
#' @param line.y vector containing the y coordinates for the line
#' @param line.y2 vector containing the y coordinates for the second line
#' @param line.y3 vector containing the y coordinates for the second line
#' @param legend.line.y string, name of line to be printed in the legend
#' @param legend.line.y2 string, name of second line to be printed in the legend
#' @param legend.line.y3 string, name of third line to be printed in the legend
#' @param show.legend boolean, specifies whether a legend is to be printed
#' @return graphic
#' @import ggplot2
#' @importFrom graphics legend
#' @importFrom stats approx coef confint lm na.omit nls





PlotOutput <- function(sub.sample,
                       x,
                       y,
                       y2 = FALSE,
                       y3 = FALSE,
                       legend.y,
                       legend.y2 = FALSE,
                       legend.y3 = FALSE,
                       x.axis,
                       y.axis,
                       x.intercept = FALSE,
                       y.intercept = FALSE,
                       legend.x.intercept = FALSE,
                       line.x,
                       line.y,
                       line.y2 = FALSE,
                       line.y3 = FALSE,
                       legend.line.y,
                       legend.line.y2 = FALSE,
                       legend.line.y3 = FALSE,
                       show.legend = show.legend) {
  # merge point data and its labeling
  point_data <-
    MergeDf(
      x = x,
      y = y,
      y2 = y2,
      legend = legend.y,
      legend.y2 = legend.y2
    )


  # merge data for intercept and labeling of intercept for legend
  if (x.intercept[1] != FALSE) {
    intercept_data <- MergeDf(
      x = rep(x.intercept, times = 2),
      y = c(min(na.omit(c(
        y, y2
      )[-length(c(y2, y))])), max(na.omit(c(
        y, y2
      )[-length(c(y2, y))]))),
      legend = legend.x.intercept
    )
  }


  # merge lines data and lines data labeling
  line_data <-
    MergeDf(
      x = line.x,
      y = line.y,
      y2 = line.y2,
      y3 = line.y3,
      legend = legend.line.y,
      legend.y2 = legend.line.y2,
      legend.y3 = legend.line.y3
    )



  title = paste0("sample ", sub.sample)


  # plot
  gdp = ggplot(environment = environment()) +
    geom_point(data = point_data, aes(x = x, y = y, shape = legend)) +   # raw data plotted as points
    geom_line(data = line_data, aes(
      x = x,
      y = y,
      lty = legend,
      color = legend
    ))    # first fitting line

  # set limits for y axis in dependence of data
  if (line.y2[1] != FALSE) {
    gdp = gdp + ylim(min(c(y, y2)[-length(c(y2, y))]), max(c(y, y2)[-length(c(y2, y))]))
  } else{
    gdp = gdp + ylim(min(c(y, line.y)), max(y))
  }


  # plot intercept as line
  if (x.intercept[1] != FALSE) {
    gdp = gdp + geom_line(data = intercept_data, aes(
      x = x,
      y = y,
      lty = legend,
      color = legend
    ))
  }



  # this code is only added if further lines are to be added
  if (line.y2[1] != FALSE) {
    gdp = gdp + geom_line(data = line_data, aes(
      x = x,
      y = y2,
      lty = legend.y2,
      color = legend.y2
    ))
  }


  if (line.y3[1] != FALSE) {
    gdp = gdp + geom_line(data = line_data, aes(
      x = x,
      y = y3,
      lty = legend.y3,
      color = legend.y3
    ))
  }

  # a second row of points and further lines are added to the plot if specified
  if (y2[1] != FALSE) {
    gdp = gdp + geom_point(data = point_data, aes(x = x, y = y2, shape = legend.y2)) +
      geom_hline(yintercept = 0, linetype = "solid") +
      geom_hline(yintercept = y.intercept, linetype = "dotted")
  }



  # styling of plot
  gdp = gdp + scale_shape_manual(name = title, values = c(21, 16)) +
    scale_linetype_manual(name = " ", values = c(1, 2, 3, 1)) +
    scale_color_manual(name = " ",
                       values = c("grey54", "grey20", "grey0", "grey0")) +
    theme_classic() +
    labs(x = x.axis, y = y.axis)


  # add legend, if wished
  if (show.legend == FALSE) {
    gdp = gdp + theme(legend.position = "none")
  } else{
    gdp = gdp + theme(
      legend.position = c(0.73, 0.72),
      legend.background = element_rect(fill = NA)
    ) +
      guides(shape = guide_legend(order = 1),
             color = guide_legend(order = 0))
  }



  print(gdp)
}
