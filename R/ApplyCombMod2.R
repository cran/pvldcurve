#' Apply a combined exponential and linear model
#'
#' a non linear model combining an exponential and a linear fit is applied to the data using nls. starting values are calculated
#' based on the data. Constraints are applied to the model based on the known constraints of the aimed model.
#'
#' @param data data frame containg x and y data to which combined exponentail and linear model is ought to be applied to
#' @param y name of column in data containing y data
#' @param x name of column in data containing x data
#'
#' @return model parameters
#' @import ggplot2
#' @importFrom graphics legend
#' @importFrom stats approx coef confint lm na.omit nls



ApplyCombMod2 <- function(data,
                          y = "y",
                          x = "x") {
  # select data
  y <- data[[y]]
  x <- data[[x]]



  # the combined exponential and linear model
  model <-
    nls(
      y ~ (a * exp(b * x) + d * x + e),

      # define the starting values
      start = c(
        a = max(y),
        b = -0.1,
        d = if (-(abs(y[length(y)] - y[length(y) - 1])) == 0) {
          -0.1
        } else{
          -(abs(y[length(y)] - y[length(y) - 1]))
        },
        # starting value for c is calculated as the negative absolute difference between the last two curve points. It
        # it is zero, than -0.1 is taken, because otherwise the calculation of the interception point of linear and exponential
        # parts is not possible
        e = min(y) + 0.001
      ),


      # define constraints for the starting values
      algorithm = "port",
      # necessary for defining the below constraints
      lower = c(-abs(min(y) * 2),-1000,-1000,-abs(min(y) * 2)),
      # defines an lower constraint for the coeffients
      upper = c(abs(max(y) * 100), 0.0000001,-0.00001, abs(max(y) * 10))
    )   # defines an upper constraint for the coeffients


  return(model)

}
