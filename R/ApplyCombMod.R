#' Apply a combined exponential and linear model
#'
#' a non linear model combining an exponential and a linear fit is applied to the data using the Gauss-Newton algorithm of nls.
#' starting values are calculated
#' based on the data. Weights are applied to the model based on the estimated insecurity of the data quality.
#'
#' @param data data frame containg x and y data to which the model is ought to be applied to
#' @param y name of column in data containing y data
#' @param x name of column in data containing x data
#' @return model parameters
#' @import ggplot2
#' @importFrom graphics legend
#' @importFrom stats approx coef confint lm na.omit nls



ApplyCombMod <- function(data,
                         y = "y",
                         x = "x") {

  # select data
  y <- data[[y]]
  x <- data[[x]]

  model <- FALSE

  try({
  # the combined exponential and linear model
  model <- nls(
    y ~ ((a * exp(b * x)) + c * x + d),

    # define the starting values
    start = c(
      a = max(y),
      b = -0.05,
      c = mean(rev(diff(y) / diff(x))[1:5]),
      d = min(y) +(mean(rev(diff(y) / diff(x))[1:5])) * max(x)
    ),
    weights = abs(c(c(
      max(y) + max(y) / 60 - y
    )))
  )
}, silent = TRUE)



    try({
  # try again with another b starting value if fitting didn't work
  if(model == FALSE){
    model <- nls(
      y ~ ((a * exp(b * x)) + c * x + d),

      # define the starting values
      start = c(
        a = max(y),
        b = -0.1,
        c = mean(rev(diff(y) / diff(x))[1:5]),
        d = min(y) +(mean(rev(diff(y) / diff(x))[1:5])) * max(x)
      ),
      weights = abs(c(c(
        max(y) + max(y) / 60 - y
      )))
    )
  }
  }, silent = TRUE)


  # try again with another b starting value if fitting didn't work
  try({
    if(model == FALSE){
      model <- nls(
    y ~ ((a * exp(b * x)) + c * x + d),

    # define the starting values
    start = c(
      a = max(y),
      b = -0.05,
      c = mean(rev(diff(y) / diff(x))[1:5]),
      d = min(y) +(mean(rev(diff(y) / diff(x))[1:5])) * max(x)
    ),
    weights = abs(c(c(
      max(y) + max(y) / 60 - y
    )))
  )
}
}, silent = TRUE)


  return(model)

}
