#' Relative Water Deficit (RWD)
#'
#' Calculates relative water deficit (\%)
#'
#' @param data data frame with columns of equal length containing at least columns with the fresh weight (g),
#' the dry weight (g) and the saturated fresh weight (g)
#' @param fresh.weight optional name of the column in data containing
#' the numeric fresh weight values (g); default: fresh.weight
#' @param dry.weight optional name of the column in data containing
#' the numeric dry weight values (g); default: dry.weight
#' @param fresh.weight.saturated optional name of the column in data containing
#' the numeric saturated fresh weight values (g); default: fresh.weight.saturated
#' @details Relative water deficit (\%) is calculated as:
#' \deqn{RWD = 100 - 100 * ((FW - DW) (FWs - DW)^-1)}
#' whereas FW = fresh weight, DW = dry weight and FWs = fresh weight at water saturation.
#' @return the original data frame extended by a numeric column with the relative water deficit (RWD) (\%).
#' @examples
#' # get example data
#' df <- leaf_drying_data
#'
#' # extend df by RWD
#' df_with_RWD <- RelativeWaterDeficit(df)
#'
#' @import ggplot2
#' @importFrom graphics legend
#' @importFrom stats approx coef confint lm na.omit nls
#'
#' @export

RelativeWaterDeficit <- function(data,
                                 fresh.weight = "fresh.weight",
                                 dry.weight = "dry.weight",
                                 fresh.weight.saturated = "fresh.weight.saturated") {
  # check validity of data
  data_in <-
    ValidityCheck(
      data,
      fresh.weight = fresh.weight,
      dry.weight = dry.weight,
      fresh.weight.saturated = fresh.weight.saturated
    )


  # calculate RWD
  RWD <- 100 - ((data_in[[fresh.weight]] - data_in[[dry.weight]]) /
                  (data_in[[fresh.weight.saturated]] - data_in[[dry.weight]])) *
    100


  return(data.frame(data, RWD))
}
