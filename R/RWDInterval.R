#' Mean relative water deficit (RWD) of an interval
#'
#' Calculates relative water deficit (\%) as mean value of a measurement interval
#'
#' @param data data frame with columns of equal length containing at least columns with the fresh weight (g),
#' the dry weight (g) and the saturated fresh weight (g), ordered by sample by descending by fresh weight. A column containing
#' the sample IDs is optionally required if several samples were measured.
#' @param sample optional name of the column in data containing the sample IDs, default: "sample"
#' @param fresh.weight optional name of the column in data containing
#' the numeric fresh weight values (g); default: "fitted.fw"
#' @param dry.weight optional name of the column in data containing
#' the numeric dry weight values (g); default: "dry.weight"
#' @param fresh.weight.saturated optional name of the column in data containing
#' the numeric saturated fresh weight values (g); default: "fresh.weight.saturated"
#' @details First, the mean fresh weight is calculated for each measurement interval. Relative water deficit (\%) is
#' then calculated as:
#' \deqn{RWD = 100 - 100 * ((mFW - DW) (FWs - DW)^-1)}
#' whereas mFW = mean fresh weight, DW = dry weight and FWs = fresh weight at water saturation.
#' @return the original data frame extended by a numeric column with the mean relative water deficit for the measurement
#' interal (RWD.interval) (\%).
#' @examples
#' # get example data
#' df <- leaf_drying_data
#'
#' # extend df by RWD
#' df_with_RWD <- RWDInterval(df, fresh.weight = "fresh.weight")
#'
#' @import ggplot2
#' @importFrom graphics legend
#' @importFrom stats approx coef confint lm na.omit nls
#'
#' @export

RWDInterval <- function(data,
                        sample = "sample",
                        fresh.weight = "fitted.fw",
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


  # extract data (to ensure that, in case there are variabels in the data with the names as
  # used for the output variabels in the functions below, the correct variabels are used)
  data_in <-
    data.frame(data_in[[sample]], data_in[[fresh.weight]], data_in[[dry.weight]],
               data_in[[fresh.weight.saturated]])
  names(data_in) <-
    c(
      paste0(sample),
      paste0(fresh.weight),
      paste0(dry.weight),
      paste0(fresh.weight.saturated)
    )




  # calculate Weight Difference
  data_in <-
    WeightDifference(data_in, sample = sample, fresh.weight = fresh.weight)


  RWD.interval <- c()

  # calculate RWD
  for (i in 1:length(unique(data_in[[sample]]))) {
    # subset data
    sub.sample <- unique(data_in[[sample]])[i]
    data_in_subset <- data_in[data_in[[sample]] == sub.sample,]


    # calculate mean fresh weight for each measurement interval
    data_in_subset$mean.fw <-
      data_in_subset[[fresh.weight]] + data_in_subset$weight.diff / 2

    RWD.interval <- c(RWD.interval,
                      100 - ((data_in_subset$mean.fw - data_in_subset[[dry.weight]]) /
                               (data_in_subset[[fresh.weight.saturated]] - data_in_subset[[dry.weight]])
                      ) * 100)

  }

  return(data.frame(data, RWD.interval))
}
