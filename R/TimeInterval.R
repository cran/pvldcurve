#' Time Interval
#'
#' Calculates time intervals (min) between temporally repeated measurements
#'
#' @param data data frame containing at least a column giving the time (and date) (class POSIXct) of the measurements ordered by
#' sample and chronologically. A column containing the sample IDs is optionally required if several samples were measured.
#' @param sample optional name of the column in data containing the sample ID, default: "sample"
#' @param date.and.time optional name of the column in data containing the time (and date) as class POSIXct,
#' default: "date.and.time"
#' @return the original data frame extended by a numerical vecor containing the time intervals
#' (min) between the measurements of a sample. The first values of each sample is NA since
#' time intervals are computed from row i and i-1.
#' @examples
#' # get example data
#' df <- leaf_drying_data
#'
#' # extend df by time interval
#' df_with_ti <- TimeInterval(df)
#'
#' @import ggplot2
#' @importFrom graphics legend
#' @importFrom stats approx coef confint lm na.omit nls
#' @export


TimeInterval <- function(data,
                         sample = "sample",
                         date.and.time = "date.and.time") {
  # Check validity of data
  data_in <-
    ValidityCheck(data, sample = sample, date.and.time = date.and.time)
  OrderCheck(data_in, sample = sample, date.and.time = date.and.time)


  # initialize vector for loop
  time.interval <- c()


  # loop which returns time interval
  for (i in unique(data_in[[sample]])) {
    data_in_subset <- data_in[data_in[[sample]] == i, ]

    ti <-
      -(difftime(data_in_subset[[date.and.time]][1:(length(data_in_subset[[date.and.time]]) -
                                                      1)],
                 data_in_subset[[date.and.time]][-1], units = "mins"))
    ti <-
      c(NA, ti) # insert NA as the first element of the time interval vector

    time.interval <- as.numeric(append(time.interval, ti))
  }


  return(data.frame(data, time.interval))
}
