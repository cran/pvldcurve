#' Time Since Start
#'
#' Calculates time since start (min) of measurement for temporally repeated measurements
#'
#' @param data data frame containing at least a column giving the the time (and date) of the
#' measurements ordered by sample and chronologically. A column containing the sample IDs is optionally required if several samples
#' were measured.
#' @param sample optional name of the column in data containing the sample
#' IDs, default: "sample".
#' @param date.and.time optional name of the column in data containing the time (and date) as class POSIXct,
#' default: "date.and.time"
#' @return The original data frame extended by a numerical vector containing time since start
#' (min) of the measurements.
#' @examples
#' # get example data frame
#' df <- leaf_drying_data
#'
#' # extend df by time since start
#' df_with_tss <- TimeSinceStart(df)
#'
#' @import ggplot2
#' @importFrom graphics legend
#' @importFrom stats approx coef confint lm na.omit nls
#'
#' @export


TimeSinceStart <- function(data,
                           sample = "sample",
                           date.and.time = "date.and.time") {
  # Check validity of data
  data_in <-
    ValidityCheck(data, sample = sample, date.and.time = date.and.time)
  OrderCheck(data_in, sample = sample, date.and.time = date.and.time)


  # extract data (to ensure that, in case there are variabels in the data with the names as
  # used for the output variabels in the functions below, the correct variabels are used)
  data_in <-
    data.frame(sample = data_in[[sample]], date.and.time = data_in[[date.and.time]])
  names(data_in) <- c(paste0(sample), paste0(date.and.time))



  # initialize vectors for loop
  time.since.start <- c()


  # loop which returns time interval and time.since start
  for (i in unique(data_in[[sample]])) {
    data_in_subset <- data_in[data_in[[sample]] == i, ]

    # calculate time intervals
    ti <-
      -(difftime(data_in_subset[[date.and.time]][1:(length(data_in_subset[[date.and.time]]) -
                                                      1)],
                 data_in_subset[[date.and.time]][-1], units = "mins"))


    tss <-
      cumsum(as.numeric(ti)) # cumulative sum of time intervals
    tss <- c(0, tss)

    time.since.start <- append(time.since.start, tss)
  }


  return(data.frame(data, time.since.start))
}
