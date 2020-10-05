#' Order Check
#'
#' Checks for the correct ordering of the data: increasing for date.and.time and time.since start, decreasing for fresh.weight and
#' water.potential. Done separatly for each sample. An individualized warning is printed if not ordered correctly.
#'
#' @param data data frame containing the data to be checked
#' @param sample name of the column containing the sample IDs, if present in data
#' @param date.and.time name of the column containing the date and time (POSIXct) of the measurements, if present in data
#' @param fresh.weight name of the column containing the numeric fresh weight values, if present in data
#' @param water.potential name of the column containing the numeric water potential values, if present in data
#' @param time.since.start name of the column containing the numeric time since start values, if present in data
#' @return no return value
#' @import ggplot2
#' @importFrom graphics legend
#' @importFrom stats approx coef confint lm na.omit nls




OrderCheck <- function(data,
                       sample = FALSE,
                       date.and.time = FALSE,
                       fresh.weight = FALSE,
                       water.potential = FALSE,
                       time.since.start = FALSE) {
  for (i in unique(data[[sample]])) {
    # subset data
    sub.sample <- unique(data[[sample]])[i]
    data_in_subset <- subset(data, sample == i)


    # check for anticipated order
    if (fresh.weight != FALSE) {
      if (any(diff(na.omit(data_in_subset[[fresh.weight]]))  >= 0)) {
        warning(
          paste0("sample ", sub.sample),
          ": Fresh weight values are not strictly descending"
        )
      }
    }

    if (water.potential != FALSE) {
      if (any(diff(na.omit(data_in_subset[[water.potential]]))  >= -min(na.omit(data_in_subset[[water.potential]])) * 0.15)) {
        warning(
          paste0("sample ", sub.sample),
          ": Water potential values are not strictly descending"
        )
      }
    }

    if (time.since.start != FALSE) {
      if (any(diff(na.omit(data_in_subset[[time.since.start]]))  <= 0)) {
        warning(
          paste0("sample ", sub.sample),
          ": Time since start values are not strictly ascending"
        )
      }
    }

    if (date.and.time != FALSE) {
      if (any(as.numeric(difftime(na.omit(data_in_subset[[date.and.time]] [1:length(data_in_subset[[date.and.time]]) -
                                                        1]), na.omit(data_in_subset[[date.and.time]][-1])))
              >= 0)) {
        warning(
          paste0("sample ", sub.sample),
          ": Date and time values are not ordered chronologically"
        )
      }
    }
  }
}
