#' Weight Difference
#'
#' Calculates weight changes between temporally repeated measurements
#'
#' @param data data frame containing at least a numeric column containing the measured weights (g), ordered chronologically by sample. A
#' column containing the sample IDs is optionally required if several samples were measured.
#' @param sample optional name of the column in data containing the sample ID, default: "sample"
#' @param fresh.weight optional name of the column in data containing the numeric fresh weight (g) values, default: "fresh.weight"
#' @return the original data frame extended by a numeric column containing the absolute
#' differences in fresh weight (g) beween the measurements of a sample. The first value of each
#' sample is NA since weight differences are computed from row i and i-1.
#' @examples
#' # get example data
#' df <- leaf_drying_data
#'
#' # extend df by weight difference
#' df_with_WD <- WeightDifference(df)
#'
#' @import ggplot2
#' @importFrom graphics legend
#' @importFrom stats approx coef confint lm na.omit nls
#'
#' @export


WeightDifference <- function(data,
                             sample = "sample",
                             fresh.weight = "fresh.weight") {
  # Check validity of data
  data_in <-
    ValidityCheck(data, sample = sample, fresh.weight = fresh.weight)
  OrderCheck(data_in, sample = sample, fresh.weight = fresh.weight)


  # initialize vectors for loop
  weight.diff <- c()


  # loop which returns weight differences
  for (i in unique(data_in[[sample]])) {
    data_in_subset <- data_in[data_in[[sample]] == i, ]

    wd <-
      (data_in_subset[[fresh.weight]][1:(length(data_in_subset[[fresh.weight]]) -
                                              1)] -
            data_in_subset[[fresh.weight]][-1])
    wd <-
      c(NA, wd) # insert NA as the first element of the weight difference vector

    weight.diff <- as.numeric(append(weight.diff, wd))
  }


  return(data.frame(data, weight.diff))
}
