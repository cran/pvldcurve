#' Leaf area fitting
#'
#' Fits randomly measured leaf area values linearly to fresh weight values. Useful if the leaf area changes during a measurement
#' series but is only randomly measured.
#'
#' @param data data frame, with columns of equal length, containing at least columns with the the fresh.weight (g)
#' and the leaf.area (cm^2) values, ordered by sample by descending fresh weight. A column containing the sample IDs is optionally required
#' if several samples were measured.At least 3 leaf area values are required.
#' @param sample string, optional name of the column in data containing the sample ID, default: "sample"
#' @param fresh.weight optional name of the column in data containing the numeric fresh weight values (g);
#' default: "fresh.weight"
#' @param leaf.area optional name of the column in data containing the numeric single-sided leaf area values (cm^2);
#' default: "leaf.area"
#' @details fits given leaf area values linearly to the respective fresh weight values and calculates leaf area values
#' for the fresh weight values based on the fit
#' @return the original data frame extended by a numeric column containing the fitted leaf area values (leaf.area.fitted)
#' @examples # get example data
#' df <- data.frame(
#'   sample = c(as.integer(rep(1, times = 6))),
#'   fresh.weight = c(1.23, 1.19, 1.15, 1.12, 1.09, 1.0),
#'   leaf.area = c(10.5, NA, NA, 9.8, NA, 8.4))
#' # fit leaf area
#' df_new <- FitLeafArea(df)
#'
#' @import ggplot2
#' @importFrom graphics legend
#' @importFrom stats approx coef confint lm na.omit nls
#'
#' @export



FitLeafArea <- function(data,
                        sample = "sample",
                        fresh.weight = "fresh.weight",
                        leaf.area = "leaf.area") {
  # check validity of data
  data_in <-
    ValidityCheck(
      data,
      sample = sample,
      fresh.weight = fresh.weight,
      leaf.area = leaf.area
    )
  OrderCheck(data, sample = sample, fresh.weight = fresh.weight)



  leaf.area.fitted <- c()

  for (i in 1:length(unique(data_in[[sample]]))) {
    # subset data
    sub.sample <- unique(data_in[[sample]])[i]
    data_in_subset_original <-
      data_in[data_in[[sample]] == sub.sample, ]
    data_in_subset <-
      data_in_subset_original[!is.na(data_in_subset_original[[fresh.weight]]), ]  # remove rows where fresh weight is NA
    data_in_subset <-
      data_in_subset[!is.na(data_in_subset[[leaf.area]]), ]  # remove rows where leaf area is NA
    data_in_subset <-
      data.frame(leaf.area = data_in_subset[[leaf.area]], fresh.weight = data_in_subset[[fresh.weight]])

    try({
      # makes sure the correct error is printed in case fitting didn't work
      all.fine <-
        FALSE   # helping variable. set to TRUE if everything in the try wrapper worked


      # only do fitting if not all leaf.area values are equal
      if (length(unique(na.omit(data_in_subset$leaf.area))) >= 2) {
        # linear fitting
        lin <-
          nls(
            leaf.area ~ (a * fresh.weight + b),
            data = data_in_subset,
            start = c(
              a = 1,
              b = mean(data_in_subset$leaf.area)
            )
          )

        a <- coef(lin)[1] # extract coefficient a from model
        b <- coef(lin)[2]



        # calculate fitted leaf area values for fresh weight values and append to previous fitted leaf.area values
        leaf.area.fitted <-
          c(leaf.area.fitted, a * data_in_subset_original[[fresh.weight]] + b)


        # if all leaf area values are equal, repeat it for the length of the output vector
      } else{
        leaf.area.fitted <- c(leaf.area.fitted, c(rep(
          na.omit(data_in_subset_original[[leaf.area]])[1],
          times = length(data_in_subset_original[[fresh.weight]])
        )))
      }



      all.fine <- TRUE
    }, silent = TRUE)


    # give warning and add NAs for leaf.area if fitting didn't work
    if (all.fine == FALSE) {
      warning(paste0("sample ", sub.sample),
              " Fitting of leaf area was unsuccessful")
      leaf.area.fitted <-
        c(leaf.area.fitted, c(rep(
          NA, times = length(data_in_subset_original[[fresh.weight]])
        )))
    }
  }

  return(data.frame(data, leaf.area.fitted))
}
