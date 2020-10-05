#' Correct continuous fresh weight measurements
#'
#' Corrects fresh weight, measured continuously on a desiccating leaf for determination of minimum conductance, by fitting
#' the fresh weight values to a combined exponential and linear model
#'
#' @param data data frame, at least with a column containing numeric fresh weight (g) and time since start (min) values,
#' ordered by sample by descending fresh.weight.
#' A column containing the sample IDs is optionally required if several samples were measured.
#' @param sample optional name of the column in data containing the sample IDs, default: "sample"
#' @param fresh.weight optional name of the column in data containing the fresh weight values (g), default: "fresh weight"
#' @param time.since.start optional name of the column in data containing the time since start (min) values, default: "time.since.start"
#' @param graph set FALSE if no plots are to be returned
#' @param show.legend set FALSE if no legend is to be be shown in the plots
#' @details Determination of minimum conductance via a leaf drying curve requires fresh weight to be measured continuously on a
#' desiccating leaf. The fresh weight values are then used to calculate leaf conductance. If several leaves are measured on one scale, the
#' measurements are prone to noises, which influence conductance values largely. Here, the fresh weight data is corrected for noises by
#' fitting it to a combined linear and exponential model using the port algorithm of nls(). \cr
#' Before using this function, check the raw
#' data for an initial plateau. If the exponential decline does not onset directly, fitting might not succeed.
#' @return the original data frame (data) extended by a numeric column containing the fitted fresh weight values ("fitted.fw")
#' @examples # get example data
#' df <- TimeSinceStart(leaf_drying_data)
#' # remove plateauing data
#' df <- df[df$fw.plateau != "yes",]
#' # extend the data frame by saturated fresh weight
#' df <- FittedFW(df)
#'
#' @import ggplot2
#' @importFrom graphics legend
#' @importFrom stats approx coef confint lm na.omit nls
#'
#' @export


FittedFW <- function(data,
                     sample = "sample",
                     fresh.weight = "fresh.weight",
                     time.since.start = "time.since.start",
                     graph = TRUE,
                     show.legend = TRUE) {
  # check validity and order of data
  data_in <-
    ValidityCheck(
      data,
      sample = sample,
      fresh.weight = fresh.weight,
      time.since.start = time.since.start
    )
  OrderCheck(
    data_in,
    sample = sample,
    fresh.weight = fresh.weight,
    time.since.start = time.since.start
  )



  fwneu <- c()

  for (i in 1:length(unique(data_in[[sample]]))) {
    # subsetting data
    sub.sample <- unique(data_in[[sample]])[i]
    data_in_subset <- data_in[data_in[[sample]] == sub.sample,]



    # extract data
    y.data <-
      data_in_subset[[fresh.weight]]
    x.data <-
      data_in_subset[[time.since.start]] / max(data_in_subset[[time.since.start]]) # normalizse x data
    norm.data <- data.frame(x.data, y.data)


    try({
      # try makes sure the execution of the code proceeds if an error occurs while fitting
      all.fine <-
        FALSE      # helping variable which is set TRUE if everything worked


      # apply combined exponential and linear model to data
      model <-
        ApplyCombMod2(norm.data, y = "y.data", x = "x.data")



      # calculate fitted fresh weight values based on normalized values
      fwneu_i <-
        as.numeric(coef(model)[1]) * exp(as.numeric(coef(model)[2]) * x.data) +
        as.numeric(coef(model)[3]) * x.data + as.numeric(coef(model)[4])



      # plot original data with fitted data
      if (graph == TRUE) {
        suppressWarnings(
          PlotOutput(
            sub.sample = sub.sample,
            x = data_in_subset[[time.since.start]],
            y = data_in_subset[[fresh.weight]],
            legend.y = "orig. fresh weight",
            x.axis = "time since start (min)",
            y.axis = "fresh weight (g)",
            line.x = data_in_subset[[time.since.start]],
            line.y = fwneu_i,
            legend.line.y = "fitted fresh weight",
            show.legend = show.legend
          )
        )
      }

      fwneu <- c(fwneu, fwneu_i)


      all.fine <- TRUE


    }, silent = TRUE)
    # belongs to the try() function. Makes sure that no error is printed if something with the fit didn't work.
    # Instead, print warning and skip to the next item of the loop

    if (all.fine == FALSE) {
      warning(paste0("sample ", sub.sample),
              ": fitting of fresh weight values didn't work")
      fwneu <-
        c(fwneu, c(rep(NA, times = length(
          data_in_subset[[fresh.weight]]
        ))))
    }


  }
  return(data.frame(data_in, fitted.fw = fwneu))
}
