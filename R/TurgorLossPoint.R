#' Turgor Loss Point
#'
#' Determines the x coordinate (RWD) of the turgor loss point in a set of experimentally obtained
#' pressure volume curves.
#'
#' @param data data frame containing columns of equal lengths giving at least the numerical
#' coordinates of the curve: water potential (bar) and RWD (\%), ordered by sample by descending water potential. A
#' column containing the sample IDs is optionally required if several samples were measured.
#' @param sample optional name of the column in data containing the sample ID, default: "sample"
#' @param water.potential optional name of the column in data containing the numeric water potential values (bar), default: "water.potential"
#' @param RWD optional name of the column in data containing numeric relative water deficit values (\%), default: "RWD"
#' @param graph set FALSE if no plots are to be returned
#' @param show.legend set FALSE if no legend is to be shown in the plots
#' @return List splitted by sample consisting of
#' \item{turgor.loss.point}{coordinates of the turgor loss point (RWD)}
#' \item{formula}{formula of the exponential and linear part of the combined fits}
#' \item{coef}{coefficients of combined model}
#' \item{conf_int}{upper (97.5 \%) and lower (2.5 \%) border of 95 \% confidence interval of model parameters}
#' If graph = TRUE, the plotted original data is displayed with the exponential and
#' linear fit of the combined model as well as the x-coordinate (RWD) of the turgor loss point.
#'
#' @details Before using this function, check the raw data for an initial plateau. If the exponential decline does not onset directly,
#' fitting might not succeed. \cr \cr
#' The data is fitted using the Gauss-Newton algorithm of nls() to a combined exponential and linear
#' model. The exponential and linear parts are extracted and RWD at turgor loss point is localized at their point of minimum distance.
#'
#' @examples
#' # get sample data
#' data <- RelativeWaterDeficit(pressure_volume_data)[pressure_volume_data$sample == 1, ]
#'
#' # identify turgor loss point in curve
#' turgor_loss_point <- TurgorLossPoint(data)
#'
#' @import ggplot2
#' @importFrom graphics legend
#' @importFrom stats approx coef confint lm na.omit nls
#'
#' @export

TurgorLossPoint <- function(data,
                            sample = "sample",
                            water.potential = "water.potential",
                            RWD = "RWD",
                            graph = TRUE,
                            show.legend = TRUE) {
  # check validity of data
  if (RWD != "water.loss") {
    # in case of calculation of saturated water content, turgor loss point is calculated with the use of water loss instead of RWD.
    # Therefore, validity check is done in the function SaturatedWaterContent
    data_in <-
      ValidityCheck(data,
                    sample = sample,
                    water.potential = water.potential,
                    RWD = RWD)
    OrderCheck(data_in, sample = sample, water.potential = water.potential)
  } else{
    data_in <- data
  }


  # prepare list to put data from for loop into
  tlp_model <- list()



  # determination of stomatal closure individually for each sample
  for (i in 1:length(unique(data_in[[sample]]))) {
    # subset data
    sub.sample <- unique(data_in[[sample]])[i]
    data_in_subset <- data_in[data_in[[sample]] == sub.sample, ]
    data_in_subset <- data_in_subset[!is.na(data_in_subset[[RWD]]),]
    data_in_subset[[water.potential]] <- -1/data_in_subset[[water.potential]]  # transform data



    try({
      # try makes sure the execution of the code proceeds if an error occurs while fitting
      all.fine <-
        FALSE      # helping variable which is set TRUE if everything worked


      # fit a combined linear and exponential model
      data_in_subset$norm.x <- data_in_subset[[RWD]] - min(data_in_subset[[RWD]]) # normalize data to start with 0 (neccessary if data was deleted with InitialFluct())
      m <-
        ApplyCombMod(data_in_subset, y = water.potential, x = "norm.x")



      # extract coefficients from model
      a <- as.numeric(coef(m)[1])
      b <- as.numeric(coef(m)[2])
      c <- as.numeric(coef(m)[3])
      d <- as.numeric(coef(m)[4])

      coef <- coef(m)

      try({conf_int <- suppressMessages(confint(m))}, silent = TRUE)


      # calculate interception point of linear and exponential part of model
       try({RWD.tlp <- suppressWarnings((log(c / (a * b)) / b) + min(data_in_subset[[RWD]]))}, silent = TRUE) # min is added because it was substracted for normalization


      # calculate data points of exponential and linear part of model
      xnew <- 1:max(data_in_subset[[RWD]])
      xnew_plot <- (1 : max(data_in_subset[[RWD]]) + min(data_in_subset[[RWD]])) # min needs to be added because it was substracted before
      all_fit <- a * exp(b * xnew) + c * xnew + d
      exp_fit <- a * exp(b * xnew) + d
      lin_fit <- c * xnew + d



      if (length(which(data_in_subset[[RWD]] > RWD.tlp)) < 3)  {
        # if there are only 3 data points following the turgor loss point,
        # the quality of the data is not good enough to give results, so a warning message is returned instead.
        warning(
          paste0("sample ", sub.sample),
          ": fitting of data to a combined model didn't work"
        )
      } else{
        # plot
        if (graph == TRUE) {
          suppressMessages(
          suppressWarnings(
            PlotOutput(
              sub.sample = sub.sample,
              x = data_in_subset[[RWD]],
              y = data_in_subset[[water.potential]],
              legend.y = "leaf water potential",
              x.axis = "Relative Water Deficit (%)",
              y.axis = expression(paste("-", Psi ^ -1 , (
                bar ^ -1
              ))),
              x.intercept = RWD.tlp,
              legend.x.intercept = "turgor loss point",
              line.y = lin_fit,
              line.y2 = exp_fit,
              line.y3 = all_fit,
              line.x = xnew_plot,
              legend.line.y = "linear part of fit",
              legend.line.y2 = "exp. part of fit",
              legend.line.y3 = "entire fit",
              show.legend = show.legend
            )
          )
          )
        }




        # add results to list
        tlp_model[[paste0("sample ", sub.sample)]] <-
          list(
            turgor.loss.point = list(RWD = RWD.tlp),
            formula = list(linear = "-1/water.potential ~ (c * RWD + d)",
                           exponential = "-1/water.potential ~ (a * exp(b * RWD) + d)"),
            coef = coef
          )

        if(exists("conf_int")){
          conf.int = list("2.5 %" = conf_int[, 1], "97.5 %" = conf_int[, 2])
          tlp_model[[paste0("sample ", sub.sample)]] <- append(tlp_model[[paste0("sample ", sub.sample)]],
                                                               list(conf.int))
          names(tlp_model[[paste0("sample ", sub.sample)]])[4] <- paste0("conf.int")
        }
      }



      all.fine <- TRUE
    }, silent = TRUE)
    # belongs to the try() function. Makes sure that no error is printed if something with the fit didn't work.
    # Instead, print warning and skip to the next item of the loop

    if (all.fine == FALSE) {
      warning(paste0("sample ", sub.sample),
              ": fitting of data to a combined model didn't work")
    }
  }

  return(tlp_model)
}
