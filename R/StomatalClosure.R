#' Point of stomatal closure
#'
#' Determines the point of stomatal closure in a set of experimentally obtained leaf drying curves.
#' Stomatal closure happens when the curve irreversibly settles to linear water loss.
#'
#' @param data data frame containing columns of equal length giving the numerical
#' coordinates of the curve: time since start (minutes), conductance (mmol m^-2 s^-1) and RWD (\%), ordered by sample by ascending
#' time since start. A column containing the sample IDs is optionally required if several samples were measured.
#' @param sample optional name of the column in data containing the sample ID (if available), default: "sample"
#' @param time.since.start optional name of the column in data containing numeric time since start values (min), default: "time.since.start"
#' @param conductance optional name of the column in data containing numeric leaf conductance values (mmol m^-2 s^-1), default: "conductance"
#' @param RWD optional name of the column in data containing numeric relative water deficit (\%) values, default: "RWD.interval" (RWD average
#' of an interval as outputted by RWDInterval)
#' @param threshold sensitivity for the determination of stomatal closure. 60 by default.
#' @param graph set FALSE if no plots are to be returned
#' @param show.legend set FALSE if no legend is to be be shown in the plots
#' @return List splitted by sample consisting of
#' \item{stomatal.closure}{coordinates of the point of stomatal closure (time.since.start, RWD, conductance)}
#' \item{formula}{formula of the exponential and linear part of the combined fits}
#' \item{coef}{coefficients of combined model}
#' \item{conf_int}{upper (97.5 \%) and lower (2.5 \%) border of 95 \% confidence interval of model parameters}
#' If graph = TRUE, the plotted original data is displayed with the exponential and
#' linear fit of the combined model as well as the x-coordinate (time.since.start) of the point of stomatal closure.
#'
#' @details Before using this function, check the raw data for an initial plateau. If the exponential decline does not onset directly,
#' fitting might not succeed. \cr
#' The conductances by time since start curves are fitted using the Gauss-Newton algorithm of nls() to a
#' combined exponential and linear model. The exponential and linear parts are extracted and time since start at stomatal closure is
#' localized at the point where the slope of the exponential part of the fit is higher than a threshold value. The threshold value
#' is calculated by the use of the parameter b of the exponential part of the fit
#' (a * exp(b * x)): -(b^2 * sens). The sensitivity constant (sens) is 60 by default and can be specified individually by the argument
#' 'threshold'. \cr
#' Minimum conductance (gmin) at stomatal closure is the conductance value of the overall fit at stomatal closure.
#' RWD at stomatal closure is then calculated by linear regression of RWD and time since start.
#'
#' @examples
#' # get example data
#' df <- WeatherAllocation(leaf_drying_data, weather_data)   # allocate weather to weight loss data
#' df <- TimeSinceStart(df) # calculate time since start
#' df <- df[df$fw.plateau != "yes",] # remove plateauing data
#' df <- FittedFW(df, graph = FALSE) # correct noises in fresh weight
#' df <- RWDInterval(df, fresh.weight = "fitted.fw") # calculate RWD based in the intervals
#' df <- Conductance(df, fresh.weight = "fitted.fw") # calculate conductance
#'
#' # identify stomatal closure in curve and get graphs
#' sc <- StomatalClosure(df)
#'
#' @import ggplot2
#' @importFrom graphics legend
#' @importFrom stats approx coef confint lm na.omit nls
#'
#' @export


StomatalClosure <- function(data,
                            sample = "sample",
                            time.since.start = "time.since.start",
                            conductance = "conductance",
                            RWD = "RWD.interval",
                            threshold = FALSE,
                            graph = TRUE,
                            show.legend = TRUE) {
  # Validity Check
  data_in <-
    ValidityCheck(
      data,
      sample = sample,
      time.since.start = time.since.start,
      conductance = conductance,
      RWD = RWD
    )
  OrderCheck(data_in, sample = sample, time.since.start = time.since.start)




  # prepare list to put data from for loop into
  sc.model <- list()



  # determination of stomatal closure individually for each sample
  for (i in 1:length(unique(data_in[[sample]]))) {
    # subsetting data
    sub.sample <- unique(data_in[[sample]])[i]
    data_in_subset <- data_in[data_in[[sample]] == sub.sample, ]
    data_in_subset <-
      data_in_subset[!is.na(data_in_subset[[conductance]]), ]
    data_in_subset <- data_in_subset[!is.na(data_in_subset[[RWD]]), ]



    try({
      # try makes sure the execution of the code proceeds if an error occurs while fitting
      all.fine <-
        FALSE      # helping variable which is set TRUE if everything worked


      # fit a combined linear and exponential model
      data_in_subset$norm.x <-
        data_in_subset[[time.since.start]] - min(data_in_subset[[time.since.start]]) # normalize data to start with 0 (neccessary if data was deleted with InitialFluct())
      m <-
        ApplyCombMod(data_in_subset,
                     y = conductance,
                     x = "norm.x")



      # extract coefficients and confidence intervals of coefficients from model
      a <- as.numeric(coef(m)[1])
      b <- as.numeric(coef(m)[2])
      c <- as.numeric(coef(m)[3])
      d <- as.numeric(coef(m)[4])

      coef <- coef(m) # all coeffiencts

      try({
        conf_int <-
          suppressMessages(confint(m))
      }, silent = TRUE)
      # extracts confidence intervals of coefficients from model and makes sure no messages are printed



      # calculate fit of exponential and linear part of model
      xnew <- 1:max(data_in_subset[[time.since.start]])
      xnew_plot <-
        (1:max(data_in_subset[[time.since.start]]) + min(data_in_subset[[time.since.start]])) # min needs to be added because it was substracted before
      all_fit_plot <- a * exp(b * xnew) + c * xnew + d
      exp_fit_plot <-
        a * exp(b * xnew) + d
      lin_fit_plot <-
        c * xnew + d
      exp_slope <- c(diff(exp_fit_plot))


      if (threshold == FALSE) {
        sens = -(b ^ 2 * 60)
      }else{
        sens = -(b ^2 * threshold)
      }

      # get point of stomatal closure and gmin at stomatal closure
      time.sc <-
        (which(exp_slope  > sens)[[1]]) + min(data_in_subset[[time.since.start]])  # min needs to be added because it was substracted for the normalization
      gmin.sc <-
        a * exp(b * (time.sc - min(data_in_subset[[time.since.start]]))) + c * (time.sc - min(data_in_subset[[time.since.start]])) + d


      # alt:
      # all_slope <- c(NA, diff(exp_fit_plot))
      # threshold == 45000/a
      # time.sc <- (which(all_slope - c  > 1/b/threshold)[[1]]) + min(data_in_subset[[time.since.start]])  # min needs to be added because it was substracted for the normalization



      # calculate RWD.sc by linear regression of RWD vs. time in the linear region
      time.linear <-
        data_in_subset[[time.since.start]][data_in_subset[[time.since.start]] > time.sc]   # time points where conductance data is linear only
      RWD.linear <-
        data_in_subset[[RWD]][data_in_subset[[time.since.start]] > time.sc]    # equivalent RWD values



      if (length(RWD.linear) < 3) {
        # if there are only 3 data points in the linear region, the quality of the data is not good enough to proceed.
        warning(
          paste0("sample ", sub.sample),
          ": not enough data points (< 3) in the linear region of the leaf drying curve"
        )
      } else{
        li <- lm(RWD.linear ~ time.linear)     # linear regression
        RWD.sc <-
          coef(li)[2] * time.sc + coef(li)[1]    # rwd at stomatal closure is where time at stomatal closure intercepts with the linear regression line




        # plot conductance vs. time since start with the analyzed curve parameters
        if (graph == TRUE) {
          suppressMessages(suppressWarnings(
            PlotOutput(
              sub.sample = sub.sample,
              x = data_in_subset[[time.since.start]],
              y = data_in_subset[[conductance]],
              legend.y = "leaf conductance",
              x.axis = "time since start (min)",
              y.axis = expression('g ' * ('mmol ' * m ^ -2 * s ^ -1)),
              x.intercept = time.sc,
              legend.x.intercept = "stomatal closure",
              line.y = all_fit_plot,
              line.y2 = lin_fit_plot,
              line.y3 = exp_fit_plot,
              line.x = xnew_plot,
              legend.line.y = "entire fit",
              legend.line.y2 = "linear part of fit",
              legend.line.y3 = "exp. part of fit",
              show.legend = show.legend
            )
          ))
        }



        # merge all data to list
        sc.model[[paste0("sample ", sub.sample)]] <-
          list(
            stomatal.closure = list(
              time.since.start = time.sc,
              RWD = as.numeric(RWD.sc),
              conductance = gmin.sc
            ),
            formula = list(linear = "conductance ~ (c * time.since.start + d)",
                           exponential = "conductance ~ (a * exp(b * time.since.start) + d)"),
            coef = coef,
            conf.int = list("2.5 %" = conf_int[, 1], "97.5 %" = conf_int[, 2])
          )

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


  return(sc.model)
}
