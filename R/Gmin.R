#' Minimum leaf conductance
#'
#' Determines mean minimum leaf conductance and minimum leaf conducance at full turgidity and stomatal closure in
#' experimentally obtained water loss curves.
#'
#' @param data data frame containing columns of equal lengths giving the
#' coordinates of the curve: time since start (minutes), conductance (mmol m^-2 s^-1) and RWD (\%), ordered by sample by ascending time
#' since start. A column containing the sample IDs is optionally required if several samples were measured.
#' @param sample optional column name in data containing the sample ID, default: sample
#' @param time.since.start optional column name in data containing the numerical values for time since start of
#' the experiment (min), default: time.since.start
#' @param conductance optional column name in data containing the numerical conductance values (mmol m^-2 s^-1), default:
#' "conductance"
#' @param RWD optional column name in data containing the numerical relative water deficit values (\%), default: "RWD.interval" (RWD
#' average of an interval, as outputted by RWDInterval)
#' @param stom.clos.threshold threshold value for stomatal closure. Automatic determination by default.
#' @param graph set FALSE if no plots are to be returned
#' @param show.legend set FALSE if no legend is to be shown in the plots
#' @details The coordinates of stomatal closure are determined via the function StomatalClosure(). \cr
#' Conductance data including and following stomatal closure are then extracted and the average is taken (mean.gmin). A linear regression is
#' applied to the data and the y axis intercept (gmin.full.sat) and the coordinate at the RWD point of stomatal
#' closure (lin.gmin) are calculated from the function. \cr \cr
#' Before using this function, check the raw data for an initial plateau. If the exponential decline does not onset directly,
#' fitting might not succeed.
#' @return List splitted by sample consisting of
#' \item{gmin}{mean minimum conductance (mean.gmin) (mmol m^-2 s^-1) after stomatal closure of the measurement interval,
#' minimum conductance at full saturation (gmin.full.sat) (mmol m^-2 s^-1) and minimum conductance at stomatal closure based on the linear
#' fit (lin.gmin) (mmol m^-2 s^-1)}
#' \item{formula}{formula of the linear regression of gmin vs. RWD}
#' \item{coef}{coefficients of linear fit}
#' \item{conf_int}{upper (97.5 \%) and lower (2.5 \%) border of 95 \% confidence interval of model parameters}
#' If graph = TRUE, the plotted original data is displayed with the x-axis intercept of the point of
#' stomatal closure and the linear regression line of gmin showing the point of y-intercept (gminfullsat).
#' @examples
#' # get example data
#' df <- WeatherAllocation(leaf_drying_data, weather_data)   # allocate weather to weight loss data
#' df <- TimeSinceStart(df) # calculate time since start
#' df <- df[df$fw.plateau != "yes",] # remove plateauing data
#' df <- FittedFW(df, graph = FALSE) # correct noises in fresh weight
#' df <- RWDInterval(df, fresh.weight = "fitted.fw") # calculate RWD based in the intervals
#' df <- Conductance(df, fresh.weight = "fitted.fw") # calculate conductance
#'
#' # calculate gmin and plot graphs
#' gmin <- Gmin(df)
#'
#' @import ggplot2
#' @importFrom graphics legend
#' @importFrom stats approx coef confint lm na.omit nls
#'
#' @export


Gmin <- function(data,
                 sample = "sample",
                 time.since.start = "time.since.start",
                 conductance = "conductance",
                 RWD = "RWD.interval",
                 stom.clos.threshold = FALSE,
                 graph = TRUE,
                 show.legend = TRUE) {
  data_in <- data

  # determine point of stomatal closure
  stomclos <-
    StomatalClosure(
      data_in,
      sample = sample,
      time.since.start = time.since.start,
      conductance = conductance,
      threshold = stom.clos.threshold,
      RWD = RWD,
      graph = FALSE
    )
  stomclos_param <- ExtractParam(stomclos)   # extract results
  sample_id <-
    gsub("sample ", "", stomclos_param$sample)    # extract sample IDs where stomatal closure could be determined



  # initialize list for loop
  gmin_list <- list()


  for (i in 1:length(sample_id)) {
    # subset data
    data_in_subset <- data_in[data_in[[sample]] == sample_id[i], ]
    stomclos_i <- as.numeric(stomclos_param$stom.clos.rwd[i])
    stomclosgmin_i <- as.numeric(stomclos_param$stom.clos.conductance[i])


    # delete all data beforehand the point of stomatal closure
    gmin_data <-
      data_in_subset[data_in_subset[[RWD]] > stomclos_i,][-c(1, 2)]
    gmin_data <- gmin_data[!is.na(gmin_data[[RWD]]), ]
    gmin_data <-
      data.frame(conductance = gmin_data[[conductance]], RWD = gmin_data[[RWD]])
    gmin_data <-
      rbind(gmin_data, c(stomclosgmin_i, stomclos_i)) # add stomatal closure coordinates of fitted conductances data


    # average of remaining data
    meangmin <- mean(gmin_data$conductance)



    try({
      # try makes sure the execution of the code proceeds if an error occurs while fitting
      all.fine <-
        FALSE      # helping variable which is set TRUE if everything worked



      # linear fitting of remaining data
      lin <-
        nls(
          conductance ~ (a * RWD + b),
          data = gmin_data,
          start = c(a = 1, b = 1),
          weights = c(RWD)
        )  # the latest data is the most reliable data, weighting the data ensures the fit is fitted best to later data

      a <- coef(lin)[1] # extract coefficient a
      b <- coef(lin)[2] # extract coefficient b

      conf_int <-
        suppressMessages(confint(lin, level = 0.95)) # extract 95 % confidence intervals of coefficients



      # extrapolate RWD to 0 and calculate fitted conductance values for plot
      linearfit.RWD <- append(0, data_in_subset[[RWD]])
      linearfit.gmin <- a * linearfit.RWD + b

      # calculate gmin at the point of stomatal closure based on the linear fit
      lin.gmin = as.numeric(a) * stomclos_i + as.numeric(b)



      # plot linear fit with original data
      if (graph == TRUE) {
        suppressWarnings(suppressMessages(
          PlotOutput(
            sub.sample = sample_id[i],
            x = data_in_subset[[RWD]],
            y = data_in_subset[[conductance]],
            legend.y = "leaf conductance",
            x.axis = "Relative Water Deficit (%)",
            y.axis = expression('g ' * ('mmol ' * m ^ -2 * s ^ -1)),
            x.intercept = stomclos_i,
            legend.x.intercept = "stomatal closure",
            line.y = linearfit.gmin,
            line.x = linearfit.RWD,
            legend.line.y = "fitted min. conductance",
            show.legend = show.legend
          )
        ))
      }


      # put data in list
      gmin_list[[paste0("sample ", sample_id[i])]] <-
        list(
          gmin = list(
            mean.gmin = meangmin,
            lin.gmin = lin.gmin ,
            gmin.full.sat = as.numeric(b)
          ),
          formula = list(gmin.linear = "gmin ~ (a * RWD + b)"),
          coef = list(a = as.numeric(a), b = as.numeric(b)),
          conf.int = list("2.5 %" = conf_int[, 1], "97.5 %" = conf_int[, 2])
        )


      all.fine <- TRUE


    }, silent = TRUE)
    # belongs to the try() function. Makes sure that no error is printed if something with the fit didn't work.
    # Instead, print warning and skip to the next item of the loop

    if (all.fine == FALSE) {
      warning(paste0("sample ", sample_id[i]),
              ": calculation of gmin didn't work")
    }


  }

  return(gmin_list)
}
