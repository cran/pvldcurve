#' Pressure Volume Curve Analysis
#'
#' Determines the coordinates of the turgor loss point, osmotic potential at full hydration
#' and apoplastic fraction
#'
#' @param data data frame containing columns of equal lengths giving the numerical
#' coordinates of the curve: water potential (bar) and RWD (\%), ordered by sample by descending water potential. A
#' column containing the sample IDs is optionally required if several samples were measured.
#' @param sample optional column name in data containing the sample ID, default: "sample"
#' @param water.potential optional column name in data containing the numeric water potential values
#' (bar), default: "water.potential"
#' @param RWD optional column name in data containing the relative water deficit values (\%), default: "RWD"
#' @param graph set FALSE if no plots are to be returned
#' @param show.legend set FALSE if no legend is to be shown in the plots
#' @details RWD at turgor loss point is derived by the function TurgorLossPoint(). \cr \cr
#' The pressure-volume curve data is converted to -1/bar. The osmotic potential is then derived by fitting a linear
#' regression line with the Gauss-Newton algorithm of nls() to the water potential data following the turgor loss point. The y- and
#' x-axis intercept of the regression line gives the osmotic potential at full hydration (op.full.sat) and the apoplastic fraction
#' (apo.fract), respectively. Water potential at turgor loss point equals the value of the osmotic potential fit at the relative
#' water deficit at turgor loss point.
#' @return List splitted by sample consisting of
#' \item{turgor.loss.point}{x and y coordinates of the turgor loss point (RWD (\%) and water.potential (bar), respectively)}
#' \item{osmotic.potential}{x and y intercepts of the osmotic potential fit (apoplasic fraction (apo.fract) (\%) and op.full.sat (bar), respectively)}
#' \item{formula}{formula of the linear osmotic potential fit}
#' \item{coef}{coefficients of the linear model}
#' \item{conf_int}{upper (97.5 \%) and lower (2.5 \%) border of 95 \% confidence interval of model parameters}
#' If graph = TRUE, the plotted tranformed data is displayed with the x- and y-axis
#' intercepts of the turgor loss point and the
#' linear regression line of the osmotic potential showing the point of y-intercept (op.full.sat) and x-intercept (apo.fract). \cr
#' Before using this function, check the raw data for an initial plateau. If the exponential decline does not onset directly,
#' fitting might not succeed.
#' @examples
#' # get example data, calculate Relative Water Deficit
#' data <- RelativeWaterDeficit(pressure_volume_data)
#'
#' # calculate pressure volume curve characteristics and plot graphs
#' pv_analysis <- OsmoticPot(data)
#'
#' @import ggplot2
#' @importFrom graphics legend
#' @importFrom stats approx coef confint lm na.omit nls
#'
#' @export


OsmoticPot <- function(data,
                       sample = "sample",
                       water.potential = "water.potential",
                       RWD = "RWD",
                       graph = TRUE,
                       show.legend = TRUE) {
  # put data in new frame
  data_in <- data


  # determine RWD at turgor loss point
  tlp.model <-
    TurgorLossPoint(
      data_in,
      sample = sample,
      water.potential = water.potential,
      RWD = RWD,
      graph = FALSE
    )
  tlp.model_param <- ExtractParam(tlp.model)    # extract results
  sample_id <-
    gsub("sample ", "", tlp.model_param$sample)    # get sample IDs of those samples where tlp could be determined



  # initialize vectors for loop
  osm_mod <- list()


  for (i in 1:length(sample_id)) {
    # subset data
    data_in_subset <- data_in[data_in[[sample]] == sample_id[i],]
    data_in_subset[[water.potential]] <-
      -1 / data_in_subset[[water.potential]]   # transformation of water potential data
    tlp_i <- tlp.model_param$rwd.tlp[i]



    # delete all data beforehand the turgor loss point, so only the water potential data in the osmotic region is left
    osm_data <- data_in_subset[data_in_subset[[RWD]] > tlp_i, ]
    osm_data <- osm_data[!is.na(osm_data[[RWD]]),]
    osm_data <-
      data.frame(RWD = osm_data[[RWD]], water.potential = osm_data[[water.potential]])



    try({
      # makes sure the correct error is printed in case fitting didn't work
      all.fine <-
        FALSE   # helping variable. set to TRUE if everything in the try wrapper worked



      # linear fitting of osmotic potential data and extraction of fitting properties
      lin <-
        nls(water.potential ~ (a * RWD + b),
            data = osm_data,
            start = c(a = 1, b = 1)) # linear fit

      a <- coef(lin)[1] # extract coefficient a from model
      b <- coef(lin)[2]
      conf_int <-
        suppressMessages(confint(lin))   # extract all confidence intervals



      # calculate apoplastic fraction and water potential at turgor loss point
      apo.fract <-
        (0 - b) / a # RWD at apoplastic fraction is where osmotic potential = 0
      WP.TLP <-
        a * tlp_i + b # osmotic potential at turgor loss point, which is water potential at tlp



      # calculate osmotic potential
      x <-
        c(0, osm_data[[RWD]], apo.fract)   # create x data starting with 0 and reaching until apoplastic fraction
      osmotic.pot <- a * x + b    # linear model
      osm.pot.full.sat <-
        osmotic.pot[1] # osmotic potential at full saturation is where x = 0



      # Plot
      if (graph == TRUE) {
        suppressWarnings(
          PlotOutput(
            sub.sample = sample_id[i],
            x = data_in_subset[[RWD]],
            y = data_in_subset[[water.potential]],
            legend.y = "leaf water potential",
            x.axis = "Relative Water Deficit (%)",
            y.axis = expression(paste("-", Psi ^ -1 , (
              bar ^-1
            ))),
            x.intercept = tlp_i,
            legend.x.intercept = "turgor loss point",
            line.y = osmotic.pot,
            line.x = x,
            legend.line.y = "fitted osmotic potential",
            show.legend = show.legend
          )
        )
      }



      osm_mod[[paste0("sample ", sample_id[i])]] <-
        list(
          turgor.loss.point = list(RWD = tlp_i, water.potential = as.numeric(-1 /
                                                                               WP.TLP)),
          osmotic.potential = list(
            op.full.sat = as.numeric(-1 / osm.pot.full.sat),
            apo.fract = 100 - as.numeric(apo.fract)
          ),
          formula = list(osmotic.pot.linear = "osmotic.potential ~ (a * RWD + b)"),
          coef = list(a = as.numeric(a), b = as.numeric(b)),
          conf.int = list("2.5 %" = conf_int[, 1], "97.5 %" = conf_int[, 2])
        )



      all.fine <- TRUE
    }, silent = TRUE)


    # give warning and add NAs for leaf.area if fitting didn't work
    if (all.fine == FALSE) {
      warning(paste0("sample ", sample_id[i]),
              " Fitting of osmotic potential was unsuccessful")
    }
  }


  return(osm_mod)
}
