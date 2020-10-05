#' Modulus of elasticity
#'
#' Determines pressure potential and the modulus of elasticity
#'
#' @param data data frame containing columns of equal lengths giving the numerical
#' coordinates of the curve: water potential (bar) and RWD (\%), ordered by sample by descending water potential. A
#' column containing the sample IDs is optionally required if several samples were measured
#' @param sample optional column name in data containing the sample ID, default: "sample"
#' @param water.potential optional column name in data containing the water potential values of the leaf
#' (bar), default: "water.potential"
#' @param RWD optional column name in data containing the relative water deficit values (\%), default: "RWD"
#' @param graph set FALSE if no plots are to be returned
#' @param show.legend set FALSE if no legend is to be shown in the plots
#' @details Relative water deficit at turgor loss point is determined via the function TurgorLossPoint() and
#' osmotic potential is calculated via the function OsmoticPot(). \cr \cr Pressure potential is derived by
#' subtracting osmotic potential from water potential. The part of the pressure potential prior the turgor loss
#' point is then fitted linearly and the modulus of elasticity (M.Elasticity) equals the slope of the fitted line. \cr \cr
#' Before using this function, check the raw data for an initial plateau. If the exponential decline does not onset directly,
#' fitting might not succeed.
#' @return List splitted by sample consisting of
#' \item{modulus.elasticity}{modulus of elasticity (bar)}
#' \item{formula}{formula of the transformed linear osmotic potential fit (1/-bar) and the pressure potential (bar) fit}
#' \item{coef}{coefficients of the osmotic (1/-bar) and pressure potential (bar) fit}
#' \item{conf_int}{upper (97.5 \%) and lower (2.5 \%) border of 95 \% confidence interval of model parameters}
#' If graph = TRUE, the original data is displayed with the x- and y-axis intercepts of the turgor loss
#' point, the osmotic potential fit and
#' the linear regression line of the pressure potential.
#' @examples
#' #get example data, calculate Relative Water Deficit
#' data <- RelativeWaterDeficit(pressure_volume_data)[pressure_volume_data$sample == 1, ]
#'
#' # determine modulus of elasticity and the fitting parameters. Do not plot results.
#' m_elasticity <- ModElasticity(data, graph = FALSE)
#'
#' @import ggplot2
#' @importFrom graphics legend
#' @importFrom stats approx coef confint lm na.omit nls
#'
#' @export


ModElasticity <- function(data,
                          sample = "sample",
                          water.potential = "water.potential",
                          RWD = "RWD",
                          graph = TRUE,
                          show.legend = TRUE) {
  data_in  <- data


  # get osmotic potential fit and turgor loss point
  pv_data <-
    OsmoticPot(
      data_in,
      sample = sample,
      water.potential = water.potential,
      RWD = RWD,
      graph = FALSE
    )
  param <-
    ExtractParam(pv_data)   # extract parameters from result list
  fit_param <-
    ExtractFitParam(pv_data)   # extract fit parameters from result list
  sample_id <-
    as.numeric(gsub("sample ", "", param$sample))    # extract sample IDs where stomatal closure could be determined



  # initialize list for loop
  me_model <- list()


  for (i in 1:length(sample_id)) {
    # subset data
    data_in_subset <- data_in[data_in[[sample]] == sample_id[i],]
    osm_a_i <- fit_param$coef.a[i]
    osm_b_i <- fit_param$coef.b[i]
    tlp_i <- param$rwd.tlp[i]
    wp.tlp_i <- param$water.pot.tlp[i]



    # Pressure Potential and bulk modulus of elasticity

    # determine osmotic potential and pressure potential and combine in data frame for plot
    osmotic.pot.y <-
      -1 / (osm_a_i * data_in_subset[[RWD]] + osm_b_i)   # receive y values for the fit and transform back to untransformed data
    pressure.pot.y <-
      data_in_subset[[water.potential]] - osmotic.pot.y    # pressure potential is calculated by subtracting osmotic from bulk potential
    pot_data <-
      data.frame(x = data_in_subset[[RWD]],
                 osmotic.pot.y,
                 water.potential = data_in_subset[[water.potential]],
                 pressure.pot.y)




    # do linear fit of pressure potential data, which is water potential data before an including turgor loss point
    press.pot.before.tlp <- subset(pot_data, pot_data$x < tlp_i)
    press.pot.before.tlp$osmotic.pot.y <- NULL
    press.pot.before.tlp$water.potential <- NULL
    press.pot.before.tlp <- rbind(press.pot.before.tlp, c(tlp_i, 0))


    # if there are not more than 2 data points before tlp, stop this iteration of the loop and give a warning
    if (length(press.pot.before.tlp[, 1]) < 3) {
      warning(
        paste0("sample ", sample_id[i]),
        ": modulus of elasticity could not be calculated due to too few (< 3) data points before the tugor loss point"
      )
    } else{
      try({
        # try makes sure the execution of the code proceeds if an error occurs while fitting
        all.fine <-
          FALSE      # helping variable which is set TRUE if everything worked



        press.lin <-
          nls(pressure.pot.y ~ (a * x + b),
              data = press.pot.before.tlp,
              start = c(a = 1, b = 1))

        a <- coef(press.lin)[1] # extract coefficient a
        b <- coef(press.lin)[2] # extract coefficient b

        conf_int <-
          suppressMessages(confint(press.lin)) # extract confidence intervals for coefficients



        # calculate pressure potential data and add to data for plot
        press.lin.data <-
          a * press.pot.before.tlp$x + b    # calculate data points of fitted line for plot
        press.lin.data <- press.lin.data[-length(press.lin.data)]
        press.lin.data <-
          append(press.lin.data, rep(NA, (
            length(pot_data$x) - length(press.lin.data)
          )))   # add NAs to fit to achieve equal vector length as pot_data



        # calculate slope of fit, which is bulk modulus of elasticity
        elasticity <-
          (press.lin.data[1] - press.lin.data[2]) / (pot_data$x[1] - pot_data$x[2])



        # Plot
        if (graph == TRUE) {
          suppressWarnings(
            PlotOutput(
              sub.sample = sample_id[i],
              x = pot_data$x,
              y = pot_data$water.potential,
              y2 = pot_data$pressure.pot.y,
              legend.y = "leaf water potential",
              legend.y2 = "pressure potential",
              x.axis = "Relative Water Deficit (%)",
              y.axis = expression(paste(Psi, ' ' , (bar))),
              x.intercept = tlp_i,
              y.intercept = wp.tlp_i,
              legend.x.intercept = "turgor loss point",
              line.y = pot_data$osmotic.pot.y,
              line.y2 = press.lin.data,
              line.x = pot_data$x,
              legend.line.y = "fitted osmotic potential",
              legend.line.y2 = "fitted press. pot < tlp",
              show.legend = show.legend
            )
          )

        }


        me_model[[paste0("sample ", sample_id[i])]] <-
          list(
            modulus.elasticity = elasticity,
            formula = list(
              pressure.pot.linear = "pressure.potential ~ (a * RWD + b))",
              osmotic.pot.linear = "1/-(osmotic.potential ~ (a * RWD + b))"
            ),
            coef = list(
              pressure.pot.linear = list(a = as.numeric(a), b = as.numeric(b)),
              osmotic.pot.linear = list(a = osm_a_i, b = osm_b_i)
            ),
            conf.int = list(
              pressure.pot.linear = list("2.5 %" = conf_int[, 1], "97.5 %" = conf_int[, 2]),
              osmotic.pot.linear =
                list(
                  "2.5 %" = list(
                    a = fit_param$conf.int.2.5.a[i],
                    b = fit_param$conf.int.2.5.b[i]
                  ),
                  "97.5 %" = list(
                    a = fit_param$conf.int.97.5.a[i],
                    b = fit_param$conf.int.97.5.b[i]
                  )
                )
            )
          )


        all.fine <- TRUE


      }, silent = TRUE)
      # belongs to the try() function. Makes sure that no error is printed if something with the fit didn't work.
      # Instead, print warning and skip to the next item of the loop

      if (all.fine == FALSE) {
        warning(
          paste0("sample ", sample_id[i]),
          ": calculation of modulus of elasticity didn't work"
        )

      }
    }
  }


  return(me_model)
}
