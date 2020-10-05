#' Leaf Conductance
#'
#' Calculates mole-based or concentraction-based conductance (stomatal or minimal
#' conductance) (mmol s^-1 m^-2 or mm s^-1) of the double-sided leaf area by experimental weight loss
#' data and weather data
#'
#' @param data data frame, with columns of equal length containg at least columns with
#' time (and date) of the fresh weight measurements,the measured fresh
#' weights (g) and the single-sided leaf area (cm^2) of the sample as well as the average relative
#' humidity (\%) and temperature (degree Celsius) during the measurement intervals. The data is to be ordered chronologically by sample.
#' A column containing the sample IDs is optionally required if several samples were measured.
#' @param sample optional name of the column in data containing the sample ID; default: "sample"
#' @param date.and.time optional name of the column in data containing the time of the fresh weight measurements
#' as class POSIXct; default: "date.and.time"
#' @param fresh.weight optional name of the column in data containing the numeric fresh weight values (g);
#' default: "fitted.fw" (fresh weight corrected by noises as outputted for leaf drying curves by the function FittedFW)
#' @param leaf.area optional name of the column in data containing the numeric single-sided leaf area values (cm^2);
#' default: "leaf.area"
#' @param humidity optional name of the column in data containing the numeric humidity values (\%);
#' default: "humidity"
#' @param temperature optional name of the column in data containing the numeric temperature values (degree Celsius);
#' default: "temperature"
#' @param atmospheric.pressure optional, giving the numeric atmospheric pressure in kPA, default = 101.325 (atmospheric pressure at sea level)
#' @param driving.force optional; possible values: mole or conc; defines whether conductance is expressed on the basis
#' of a mole fraction-based (default) or a concentration-based driving force
#' @details Calculates mole-based conductance (mmol s^-1 m^-2) as:
#' \deqn{g = T / VPD}
#' whereas T = transpiration (mmol s^-1 m^-2) is calculated as:
#' \deqn{T = \Delta FM * 1000 * (\Delta t * 60 * LA * 2 /10000 * 18.01528)^-1}
#' whereas \eqn{\Delta}FM = fresh matter reduction (g), \eqn{\Delta}t = time interval (min),
#' LA = single-sided leaf area (cm^2)
#' and VPD = vapor pressure deficit (mol * mol^-1) is calculated as:
#' \deqn{ VPD = (1 - RH / 100) * (VPsat / AP)}
#' whereas RH = relative humidity (\%), VPsat = saturation vapor pressure (kPA),
#' AP = atmospheric pressure (kPA), whereas:
#' \deqn{VPsat = 0.61121 * exp ((18.678 - T / 234.5) (T * 257.14 + T))}
#' where T = air temperature (degree Celsius) \cr \cr
#' Concentration based conductance (mm s^-1) is derived from mole-based conductance g(mol) as:
#' \deqn{g(conc) = g(mol) * R * (T + 273.15) / AP / 1000}
#' whereas: R = gas constant (8.3144598 J (mol * K)^-1) and T = absolute temperature (degree Celsius)
#' @return The original data frame extended by a numeric column with the mole-based or the concentration-
#' based conductance (mmol s^-1 m^-2, mm s^-1) of
#' the double-sided leaf area (conductance). The first value of each sample is NA since conductance values are computed
#' from row i and i-1
#' @examples
#' # get example data
#' weight_loss_data <- leaf_drying_data
#' weather_data <- weather_data
#' df <- WeatherAllocation(weight_loss_data, weather_data)   # allocate weather to weight loss data
#'
#' # extend the data frame by mole-based conductance values
#' df_with_conductance <- Conductance(df, fresh.weight = "fresh.weight")
#'
#' # extend the data frame by concentration-based conductance values
#' df_with_conductance <- Conductance(df, fresh.weight = "fresh.weight", driving.force = "conc")
#'
#' # calculate with atmospheric pressure of 99.8 kPA
#' df_with_conductance <- Conductance(df, fresh.weight = "fresh.weight", atmospheric.pressure = 99.8)
#'
#' @import ggplot2
#' @importFrom graphics legend
#' @importFrom stats approx coef confint lm na.omit nls
#'
#' @export


Conductance <- function(data,
                        sample = "sample",
                        fresh.weight = "fitted.fw",
                        date.and.time = "date.and.time",
                        leaf.area = "leaf.area",
                        humidity = "humidity",
                        temperature = "temperature",
                        atmospheric.pressure = 101.35,
                        driving.force = "mole") {
  # check validity of data
  data_in <-
    ValidityCheck(
      data,
      sample = sample,
      fresh.weight = fresh.weight,
      leaf.area = leaf.area,
      date.and.time = date.and.time,
      humidity = humidity,
      temperature = temperature
    )


  # extract data (to ensure that, in case there are variabels in the data with the names as
  # used for the output variabels in the functions below, the correct variabels are used)
  data_in <-
    data.frame(data_in[[sample]], data_in[[fresh.weight]], data_in[[date.and.time]],
               data_in[[leaf.area]], data_in[[humidity]], data_in[[temperature]])
  names(data_in) <-
    c(
      paste0(sample),
      paste0(fresh.weight),
      paste0(date.and.time),
      paste0(leaf.area),
      paste0(humidity),
      paste0(temperature)
    )




  # use functions to calculate weight difference, time interval and vapor pressure deficit
  data_in <-
    WeightDifference(data_in, sample = sample, fresh.weight = fresh.weight)
  data_in <-
    TimeInterval(data_in, sample = sample, date.and.time = date.and.time)
  data_in <-
    VaporPressureDeficit(
      data_in,
      humidity = humidity,
      temperature = temperature,
      atmospheric.pressure =  atmospheric.pressure
    )



  # calculate mole fraction-based conductance
  conductance <-
    ((data_in$weight.diff / 18.01528 * 1000) / (data_in$time.interval * 60) /
       data_in$vapor.pressure.deficit / (data_in[[leaf.area]] * 2 / 10000)
    )



  # convert to concentration-based conductance
  if (driving.force == "conc") {
    conductance / 1000 * 8.3144598 * (data_in[[temperature]] + 273.15) / atmospheric.pressure / 1000
  }

  return(data.frame(data, conductance))
}
