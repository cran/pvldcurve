#' Vapor Pressure Deficit (VPD)
#'
#' Calculates mole fraction vapor pressure deficit (mol * mol^-1) of the air.
#'
#' @param data data frame at least with two numeric columns of equal length containg humidity (\%)
#' and temperature (degree Celsius)
#' @param humidity optional name of the column in data containing the humidity values;
#' default: "humidity"
#' @param temperature optional name of the column in data containing the temperature values;
#' default: "temperature"
#' @param atmospheric.pressure optional; default = 101.325 (atmospheric pressure at sea level)
#' @details Mole fraction vapor pressure deficit is calculated as:
#' \deqn{(1 - RH / 100) * (VPsat / AP)}
#' whereas RH = relative humidity (\%), VPsat = saturation vapor pressure (kPA),
#' AP = atmospheric pressure (kPA), whereas:
#' \deqn{VPsat = 0.61121 exp ((18.678 - T 234.5^-1) (T 257.14 + T))}
#' where T = air temperature (degree Celsius)
#' @return The original data frame extended by a numeric column with the vapor pressure deficit
#' (mol * mol^-1).
#' @examples
#' # get example data
#' df <- weather_data
#'
#' # calculate vapor pressure deficit from weather data measured at sea level
#' df_with_VPD <- VaporPressureDeficit(df)
#'
#' # calculate vapor pressure deficit from weather data measured at 2000 m altitude
#' df_with_VPD <- VaporPressureDeficit(df, atmospheric.pressure = 79.495)
#'
#' @import ggplot2
#' @importFrom graphics legend
#' @importFrom stats approx coef confint lm na.omit nls
#'
#' @export



VaporPressureDeficit <- function(data,
                                 humidity = "humidity",
                                 temperature = "temperature",
                                 atmospheric.pressure = 101.325) {
  # Check Validity of data
  data_in <-
    ValidityCheck(data, humidity = humidity, temperature = temperature)


  # calculate saturation vapor pressure from function
  data_in <-
    SaturationVaporPressure(data_in, temperature = temperature)


  # calculate vapor pressure deficit
  vapor.pressure.deficit <- ((1 - (data_in[[humidity]] / 100)) *
                               (data_in$saturation.vapor.pressure / atmospheric.pressure))



  return(data.frame(data, vapor.pressure.deficit))
}
