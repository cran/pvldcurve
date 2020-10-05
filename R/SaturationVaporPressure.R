#' Saturation vapor pressure (VPsat)
#'
#' Calculates saturation vapor pressure (kPa) using the Arden Buck equation
#'
#' @param data data frame with at least a numeric column containing temperature (degree Celsius)
#' @param temperature optional name of the column in data containing the temperature values;
#' default: "temperature"
#' @return the original data frame extended by a numeric column with the saturation vapor pressure (kPa).
#' @details
#' Calculates saturation vapor pressure (kPa) over liquid by temperature using the Arden Buck equation:
#' \deqn{VPsat = 0.61121 exp ((18.678 - T 234.5^-1) (T 257.14 + T))}
#' where T = air temperature (degree Celsius)
#' @examples
#' # generate example data frame
#' df <- data.frame(temperature = c(23.1, 23.2))
#'
#' # extend df by saturation vapor pressure
#' df_with_VPsat <- SaturationVaporPressure(df)
#'
#' @import ggplot2
#' @importFrom graphics legend
#' @importFrom stats approx coef confint lm na.omit nls
#'
#' @export


SaturationVaporPressure <-
  function(data, temperature = "temperature") {
    # check validity of data
    data_in <- ValidityCheck(data, temperature = temperature)


    # calculate saturation vapor pressure
    saturation.vapor.pressure <-
      (0.61121 * exp((18.678 - (data_in[[temperature]] / 234.5)) *
                       (data_in[[temperature]] / (257.14 + data_in[[temperature]]))))


    return(data.frame(data, saturation.vapor.pressure))
  }
