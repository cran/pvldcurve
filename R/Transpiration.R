#' Leaf transpiration
#'
#' Calculates transpiration of a plant part from experimentally determined weight loss per time unit
#' and double-sided leaf area.
#'
#' @param data data frame with columns of equal length containg at least columns with the time (and date) of the fresh weight
#' measurements as well as columns with the measured fresh weights (g) and the single-sided leaf area (cm^2) of the sample. The
#' data is to be ordered chronologically by sample. A column containing the sample IDs is optionally required if several samples were measured.
#' @param sample optional name of the column in data containing the sample ID; default: "sample"
#' @param date.and.time optional name of the column in data containing the time of the fresh weight measurements
#' as class POSIXct; default: "date.and.time"
#' @param fresh.weight optional name of the column in data containing the numeric fresh weight values (g);
#' default: "fitted.fw"
#' @param leaf.area optional name of the column in data containing the numeric single-sided leaf area values (cm^2);
#' default: "leaf.area"
#' @param output.unit optional; possible values: "mg" or "mmol"; defines whether transpiration is given in
#' mmol m^-2 s^-1 (Default) or in mg m^-2 s^-1
#' @details Transpiration (mmol s^-1 m^-2) is calculated as:
#' \deqn{T = \Delta FM * 1000 * (\Delta t * 60 * LA * 2 /10000 * 18.01528)^-1}
#' whereas T = transpiration, \eqn{\Delta}FW = reduction of fresh weight (g),
#' \eqn{\Delta}t = time unit (min), LA = single-sided leaf area (cm^2)
#' @return The original data frame extended by a numeric column with the transpiration
#' (mg s^-1 m^-2 or mmol s^-1 m^-2) of the double-sided leaf area. The first value of each sample is
#' NA, since transpiration values are computed from row i and i-1.
#' @examples
#' # get example data and allocate
#' df <- WeatherAllocation(leaf_drying_data, weather_data)
#'
#' # extend df by transpiration in mmol s^-1 m^-2
#' df_with_transpiration <- Transpiration(df, fresh.weight = "fresh.weight")
#'
#' # extend df by transpiration in mg s^-1 m^-2
#' df_with_transpiration <- Transpiration(df, fresh.weight = "fresh.weight", output.unit = "mg")
#'
#' @import ggplot2
#' @importFrom graphics legend
#' @importFrom stats approx coef confint lm na.omit nls
#'
#' @export


Transpiration <- function(data,
                          sample = "sample",
                          date.and.time = "date.and.time",
                          fresh.weight = "fitted.fw",
                          leaf.area = "leaf.area",
                          output.unit = "mmol") {
  # Check Validity of data
  data_in <-
    ValidityCheck(
      data,
      sample = sample,
      date.and.time = date.and.time,
      fresh.weight = fresh.weight,
      leaf.area = leaf.area
    )
  OrderCheck(
    data_in,
    sample = sample,
    date.and.time = date.and.time,
    fresh.weight = fresh.weight
  )


  # extract data (to ensure that, in case there are variabels in the data with the names as
  # used for the output variabels in the functions below, the correct variabels are used)
  data_in <-
    data.frame(data_in[[sample]], data_in[[fresh.weight]], data_in[[date.and.time]],
               data_in[[leaf.area]])
  names(data_in) <-
    c(paste0(sample),
      paste0(fresh.weight),
      paste0(date.and.time),
      paste0(leaf.area))




  # check if output unit is correctly defined
  if (output.unit != "mmol" & output.unit != "mg") {
    stop("output.unit must be either 'mmol' or 'mg'")
  } else{
    # use functions to calculate weight difference and time interval
    data_in <-
      WeightDifference(data_in, sample = sample, fresh.weight = fresh.weight)
    data_in <-
      TimeInterval(data_in, sample = sample, date.and.time = date.and.time)


    # calculate transpiration in mg
    transpiration <-
      ((data_in$weight.diff * 1000) / ((data_in$time.interval * 60) *
                                         ((data_in[[leaf.area]] * 2) / 10000)))

    # convert to mmol if output.unit = mmol
    if (output.unit == "mmol") {
      transpiration <- transpiration / 18.01528
    }


    return(data.frame(data, transpiration))
  }
}
