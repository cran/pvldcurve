#' Weather Allocation
#'
#' Calculates average weather (humidity, temperature) values within a measurement period.
#'
#' @param weight_loss_data data frame containing at least a column named "date.and.time" of the
#' class POSIXct with the time (and date) of the measuring events. A column named "sample"
#' containing the sample IDs is optionally required if several samples were measured.
#' @param weather_data data frame containing at least a column named "date.and.time" of the
#' class POSIXct with the time (and date) of the weather measurements and two columns
#' named "humidity" and "temperature" containing the numerical weather data
#' @details Averages within a measurement period are determined by
#' approximate integration and normalization of the weather as a function of time.
#' @return The original weight loss data frame extended by the approximatively integrated and
#' normalized weather data for each period between two weight measurements. The first value of
#' each sample is NA since weather values are averaged from row i to i-1.
#' @examples
#' # get example data
#' weight_loss_data <- leaf_drying_data
#' weather_data <- weather_data
#'
#' # allocate averaged weather data to weight loss data
#' weight_loss_data_with_weather <- WeatherAllocation(weight_loss_data, weather_data)
#'
#' @import ggplot2
#' @importFrom graphics legend
#' @importFrom stats approx coef confint lm na.omit nls
#'
#' @export


WeatherAllocation <- function(weight_loss_data, weather_data) {
  # check validity of data
  weights <-
    ValidityCheck(weight_loss_data,
                  sample = "sample",
                  date.and.time = "date.and.time")
  OrderCheck(weights, sample = "sample", date.and.time = "date.and.time")
  weather_data <-
    ValidityCheck(
      weather_data,
      date.and.time = "date.and.time",
      humidity = "humidity",
      temperature = "temperature"
    )


  # check if date and time of weather is ordered chronologically
  if (any(difftime(weather_data$date.and.time [1:length(weather_data$date.and.time) -
                                               1], weather_data$date.and.time[-1])
          >= 0)) {
    warning("Date and time values in weather data are not ordered chronologically")
  }


  if (!(max(na.omit(weather_data$date.and.time)) > max(na.omit(weights$date.and.time))) |
      !(min(na.omit(weather_data$date.and.time)) < min(na.omit(weights$date.and.time)))) {
    stop(
      "Date and times of weight data measurements are not within the date and time range of weather data measurements"
    )
  }



  # interpolate weather data to date and times of weight measurements
  weight.times <-
    unique(sort(weights$date.and.time)) # sort times of weight measurements chronologically and remove duplicates

  temperature <-
    approx(
      weather_data$date.and.time,
      weather_data$temperature,
      xout = weight.times,
      method = "linear",
      rule = 2
    )$y
  humidity <-
    approx(
      weather_data$date.and.time,
      weather_data$humidity,
      xout = weight.times,
      method = "linear",
      rule = 2
    )$y




  # merge original and interpolated weather data to new data frame and sort chronologically
  all_weather <-
    data.frame(
      "date.and.time" = c(weight.times, weather_data$date.and.time),
      "temperature" = c(temperature, weather_data$temperature),
      "humidity" = c(humidity, weather_data$humidity)
    )

  all_weather <-
    all_weather[with(all_weather, order(date.and.time)), ] # sort




  # add time intervals between weather data
  all_weather$time.interval <-
    NULL  # if a column named time interval is already present, delete it so no mis-reference is done
  all_weather <- suppressWarnings(TimeInterval(all_weather))



  # initialize vector for loop
  temperature <- c()
  humidity <- c()


  for (i in unique(weights$sample)) {
    weights_subset <-
      subset(weights, sample == i)   # extract data where sample = i



    # initialize vector
    temperature.subset <- c()
    humidity.subset <- c()


    # for loop which returns weather values for the measurement periods
    for (e in 1:nrow(weights_subset[-1,])) {
      # don't do the last measurement


      # select the weather data within the weight measurement period
      weather <-
        all_weather[all_weather$date.and.time >= weights_subset$date.and.time[e] &
                      all_weather$date.and.time <=  weights_subset$date.and.time[e +
                                                                                   1],]


      # do normalized approximate integration for weather
      temp <-
        sum(weather$temperature * weather$time.interval) / sum(weather$time.interval)
      temperature.subset <-
        append(temperature.subset, temp) # put integrated value into initialized vector

      hum <-
        sum(weather$humidity * weather$time.interval) / sum(weather$time.interval)
      humidity.subset <-
        append(humidity.subset, hum) # put integrated value into initialized vector
    }



    temperature <-
      append(temperature, c(NA, temperature.subset))   # put integrated values into initialized vector
    humidity <-
      append(humidity, c(NA, humidity.subset))   # put integrated values into initialized vector

  }


  return(data.frame(weights, humidity, temperature))

}
