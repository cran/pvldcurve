#' Ensures the validity of the input data
#' @param data data frame containing the data to be checked
#' @param sample name of column containing the sample ID (default: sample)
#' @param leaf.area name of column containing the leaf area (cm^2) (default: leaf.area)
#' @param date.and.time name of column containing the date and time (default: date and time)
#' @param dry.weight name of column containing the dry weight (g) (default: dry weight)
#' @param fresh.weight.saturated name of column containing the saturated fresh weight (g) (default: fresh.weight.saturated)
#' @param fresh.weight name of column containing the fresh weight (g) (default: fresh.weight)
#' @param water.potential name of column containing the water potential (bar) (default: water.potential)
#' @param RWD name of column containing the relative water deficit (default: RWD)
#' @param conductance name of column containing the conductance values (default: conductance)
#' @param time.since.start name of column containing the time since start values (default: time.since.start)
#' @param temperature name of column containing the temperature values (default: temperature)
#' @param humidity name of column containing the humidity values (default: humidity)
#' @return no return value
#' @import ggplot2
#' @importFrom graphics legend
#' @importFrom stats approx coef confint lm na.omit nls



ValidityCheck <- function(data,
                          sample = FALSE,
                          leaf.area = FALSE,
                          date.and.time = FALSE,
                          dry.weight = FALSE,
                          fresh.weight.saturated = FALSE,
                          fresh.weight = FALSE,
                          water.potential = FALSE,
                          RWD = FALSE,
                          conductance = FALSE,
                          time.since.start = FALSE,
                          temperature = FALSE,
                          humidity = FALSE) {
  # Create dummy sample vector for calculations if not available. put data into new data frame.
  # Check if sample is integer and ordered increasingly
  if(!(sample %in% names(data))) {
    data_in <- cbind(data, sample = rep(1, times = length(data[, 1])))
    if(sample != FALSE){
      warning(paste0("Column ", sample, " is missing in data or is not named as defined. The complete data set is handled as one sample"))
    }
  }else{
    data_in <- data
    if(sample != FALSE){
      if(!(is.integer(data_in[[sample]]))){
        stop(paste0("Column ", sample, " must be of structure integer"))
        if(is.unsorted(data_in[[sample]])){
          stop(paste0("Column ", sample, " must be ordered increasingly"))
        }
      }
    }
  }




  if (leaf.area != FALSE) {
    # check validity of data only if requested with function call
    ValidityCheckDetail(data_in, leaf.area)   # here, it is checked whether variable exists in data frame with defined name, whether it is numeric and whether it has equal length to other columns
    if (any(!(round(data_in[[leaf.area]]) %in% 2:2000 |
              (is.na(data_in[[leaf.area]]))))) {
      # this part is specific for every variable
      warning(
        "Single-sided leaf area (cm^2) values exceed the expected range (2:2000)"
      )
    }
  }

  if (fresh.weight != FALSE) {
    ValidityCheckDetail(data_in, fresh.weight)
    if (any(!(round(data_in[[fresh.weight]]) %in% 0:100 |
              (is.na(data_in[[fresh.weight]]))))) {
      warning("Fresh weight (g) values exceed the expected range (0:100) or are missing")
    }
  }

  if (fresh.weight.saturated != FALSE) {
    ValidityCheckDetail(data_in, fresh.weight.saturated)
    if (any(!(round(data_in[[fresh.weight.saturated]]) %in% 0:150))) {
      warning("Saturated fresh weight (g) values exceed the expected range (0:150) or are missing")
    }
  }

  if (dry.weight != FALSE) {
    ValidityCheckDetail(data_in, dry.weight)
    if (any(!(round(data_in[[dry.weight]]) %in% 0:20))) {
      warning("Dry weight (g) values exceed the expected range (0:20) or are missing")
    }
  }

  if (RWD != FALSE) {
    ValidityCheckDetail(data_in, RWD)
    if (any(!(round(data_in[[RWD]]) %in% 0:100 |
              (is.na(data_in[[RWD]])))) |
        all(!(round(data_in[[RWD]]) > 2))) {
      warning("Unexpected values for RWD (%)")
    }
  }

  if (water.potential != FALSE) {
    ValidityCheckDetail(data_in, water.potential)
    if (any(!(round(data_in[[water.potential]]) %in% -50:0)) |
        all(round(data_in[[water.potential]]) > -2)) {
      warning("Unexpected or missing values for water potential (bar)")
    }
  }

  if (humidity != FALSE) {
    ValidityCheckDetail(data_in, humidity)
    if (any(!(round(data_in[[humidity]]) %in% 0:100 |
              (is.na(data_in[[humidity]]))))) {
      warning("Relative humidity (%) values exceed the expected range (0:100)")
    }
  }

  if (temperature != FALSE) {
    ValidityCheckDetail(data_in, temperature)
    if (any(!(round(data_in[[temperature]]) %in% -10:60 |
              (is.na(data_in[[temperature]]))))) {
      warning("Temperature (%) values exceed the expected range (-10:60)")
    }
  }

  if (time.since.start != FALSE) {
    ValidityCheckDetail(data_in, time.since.start)
    if (any(!(round(data_in[[time.since.start]]) %in% 0:3000)) |
        all(!(round(data_in[[time.since.start]]) > 60))) {
      warning("Unexpected or missing values for time (min) since start of the experiment")
    }
  }

  if (conductance != FALSE) {
    ValidityCheckDetail(data_in, conductance)
    if (any(!(round(data_in[[conductance]]) %in% 0:2000 |
              (is.na(data_in[[conductance]]))))) {
      warning("Conductance values (mmol * cm^-2 * s^-1) exceed the expected range (0:2000)")
    }
  }

  if (date.and.time != FALSE) {
    if (!(date.and.time %in% names(data_in))) {
      stop("Column date.and.time is missing in data or is not named as defined")
    } else{
      if(sum(is.na(data_in[[date.and.time]])) > sum(!(is.na(data_in[[date.and.time]])))){
        stop(paste0("Column date.and.time contains more NA values than non-NA values"))
      }else{
        if (any(!(class(data_in[[date.and.time]]) == "POSIXct" |
                  class(data_in[[date.and.time]]) == "POSIXt"))) {
          stop("Date/time is not defined as a basic R POSIXct class")
        } else{
          if (!(length(data_in[, 1]) == length(data_in[[date.and.time]]))) {
            stop("columns in data are not of equal length")
          }
        }
      }
    }
  }



  return(data.frame(data_in))
}
