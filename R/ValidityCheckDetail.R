#' Checks if column exists in data, is numeric and has the same lenghts as the others existence
#' @param data_in data frame to be checked
#' @param value column in data
#' @return no return value
#' @import ggplot2
#' @importFrom graphics legend
#' @importFrom stats approx coef confint lm na.omit nls



ValidityCheckDetail <- function(data_in,
                                value) {
  if (!(value %in% names(data_in))) {
    stop(paste0("Column ", value),
         " is missing in data or is not named as defined")
    }else{
      if (!(is.numeric(data_in[[value]]))) {
        stop(paste0("Non-numeric ", value), " values")
      } else{
        if (!(length(data_in[, 1]) == length(data_in[[value]]))) {
          stop("Columns in data are not of equal length")
        } else{
          if(sum(is.na(data_in[[value]])) > sum(!(is.na(data_in[[value]])))){
            warning(paste0("Column ", value),
                 " contains more NA values than non-NA values")
          }
      }
    }
  }
}
