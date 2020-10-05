#' Extracts the fitting parameters from results list
#'
#' Extracts the coefficients and confidence intervals from the fitting results of the functions analysing the pressure volume curve
#' (TurgorLossPoint, OsmoticPot and ModElasticity) or the functions analysing the leaf drying curve
#' (StomatalClosure and Gmin)
#'
#' @param result_list output list from the functions TurgorLossPoint, OsmoticPot, ModElasticity,
#' StomatalClosure or Gmin
#' @return data frame containing the coefficients and the 0.95 confidence interval of the coefficients from the fit
#'
#' @import ggplot2
#' @importFrom graphics legend
#' @importFrom stats approx coef confint lm na.omit nls



ExtractFitParam <- function(result_list) {
  # initialize data.frame with column containing the sample IDs
  results <- data.frame(sample = names(result_list))



  # extract coefficients from results list if existent
  if (!(is.null(result_list[[1]]$coef))) {
    coef.a <- c()
    coef.b <- c()

    for (i in 1:length(result_list)) {
      coef.a <- c(coef.a, as.numeric(result_list[[i]]$coef[1]))
      coef.b <- c(coef.b, as.numeric(result_list[[i]]$coef[2]))
    }
    results <- cbind(results, coef.a, coef.b)
  }




  # extract confidence intervals from results list if existent
  if (!(is.null(result_list[[1]]$conf.int))) {
    conf.int.2.5.a <- c()
    conf.int.2.5.b <- c()
    conf.int.97.5.a <- c()
    conf.int.97.5.b <- c()

    for (i in 1:length(result_list)) {
      conf.int.2.5.a <-
        c(conf.int.2.5.a,
          as.numeric(result_list[[i]]$conf.int$"2.5 %"[1]))
      conf.int.2.5.b <-
        c(conf.int.2.5.b,
          as.numeric(result_list[[i]]$conf.int$"2.5 %"[2]))
      conf.int.97.5.a <-
        c(conf.int.97.5.a,
          as.numeric(result_list[[i]]$conf.int$"97.5 %"[1]))
      conf.int.97.5.b <-
        c(conf.int.97.5.b,
          as.numeric(result_list[[i]]$conf.int$"97.5 %"[2]))
    }
    results <-
      cbind(results,
            conf.int.2.5.a,
            conf.int.2.5.b,
            conf.int.97.5.a,
            conf.int.97.5.b)
  }




  return(data.frame(results))
}
