#' Extracts parameters from result list
#'
#' Extracts the curve parameters from the result lists of the functions analysing the pressure volume curve
#' (TurgorLossPoint, OsmoticPot and ModElasticity) or the functions analysing the leaf drying curve
#' (StomatalClosure and Gmin)
#'
#' @param result_list output list from the functions TurgorLossPoint, OsmoticPot, ModElasticity,
#' StomatalClosure or Gmin
#' @return data frame containing the results from the curve analysis only, depending on the function used, relative water deficit at
#' turgor loss point (rwd.tlp), water potential at turgor loss point (water.pot.tlp), apoplastic fraction (apo.fract),
#' osmotic potential at full saturation (osmotic.pot.full.sat), modulus of elasticity (modulus.elasticity),
#' time since start of desiccation of stomatal closure (stom.clos.time), relative water deficit at stomatal closure
#' (stom.clos.rwd), minimum conductance determined by the intercept of the leaf drying curve fit with the point of stomatal closure
#' (stom.clos.gmin), by taking the mean from all minimum conductance values
#' (mean.gmin) or minimum conductance determined by extrapolation of minimum conductance curve linearly to full
#' saturation (gmin.full.sat)
#' @examples
#' # use pressure volume data provided by package
#' pv_data <- pressure_volume_data
#'
#' # do pressure volume curve analysis
#' pv_data <- RelativeWaterDeficit(pv_data)
#' results <- OsmoticPot(pv_data, graph = FALSE)
#'
#' # extract curve values
#' ExtractParam(results)
#'
#'
#' @import ggplot2
#' @importFrom graphics legend
#' @importFrom stats approx coef confint lm na.omit nls
#'
#' @export

ExtractParam <- function(result_list) {
  # initialize data.frame with column containing the sample IDs
  results <- data.frame(sample = names(result_list))



  # extract relative water deficit at turgor loss point from results list if existent
  if (!(is.null(result_list[[1]]$turgor.loss.point$RWD))) {
    rwd.tlp <- c()

    for (i in 1:length(result_list)) {
      rwd.tlp <- c(rwd.tlp, result_list[[i]]$turgor.loss.point$RWD)
    }
    results <- cbind(results, rwd.tlp)  # add to results list
  }



  # extract water potential at turgor loss point from results list if existent
  if (!(is.null(result_list[[1]]$turgor.loss.point$water.potential))) {
    water.pot.tlp <- c()

    for (i in 1:length(result_list)) {
      water.pot.tlp <-
        c(water.pot.tlp,
          result_list[[i]]$turgor.loss.point$water.potential)
    }
    results <- cbind(results, water.pot.tlp)
  }



  # extract apoplastic fraction from results list if existent
  if (!(is.null(result_list[[1]]$osmotic.potential$apo.fract))) {
    apo.fract <- c()

    for (i in 1:length(result_list)) {
      apo.fract <-
        c(apo.fract, result_list[[i]]$osmotic.potential$apo.fract)
    }
    results <- cbind(results, apo.fract)
  }



  # extract osmotic potential at full saturation from results list if existent
  if (!(is.null(result_list[[1]]$osmotic.potential$op.full.sat))) {
    op.full.sat <- c()

    for (i in 1:length(result_list)) {
      op.full.sat <-
        c(op.full.sat,
          result_list[[i]]$osmotic.potential$op.full.sat)
    }
    results <- cbind(results, osmotic.pot.full.sat = op.full.sat)
  }


  # extract mean gmin from results list if existent
  if (!(is.null(result_list[[1]]$gmin$mean.gmin))) {
    mean.gmin <- c()

    for (i in 1:length(result_list)) {
      mean.gmin <- c(mean.gmin, result_list[[i]]$gmin$mean.gmin)
    }
    results <- cbind(results, mean.gmin)
  }


  # extract mean gmin from results list if existent
  if (!(is.null(result_list[[1]]$gmin$lin.gmin))) {
    lin.gmin <- c()

    for (i in 1:length(result_list)) {
      lin.gmin <- c(lin.gmin, result_list[[i]]$gmin$lin.gmin)
    }
    results <- cbind(results, lin.gmin)
  }

  # extract gmin at full saturation from results list if existent
  if (!(is.null(result_list[[1]]$gmin$gmin.full.sat))) {
    gmin.full.sat <- c()

    for (i in 1:length(result_list)) {
      gmin.full.sat <-
        c(gmin.full.sat, result_list[[i]]$gmin$gmin.full.sat)
    }
    results <- cbind(results, gmin.full.sat)
  }



  # extract modulus of elasticity from results list if existent
  if (!(is.null(result_list[[1]]$modulus.elasticity))) {
    modulus.elasticity <- c()

    for (i in 1:length(result_list)) {
      modulus.elasticity <-
        c(modulus.elasticity, result_list[[i]]$modulus.elasticity)
    }
    results <- cbind(results, modulus.elasticity)
  }



  # extract time since start at stomatal closure from results list if existent
  if (!(is.null(result_list[[1]]$stomatal.closure$time.since.start))) {
    time.since.start <- c()

    for (i in 1:length(result_list)) {
      time.since.start <-
        c(time.since.start,
          result_list[[i]]$stomatal.closure$time.since.start)
    }
    results <- cbind(results, stom.clos.time = time.since.start)
  }


  # extract RWD at stomatal closure from results list if existent
  if (!(is.null(result_list[[1]]$stomatal.closure$RWD))) {
    RWD <- c()

    for (i in 1:length(result_list)) {
      RWD <- c(RWD, result_list[[i]]$stomatal.closure$RWD)
    }
    results <- cbind(results, stom.clos.rwd = RWD)
  }

  # extract conductance at stomatal closure from results list if existent
  if (!(is.null(result_list[[1]]$stomatal.closure$conductance))) {
    gmin <- c()

    for (i in 1:length(result_list)) {
      gmin <- c(gmin, result_list[[i]]$stomatal.closure$conductance)
    }
    results <- cbind(results, stom.clos.conductance = gmin)
  }


  return(data.frame(results))
}
