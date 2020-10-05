#' Saturated fresh weight estimation
#'
#' Calculates saturated fresh weight by fitting fresh weight values above the turgor loss point linearly to water potential values.
#'
#' @param data data frame, at least with a column containing numeric water potential (bar), fresh.weight (g) and dry.weight (g) values,
#' ordered by sample by descending water potential.
#' A column containing the sample IDs is optionally required if several samples were measured.
#' @param sample optional name of the column in data containing the sample IDs, default: "sample"
#' @param water.potential optional name of the column in data containing the water potential values (bar), default = "water.potential"
#' @param fresh.weight optional name of the column in data containing the fresh weight values (g), default: "fresh weight"
#' @param dry.weight
#' optional name of the column containing the dry weight values (g), default: "dry.weight"
#' @details Above the turgor loss point, a linear relationship between water content and water potential exists. Based on this premise,
#' saturated water content is found where water potential is zero. First, turgor loss point is calculated based on the relative leaf water
#' loss (fresh weight minus dry weight relativized by the maximum
#' leaf water content value). Then, data above the turgor loss point is extracted and fresh weight is fitted linearly to water potential.
#' The point where water potential of the linear regression line is zero is the saturated water content.
#' @return the original data frame (data) extended by a numeric column containing the saturated fresh weight values ("fresh.weight.saturated")
#' @examples # get example data
#' df <- pressure_volume_data
#' # extend the data frame by saturated fresh weight
#' df <- FWSaturated(df)
#'
#' @import ggplot2
#' @importFrom graphics legend
#' @importFrom stats approx coef confint lm na.omit nls
#'
#' @export


FWSaturated <- function(data,
                        sample = "sample",
                        water.potential = "water.potential",
                        fresh.weight = "fresh.weight",
                        dry.weight = "dry.weight") {
  # check validity of data
  data_in <-
    ValidityCheck(
      data,
      sample = sample,
      water.potential = water.potential,
      fresh.weight = fresh.weight,
      dry.weight = dry.weight
    )
  OrderCheck(
    data_in,
    sample = sample,
    water.potential = water.potential,
    fresh.weight = fresh.weight
  )


  # extract data (to ensure that, in case there are variabels in the data with the names as
  # used for the output variabels in the functions below, the correct variabels are used)
  data_in <-
    data.frame(data_in[[sample]], data_in[[fresh.weight]], data_in[[water.potential]],
               data_in[[dry.weight]])
  names(data_in) <-
    c(
      paste0(sample),
      paste0(fresh.weight),
      paste0(water.potential),
      paste0(dry.weight)
    )




  # calculate leaf water
  data_in$leaf.water <-
    data_in[[fresh.weight]] - data_in[[dry.weight]]



  sat.fw <- c()


  for (i in 1:length(unique(data_in$sample))) {
    # subset data
    sub.sample <- unique(data_in$sample)[i]
    data_in_subset <- data_in[data_in[[sample]] == sub.sample,]


    # calculate relative water loss as alternative to RWD for the use in TurgorLossPoint
    data_in_subset$water.loss1 <-
      100 - (data_in_subset$leaf.water * 100 / max(data_in_subset$leaf.water))
    data_in_subset$water.loss <-
      data_in_subset$water.loss1 - min(data_in_subset$water.loss1) # normalize data to start with 0 (neccessary if data was deleted with InitialFluct())


    # get turgor loss point
    tlp <-
      suppressWarnings(TurgorLossPoint(data_in_subset, RWD = "water.loss", graph = FALSE))



    # print warning, if tlp could not be determined
    if (length(tlp) == 0) {
      warning(
        paste0("sample ", sub.sample),
        ": the saturated water content could not be calculated due to unsuccessful determination of the tugor loss point"
      )
      sat.fw_i <- NA
    } else{
      # extract data above turgor loss point, because in this region a linear relationship between leaf water and water potential exists
      tlp_param <- ExtractParam(tlp)   # get turgor loss points
      data_in_subset_sub <-
        data_in_subset[data_in_subset$water.loss < (as.numeric(tlp_param[2]) + min(data_in_subset$water.loss1)), ]
      data_in_subset_sub <-
        data.frame(fresh.weight = data_in_subset_sub[[fresh.weight]],
                   water.potential = data_in_subset_sub[[water.potential]])



      # if there are not more than 2 data points before tlp, stop this iteration of the loop and give a warning
      if (length(data_in_subset_sub[, 1]) < 3) {
        warning(
          paste0("sample ", sub.sample),
          ": the saturated water content could not be calculated due to too few (< 3) data points before the tugor loss point"
        )
        sat.fw_i <- NA
      } else{
        # do linear regression
        m <-
          lm(fresh.weight ~ water.potential, data = data_in_subset_sub)



        # saturated water content is where water.potential of the linear regression line is 0, e.g. intercept of linear regression
        sat.fw_i <- as.numeric(coef(m)[1])
      }
    }

    sat.fw <-
      c(sat.fw, rep(sat.fw_i, times = length(data_in_subset[, 1])))   # repeat result  as often as necessary in order to add to data frame

  }


  return(data.frame(data, fresh.weight.saturated = sat.fw))
}
