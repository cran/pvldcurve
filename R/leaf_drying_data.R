#' Experimentally determined leaf drying data
#'
#' A dataset containing repeatedly measured fresh weights of transpiring leaves subjected to
#' different soil moisture conditions during their growth (n = 6) and their saturated fresh weight and dry weight.
#' togeher with their saturated fresh weight, their fresh weight before saturation, their dry weight and leaf area.
#'
#' \itemize{
#'   \item treatment: Soil moisture conditions during the last 6 days of Kohlrabi growth (10-30 % (10 %) or 40-60 % (40 %))
#'   \item sample: Sample ID (1 - 12)
#'   \item dry.weight: Dry weight of the sample in gramm (0.3922 - 0.6692)
#'   \item fresh.weight.harvest: Fresh weight directly after cutting before over night saturation of the sample in gramm (2.8062 - 7.1009)
#'   \item fresh.weight.saturated: Saturated fresh weight of the sample in gramm (4.2186 - 7.3179)
#'   \item leaf.area: leaf area of the sample in cm^2 (91 - 148)
#'   \item date.and.time: Time (and date) of the fresh weight measurement of the transpiring sample (2019-03-26 09:48:00 - 2019-03-27 09:51:00)
#'   \item fresh.weight. Fresh weight of the transpiring sample in gramm (3.5112 - 7.3179)
#'   \item fw.plateau: Indicates plateauing fresh.weight values before the onset of the exponential decline. Remove values for analysis where fw.plateau = yes
#' }
#' @format A data frame with 172 rows and 9 variables:
"leaf_drying_data"
