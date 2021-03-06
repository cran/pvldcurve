% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RelativeWaterDeficit.R
\name{RelativeWaterDeficit}
\alias{RelativeWaterDeficit}
\title{Relative Water Deficit (RWD)}
\usage{
RelativeWaterDeficit(data, fresh.weight = "fresh.weight",
  dry.weight = "dry.weight",
  fresh.weight.saturated = "fresh.weight.saturated")
}
\arguments{
\item{data}{data frame with columns of equal length containing at least columns with the fresh weight (g),
the dry weight (g) and the saturated fresh weight (g)}

\item{fresh.weight}{optional name of the column in data containing
the numeric fresh weight values (g); default: fresh.weight}

\item{dry.weight}{optional name of the column in data containing
the numeric dry weight values (g); default: dry.weight}

\item{fresh.weight.saturated}{optional name of the column in data containing
the numeric saturated fresh weight values (g); default: fresh.weight.saturated}
}
\value{
the original data frame extended by a numeric column with the relative water deficit (RWD) (\%).
}
\description{
Calculates relative water deficit (\%)
}
\details{
Relative water deficit (\%) is calculated as:
\deqn{RWD = 100 - 100 * ((FW - DW) (FWs - DW)^-1)}
whereas FW = fresh weight, DW = dry weight and FWs = fresh weight at water saturation.
}
\examples{
# get example data
df <- leaf_drying_data

# extend df by RWD
df_with_RWD <- RelativeWaterDeficit(df)

}
