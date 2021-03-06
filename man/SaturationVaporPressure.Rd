% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SaturationVaporPressure.R
\name{SaturationVaporPressure}
\alias{SaturationVaporPressure}
\title{Saturation vapor pressure (VPsat)}
\usage{
SaturationVaporPressure(data, temperature = "temperature")
}
\arguments{
\item{data}{data frame with at least a numeric column containing temperature (degree Celsius)}

\item{temperature}{optional name of the column in data containing the temperature values;
default: "temperature"}
}
\value{
the original data frame extended by a numeric column with the saturation vapor pressure (kPa).
}
\description{
Calculates saturation vapor pressure (kPa) using the Arden Buck equation
}
\details{
Calculates saturation vapor pressure (kPa) over liquid by temperature using the Arden Buck equation:
\deqn{VPsat = 0.61121 exp ((18.678 - T 234.5^-1) (T 257.14 + T))}
where T = air temperature (degree Celsius)
}
\examples{
# generate example data frame
df <- data.frame(temperature = c(23.1, 23.2))

# extend df by saturation vapor pressure
df_with_VPsat <- SaturationVaporPressure(df)

}
