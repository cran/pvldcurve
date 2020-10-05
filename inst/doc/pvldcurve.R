## ---- eval = FALSE-------------------------------------------------------
#  pressure_volume_data
#  leaf_drying_data
#  weather_data

## ---- eval = FALSE-------------------------------------------------------
#  pv <- RelativeWaterDeficit(pressure_volume_data)

## ---- eval = FALSE-------------------------------------------------------
#  pv <- RelativeWaterDeficit(pressure_volume_data)
#  TurgorLossPoint(pv)

## ---- echo = FALSE-------------------------------------------------------
suppressMessages(suppressWarnings(library(pvldcurve)))
pv <- RelativeWaterDeficit(pressure_volume_data)
pv_sub <- subset(pv, sample == 3) 
tlp <- TurgorLossPoint(pv_sub)

## ---- eval = FALSE-------------------------------------------------------
#  pv <- RelativeWaterDeficit(pressure_volume_data)
#  OsmoticPot(pv)

## ---- echo = FALSE-------------------------------------------------------
library(pvldcurve)
pv <- RelativeWaterDeficit(pressure_volume_data)
pv_sub <- subset(pv, sample == 3) 
osm <- OsmoticPot(pv_sub)

## ---- eval = FALSE-------------------------------------------------------
#  ModElasticity(pv)

## ---- echo = FALSE-------------------------------------------------------
pv <- RelativeWaterDeficit(pressure_volume_data)
pv_sub <- subset(pv, sample == 3) 
mod <- ModElasticity(pv_sub)

## ---- eval = FALSE-------------------------------------------------------
#  library(pvldcurve)
#  data <- WeatherAllocation(leaf_drying_data, weather_data)

## ---- eval = FALSE-------------------------------------------------------
#  data <- VaporPressureDeficit(data)

## ---- eval = FALSE-------------------------------------------------------
#  data <- SaturationVaporPressure(data)

## ---- eval = FALSE-------------------------------------------------------
#  data <- Transpiration(data,
#                        fresh.weight = "fresh.weight")

## ---- eval = FALSE-------------------------------------------------------
#  data:wd <- WeightDifference(data)
#  data_ti <- TimeInterval(data)

## ---- eval = FALSE-------------------------------------------------------
#  data <- Conductance(data,
#                      fresh.weight = "fresh.weight")

## ---- eval = FALSE-------------------------------------------------------
#  data <- Conductance(data,
#                      fresh.weight = "fresh.weight",
#                      driving.force = "conc")

## ---- eval = FALSE-------------------------------------------------------
#  data <- RWDInterval(leaf_drying_data,
#                      fresh.weight = "fresh.weight")
#  data <- TimeSinceStart(data)

## ---- eval = FALSE-------------------------------------------------------
#  data <- WeatherAllocation(leaf_drying_data, weather_data)
#  data <- TimeSinceStart(data)
#  data <- RWDInterval(data,
#                      fresh.weight = "fresh.weight")
#  data <- data[data$fw.plateau != "yes",]
#  data <- Conductance(data,
#                      fresh.weight = "fresh.weight")
#  
#  StomatalClosure(data)

## ---- echo = FALSE-------------------------------------------------------
library(pvldcurve)
df <- WeatherAllocation(leaf_drying_data, weather_data) 
df <- TimeSinceStart(df)
df <- RWDInterval(df, 
                  fresh.weight = "fresh.weight")
df <- df[df$fw.plateau != "yes",] 
df <- Conductance(df, 
                  fresh.weight = "fresh.weight")
df_sub <- subset(df, sample == 8) 
sc <- StomatalClosure(df_sub)

## ---- eval = FALSE-------------------------------------------------------
#  data <- WeatherAllocation(leaf_drying_data, weather_data)
#  data <- TimeSinceStart(data)
#  data <- data[data$fw.plateau != "yes",]
#  data <- FittedFW(data)

## ---- echo = FALSE-------------------------------------------------------
library(pvldcurve)
df <- WeatherAllocation(leaf_drying_data, weather_data) 
df <- TimeSinceStart(df)
df <- df[df$fw.plateau != "yes",] 
df_sub <- subset(df, sample == 8) 
sc <- FittedFW(df_sub)

## ---- eval = FALSE-------------------------------------------------------
#  data <- WeatherAllocation(leaf_drying_data, weather_data)
#  data <- TimeSinceStart(data)
#  data <- data[data$fw.plateau != "yes",]
#  data <- FittedFW(data, graph = FALSE)
#  data <- RWDInterval(data,
#                      fresh.weight = "fitted.fw")
#  data <- Conductance(data,
#                      fresh.weight = "fitted.fw")
#  
#  StomatalClosure(data)

## ---- echo = FALSE-------------------------------------------------------
library(pvldcurve)
df <- WeatherAllocation(leaf_drying_data, weather_data) 
df <- TimeSinceStart(df)
df <- df[df$fw.plateau != "yes",] 
df <- subset(df, sample == 8) 
df <- FittedFW(df, graph = FALSE)
df <- RWDInterval(df, 
                  fresh.weight = "fitted.fw")
df <- Conductance(df, 
                  fresh.weight = "fitted.fw")
sc <- StomatalClosure(df)

## ---- eval = FALSE-------------------------------------------------------
#  data <- WeatherAllocation(leaf_drying_data, weather_data)
#  data <- TimeSinceStart(data)
#  data <- data[data$fw.plateau != "yes",]
#  data <- FittedFW(data, graph = FALSE)
#  data <- RWDInterval(data, fresh.weight = "fitted.fw")
#  data <- Conductance(data, fresh.weight = "fitted.fw")
#  
#  Gmin(data)

## ---- echo = FALSE-------------------------------------------------------
data <- WeatherAllocation(leaf_drying_data, weather_data) 
data <- TimeSinceStart(data)
data <- data[data$fw.plateau != "yes",] 
data <- FittedFW(data, graph = FALSE)
data <- RWDInterval(data, 
                    fresh.weight = "fitted.fw")
data <- Conductance(data, 
                    fresh.weight = "fitted.fw")
df_sub <- subset(data, sample == 8) 
gmin <- Gmin(df_sub)

