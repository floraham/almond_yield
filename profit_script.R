##Profit calculation 
#' Calculating almond profit
#'
#' @param climate_data a dataframe containing daily timeseries data from 1988 (water year 1989), max in min temperatures (Celsius) and precipitation
#' @param baseline_profit average rate of profit from almond production in USD, default=20000 
#' @param acres average acreage of almond farm, default=300
#' @param unit_price average unit price of almonds in USD, default=4000
#' @param unit_cost average unit cost of almond production in USD, default = 1000
#'
#' @return expected and mean profits from almond yield
#' @export
#'
#' @examples

#Inputs: timeseries climate dataframe, baseline profits, acres of crops, unit price of crops, unit cost of crops
#Outputs: gross profit and mean gross profit


calculate_profit <- function(climate_data, 
                              baseline_profit = 20000, 
                              acres = 300, 
                              unit_price = 4000, 
                              unit_cost = 1000) {
  require(dplyr)
  require(here)
  require(tidyverse)
  
  #calculate yield_anomaly using the almond_yield script 
  per_acre_yield <- almond_yield(climate_data)
  
  #calculate the expected profit; per_acre_yield* acres_harvested * unit_price 
  gross_profit = 
    baseline_profit + 
    (per_acre_yield * acres * unit_price) - 
    (per_acre_yield * acres* unit_cost)
  
  # baseline_profit + (per_acre_yield*acres)(unit_price - unit_cost)
  
  
  years <- (1989:2010)
  return(data.frame(
    year = years, gross_profit = gross_profit,  yield = per_acre_yield))
  
}





