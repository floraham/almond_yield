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
  per_acre_yield <- almond_yield(feb_min_T, jan_precip_sum)
  
  #calculate the total_yield_anomaly of all the acres (in tons)
  total_yield <- per_acre_yield * acres
  
  #calculate the expected profit
  gross_profit = baseline_profit + (total_yield * unit_price) - (total_yield * unit_cost)

  return(list(
    gross_profit = gross_profit,
    mean_profit = mean(gross_profit, na.rm = TRUE)
  ))
}
#calculate_profit(climate_data)




