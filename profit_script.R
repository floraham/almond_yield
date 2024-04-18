##Profit calculation 
#' Calculating almond profit
#'
#' @param climate_data 
#' @param baseline_profit 
#' @param acres 
#' @param unit_price 
#' @param unit_cost 
#'
#' @return
#' @export
#'
#' @examples


calculate_profit <- function(climate_data, 
                              baseline_profit = 20000, 
                              acres = 300, 
                              unit_price = 4000, 
                              unit_cost = 3950) {
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




