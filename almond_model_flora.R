#' simple model of almond yield anomaly response to climate
#'
#' @param climate_data 
#' @param min_T_feb_coef  coefficient of daily times series of min T in Feb between years 1988-2010, default=-0.015
#' @param min_T_feb_coefsq default=-0.0046
#' @param precip_jan_coef coefficient of daily times series of sum precip in Jan between years 1988-2010, default = -0.07 
#' @param precip_jan_coefsq default = -0.07
#' @param intercept default = 0.28 
#'
#' @return almond yield anomoly; ton/acre
#' @export
#'
#' @examples


almond_yield <- function(climate_data) {
  min_temp <- climate_data %>% group_by(month, year) %>% summarize(min_T = min(tmin_c), .groups = "drop")
  feb_min_T <- ((min_temp %>% filter(month == 2) %>% select(min_T)))$min_T
  years <- unique(min_temp$year)
  
  #extract_precip_jan from climate data df....
  precip_sum <- climate_data %>% group_by(month, year) %>% summarize(sum_precip = sum(precip), .groups = "drop")
  jan_precip_sum <- (precip_sum %>% filter(month == 1))$sum_precip 

  
  #calculation equation
  al_yield = 
    -0.015 * (feb_min_T) +
    -0.0046 * ((feb_min_T) ^ 2) +
    -0.07 * (jan_precip_sum) +
    0.0043 * ((jan_precip_sum) ^ 2) + 
    0.28

  return(yield = al_yield)
}



