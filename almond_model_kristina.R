#' simple model of almond yield anomaly response to climate
#'
#' @param temp_feb coefficient of minimum temperatures for February
#' @param precip_jan coefficient of precipitation sum for January
#'
#' @return almond yield anomoly; ton/acre
#' @export
#'
#' @examples

#Inputs: daily times series of minimum, maximum daily Temperatures and precipitation
#Outputs: maximum, minimum and mean yield anomoly for a input time series

##attach libraries 
library(here)
library(tidyverse)

#load climate data
climate_data <- read_csv(here("clim.csv")) %>% data.frame()

#extracting precipitation and temperature data from relevant months 

temp_feb <- climate_data %>% group_by(month, year) %>% filter(month == 2) %>% summarise(tmin_c = min(tmin_c), .groups = "drop")

precip_jan <- climate_data %>% group_by(month, year) %>% filter(month == 1) %>% summarize(precip = sum(precip), .groups = "drop")

almond_yield <- function(temp_feb, precip_jan) {
  
  #calculation equation
  al_yield = 
    -0.015 * (feb_min_T) +
    -0.0046 * ((feb_min_T) ^ 2) +
    -0.07 * (jan_precip_sum) +
    0.0043 * ((jan_precip_sum) ^ 2) + 
    0.28
  
  #find max, min, mean of yields 
  maxy = max(al_yield)
  miny = min(al_yield)
  meany = mean(al_yield)
  
  #return almond yield results, maxy, miny, and meany
  return(paste0("al_yield:", list(al_yield), "maxy:", list(maxy), "miny:", list(miny), "meany:", list(meany)))
  
}

almond_yield(temp_feb, precip_jan)
