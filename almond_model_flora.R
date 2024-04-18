#' simple model of almond yield anomaly response to climate
#'
#' @param clim climate data csv
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

#Inputs: daily times series of minimum, maximum daily Temperatures and precipitation
#Outputs: maximum, minimum and mean yield anomoly for a input time series

##attach libraries 
library(here)
library(tidyverse)

#load climate data
climate_data <- read.csv("~/Documents/2_Areas/Areas_MEDS/Spring_Quarter/EDS230_Env_mod/homework/assignment2/almond_yield/clim.csv") %>% data.frame()

almond_yield <- function(clim) {
  
  #extract_min_T_feb from climate data df
  min_temp <- climate_data %>% group_by(month, year) %>% summarize(min_T = min(tmin_c), .groups = "drop")
  feb_min_T <- (min_temp %>% filter(month == 2) %>% select(min_T))$min_T
  
  #extract_precip_jan from climate data df.... QUESTION: IS PRECIPITATION A MEAN OR A SUM?? 
  
  precip_sum <- climate_data %>% group_by(month, year) %>% summarize(sum_precip = sum(precip), .groups = "drop")
  jan_precip_sum <- (precip_sum %>% filter(month == 1))$sum_precip 
  
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
  
  #return this list so that we can check answers w results 
  return(list(al_yield, maxy, miny, meany))
}

almond_yield(clim) 


