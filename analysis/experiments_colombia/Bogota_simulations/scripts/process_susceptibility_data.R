##===============================#
## Plot deaths for forecast
## FRED-COVID19
## Author: Guido España & Sean Cavany
## 2020
##===============================#
## Setup-------------
##===============================#
library(dplyr)
library(tidyverse)
library(RColorBrewer)
library(lubridate)


##===============================#
## Functions-------------
##===============================#
get_logistic_curve <- function(ages_in, offset_low_in, offset_high_in, rate_in, cutoff_in){
    offset_low_in + (offset_high_in - offset_low_in) / (1 + exp(-rate_in * (ages_in - cutoff_in)))
}

get_logistic_curve_normalized <- function(ages_in, offset_low_in, rate_in, cutoff_in){
    offset_low_in + (1 - offset_low_in) / (1 + exp(-rate_in * (ages_in - cutoff_in)))
}

get_SSE_logistic <- function(data, ages, params){
    y = get_logistic_curve(ages, params[1], params[2], params[3],params[4])
    sse = sum((y - data)^2)
    return(sse)
}

get_SSE_logistic_normalized <- function(data, ages, params){
    y = get_logistic_curve_normalized(ages, params[1], params[2], params[3])
    sse = sum((y - data)^2)
    return(sse)
}


##===============================#
## Setup-------------
##===============================#
age_groups_min = c(0, 10, 20, 30, 40, 50, 60, 70)
age_groups_max = c(9, 19, 29, 39, 49, 59, 69, 120)
age_midpts = (age_groups_max  + age_groups_min)/2
age_widths = age_groups_max - age_groups_min + 1
age_groups = c(age_groups_min, 120)
ages = 0:120
susc_mean = c(0.4,0.38,0.79,0.86,0.8,0.82,0.88,0.74)
susc_mean = c(0.4,0.38,0.79,0.86,0.8,0.82,0.88,0.74)
susc_025 = c(0.25,0.27,0.59,0.69,0.61,0.63,0.70,0.56)
susc_975 = c(0.57,0.53,0.96,0.98,0.96,0.97,0.99,0.90)

init_params = c(offset_low = 0.39, high = 1.0, rate_in = 0.2, cutoff = 20)
fit_mean = optim(init_params, get_SSE_logistic, data=susc_mean, ages=age_midpts)
fit_low = optim(init_params, get_SSE_logistic, data=susc_025, ages=age_midpts)
fit_high = optim(init_params, get_SSE_logistic, data=susc_975, ages=age_midpts)

mean_logistic_midpts = get_logistic_curve(age_midpts, fit_mean$par[1], fit_mean$par[2], fit_mean$par[3], fit_mean$par[4])
mean_logistic = get_logistic_curve(ages, fit_mean$par[1], fit_mean$par[2], fit_mean$par[3], fit_mean$par[4])

fit_sus_df = as.data.frame(rbind(fit_mean$par, fit_low$par, fit_high$par), stringsAsFactors = F)
fit_sus_df$estimate = c('mean','low','high')

write_csv(fit_sus_df, path = "./input_files/age_susceptibility_fit.csv")

