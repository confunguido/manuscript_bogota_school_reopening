##===============================#
## Process google mobility data
## Author: Guido España
## 2020
##===============================#
## Setup-------------
##===============================#
library(dplyr)
library(tidyverse)
library(mgcv)
library(MASS)

forecast_date = "2021-08-31"
reps_in = 500
args = (commandArgs(TRUE))

if(length(args) >= 1){
    forecast_date = args[1]
    if(length(args) >= 2){
        reps_in = as.numeric(reps_in)
    }
}
forecast_date = as.Date(forecast_date)

##===============================#
## Read data-------------
##===============================#
interventions_df = read_csv('./input_files/interventions_Colombia.csv')

## Update file
download.file('https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv?cachebust=911a386b6c9c230f',
              './input_files/USA_google_movement_data.csv')

if(!file.exists('./input_files/USA_google_movement_data.csv')){
    stop("Incidence data not found")
}

google_data = read_csv('./input_files/USA_google_movement_data.csv') %>%
    dplyr::filter(sub_region_1 %in% interventions_df$state_name) %>%
    mutate(day = as.numeric(date - min(date))) %>%
    dplyr::select(ends_with('baseline'), date, sub_region_1) %>%
    tidyr::gather(key = category, value = trend_mobility, -c('sub_region_1', 'date'))

categories_google = unique(google_data$category)
layout(matrix(1:length(categories_google), nrow = floor(length(categories_google)/2)))
for(ct in 1:length(categories_google)){
   tmp_data_base = dplyr::filter(google_data, sub_region_1 == 'Bogota', category == categories_google[ct])
   plot(tmp_data_base$date, tmp_data_base$trend_mobility, type = "l", ylab = categories_google[ct])
   abline(h = 1)
}

##===============================#
## Fit to data-------------
##===============================#
##ct = "residential_percent_change_from_baseline"
ct = "transit_stations_percent_change_from_baseline"
##ct = "grocery_and_pharmacy_percent_change_from_baseline"
shelter_df = tibble()
for(ss in 1:nrow(interventions_df)){
    tmp_data_base = dplyr::filter(google_data, category == ct, sub_region_1 == interventions_df$state_name[ss]) %>%
        group_by(date) %>% summarize(trend_mobility = -mean(trend_mobility, na.rm = T)) %>%
        ungroup()
    
    min_mobility = min(tmp_data_base$trend_mobility[tmp_data_base$date < as.Date('2020-03-30')])

    ##    tmp_data_base$trend_mobility = (tmp_data_base$trend_mobility - min_mobility)/100
    
    tmp_data_base$trend_mobility = tmp_data_base$trend_mobility / 100    
    tmp_data_base$day = 1:nrow(tmp_data_base)

    tmp_data_base_xmas = tmp_data_base[tmp_data_base$date > as.Date('2020-12-24'),]
    tmp_data_base = tmp_data_base[tmp_data_base$date <= as.Date('2020-12-24'),]
    ## Fit two models, one before xmas, one after xmas
    mod_gam1 = gam(trend_mobility ~ s(day), data=tmp_data_base, family=gaussian(link='identity'))
    mod_gam2 = gam(trend_mobility ~ s(day), data=tmp_data_base_xmas, family=gaussian(link='identity'))


    ## Predict only based on last months trends            
    tmp_shelter = tibble(date = tmp_data_base$date)    
    tmp_shelter$day = 1:nrow(tmp_shelter)            
    tmp_shelter$shelter_trend = predict(mod_gam1, newdata = tmp_shelter)
    
    tmp_shelter_xmas = tibble(date = tmp_data_base_xmas$date)    
    tmp_shelter_xmas$day = (1:nrow(tmp_shelter_xmas)) + max(tmp_shelter$day)
    tmp_shelter_xmas$shelter_trend = predict(mod_gam2, newdata = tmp_shelter_xmas)

    ## Combine again
    tmp_shelter = bind_rows(tmp_shelter, tmp_shelter_xmas)
    tmp_data_base = bind_rows(tmp_data_base, tmp_data_base_xmas)
        
    ##tmp_shelter$shelter_trend = tmp_shelter$shelter_trend - min(tmp_shelter$shelter_trend)
    ##tmp_shelter$shelter_trend = tmp_shelter$shelter_trend / max(tmp_shelter$shelter_trend)


    current_trends = dplyr::filter(tmp_shelter, date >= max(tmp_data_base$date) - 30)
    current_trends$day = 1:nrow(current_trends)
    mod_lm = lm(shelter_trend ~ day, data = current_trends)

    pred_data = tibble(date = seq(from=min(current_trends$date), to = forecast_date, by = 1)) 
    pred_data$day = 1:nrow(pred_data)    
    pred_data$shelter_trend = predict(mod_lm, newdata = pred_data)

    pred_data = pred_data %>% dplyr::filter(date > max(tmp_shelter$date))

    ## Project after january to be mobility to be as in november
    nov_data = tmp_shelter[lubridate::month(tmp_shelter$date) == 11,]
    pred_data$shelter_trend = current_trends$shelter_trend[nrow(current_trends)]
    pred_data$shelter_trend = min(nov_data$shelter_trend)
    
    shelter_reps = bind_rows(tmp_shelter, pred_data) %>%
        mutate(replicate = 1,
               State = interventions_df$State[ss],
               state_name = interventions_df$state_name[ss]) %>%   
        dplyr::select(date, replicate, shelter_trend, State, state_name)
    
    shelter_reps$shelter_trend[shelter_reps$shelter_trend < 0] = 0
    
    ## plot(shelter_reps$date, shelter_reps$shelter_trend, type = "l", col = "navy")

    ## lines(tmp_data_base$date, tmp_data_base$trend_mobility, col = "black")    
    ##shelter_reps$shelter_trend[shelter_reps$shelter_trend > 1] = 1    
    shelter_df = bind_rows(shelter_df, shelter_reps)
    shelter_df$shelter_trend = shelter_df$shelter_trend - min(shelter_df$shelter_trend)
}

write_csv(shelter_df,'./input_files/interventions_covid_timevarying_shelter.csv')

##===============================#
## Process ESC weights -----------
##===============================#
## This script depends on the mobility trends file being created from grandata
## Not updated here because the grandata website sometimes freezes
grandata_mov_df = read_csv('./input_files/11001_grandata_mobility_trends.csv')

city_pop = grandata_mov_df %>%
    group_by(SCACODIGO) %>%
    summarize(pop = mean(pop)) %>%
    ungroup() %>%
    summarize(pop = sum(pop))

## Define each ESC contribution
esc_weight_df = grandata_mov_df %>%
    group_by(date) %>%
    mutate(shelter_weight = shelter_trend * pop / sum(shelter_trend * pop)) %>%
    ungroup() %>%
    mutate(shelter_weight = ifelse(is.na(shelter_weight), 0, shelter_weight))

last_weight = filter(esc_weight_df, date == max(esc_weight_df$date)) %>%
    dplyr::select(-date)

missing_dates = expand.grid(date = seq(from = max(esc_weight_df$date) + 1, to = max(shelter_df$date), by = 1), ind = 1:nrow(last_weight))

esc_weight_df = bind_rows(esc_weight_df, mutate(last_weight[missing_dates$ind,], date = missing_dates$date))

esc_shelter_df = esc_weight_df %>% dplyr::select(date, SCACODIGO, pop, shelter_weight) %>%
    left_join(shelter_df, by = 'date') %>%
    mutate(shelter_pop = shelter_trend * city_pop$pop) %>%
    mutate(shelter_esc_trend = shelter_weight * shelter_trend * city_pop$pop / pop) %>%
    mutate(shelter_esc_trend = ifelse(shelter_esc_trend > 1, 1, shelter_esc_trend)) %>%
    mutate(shelter_trend = shelter_esc_trend) %>%
    dplyr::select(-shelter_esc_trend)

write_csv(esc_shelter_df,'./input_files/11001_mobility_trends.csv')

##=================================================#
## make figures------
##=================================================#
jpeg('./shelter_google_grandata_comparison.jpeg')
tmp_grandata_city = grandata_mov_df %>%
    group_by(date) %>%
    summarize(city_shelter = sum(shelter_trend * pop / city_pop$pop, na.rm = T)) %>%
    ungroup()

tmp_google_city = esc_shelter_df %>%
    group_by(date) %>%
    summarize(city_shelter = sum(shelter_trend * pop / city_pop$pop, na.rm = T)) %>%
    ungroup()

plot(shelter_df$date, shelter_df$shelter_trend, type = "l")
lines(tmp_google_city$date, tmp_google_city$city_shelter, col = "red")
lines(tmp_grandata_city$date, tmp_grandata_city$city_shelter, col = "green")

dev.off()

