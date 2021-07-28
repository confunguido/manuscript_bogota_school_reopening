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


##===============================#
## Inputs-------------
##===============================#
forecast_date = "2020-12-31"
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

if(!file.exists('./input_files/USA_google_movement_data.csv')){
    stop("Incidence data not found")
}

google_data = read_csv('./input_files/USA_google_movement_data.csv') %>%
    dplyr::filter(sub_region_1 %in% interventions_df$state_name) %>%
    mutate(day = as.numeric(date - min(date))) %>%
    dplyr::select(ends_with('baseline'), date, sub_region_1) %>%
    tidyr::gather(key = category, value = trend_mobility, -c('sub_region_1', 'date'))

## categories_google = unique(google_data$category)
## layout(matrix(1:length(categories_google), nrow = floor(length(categories_google)/2)))
## for(ct in 1:length(categories_google)){
##    tmp_data_base = dplyr::filter(google_data, sub_region_1 == 'Bogota', category == categories_google[ct])
##    plot(tmp_data_base$date, tmp_data_base$trend_mobility, type = "l", ylab = categories_google[ct])
##    abline(h = 1)
## }

##===============================#
## Fit to data-------------
##===============================#
ct = "retail_and_recreation_percent_change_from_baseline"

shelter_df = tibble()
for(ss in 1:nrow(interventions_df)){
    tmp_data_base = dplyr::filter(google_data, category == ct, sub_region_1 == interventions_df$state_name[ss]) %>%
        group_by(date) %>% summarize(trend_mobility = -mean(trend_mobility, na.rm = T)) %>%
        ungroup() %>%
        mutate(trend_mobility = trend_mobility / max(trend_mobility))
    
    tmp_data_base$day = 1:nrow(tmp_data_base)

    mod_gam1 = gam(trend_mobility ~ s(day), data=tmp_data_base, family=gaussian(link='identity'))

    ## Predict only based on last months trends
    current_trends = dplyr::filter(tmp_data_base, date >= max(tmp_data_base$date) - 30)
    current_trends$day = 1:nrow(current_trends)
    mod_lm = lm(trend_mobility ~ day, data = current_trends)

    pred_data = tibble(date = seq(from=min(current_trends$date), to = forecast_date, by = 1)) 
    pred_data$day = 1:nrow(pred_data)    
    pred_data$shelter_trend = predict(mod_lm, newdata = pred_data)

            
    tmp_shelter = tibble(date = tmp_data_base$date)    
    tmp_shelter$day = 1:nrow(tmp_shelter)    
        
    tmp_shelter$shelter_trend = predict(mod_gam1, newdata = tmp_shelter)

    pred_data = pred_data %>% dplyr::filter(date > max(tmp_shelter$date))
    pred_data$shelter_trend = tmp_shelter$shelter_trend[nrow(tmp_shelter)]
    
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
}
shelter_df = shelter_df %>%
    rename(FacemaskTrends = shelter_trend,
           Date = date) %>%
    mutate(FacemaskTrends = FacemaskTrends / max(FacemaskTrends)) %>%
    dplyr::select(Date, state_name, State, FacemaskTrends)

write_csv(shelter_df,'./input_files/facemask_timeseries_compliance_community.csv')
