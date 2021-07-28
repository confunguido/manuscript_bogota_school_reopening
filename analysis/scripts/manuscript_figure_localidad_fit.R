##===============================#
## Plot model fit for Bogotá
## Author: Guido España
## 2020
##===============================#
## Setup-------------
##===============================#
set.seed(123456)
library(dplyr)
library(tidyverse)
library(RColorBrewer)
library(lubridate)

library(sf)
library(raster)
library(rgdal)

##===============================#
## Functions------------
##===============================#
get_SSE <- function(data_in, model_in){
    return(sum((data_in - model_in)^2))
}
lowCI <- function(x){
    return(quantile(x, probs = 0.025, na.rm = T))
}
highCI <- function(x){
    return(quantile(x, probs = 0.975, na.rm = T))
}
medCI  <- function(x){
    return(quantile(x, probs = 0.5, na.rm = T))
}

##===============================#
## Data ------------
##===============================#
ss = 11001
outdir = '../experiments_colombia/Bogota_simulations/output/SHORT_FORECAST'
outdir_fit = '../experiments_colombia/Bogota_simulations/output/CALIBRATION'
asymp_inf = 1.0
fm_ef = 0.73
ksus = 10.0
outdir_st = file.path(outdir, sprintf('FRED_%d_postcalibration_asymp_%.2f_fm_%.2f_ksus_%.2f_mov_out', ss, asymp_inf,fm_ef, ksus))
outdir_fit_st = file.path(outdir_fit, sprintf('FRED_%d_calibration_asymp_%.2f_fm_%.2f_ksus_%.2f_mov_out', ss, asymp_inf,fm_ef, ksus))
interventions_df = read_csv('../experiments_colombia/Bogota_simulations/input_files/interventions_Colombia.csv') 


brk_ages = c(0, seq(from=10,by = 10, to = 80), 120)
brk_lbls = sprintf("ACF%d_%d", brk_ages[-length(brk_ages)], brk_ages[-1])
brk_data_lbls = sprintf("A%d_%d", brk_ages[-length(brk_ages)], brk_ages[-1])
base_age_df = data.frame(AgeGroup = brk_lbls, Deaths = 0, stringsAsFactors = F)
base_age_data = data.frame(AgeGroup = brk_data_lbls, stringsAsFactors = F)

localidad_list = read_csv('../data/Bogota_localidades_ID.csv') %>%
    filter(Localidad_ID != 20)


BOG_data = read_csv('../output/BOG_covid_death_data.csv') %>%
    mutate(MunCode = as.numeric(MunCode)) %>%
    filter(MunCode %in% interventions_df$State) %>%
    left_join(interventions_df, by = c("MunCode" = "State")) 

validation_data = read_csv(file.path('../experiments_colombia/input_files', 'COL_covid_death_data.csv')) %>%
    mutate(MunCode = as.numeric(MunCode)) %>%
    filter(MunCode %in% interventions_df$State) %>%
    left_join(interventions_df, by = c("MunCode" = "State")) 

fit_data = read_csv(file.path(outdir_fit_st, 'COL_covid_death_data.csv')) %>%
    mutate(MunCode = as.numeric(MunCode)) %>%
    filter(MunCode %in% interventions_df$State) %>%
    left_join(interventions_df, by = c("MunCode" = "State")) 

fit_date = as.Date("2020-10-31")
brk_ages = c(0, seq(from=10,by = 10, to = 80), 120)
brk_lbls = sprintf("ACF%d_%d", brk_ages[-length(brk_ages)], brk_ages[-1])
brk_data_lbls = sprintf("A%d_%d", brk_ages[-length(brk_ages)], brk_ages[-1])
base_age_df = data.frame(AgeGroup = brk_lbls, Deaths = 0, stringsAsFactors = F)
base_age_data = data.frame(AgeGroup = brk_data_lbls, stringsAsFactors = F)

age_data = read_csv(file.path('../output','Age_BOG_covid_data.csv')) %>%
    mutate(MunCode = 11001) %>%
    filter(Date <= fit_date, MunCode %in% interventions_df$State) 

##===============================#
## Process output-------------
##===============================#
data_out = file.path(outdir_st,'fred_output.csv')

params_out = file.path(outdir_st, 'FRED_parameters_out.csv')
params_sweep_df = read_csv(params_out)
fred_sweep_df = read_csv(data_out) %>% 
    right_join(params_sweep_df, by = c("job_id" = "job_id")) %>%
    mutate(Date = as.Date("2020-01-01") + Day)    


fred_sweep_df = fred_sweep_df %>% group_by(seed,state_code) %>% mutate(CumCF = cumsum(CF_mean), RR_max = max(RR_mean, na.rm = T)) %>% ungroup()

param_names_fit = c('workplace_contact_factor', 'neighborhood_contact_factor', 'holiday_contact_rate', 'shelter_in_place_compliance', 'influenza_transmissibility', 'facemask_compliance')

(summary(params_sweep_df[,param_names_fit]))

##===============================#
## PLOT Localidad fit --------
##===============================#
## First, summarize all data
col_main = "gray"

jpeg('../figures/manuscript_figure_localidad_fit.jpeg', width=6.5,height=5, units="in", res = 300)
layout(matrix(1:20,nrow = 4, byrow = T))
##par(mar = c(1,1,3,1), oma = c(1,4,1,1))
par(mar = c(2,1,1,2.5), oma = c(2,2.5,1,0))
pplt = par("plt")
adjx = (0 - pplt[1]) / (pplt[2] - pplt[1])

tmp_fred =  fred_sweep_df %>%
    group_by(state_code, Date, job_id) %>%
    ungroup()%>% dplyr::select(Date, job_id, starts_with('CFLoc')) %>%
    gather(key = AgeGroup, value = Deaths, -c('job_id','Date'))%>%
    mutate(Localidad = as.numeric(str_replace_all(AgeGroup, "CFLoc_([0-9]+?)_A[0-9]+_[0-9]+","\\1"))) %>%
    mutate(AgeGroup = str_replace_all(AgeGroup, "CFLoc_[0-9]+_(A[0-9]+_[0-9]+?)","\\1"))

for(ll in 1:length(localidad_list$Localidad)){ 
    print(localidad_list$Localidad_ID[ll])
    tmp_fred_lowCI = tmp_fred %>%
        filter(Localidad == localidad_list$Localidad_ID[ll]) %>%
        group_by(Date, job_id) %>%
        summarize(Deaths = sum(Deaths, na.rm = T)) %>%
        ungroup() %>% group_by(Date) %>%
        summarize(Deaths = lowCI(Deaths)) %>%
        ungroup() %>%
        mutate(CF = cumsum(Deaths))

    tmp_fred_highCI = tmp_fred %>%
        filter(Localidad == localidad_list$Localidad_ID[ll]) %>%
        group_by(Date, job_id) %>%
        summarize(Deaths = sum(Deaths, na.rm = T)) %>%
        ungroup() %>% group_by(Date) %>%
        summarize(Deaths = highCI(Deaths)) %>%
        ungroup() %>% mutate(CF = cumsum(Deaths))

    tmp_fred_median = tmp_fred %>%
        filter(Localidad == localidad_list$Localidad_ID[ll]) %>%
        group_by(Date, job_id) %>%
        summarize(Deaths = sum(Deaths, na.rm = T)) %>%
        ungroup() %>% group_by(Date) %>%
        summarize(Deaths = medCI(Deaths)) %>%
        ungroup() %>% mutate(CF = cumsum(Deaths))


    tmp_data = filter(BOG_data, MunCode == ss, Localidad == localidad_list$Localidad_ID[ll]) %>%
        group_by(Date) %>% summarize(Deaths = sum(Deaths), Cases = sum(Cases)) %>%
        ungroup()%>%
        mutate(CumDeaths = cumsum(Deaths))

    plot(tmp_fred_median$Date, (tmp_fred_median$CF), xaxs = "i", yaxs = "i", type = "l", lwd = 3,
         col = col_main,main = "",
         xlim = c(as.Date("2020-03-01"), fit_date), ylim = c(0,max(max(tmp_fred_highCI$CF), max(tmp_data$Deaths))*1.1), xaxt = 'n')
    axis.Date(1, tmp_fred_median$Date, cex.axis = 0.5)
    polygon(x = c(tmp_fred_highCI$Date, rev(tmp_fred_lowCI$Date)),
            y = c(tmp_fred_highCI$CF,
                  rev(tmp_fred_lowCI$CF)),
            border = adjustcolor(col_main, alpha.f = 0.7),
            col = adjustcolor(col_main, alpha.f = 0.2))    
    lines(tmp_fred_median$Date, tmp_fred_median$CF, lwd = 1.5, 
          col = adjustcolor(col_main, alpha.f = 0.2)) 
    mtext(localidad_list$Localidad[ll], side = 3, cex = 0.6)
    points(tmp_data$Date, tmp_data$CumDeaths, col = "black", lwd = 0.3, pch = 16, cex = 0.5)
}
mtext("Deaths", side = 2, outer = T, line = 1.3, cex = 0.8)
dev.off()
