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

esc_localidad = read_csv('../synthetic_populations/data/processed_data/geodata/Localidad_Unidad_Catastral.csv')
age_pop_data = read_csv('../synthetic_populations/data/processed_data/popdata/bogota_population_data_sec.csv')%>%
    left_join(esc_localidad %>% dplyr::select(SCACODIGO, Localidad), by = c("Zone" = "SCACODIGO")) %>%
    group_by(AgeGroup, Localidad) %>%
    summarize(Pop = sum(Pop)) %>%
    ungroup()%>%
    separate(AgeGroup, into = c('MinAge','MaxAge'), remove = F) %>%
    mutate(MinAge = as.numeric(MinAge)) %>%
    arrange(MinAge)

age_loc_data = read_csv('../output/Age_Localidad_BOG_covid_cases_data.csv') %>%
    filter(Date <= fit_date) %>%
    group_by(MunCode, AgeGroup, Localidad) %>%
    summarize(Cs = sum(Cs, na.rm =T), Deaths = sum(Deaths, na.rm = T)) %>%
    ungroup()

##===============================#
## PLOT Localidad fit --------
##===============================#
## First, summarize all data
col_main = "gray"

jpeg('../figures/manuscript_figure_localidad_fit_age.jpeg', width=6.5,height=5, units="in", res = 300)
layout(matrix(1:20,nrow = 4, byrow = T))
par(mar = c(2,1,1,2.5), oma = c(2,2.5,1,0))
pplt = par("plt")
adjx = (0 - pplt[1]) / (pplt[2] - pplt[1])

for(ll in 1:length(localidad_list$Localidad)){ 
    print(localidad_list$Localidad[ll])
    tmp_fred_CF = fred_sweep_df %>%
        group_by(state_code, job_id) %>%
        summarize_at(vars(starts_with('CFLoc')), 'sum', rm.na = T) %>%
        ungroup() %>% dplyr::select(-state_code) %>%
        gather(key = AgeGroup, value = Deaths, -job_id) %>%
        mutate(Localidad = as.numeric(str_replace_all(AgeGroup, "CFLoc_([0-9]+?)_A[0-9]+_[0-9]+","\\1"))) %>%
        mutate(AgeGroup = str_replace_all(AgeGroup, "CFLoc_[0-9]+_(A[0-9]+_[0-9]+?)","\\1")) %>%
        filter(Localidad == localidad_list$Localidad_ID[ll])

    tmp_fred_Cs = fred_sweep_df %>%
        group_by(state_code, job_id) %>%
        summarize_at(vars(starts_with('CasesLoc')), 'sum', rm.na = T) %>%
        ungroup() %>% dplyr::select(-state_code) %>%
        gather(key = AgeGroup, value = Cases, -job_id) %>%
        mutate(Localidad = as.numeric(str_replace_all(AgeGroup, "CasesLoc_([0-9]+?)_A[0-9]+_[0-9]+","\\1"))) %>%
        mutate(AgeGroup = str_replace_all(AgeGroup, "CasesLoc_[0-9]+_(A[0-9]+_[0-9]+?)","\\1")) %>%
        filter(Localidad == localidad_list$Localidad_ID[ll])
    
    
    total_deaths = tmp_fred_CF %>% group_by(job_id) %>% summarize(TotalDeaths = sum(Deaths, na.rm = T))
    
    tmp_fred_CF = left_join(tmp_fred_CF, total_deaths, by = "job_id") %>%
        mutate(NormalizedDeaths = Deaths / TotalDeaths)

    age_cf_columns = unique(tmp_fred_CF$AgeGroup)
    inds_to_plot = c(1:length(age_cf_columns))

    plot(-100, -100, xlim = c(0.5, length(age_cf_columns)+0.5), 
         ylim = c(0,0.6), main = "",
         las = 2, xaxt = 'n', xlab = '', ylab = '',cex.axis=0.8)
    mtext(localidad_list$Localidad[ll], side = 3, cex = 0.6)
    axis(side = 1, at = inds_to_plot, labels = str_replace(age_cf_columns, 'A', ''),cex.axis=0.4, las = 2)
    for(nn in 1:length(age_cf_columns)){
        x_to_plot = inds_to_plot[nn]
        boxplot(x = tmp_fred_CF$NormalizedDeaths[tmp_fred_CF$AgeGroup == age_cf_columns[nn]],
                at = x_to_plot, add = T, lwd =1.5,
                col = adjustcolor("gray", alpha.f = 0.5),
                outcol = "gray",
                outpch = 16,
                border = "gray",
                medcol = "gray", 
                pars = list(xaxt = 'n', yaxt = 'n', axes = F))
    }

    tmp_age_data = age_loc_data %>%
        filter(MunCode == ss, Localidad == localidad_list$Localidad_ID[ll]) %>%
        group_by(AgeGroup) %>%
        summarize(Deaths = sum(Deaths, na.rm = T)) %>%
        ungroup() %>%
        right_join(base_age_data, by = "AgeGroup")
    tmp_age_data$TotalDeaths = sum(tmp_age_data$Deaths)
    tmp_age_data = tmp_age_data %>%
        mutate(PDeaths = Deaths / TotalDeaths)
    
    points(x = inds_to_plot,
           y = tmp_age_data$PDeaths ,
           pch = 19, cex = 0.5, col = 'black', bg= 'black')
}
mtext("Proportion of deaths by age", side = 2, outer = T, line = 1.3, cex = 0.8)
mtext("Age group", side = 1, outer = T, line = 1.0, cex = 0.8)
dev.off()

