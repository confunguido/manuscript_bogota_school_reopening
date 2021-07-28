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


get_loglikelihood_tests <- function(data_pos_in, data_neg_in, model_symp_in, model_pop_in, model_prev_inf_in,
                                    ratio_in, test_sensitivity = 0.777, test_specificity = 1.0){
    ## For some reason some positive cases are negative:
    ## Exclude those data points
    data_pos_in[which(data_pos_in < 0)] = NA
    data_neg_in[which(data_neg_in < 0)] = NA
    
    prevalence = model_symp_in / model_pop_in
    infected_prev = (model_prev_inf_in) / model_pop_in
    
    probC = prevalence / (prevalence + ratio_in * (1 - prevalence))
    probI = infected_prev*ratio_in / (prevalence + ratio_in * (1 - prevalence))
    probU = (1 - infected_prev - prevalence)*ratio_in / (prevalence + ratio_in * (1 - prevalence))

    prob = (probC + probI)*test_sensitivity + probU*(1-test_specificity)
    ll_out = sum(dbinom(data_pos_in, data_pos_in + data_neg_in, prob, log=T), na.rm = T)
    return(-ll_out)
}


get_pos_test_model <- function(model_symp_in, model_pop_in, model_prev_inf_in,
                               ratio_in, test_sensitivity = 0.777,
                               test_specificity = 1.0){
    ratio_in = ratio_in[1]
    prevalence = model_symp_in / model_pop_in
    infected_prev = (model_prev_inf_in) / model_pop_in
    
    probC = prevalence / (prevalence + ratio_in * (1 - prevalence))
    probI = infected_prev*ratio_in / (prevalence + ratio_in * (1 - prevalence))
    probU = (1 - infected_prev - prevalence)*ratio_in / (prevalence + ratio_in * (1 - prevalence))
    
    prob = (probC + probI)*test_sensitivity + probU*(1-test_specificity)
    return(prob)
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

fit_date = as.Date("2021-01-18")
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

param_names_fit = c('workplace_contact_factor', 'neighborhood_contact_factor', 'holiday_contact_rate', 'shelter_in_place_compliance', 'influenza_transmissibility', 'facemask_compliance', 'holiday_contact_rate')

(summary(params_sweep_df[,param_names_fit]))



##===============================#
## Calculate %pos --------
##===============================#
test_fred = fred_sweep_df %>% drop_na() %>%
    filter(intervention_id == "default") %>%
    group_by(Day, Date, start_date, school_closure_day) %>%
    summarize(Cs_median = quantile(Cs_mean, probs = c(0.5), na.rm = T),
              Cs_low = quantile(Cs_mean, probs = c(0.025), na.rm = T),
              Cs_high = quantile(Cs_mean, probs = c(0.975), na.rm = T),
              N_mean = mean(N_mean),
              PrevInf_median = quantile(Cs_mean, probs = c(0.5), na.rm = T),
              PrevInf_low = quantile(Cs_mean, probs = c(0.025), na.rm = T),
              PrevInf_high = quantile(Cs_mean, probs = c(0.975), na.rm = T))%>%
    ungroup()

num_tests = read_csv('../epidata/20210125_NumeroDePruebasPorTipo.csv') %>%
    mutate(Date =  parse_date_time(FechaMuestra, orders = c("d m y"), locale = "es_ES.UTF-8"),
           N_PCR = as.numeric(`NUMERO DE PRUEBAS PCR`),
           N_ANTIGEN = as.numeric(`NUMERO DE PRUEBAS ANTIGENOS`)) %>%
    mutate(Date = as.Date(Date)) %>%
    dplyr::select(Date, N_PCR, N_ANTIGEN)


pcr_pos = read_csv('../epidata/20210125_Positividad_PCR.csv') %>%
    mutate(Date =  as.Date(parse_date_time(FechaMuestra, orders = c("d m y"), locale = "es_ES.UTF-8")),
           Pos_PCR = as.numeric(`POSITIVIDAD SEGUN FECHA MUESTRA`)) %>%
    dplyr::select(Date, Pos_PCR)

ag_pos = read_csv('../epidata/20210125_Positividad_AG.csv') %>%
    mutate(Date =  as.Date(parse_date_time(FechaMuestra, orders = c("d m y"), locale = "es_ES.UTF-8")),
           Pos_AG = as.numeric(`POSITIVIDAD SEGUN FECHA MUESTRA`)) %>%
    dplyr::select(Date, Pos_AG)

num_tests = num_tests %>%
    left_join(ag_pos, by = 'Date') %>%
    left_join(pcr_pos, by = 'Date') %>%
    mutate(Pos_PCR = Pos_PCR * N_PCR,
           Pos_AG = Pos_AG * N_ANTIGEN) %>%
    mutate(Neg_PCR = N_PCR - Pos_PCR,
           Neg_AG = N_ANTIGEN - Pos_AG)

## Get positive and negative from data
## Adjust ratio

test_data = left_join(num_tests, test_fred, by = 'Date')
pcr_sens = 0.85
ag_sens = 0.75
## PCR fit0
opt = optimize(
    f = function(x){
        get_loglikelihood_tests(
            round(test_data$Pos_PCR), round(test_data$Neg_PCR),
            test_data$Cs_median, test_data$N_mean, test_data$PrevInf_median, x,  test_sensitivity = pcr_sens, test_specificity = 1.0)
    },
    interval = c(0,1.0)
)
fit_ratio_pcr = opt$minimum

test_data = test_data %>%
    mutate(ProbPosPCR_median = get_pos_test_model(Cs_median, N_mean, PrevInf_median, fit_ratio_pcr,test_sensitivity = pcr_sens, test_specificity = 1.0),
           ProbPosPCR_high = get_pos_test_model(Cs_high, N_mean, PrevInf_high, fit_ratio_pcr,test_sensitivity = pcr_sens, test_specificity = 1.0),
           ProbPosPCR_low = get_pos_test_model(Cs_low, N_mean, PrevInf_high, fit_ratio_pcr,test_sensitivity = pcr_sens, test_specificity = 1.0))

## Antigen fit
opt = optimize(
    f = function(x){
        get_loglikelihood_tests(
            round(test_data$Pos_AG), round(test_data$Neg_AG),
            test_data$Cs_median, test_data$N_mean, test_data$PrevInf_median, x,  test_sensitivity = ag_sens, test_specificity = 1.0)
    },
    interval = c(0,1.0)
)
fit_ratio_ag = opt$minimum

test_data = test_data %>%
    mutate(ProbPosAG_median = get_pos_test_model(Cs_median, N_mean, PrevInf_median, fit_ratio_ag,test_sensitivity = ag_sens, test_specificity = 1.0),
           ProbPosAG_high = get_pos_test_model(Cs_high, N_mean, PrevInf_high, fit_ratio_ag,test_sensitivity = ag_sens, test_specificity = 1.0),
           ProbPosAG_low = get_pos_test_model(Cs_low, N_mean, PrevInf_high, fit_ratio_ag,test_sensitivity = ag_sens, test_specificity = 1.0))

ratio_df = data.frame(Test = c('PCR', 'AG'), Ratio = c(fit_ratio_pcr, fit_ratio_ag),stringsAsFactors = F)
write_csv(ratio_df, '../output/calibrated_ratio_model_test.csv')

##===============================#
## plot %pos --------
##===============================#
jpeg('../figures/manuscript_figure_model_fit_bogota_testpos.jpeg', width=6.5,height=2.5, units="in", res = 300)
layout(matrix(1:2,nrow = 1, byrow = T))
par(mar = c(1,1,1,2.5), oma = c(2,2.5,1,0))
pplt = par("plt")
adjx = (0 - pplt[1]) / (pplt[2] - pplt[1])

col_main = "gray"
col_palette = brewer.pal(n = 6, name='Dark2')
## PCR
plot(test_data$Date, (test_data$ProbPosPCR_median), xaxs = "i", yaxs = "i", type = "l", lwd = 3, col = col_main,
     main = "", xlab = "", ylab = "", xlim = c(as.Date("2020-01-01"), fit_date), xaxt = 'n',
     panel.first = grid(ny = NA, nx = 6),
     ylim = c(0,0.6), las = 2)


polygon(x = c(test_data$Date, rev(test_data$Date)),
        y = c(test_data$ProbPosPCR_high,
              rev(test_data$ProbPosPCR_low)),
        border = adjustcolor(col_main, alpha.f = 0.3), col = adjustcolor(col_main, alpha.f = 0.1))
axis.Date(1,test_data$Date)
points(test_data$Date, test_data$Pos_PCR / test_data$N_PCR, col = 'black', pch = 19, cex = 0.1)
mtext("PCR", side = 3, line = 0)
mtext("A", side = 3, line = 0.5, outer = F, cex = 1.0, adj = adjx)
## ANTIGEN
plot(test_data$Date, (test_data$ProbPosAG_median), xaxs = "i", yaxs = "i", type = "l", lwd = 3, col = col_palette[1],
     main = "", xlab = "", ylab = "", xlim = c(as.Date("2020-01-01"), fit_date), xaxt = 'n',
     panel.first = grid(ny = NA, nx = 6),
     ylim = c(0,0.6), las = 2)

axis.Date(1,test_data$Date)
polygon(x = c(test_data$Date, rev(test_data$Date)),
        y = c(test_data$ProbPosAG_high,
              rev(test_data$ProbPosAG_low)),
        border = adjustcolor(col_main, alpha.f = 0.3), col = adjustcolor(col_palette[1], alpha.f = 0.1))

points(test_data$Date, test_data$Pos_AG / test_data$N_ANTIGEN, col = 'black', pch = 19, cex = 0.1)
mtext("Antigen", side = 3, line = 0)
mtext("Proportion tests positive", side = 2, line = 1.5, outer = T)
mtext("B", side = 3, line = 0.5, outer = F, cex = 1.0, adj = adjx)
dev.off()

