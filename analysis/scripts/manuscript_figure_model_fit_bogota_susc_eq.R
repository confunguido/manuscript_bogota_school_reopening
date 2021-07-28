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
ksus = 1
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
## PLOT Model fit --------
##===============================#
## First, summarize all data
tmp_fred = fred_sweep_df %>% drop_na() %>%
    filter(intervention_id == "default") %>%
    group_by(Day, Date, start_date, school_closure_day) %>%
    summarize(CF_median = quantile(CF_mean, probs = c(0.5), na.rm = T),
              CF_low = quantile(CF_mean, probs = c(0.025), na.rm = T),
              CF_high = quantile(CF_mean, probs = c(0.975), na.rm = T),
              AR_median = medCI(AR_mean)/100,
              AR_low = lowCI(AR_mean)/100,
              AR_high = highCI(AR_mean)/100,
              RR_median = medCI(RR_mean),
              RR_low = lowCI(RR_mean),
              RR_high = highCI(RR_mean),
              shelter_in_place_delay_mean = mean(shelter_in_place_delay_mean))%>%
    ungroup()


tmp_fred_age = fred_sweep_df %>% drop_na() %>%
    filter(intervention_id == "default") %>%
    dplyr::select(job_id, Day, Date,school_closure_day, start_date,  matches('^ARA[0-9]+'))%>%
    gather(key = AgeGroup, value = Infections, -c('job_id', 'Day', 'Date', 'school_closure_day', 'start_date')) %>%
    mutate(AgeGroup = str_replace(AgeGroup, "AR","")) %>%
    group_by(Day, Date, start_date, school_closure_day, AgeGroup) %>%
    summarize(C_median = median(Infections))%>%
    ungroup()


col_main = "gray"
col_palette = brewer.pal(n = 6, name='Dark2')
jpeg('../figures/manuscript_figure_model_fit_bogota_susc_eq.jpeg', width=6.5,height=4, units="in", res = 300)
layout(matrix(1:4,nrow = 2, byrow = T))
par(mar = c(1,1,1,2.5), oma = c(2,2.5,1,0))
pplt = par("plt")
adjx = (0 - pplt[1]) / (pplt[2] - pplt[1])
plot(tmp_fred$Date, (tmp_fred$CF_median), xaxs = "i", yaxs = "i", type = "l", lwd = 3, col = col_main,
     main = "",  ylab = "", xlim = c(as.Date("2020-01-01"), fit_date), ylim = c(0,200), las = 2,panel.first = grid(ny = NA),
     xaxt = 'n')

mtext("Deaths", side = 2, line = 2.5)
polygon(x = c(tmp_fred$Date, rev(tmp_fred$Date)),
        y = c(tmp_fred$CF_high,
              rev(tmp_fred$CF_low)),
        border = adjustcolor(col_main, alpha.f = 0.3),
        col = adjustcolor(col_main, alpha.f = 0.1))    
lines(tmp_fred$Date, tmp_fred$CF_median, lwd = 1.5, 
      col = adjustcolor(col_main, alpha.f = 0.2)) 


tmp_data_fit = filter(fit_data, MunCode == ss) %>%
    group_by(Date) %>% summarize(Deaths = sum(Deaths), Cases = sum(Cases)) %>%
    ungroup()


tmp_data = filter(validation_data, MunCode == ss) %>%
    group_by(Date) %>% summarize(Deaths = sum(Deaths), Cases = sum(Cases)) %>%
    ungroup()

##int_timeline = c(as.Date("2020-01-01"), as.Date(interventions_df$close_date[nn]))
## points(tmp_data$Date, tmp_data$Deaths, col = "black", lwd = 0.3, pch = 18, cex = 0.8)
points(tmp_data_fit$Date, (tmp_data_fit$Deaths), col = "black", lwd = 0.3, pch = 16, cex = 0.3)
abline(v = as.Date(fit_date))
mtext("A", side = 3, line = 0.5, outer = F, cex = 1.0, adj = adjx)

## Plot Attack Rate
plot(tmp_fred$Date, (tmp_fred$AR_median), xaxs = "i", yaxs = "i", type = "l", lwd = 3, col = col_main,
     main = "", xlab = "", ylab = "", xlim = c(as.Date("2020-01-01"), fit_date), xaxt = 'n',
     panel.first = grid(ny = NA),
     ylim = c(0,0.6), las = 2)


mtext("Attack Rate", side = 2, line = 2.2)
polygon(x = c(tmp_fred$Date, rev(tmp_fred$Date)),
        y = c(tmp_fred$AR_high,
              rev(tmp_fred$AR_low)),
        border = adjustcolor(col_main, alpha.f = 0.3), col = adjustcolor(col_main, alpha.f = 0.1))

lines(tmp_fred$Date, tmp_fred$AR_median, lwd = 1.5, 
      col = adjustcolor(col_main, alpha.f = 0.2)) 
points(as.Date('2020-11-07'), 0.3, col = "black", pch = 19, cex = 0.5)
arrows(as.Date('2020-11-07'), 0.27, as.Date('2020-11-07'), 0.33, col = "black", code = 3, cex = 0.5, length = 0.1, angle = 90)
mtext("B", side = 3, line = 0.5, outer = F, cex = 1.0, adj = adjx)

## Plot Reproductive number
plot(tmp_fred$Date, (tmp_fred$RR_median), xaxs = "i", yaxs = "i", type = "l", lwd = 3, col = col_main,
     main = "", xlab = "", ylab = "", xlim = c(as.Date("2020-01-01"), fit_date),
     las = 2,xaxt = "n",
     ylim = c(0,3))
mtext("R(t)", side = 2, line = 2.5)

axis.Date(1,tmp_fred$Date)
grid(ny = NA)
polygon(x = c(tmp_fred$Date, rev(tmp_fred$Date)),
        y = c(tmp_fred$RR_high,
              rev(tmp_fred$RR_low)),
        border = adjustcolor(col_main, alpha.f = 0.3),
        col = adjustcolor(col_main, alpha.f = 0.1))    
lines(tmp_fred$Date, tmp_fred$RR_median, lwd = 1.5, 
      col = adjustcolor(col_main, alpha.f = 0.2)) 

abline(h = 1.0)
mtext("C", side = 3, line = 0.5, outer = F, cex = 1.0, adj = adjx)

## Plot Attack Rate by age    
age_groups = unique(tmp_fred_age$AgeGroup)
age_groups = c("A0_6","A6_11","A11_17","A17_40","A40_60", "A60_120")
col_palette = colorRampPalette(col_palette)(length(age_groups))
for(aa in 1:length(age_groups)){
    tmp_age_fred = filter(tmp_fred_age, AgeGroup == age_groups[aa])
    if(aa == 1){
        plot(tmp_age_fred$Date, cumsum(tmp_age_fred$C_median),
             xaxs = "i", yaxs = "i", type = "l", lwd = 3, col = col_palette[aa],
             main = "", xlab = "", ylab = "", xlim = c(as.Date("2020-01-01"), fit_date),
             ylim = c(0, 0.6), xaxt = "n", las = 2)
    }else{
        lines(tmp_age_fred$Date, cumsum(tmp_age_fred$C_median), col = col_palette[aa], lwd = 2)
    }            
}
axis.Date(1,tmp_fred$Date)
grid(ny = NA)
mtext("Attack Rate", side = 2, line = 2.2)
legend("top",str_replace(age_groups, "A([0-9]+)_([0-9]+)","\\1-\\2"), col = col_palette, lty = rep(1, length(age_groups)), lwd = 1, horiz = T, cex = 0.4)

mtext("D", side = 3, line = 0.5, outer = F, cex = 1.0, adj = adjx)
dev.off()

##==============================================#
## Print statesments for manuscript--------------
##==============================================#
## Combined data frame
tmp_fred_cb = left_join(tmp_data_fit, tmp_fred, by = c('Date')) %>%
    mutate(inCI = (Deaths >= CF_low  & Deaths <= CF_high)) %>%
    dplyr::select(Date, Deaths, Cases, CF_median, CF_low, CF_high, inCI, AR_median, AR_low, AR_high)

(sum(tmp_fred_cb$inCI) / nrow(tmp_fred_cb))
(tmp_fred_cb[which.max(tmp_fred_cb$Deaths),])
(tail(tmp_fred_cb))
(tmp_fred_cb %>% filter(Date == as.Date('2020-11-07')) %>%dplyr::select(AR_median, AR_low, AR_high))
(tmp_fred_cb %>% filter(Date == as.Date(fit_date)) %>%dplyr::select(AR_median, AR_low, AR_high))

## Calibrated parameters
(params_sweep_df[,param_names_fit] %>%
    gather(key = parameter, value = value) %>%
    group_by(parameter) %>%
    summarize(medParam = medCI(value),
              lowCI = lowCI(value),
              highCI = highCI(value)))

