##===============================#
## Get msa names
## Author: Guido España
## 2020
##===============================#
## Setup-------------
##===============================#
set.seed(123456)
library(dplyr)
library(tidyverse)
library(RColorBrewer)
library(xtable)
library(lubridate)


##===============================#
## Functions------------
##===============================#
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
asymp_inf = 1.0
fm_ef = 0.73
ksus = 10
mov_in = 1.0
outdir_st = file.path(outdir, sprintf('FRED_%d_school_psa_asymp_%.2f_fm_%.2f_ksus_%.2f_mov_%.2f_out', ss, asymp_inf,fm_ef, ksus, mov_in))
outdir_fit = '../experiments_colombia/Bogota_simulations/output/CALIBRATION'
outdir_fit_st = file.path(outdir_fit, sprintf('FRED_%d_calibration_asymp_%.2f_fm_%.2f_ksus_%.2f_mov_out', ss, asymp_inf,fm_ef, ksus))
interventions_df = read_csv('../experiments_colombia/Bogota_simulations/input_files/interventions_Colombia.csv') 


brk_ages = c(0, seq(from=10,by = 10, to = 80), 120)
brk_lbls = sprintf("ACF%d_%d", brk_ages[-length(brk_ages)], brk_ages[-1])
brk_data_lbls = sprintf("A%d_%d", brk_ages[-length(brk_ages)], brk_ages[-1])
base_age_df = data.frame(AgeGroup = brk_lbls, Deaths = 0, stringsAsFactors = F)
base_age_data = data.frame(AgeGroup = brk_data_lbls, stringsAsFactors = F)

localidad_list = read_csv('../data/Bogota_localidades_ID.csv') %>%
    filter(Localidad_ID != 20)

school_reopen_date = as.Date('2021-01-25')


fit_data = read_csv(file.path(outdir_fit_st, 'COL_covid_death_data.csv')) %>%
    mutate(MunCode = as.numeric(MunCode)) %>%
    filter(MunCode %in% interventions_df$State) %>%
    left_join(interventions_df, by = c("MunCode" = "State")) 

bog_data = read_csv('../output/COL_covid_death_data.csv') %>%
    filter(MunCode == as.character(ss))

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

scenario_labels = data.frame(intervention_name = unique(fred_sweep_df$intervention_name),                             
                             stringsAsFactors = F) %>%
    mutate(intervention_id = str_replace(intervention_name, "Schools_","")) %>%
    mutate(intervention_id = str_replace(intervention_id, "Open_",""))
fred_sweep_df = fred_sweep_df %>% left_join(scenario_labels, by = "intervention_name")

interventions_extreme =  c("Closed",  "Open", "OpenNFM",  "OpenUniversities")

##===============================#
## plot figure --------
##===============================#

tmp_fred = fred_sweep_df %>%
    group_by(intervention_id, Date) %>%
    summarize(C_median = medCI(C_mean),
              C_low = lowCI(C_mean),
              C_high = highCI(C_mean),
              CF_median = medCI(CF_mean),
              CF_low = lowCI(CF_mean),
              CF_high = highCI(CF_mean),
              Rt_median = medCI(RR_mean),
              Rt_low = lowCI(RR_mean),
              Rt_high = highCI(RR_mean)) %>%
    ungroup()
closed_df = filter(tmp_fred, intervention_id == "Closed")
open_df = filter(tmp_fred, intervention_id == "Open")
open_df_all = filter(tmp_fred, intervention_id == "OpenUniversities")
open_df_all_nfm = filter(tmp_fred, intervention_id == "OpenNFM")

scenarios_extreme = c("Closed", "OpenNFM")
scenarios_open = c("Open", "OpenUniversities")
scenarios_list = c("Closed", "Open", "OpenUniversities", "OpenNFM")
colors_scenarios = c(1,2,3,4)
names(colors_scenarios) = scenarios_list

### a-----------------------
col_palette = brewer.pal(n = 6, name='Set1')
jpeg('../figures/manuscript_figure_reopening_all.jpeg', width=6.5,height=4, units="in", res = 300)
layout(rbind(rep(1,2),matrix(2:5,nrow = 2, byrow = T)), heights = c(1.5,3.0,3.0))
par(mar = c(1,2.5,1,1), oma = c(4,2,0.0,2))
pplt = par("plt")
adjx = (0 - pplt[1]) / (pplt[2] - pplt[1])

plot(0,0, lwd = 0, xaxt = 'n', yaxt = 'n', bty = 'n')
legend("top", legend = c("All closed", "School opening\n at full capacity", "School + university opening\n at full capacity", "No interventions"), col = col_palette[1:4], lwd = c(2,2,2,2), lty = c(1,1,1,1), cex = 0.7, horiz = T)


for(ii in 1:length(scenarios_extreme)){
    fred_scenario = filter(tmp_fred, intervention_id == scenarios_extreme[ii])
    if(ii == 1){
        ## DEATHS
        plot(fred_scenario$Date, fred_scenario$CF_median, type = "l", col = col_palette[colors_scenarios[scenarios_extreme[ii]]], xlab = "", ylab  = "", lwd = 1, ylim = c(0,850),xlim = c(as.Date("2020-01-01"), max(tmp_fred$Date)), xaxt = 'n', xaxs = 'i', yaxs = 'i', las = 2)
        mtext("Deaths", side = 2, line = 2.5, cex = 0.6)
    }else{
        lines(fred_scenario$Date, fred_scenario$CF_median, lwd = 1, 
              col = col_palette[colors_scenarios[scenarios_extreme[ii]]]) 
    }
    polygon(x = c(fred_scenario$Date, rev(fred_scenario$Date)),
            y = c(fred_scenario$CF_high,
                  rev(fred_scenario$CF_low)),
            border = adjustcolor(col_palette[colors_scenarios[scenarios_extreme[ii]]], alpha.f = 0.05),
            col = adjustcolor(col_palette[colors_scenarios[scenarios_extreme[ii]]], alpha.f = 0.1))    
}
tmp_data_fit = filter(fit_data, MunCode == ss) %>%
    group_by(Date) %>% summarize(Deaths = sum(Deaths), Cases = sum(Cases)) %>%
    ungroup()
points(tmp_data_fit$Date, (tmp_data_fit$Deaths), col = "black", lwd = 0.3, pch = 19, cex = 0.2)
grid(NA, 7)
abline(v = school_reopen_date)
mtext("A", side = 3, line = 0.5, outer = F, cex = 0.8, adj = adjx)

for(ii in 1:length(scenarios_open)){
    fred_scenario = filter(tmp_fred, intervention_id == scenarios_open[ii])
    if(ii == 1){
        ## DEATHS
        plot(fred_scenario$Date, fred_scenario$CF_median, type = "l", col = col_palette[colors_scenarios[scenarios_open[ii]]], xlab = "", ylab  = "", lwd = 1, ylim = c(0,250),xlim = c(as.Date("2020-01-01"), max(tmp_fred$Date)), xaxt = 'n', xaxs = 'i', yaxs = 'i', las = 2)
        mtext("Deaths", side = 2, line = 2.5, cex = 0.6)
    }else{
        lines(fred_scenario$Date, fred_scenario$CF_median, lwd = 1, 
              col = col_palette[colors_scenarios[scenarios_open[ii]]]) 
    }
    polygon(x = c(fred_scenario$Date, rev(fred_scenario$Date)),
            y = c(fred_scenario$CF_high,
                  rev(fred_scenario$CF_low)),
            border = adjustcolor(col_palette[colors_scenarios[scenarios_open[ii]]], alpha.f = 0.05),
            col = adjustcolor(col_palette[colors_scenarios[scenarios_open[ii]]], alpha.f = 0.1))    
}
abline(v = school_reopen_date)
mtext("B", side = 3, line = 0.5, outer = F, cex = 0.8, adj = adjx)
## tmp_data_bog = bog_data%>%
##     group_by(Date) %>% summarize(Deaths = sum(Deaths), Cases = sum(Cases)) %>%
##     ungroup()
## points(tmp_data_bog$Date, (tmp_data_bog$Deaths), col = "black", lwd = 0.3, pch = 19, cex = 0.5)

tmp_data_fit = filter(fit_data, MunCode == ss) %>%
    group_by(Date) %>% summarize(Deaths = sum(Deaths), Cases = sum(Cases)) %>%
    ungroup()
points(tmp_data_fit$Date, (tmp_data_fit$Deaths), col = "black", lwd = 0.3, pch = 19, cex = 0.2)


grid(NA, 5)
## AR
for(ii in 1:length(scenarios_list)){
    fred_scenario = filter(tmp_fred, intervention_id == scenarios_list[ii])
    if(ii == 1){        
        plot(closed_df$Date, cumsum(closed_df$C_median) / mean(fred_sweep_df$N_mean), type = "l", col = col_palette[ii], ylim = c(0,1.0), xlab = "Date", ylab  = "AR", lwd = 1,xlim = c(as.Date("2020-01-01"), max(tmp_fred$Date)), xaxt = 'n',  xaxs = 'i', yaxs = 'i', las = 2)
        date_ticks =  seq(from=min(tmp_fred$Date),to=max(tmp_fred$Date), by = '3 month')
        date_labels = month(date_ticks,label = T)
        axis.Date(1,tmp_fred$Date, at = date_ticks, labels = date_labels)
    }else{
        lines(fred_scenario$Date, cumsum(fred_scenario$C_median) / mean(fred_sweep_df$N_mean), lwd = 1, 
              col =col_palette[ii])
    }
    polygon(x = c(fred_scenario$Date, rev(fred_scenario$Date)),
            y = c(cumsum(fred_scenario$C_low) / mean(fred_sweep_df$N_mean),
                  rev(cumsum(fred_scenario$C_high)  / mean(fred_sweep_df$N_mean))),
            border = adjustcolor(col_palette[ii], alpha.f = 0.05),
            col = adjustcolor(col_palette[ii], alpha.f = 0.1))

}
mtext("C", side = 3, line = 0.5, outer = F, cex = 0.8, adj = adjx)
abline(v = school_reopen_date)
mtext("Attack rate", side = 2, cex = 0.6, line = 2.5)
grid(NA, 5)
## Rt
for(ii in 1:length(scenarios_list)){
    fred_scenario = filter(tmp_fred, intervention_id == scenarios_list[ii])
    if(ii == 1){        
        plot(fred_scenario$Date, (fred_scenario$Rt_median), type = "l", col = col_palette[ii], xlab = "Date", ylab  = "R(t)", lwd = 1, ylim = c(0.0,4.0),xlim = c(as.Date("2020-01-01"), max(tmp_fred$Date)), xaxt = 'n', xaxs = 'i', yaxs = 'i', las = 2)
        date_ticks =  seq(from=min(tmp_fred$Date),to=max(tmp_fred$Date), by = '3 month')
        date_labels = month(date_ticks,label = T)
        axis.Date(1,tmp_fred$Date, at = date_ticks, labels = date_labels)
    }else{
        lines(fred_scenario$Date, fred_scenario$Rt_median, lwd = 1, col = col_palette[ii])
    }
    polygon(x = c(fred_scenario$Date, rev(fred_scenario$Date)),
            y = c(fred_scenario$Rt_high,
                  rev(fred_scenario$Rt_low)),
        border = adjustcolor(col_palette[ii], alpha.f = 0.05),
        col = adjustcolor(col_palette[ii], alpha.f = 0.1))    
    
}
mtext("Reproduction number", side = 2, cex = 0.6, line = 2.5)
mtext("D", side = 3, line = 0.5, outer = F, cex = 0.8, adj = adjx)
abline(v = school_reopen_date)
grid(NA, 4)

mtext("Date", side = 1, line = 2, outer = T)
dev.off()


##=============================================#
## Data for manuscript--------
##=============================================#
fred_sweep_df %>% filter(Date >= school_reopen_date) %>%
    group_by(intervention_id, job_id) %>%
    summarize(CF_mean = sum(CF_mean, na.rm = T)) %>%
    ungroup() %>%
    group_by(intervention_id) %>%
    summarize(CF_median = quantile(CF_mean, probs = c(0.5), na.rm = T),
              CF_low = quantile(CF_mean, probs = c(0.025), na.rm = T),
              CF_high = quantile(CF_mean, probs = c(0.975), na.rm = T))%>%    
    ungroup() %>%
    filter(intervention_id %in% scenarios_list)


fred_sweep_df %>%
group_by(intervention_id, job_id) %>%
        summarize(
              AR_mean = sum(C_mean, na.rm = T) / N_mean[1]) %>%
    ungroup() %>%
    group_by(intervention_id) %>%
    summarize(
              AR_median = quantile(AR_mean, probs = 0.5),
              AR_low = quantile(AR_mean, probs = 0.025),
              AR_high = quantile(AR_mean, probs = 0.975)) %>%
    ungroup() %>%
    filter(intervention_id %in% scenarios_list)

