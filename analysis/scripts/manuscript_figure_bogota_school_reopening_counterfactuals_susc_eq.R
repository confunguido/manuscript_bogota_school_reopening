##===============================#
## School reopening impact by:
## grades and income
## Author: Guido España
## 2020
##===============================#
## Setup-------------
##===============================#
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
ksus = 1
mov_factor_in = 1
outdir_st = file.path(outdir, sprintf('FRED_%d_school_psa_asymp_%.2f_fm_%.2f_ksus_%.2f_mov_%.2f_out', ss, asymp_inf,fm_ef, ksus, mov_factor_in))
interventions_df = read_csv('../experiments_colombia/Bogota_simulations/input_files/interventions_Colombia.csv') 

school_reopen_date = as.Date('2021-01-15')

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


##===============================#
## PLOT CF grades(cap) --------
##===============================#
cap_list = c(0.75, 0.35)
scenarios_list = c("Closed", "Open","Delay_1", "Delay_2")
scenarios_labels = c("Closed", "Open(-universities)","Delay(1 month)", "Delay(2 months)")
scenarios_cap_list = c("Closed", "OpenCap","OpenCap_Delay_1", "OpenCap_Delay_2")

tmp_fred = fred_sweep_df %>%
    group_by(intervention_id, Date) %>%
    summarize(C_median = medCI(C_mean),
              C_low = lowCI(C_mean),
              C_high = highCI(C_mean),
              CF_median = mean(CF_mean, na.rm = T),
              CF_low = lowCI(CF_mean),
              CF_high = highCI(CF_mean),
              AR_median = medCI(AR_mean)/100,
              AR_low = lowCI(AR_mean)/100,
              AR_high = highCI(AR_mean)/100) %>%
    ungroup()

##a------------------------
col_palette = brewer.pal(n = 6, name='Set1')
jpeg('../figures/manuscript_figure_bogota_school_reopening_counterfactuals_susc_eq.jpeg', width=6.5, height=3.8, units="in", res = 300)
layout(rbind(rep(1,3),matrix(2:7,nrow = 2, byrow = T)), heights = c(1,3,3))

par(mar = c(1,0.5,1,1), oma = c(2,4,0.0,2))
pplt = par("plt")
adjx = (0 - pplt[1]) / (pplt[2] - pplt[1])

plot(0,0, lwd = 0, xaxt = 'n', yaxt = 'n', bty = 'n')
legend("top", legend = scenarios_labels, col = col_palette[1:length(scenarios_list)], lwd = c(2,2,2,2,2), lty = c(1,1,1,1), cex = 0.8, horiz = T)

## Deaths
for(ii in 1:length(scenarios_list)){
    fred_scenario = tmp_fred %>% filter(intervention_id %in% scenarios_list[ii])
    if(ii == 1){
        plot(fred_scenario$Date, fred_scenario$CF_median, type = "l", col = col_palette[ii], xlab = "", ylab  = "", lwd = 1, ylim = c(0,100),xlim = c(as.Date("2020-01-01"), max(tmp_fred$Date)), xaxt = 'n', xaxs = 'i', yaxs = 'i', las = 2, yaxt = 'n')
        mtext("Deaths", side = 2, line = 2.5, cex = 1.0)
        axis(2,las = 2)
        ##axis.Date(1,fred_scenario$Date)
        grid(ny = 5, nx = NA)
    }else{
        lines(fred_scenario$Date, fred_scenario$CF_median, lwd = 1, 
              col = col_palette[ii]) 
    }
}
abline(v = school_reopen_date)
mtext("A", side = 3, line = 0.5, outer = F, cex = 0.8, adj = adjx)
mtext("Full capacity", side = 3)


for(cc in 1:length(cap_list)){
    print(cc)
    scenarios_cap_list = c(
        "Closed",
        sprintf("OpenCap_%.2f", cap_list[cc]), sprintf("OpenCap_%.2f_Delay_%d", cap_list[cc], 1:2))

    for(ii in 1:length(scenarios_cap_list)){
        fred_scenario = tmp_fred %>% filter(intervention_id %in% scenarios_cap_list[ii])       
        if(ii == 1){
            plot(fred_scenario$Date, fred_scenario$CF_median, type = "l", col = col_palette[ii], xlab = "", ylab  = "", lwd = 1, ylim = c(0,100),xlim = c(as.Date("2020-01-01"), max(tmp_fred$Date)), xaxt = 'n', xaxs = 'i', yaxs = 'i', las = 2, yaxt = 'n')
            ##axis.Date(1,fred_scenario$Date)
            grid(ny = 5, nx = NA)
        }else{
            lines(fred_scenario$Date, fred_scenario$CF_median, lwd = 1, 
                  col = col_palette[ii]) 
        }
    }
    mtext(LETTERS[1+cc], side = 3, line = 0.5, outer = F, cex = 0.8, adj = adjx)
    mtext(sprintf("Capacity at %.0f%%",cap_list[cc]*100), side = 3)
    abline(v = school_reopen_date)
}
abline(v = school_reopen_date)

## Attack rate
for(ii in 1:length(scenarios_list)){
    fred_scenario = tmp_fred %>% filter(intervention_id %in% scenarios_list[ii])
    if(ii == 1){
        plot(fred_scenario$Date, fred_scenario$AR_median, type = "l", col = col_palette[ii], xlab = "", ylab  = "", lwd = 1, ylim = c(0,1.0),xlim = c(as.Date("2020-01-01"), max(tmp_fred$Date)), xaxt = 'n', xaxs = 'i', yaxs = 'i', las = 2, yaxt = 'n')
        mtext("Attack rate", side = 2, line = 2.5, cex = 1.0)
        axis(2,las = 2)
        axis.Date(1,fred_scenario$Date)
        grid(ny = 5, nx = NA)
    }else{
        lines(fred_scenario$Date, fred_scenario$AR_median, lwd = 1, 
              col = col_palette[ii]) 
    }
}


mtext("D", side = 3, line = 0.5, outer = F, cex = 0.8, adj = adjx)
abline(v = school_reopen_date)
for(cc in 1:length(cap_list)){
    scenarios_cap_list = c("Closed", sprintf("OpenCap_%.2f", cap_list[cc]), sprintf("OpenCap_%.2f_Delay_%d", cap_list[cc], 1:2))
    for(ii in 1:length(scenarios_cap_list)){
        fred_scenario = tmp_fred %>% filter(intervention_id %in% scenarios_cap_list[ii])
        if(ii == 1){
            plot(fred_scenario$Date, fred_scenario$AR_median, type = "l", col = col_palette[ii], xlab = "", ylab  = "", lwd = 1, ylim = c(0,1.0),xlim = c(as.Date("2020-01-01"), max(tmp_fred$Date)), xaxt = 'n', xaxs = 'i', yaxs = 'i', las = 2, yaxt = 'n')
            axis.Date(1,fred_scenario$Date)
            grid(ny = 5, nx = NA)
        }else{
            lines(fred_scenario$Date, fred_scenario$AR_median, lwd = 1, 
                  col = col_palette[ii]) 
        }
    }
    abline(v = school_reopen_date)
    mtext(LETTERS[4+cc], side = 3, line = 0.5, outer = F, cex = 0.8, adj = adjx)
}

dev.off()

