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
ksus = 10
mov_in = 0.0
outdir_st = file.path(outdir, sprintf('FRED_%d_school_psa_asymp_%.2f_fm_%.2f_ksus_%.2f_mov_%.2f_out', ss, asymp_inf,fm_ef, ksus, mov_in))
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

interventions_extreme =  c("Closed",  "Open", "OpenNFM",  "OpenUniversities")
income_scenarios = c('income1', 'income2', 'income3', 'income4')
grades_scenarios = c('prek',  'primary', 'secondary','prekprimary', 'prekprimarysecondary')
grades_labels = c('pre-k', 'primary', 'secondary', 'pre-k+primary\n(35%-100%)', 'pre-k(75%)\n primary(35-100%)\nsecondary(35%)')

##===============================#
## PLOT CF grades(cap) --------
##===============================#
tmp_fred = fred_sweep_df %>%
    group_by(intervention_id, Date) %>%
    summarize(C_median = medCI(C_mean),
              C_low = lowCI(C_mean),
              C_high = highCI(C_mean),
              CF_median = medCI(CF_mean),
              CF_low = lowCI(CF_mean),
              CF_high = highCI(CF_mean),
              AR_median = medCI(AR_mean)/100,
              AR_low = lowCI(AR_mean)/100,
              AR_high = highCI(AR_mean)/100) %>%
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
capacity_list = c(0.35,0.5,0.75,0.9,1.0)

##a------------------------
col_palette = brewer.pal(n = 6, name='Set1')
jpeg('../figures/manuscript_figure_bogota_school_reopening_grades_timeseries.jpeg', width=6.5, height=3.5, units="in", res = 300)
layout(rbind(rep(1,5),matrix(2:11,nrow = 2, byrow = T)),
       heights = c(1.1,2.0,2.0))
par(cex = 0.5)
par(mar = c(0.5,0.5,1,0.5), oma = c(2.0,3.5,0.0,1))
pplt = par("plt")
adjx = (0 - pplt[1]) / (pplt[2] - pplt[1])

plot(0,0, lwd = 0, xaxt = 'n', yaxt = 'n', bty = 'n')
legend("top", legend = grades_labels, col = col_palette[1:length(grades_labels)], lwd = c(2,2,2,2), lty = c(1,1,1,1), horiz = T, bty = 'n')
par(cex = 0.8)

## Grade-based reopening
for(cc in 1:length(capacity_list)){
    tmp_capacity = capacity_list[cc]
    for(ii in 1:length(grades_scenarios)){
        fred_scenario = tmp_fred %>% filter(intervention_id %in% sprintf('%s-cap%.2f',grades_scenarios[ii],tmp_capacity))
        if(ii == 1){
            plot(fred_scenario$Date, fred_scenario$CF_median, type = "l", col = col_palette[ii], xlab = "", ylab  = "", lwd = 1, ylim = c(0,100),xlim = c(as.Date("2020-01-01"), max(tmp_fred$Date)), xaxt = 'n', xaxs = 'i', yaxs = 'i', las = 2, yaxt = 'n')
            if(cc == 1){
                mtext("Deaths", side = 2, line = 2.5, cex = 0.8)
                axis(2,las = 2)                
            }
            grid(ny = 5, nx = NA)
        }else{
            lines(fred_scenario$Date, fred_scenario$CF_median, lwd = 1, 
                  col = col_palette[ii]) 
        }
    }
    abline(v = school_reopen_date)
    mtext(sprintf("Capacity: %.0f%%", 100*tmp_capacity), side = 3, cex = 0.6)
}

## Attack rate
for(cc in 1:length(capacity_list)){
    tmp_capacity = capacity_list[cc]
    for(ii in 1:length(grades_scenarios)){
        fred_scenario = tmp_fred %>% filter(intervention_id %in% sprintf('%s-cap%.2f',grades_scenarios[ii],tmp_capacity))
        if(ii == 1){
            plot(fred_scenario$Date, fred_scenario$AR_median, type = "l", col = col_palette[ii], xlab = "", ylab  = "", lwd = 1, ylim = c(0,0.8),xlim = c(as.Date("2020-01-01"), max(tmp_fred$Date)), xaxt = 'n', xaxs = 'i', yaxs = 'i', las = 2,yaxt = 'n')
            if(cc == 1){
                mtext("Attack rate", side = 2, line = 2.5, cex = 0.8)
                axis(2, las =2)
            }
            date_ticks =  seq(from=min(tmp_fred$Date),to=max(tmp_fred$Date), by = '3 month')
            date_labels = month(date_ticks,label = T)
            axis.Date(1,tmp_fred$Date, at = date_ticks, labels = date_labels, cex.axis = 0.7, las = 2)
            grid(ny = 4, nx = NA)
        }else{
            lines(fred_scenario$Date, fred_scenario$AR_median, lwd = 1, 
                  col = col_palette[ii]) 
        }
    }
    abline(v = school_reopen_date)
}

dev.off()

