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

error.bar <- function(x, y, upper, lower, length=0.01,...){
  arrows(x,upper, x, lower, angle=90, code=3, length=length, ...)
}

##===============================#
## Data ------------
##===============================#
ss = 11001
outdir = '../experiments_colombia/Bogota_simulations/output/SHORT_FORECAST'
asymp_inf = 1.0
asymp_low = 0.75
fm_ef = 0.73
ksus = 10
mov_in = 1.0


outdir_st = file.path(outdir, sprintf('FRED_%d_school_psa_asymp_%.2f_fm_%.2f_ksus_%.2f_mov_%.2f_out', ss, asymp_inf,fm_ef, ksus, mov_in))

outdir_inf = file.path(outdir, sprintf('FRED_%d_school_psa_asymp_%.2f_fm_%.2f_ksus_%.2f_mov_%.2f_out', ss, asymp_low,fm_ef, ksus, mov_in))
interventions_df = read_csv('../experiments_colombia/Bogota_simulations/input_files/interventions_Colombia.csv') 


school_reopen_date = as.Date('2021-01-25')

bog_data = read_csv('../output/COL_covid_death_data.csv') %>%
    filter(MunCode == as.character(ss))


##===============================#
## Process output-------------
##===============================#
data_out = file.path(outdir_st,'fred_output.csv')
data_out_inf = file.path(outdir_inf,'fred_output.csv')


params_out = file.path(outdir_st, 'FRED_parameters_out.csv')
params_sweep_df = read_csv(params_out)
fred_sweep_df = read_csv(data_out) %>% 
    right_join(params_sweep_df, by = c("job_id" = "job_id")) %>%
    mutate(Date = as.Date("2020-01-01") + Day)    


fred_sweep_df = fred_sweep_df %>% group_by(seed,state_code) %>% mutate(CumCF = cumsum(CF_mean), RR_max = max(RR_mean, na.rm = T)) %>% ungroup()

params_inf_out = file.path(outdir_inf, 'FRED_parameters_out.csv')
params_sweep_inf = read_csv(params_inf_out)
fred_sweep_inf = read_csv(data_out_inf) %>% 
    right_join(params_sweep_inf, by = c("job_id" = "job_id")) %>%
    mutate(Date = as.Date("2020-01-01") + Day)    


fred_sweep_inf = fred_sweep_inf %>% group_by(seed,state_code) %>% mutate(CumCF = cumsum(CF_mean), RR_max = max(RR_mean, na.rm = T)) %>% ungroup()


scenario_labels = data.frame(intervention_name = unique(fred_sweep_df$intervention_name),                             
                             stringsAsFactors = F) %>%
    mutate(intervention_id = str_replace(intervention_name, "Schools_","")) %>%
    mutate(intervention_id = str_replace(intervention_id, "Open_",""))

fred_sweep_df = fred_sweep_df %>% left_join(scenario_labels, by = "intervention_name")
fred_sweep_inf = fred_sweep_inf %>% left_join(scenario_labels, by = "intervention_name")

interventions_extreme =  c("Closed",  "Open", "OpenNFM",  "OpenUniversities")
income_scenarios = c('income1', 'income2', 'income3', 'income4')
income_labels = c('MPI(Q0-Q1)', 'MPI(Q1-Q2)', 'MPI(Q2-Q3)', 'MPI(Q3-Q4)')
grades_scenarios = c('prek',  'primary', 'secondary','prekprimary', 'prekprimarysecondary')
grades_labels = c('prek', 'primary', 'secondary', 'prek+primary\n(35%-100%)', 'prek(75%)\n primary(35-100%)\nsecondary(35%)')

##===============================#
## Gather  output ------------
##===============================#
tmp_fred = fred_sweep_df %>% filter(Date >= school_reopen_date) %>%
    group_by(intervention_id, job_id) %>%
        summarize(CF_mean = sum(CF_mean, na.rm = T),
              C_mean = sum(C_mean, na.rm = T),
              AR_mean = sum(C_mean, na.rm = T),
              Cs_mean = sum(Cs_mean, na.rm = T),
              Hospitalized_mean = sum(Hospitalized_mean, na.rm = T)) %>%
    ungroup() %>%
    group_by(intervention_id) %>%
    summarize(CF_median = quantile(CF_mean, probs = c(0.5), na.rm = T),
              CF_low = quantile(CF_mean, probs = c(0.025), na.rm = T),
              CF_high = quantile(CF_mean, probs = c(0.975), na.rm = T),
              AR_median = quantile(AR_mean, probs = 0.5),
              AR_low = quantile(AR_mean, probs = 0.025),
              AR_high = quantile(AR_mean, probs = 0.975)) %>%
    ungroup()

tmp_fred_inf = fred_sweep_inf %>% filter(Date >= school_reopen_date) %>%
    group_by(intervention_id, job_id) %>%
        summarize(CF_mean = sum(CF_mean, na.rm = T),
              C_mean = sum(C_mean, na.rm = T),
              AR_mean = sum(C_mean, na.rm = T),
              Cs_mean = sum(Cs_mean, na.rm = T),
              Hospitalized_mean = sum(Hospitalized_mean, na.rm = T)) %>%
    ungroup() %>%
    group_by(intervention_id) %>%
    summarize(CF_median = quantile(CF_mean, probs = c(0.5), na.rm = T),
              CF_low = quantile(CF_mean, probs = c(0.025), na.rm = T),
              CF_high = quantile(CF_mean, probs = c(0.975), na.rm = T),
              AR_median = quantile(AR_mean, probs = 0.5),
              AR_low = quantile(AR_mean, probs = 0.025),
              AR_high = quantile(AR_mean, probs = 0.975)) %>%
    ungroup()


##===============================#
## PLOT CF grades(cap) --------
##===============================#
col_palette = brewer.pal(n = 6, name='Greys')
jpeg('../figures/manuscript_figure_bogota_school_reopening_grades_asymp_inf.jpeg', width=6.5, height=3.2, units="in", res = 300)
layout(matrix(1:1,nrow = 1, byrow = T))
par(mar = c(2,2,0,1.0), oma = c(1,3,2,1))
pplt = par("plt")
adjx = (0 - pplt[1]) / (pplt[2] - pplt[1])

grades_capacity_scenarios = expand.grid(grades_scenarios, c(0.35,0.5,0.75,0.9,1.0), stringsAsFactors = F)

## Filter grades
tmp_fred_int = tmp_fred %>% filter(intervention_id %in% sprintf('%s-cap%.2f',grades_capacity_scenarios[,1], grades_capacity_scenarios[,2])) %>% dplyr::select(intervention_id, CF_median) %>%
    separate(intervention_id, c('Grade', 'Capacity'), sep = "-") %>%
    spread(key = Capacity, value = CF_median) %>%
    column_to_rownames("Grade")

tmp_fred_int = tmp_fred_int[grades_scenarios,]
tmp_fred_int = t(as.matrix(tmp_fred_int))

tmp_fred_int_inf = tmp_fred_inf %>% filter(intervention_id %in% sprintf('%s-cap%.2f',grades_capacity_scenarios[,1], grades_capacity_scenarios[,2])) %>% dplyr::select(intervention_id, CF_median) %>%
    separate(intervention_id, c('Grade', 'Capacity'), sep = "-") %>%
    spread(key = Capacity, value = CF_median) %>%
    column_to_rownames("Grade")

tmp_fred_int_inf = tmp_fred_int_inf[grades_scenarios,]
tmp_fred_int_inf = t(as.matrix(tmp_fred_int_inf))

tmp_fred_low = tmp_fred %>% filter(intervention_id %in% sprintf('%s-cap%.2f',grades_capacity_scenarios[,1], grades_capacity_scenarios[,2])) %>% dplyr::select(intervention_id, CF_low) %>%
    separate(intervention_id, c('Grade', 'Capacity'), sep = "-") %>%
    spread(key = Capacity, value = CF_low) %>%
    column_to_rownames("Grade")

tmp_fred_low = tmp_fred_low[grades_scenarios,]
tmp_fred_low = t(as.matrix(tmp_fred_low))

tmp_fred_high = tmp_fred %>% filter(intervention_id %in% sprintf('%s-cap%.2f',grades_capacity_scenarios[,1], grades_capacity_scenarios[,2])) %>% dplyr::select(intervention_id, CF_high) %>%
    separate(intervention_id, c('Grade', 'Capacity'), sep = "-") %>%
    spread(key = Capacity, value = CF_high) %>%
    column_to_rownames("Grade")

tmp_fred_high = tmp_fred_high[grades_scenarios,]
tmp_fred_high = t(as.matrix(tmp_fred_high))



tmp_bar <- barplot(tmp_fred_int, beside = T, col = col_palette[1:nrow(tmp_fred_int)], 
                   ylim = c(0,max(tmp_fred$CF_high[tmp_fred$intervention_id == "OpenUniversities"])*1.5),
                  main = "",
                  yaxt = 'n',xaxt = 'n')


error.bar(tmp_bar,tmp_fred_int, tmp_fred_high, tmp_fred_low, length = 0.02, col = "#000000")
## Plot with high mobility
for(nn in 1:ncol(tmp_bar)){
    points(tmp_bar[,nn], tmp_fred_int_inf[,nn], pch = 18, col = "red", cex = 0.6)
}


axis(side = 2, las = 1)
axis(side = 1, labels = grades_labels, at = tmp_bar[3,],cex.axis = 0.4, tick = FALSE, line = -0.5)
abline(h = tmp_fred$CF_median[tmp_fred$intervention_id == "Closed"], lwd = 1, lty = 3)
abline(h = tmp_fred$CF_low[tmp_fred$intervention_id == "Closed"], lwd = 0.5, lty = 3)
abline(h = tmp_fred$CF_high[tmp_fred$intervention_id == "Closed"], lwd = 0.5, lty = 3)

abline(h = tmp_fred$CF_median[tmp_fred$intervention_id == "OpenUniversities"], lwd = 1, lty = 3, col = "red")
abline(h = tmp_fred$CF_low[tmp_fred$intervention_id == "OpenUniversities"], lwd = 0.5, lty = 3, col = "red")
abline(h = tmp_fred$CF_high[tmp_fred$intervention_id == "OpenUniversities"], lwd = 0.5, lty = 3, col = "red")

text(x = max(tmp_bar[,]) + 0.3, y =  tmp_fred$CF_median[tmp_fred$intervention_id == "OpenUniversities"]*1.2, "Fully\nOpened", col = "red", cex = 0.35, adj = 0)
text(x = max(tmp_bar) + 0.5, y =  tmp_fred$CF_median[tmp_fred$intervention_id == "Closed"]*1.2, "Closed", col = "black", cex = 0.35, adj = 0)

legend("top", c(sprintf("%.0f%%",(100* as.numeric(str_replace(rownames(tmp_fred_int), 'cap', '')))), 'Low asymp.\n infect.'),
       fill = c(col_palette[1:nrow(tmp_fred_int)], NA), border = c(rep('black',nrow(tmp_fred_int)),NA), cex = 0.6,
       pch = c(rep(-1,nrow(tmp_fred_int)), 18), col = c(rep(NA, nrow(tmp_fred_int)), 'red'), horiz = T)

mtext("Cumulative deaths from Jan. 25 to Aug. 31", side = 2, line = 3, cex = 0.8)
mtext("Children less susceptible than adults", side = 4, line = 0.5, cex = 0.5)
##legend("top", c("High mobility"), fill = NA, border = NA, cex = 0.8, pch = 8, col = "blue")
mtext("A", side = 3, line = 0.5, outer = F, cex = 1.0, adj = adjx)

dev.off()



##===============================#
## Data for manuscript (lowinf)--------
##===============================#
tmp_fred = fred_sweep_inf %>% filter(Date >= school_reopen_date) %>%
    group_by(intervention_id, job_id) %>%
        summarize(CF_mean = sum(CF_mean, na.rm = T),
              C_mean = sum(C_mean, na.rm = T),
              AR_mean = sum(C_mean, na.rm = T),
              Cs_mean = sum(Cs_mean, na.rm = T),
              Hospitalized_mean = sum(Hospitalized_mean, na.rm = T)) %>%
    ungroup() %>%
    group_by(intervention_id) %>%
    summarize(CF_median = quantile(CF_mean, probs = c(0.5), na.rm = T),
              CF_low = quantile(CF_mean, probs = c(0.025), na.rm = T),
              CF_high = quantile(CF_mean, probs = c(0.975), na.rm = T),
              AR_median = quantile(AR_mean, probs = 0.5),
              AR_low = quantile(AR_mean, probs = 0.025),
              AR_high = quantile(AR_mean, probs = 0.975)) %>%
    ungroup()
(tmp_fred %>% filter(intervention_id == "Closed"))
grades_capacity_scenarios = expand.grid(grades_scenarios, c(0.35,0.5,0.75,0.9,1.0), stringsAsFactors = F)
for(aa in c('CF_median', 'CF_low', 'CF_high')){
    tmp_fred_int = tmp_fred %>% filter(intervention_id %in% sprintf('%s-cap%.2f',grades_capacity_scenarios[,1], grades_capacity_scenarios[,2])) %>% dplyr::select(intervention_id, !!aa) %>%
        separate(intervention_id, c('Grade', 'Capacity'), sep = "-") %>%
        spread(key = Capacity, value = !!aa) %>%
        column_to_rownames("Grade")
    print(aa)
    print(tmp_fred_int)
}


income_capacity_scenarios = expand.grid(income_scenarios, c(0.35,0.5,0.75,0.9,1.0), stringsAsFactors = F)
for(aa in c('CF_median', 'CF_low', 'CF_high')){
    tmp_fred_int = tmp_fred %>% filter(intervention_id %in% sprintf('%s-cap%.2f',income_capacity_scenarios[,1], income_capacity_scenarios[,2])) %>% dplyr::select(intervention_id, !!aa) %>%
        separate(intervention_id, c('Income', 'Capacity'), sep = "-") %>%
        spread(key = Capacity, value = !!aa) %>%
        column_to_rownames("Income")
    print(sprintf('INCOME:%s',aa))
    print(tmp_fred_int)
}
(tmp_fred %>% filter(intervention_id == "Closed"))



##===============================#
## Data for manuscript same inf --------
##===============================#
tmp_fred = fred_sweep_df %>% filter(Date >= school_reopen_date) %>%
    group_by(intervention_id, job_id) %>%
        summarize(CF_mean = sum(CF_mean, na.rm = T),
              C_mean = sum(C_mean, na.rm = T),
              AR_mean = sum(C_mean, na.rm = T),
              Cs_mean = sum(Cs_mean, na.rm = T),
              Hospitalized_mean = sum(Hospitalized_mean, na.rm = T)) %>%
    ungroup() %>%
    group_by(intervention_id) %>%
    summarize(CF_median = quantile(CF_mean, probs = c(0.5), na.rm = T),
              CF_low = quantile(CF_mean, probs = c(0.025), na.rm = T),
              CF_high = quantile(CF_mean, probs = c(0.975), na.rm = T),
              AR_median = quantile(AR_mean, probs = 0.5),
              AR_low = quantile(AR_mean, probs = 0.025),
              AR_high = quantile(AR_mean, probs = 0.975)) %>%
    ungroup()
(tmp_fred %>% filter(intervention_id %in% c("Closed", "Open", "OpenUniversities")))


grades_capacity_scenarios = expand.grid(grades_scenarios, c(0.35,0.5,0.75,0.9,1.0), stringsAsFactors = F)
for(aa in c('CF_median', 'CF_low', 'CF_high')){
    tmp_fred_int = tmp_fred %>% filter(intervention_id %in% sprintf('%s-cap%.2f',grades_capacity_scenarios[,1], grades_capacity_scenarios[,2])) %>% dplyr::select(intervention_id, !!aa) %>%
        separate(intervention_id, c('Grade', 'Capacity'), sep = "-") %>%
        spread(key = Capacity, value = !!aa) %>%
        column_to_rownames("Grade")
    print(aa)
    print(tmp_fred_int)
}


