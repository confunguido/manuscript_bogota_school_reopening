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
plot_color_bar = function(myColStrip_in, myColBreaks_in,myColBar_in, myTicks_in, plot_line = FALSE, label_in = "",...){
  image(t(myColStrip_in),
        col = myColBar_in, breaks = myColBreaks_in,
        xlim = c(0.0,1.0), axes = F)
  ylims = par("usr")[c(3,4)]
  myBarTicks = seq(from = ylims[1], to = ylims[2], by = diff(ylims)/(length(myTicks_in) - 1))
  myBarTickLabels = myTicks_in
  axis(4,labels = rep("",length(myBarTicks)), at = myBarTicks,cex.axis = 0.8,tck = -0.1)
  axis(4, at =  myBarTicks, labels = myBarTickLabels, cex.axis = 0.8,line = 0.1,lwd = 0, las = 2)
  if(plot_line){
    abline(h = myBarTicks[2], col = "black", lwd = 2) 
  }
  mtext(label_in, side = 4, outer = F, lwd = 2,...)
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
mov_in = 1.0
outdir_st = file.path(outdir, sprintf('FRED_%d_school_psa_asymp_%.2f_fm_%.2f_ksus_%.2f_mov_%.2f_out', ss, asymp_inf,fm_ef, ksus, mov_in))
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

fit_date = as.Date("2021-01-25")
forecast_date = as.Date("2021-06-30")
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
## Process geo shelter-------------
##===============================#
unidad_catastro = read_csv('../data/Localidad_Unidad_Catastral.csv')
esc_shp = rgdal::readOGR('../data/scat_shp/scat_shp.shp')
upz_shp = rgdal::readOGR('../data/UPZ_Bogota/UPla.shp')
localidad_shp = rgdal::readOGR('../data/localidades_bogota/poligonos-localidades.shp')
grand_shp = rgdal::readOGR('../data/GrandData_Bogota/GrandData_Bogota.shp') # Local storage
house_esc = read_csv('../synthetic_populations/data/processed_data/popdata/population_by_ESC_bogota.csv')
esc_mov = esc_shp[esc_shp@data$SCACODIGO %in% house_esc$ESC,]

shelter_geo = read_csv('../experiments_colombia/Bogota_simulations/input_files/11001_grandata_mobility_trends.csv')
date_seq = seq(from = as.Date('2020-03-02'), to = as.Date('2020-11-01'), by = 1)

##===============================#
## PLOT Model fit --------
##===============================#
## First, summarize all data
tmp_fred = fred_sweep_df %>% drop_na() %>%
    filter(intervention_name == "Schools_Closed") %>%
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
              N_sheltering_median = 1 -  medCI(N_sheltering_mean/N_mean[1]),
              N_sheltering_low = 1-  lowCI(N_sheltering_mean/N_mean[1]),
              N_sheltering_high = 1 -  highCI(N_sheltering_mean/N_mean[1]),
              shelter_in_place_delay_mean = mean(shelter_in_place_delay_mean))%>%
    ungroup()

tmp_fred_pred = filter(tmp_fred, Date > as.Date(fit_date))
tmp_fred_high_mov = filter(tmp_fred, Date > as.Date(fit_date)) %>%
    mutate(N_sheltering_median = 1.0)

col_main = "black"
col_palette = brewer.pal(n = 6, name='Dark2')
jpeg('../figures/manuscript_figure_shelter_timeseries.jpeg', width=6.5,height=4, units="in", res = 300)
layout(rbind(rep(1,4),2:5), widths = c(3,3,3,1.0))

par(mar = c(1,2,2,1.0), oma = c(1,3,1,3))
pplt = par("plt")
adjx = (0 - pplt[1]) / (pplt[2] - pplt[1])
plot(tmp_fred$Date, (tmp_fred$N_sheltering_median), xaxs = "i", yaxs = "i", type = "l", lwd = 3, col = col_main,
     main = "",  ylab = "", xlim = c(as.Date("2020-03-01"), forecast_date), ylim = c(0,1.01), las = 2,
     xaxt = 'n', xlab = "")

legend("bottomright", legend = c("fit", "moderate mobility", "high mobility"), col = c("black", "gray", "blue"), lwd = c(2,2), lty = c(1,1), bty = 'n')
mtext("Proportion not staying at home", side = 2, line = 2.5, cex = 0.6)
polygon(x = c(tmp_fred$Date, rev(tmp_fred$Date)),
        y = c(tmp_fred$N_sheltering_high,
              rev(tmp_fred$N_sheltering_low)),
        border = adjustcolor(col_main, alpha.f = 0.3),
        col = adjustcolor(col_main, alpha.f = 0.1))    
lines(tmp_fred$Date, tmp_fred$N_sheltering_median, lwd = 1.5, 
      col = adjustcolor(col_main, alpha.f = 0.2))

date_ticks =  seq(from=min(tmp_fred$Date),to=max(tmp_fred$Date), by = '1 month')
date_labels = month(date_ticks,label = T)
axis.Date(1,tmp_fred$Date, at = date_ticks, labels = date_labels)
lines(tmp_fred_pred$Date, tmp_fred_pred$N_sheltering_median, lwd = 2, col = "gray")
lines(tmp_fred_high_mov$Date, tmp_fred_high_mov$N_sheltering_median, lwd = 2, col = "blue")
mtext("A", side = 3, line = -0.5, outer = F, cex = 1.0, adj = adjx)
abline(v = as.Date('2021-01-25'))
## Plot map
date_list = as.Date(c('2020-04-01', '2020-08-01', '2020-11-01'))
mov_brks = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0)
for(dd in 1:length(date_list)){
    tmp_df = shelter_geo %>% filter(date == date_list[dd])
 
    tmp_mov_df = esc_mov@data %>% 
        left_join(tmp_df, by = c("SCACODIGO"))

    myColStrip = as.matrix((mov_brks + diff(mov_brks)[1] / 2)[-length(mov_brks)])
    my_colors = rev(brewer.pal(5,"GnBu"))
    my_colors = colorRampPalette(my_colors)(length(mov_brks)-1)
    mov_brk <- cut(as.numeric(tmp_mov_df$shelter_trend), breaks = mov_brks, include.lowest = T, right = F)
    my_colors = my_colors[as.numeric(mov_brk)]
    ##plot(upz_shp[upz_shp@data$UPlTipo == 1,], lwd = 0.2)
    plot(esc_mov, col = my_colors, main = "", add = F,lwd = 0.05)
    mtext(date_list[dd], side = 1, cex = 0.6)
    if(dd == 1){
        mtext(LETTERS[dd + 1], side = 3, line = 0.5, outer = F, cex = 1.0, adj = adjx)
    }
    my_colors = rev(brewer.pal(5,"GnBu"))
    my_colors = colorRampPalette(my_colors)(length(mov_brks)-1)
    ##plot(upz_shp[upz_shp@data$UPlTipo == 1,], lwd = 0.2, add = T)
}

plot_color_bar(myColStrip_in = myColStrip, 
               myColBreaks_in = mov_brks, myColBar_in = rev(my_colors), 
               myTicks_in = mov_brks, label_in = "Relative mobility", 
               cex = 0.6, line = 2)

dev.off()
