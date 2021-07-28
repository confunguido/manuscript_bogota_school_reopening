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
  axis(4, at =  myBarTicks, labels = myBarTickLabels, cex.axis = 0.8,line = -1.0,lwd = 0)
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

fit_date = as.Date("2020-12-31")
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

## Get population by localidad
esc_localidad = read_csv('../data/Localidad_Unidad_Catastral.csv')
age_pop_data = read_csv('../synthetic_populations/data/processed_data/popdata/bogota_population_data_sec.csv')
sample_id_data = read_csv('../data/Bogota_localidades_serological_ID.csv')

pop_data = age_pop_data %>% left_join(esc_localidad, by = c('Zone' = 'SCACODIGO')) %>%
    group_by(Localidad) %>%
    summarize(Pop = sum(Pop)) %>%
    ungroup()  %>%
    left_join(sample_id_data %>% dplyr::select(Localidad_ID, Estrato_muestral), by = c("Localidad" = "Localidad_ID")) %>%
    group_by(Estrato_muestral) %>%
    summarize(Pop = sum(Pop))%>%
    ungroup()

pop_localidad = age_pop_data %>% left_join(esc_localidad, by = c('Zone' = 'SCACODIGO')) %>%
    group_by(Localidad) %>%
    summarize(Pop = sum(Pop)) %>%
    ungroup()  

bog_serological_df = data.frame(
    Estrato_muestral = c(1,2,3,4),
    Median = c(0.24,0.31,0.34,0.3),
    HighCI = c(0.21,0.27,0.3,0.26),
    LowCI = c(0.28,0.35,0.38,0.34),
    stringsAsFactors = F)

##===============================#
## PLOT Localidad fit --------
##===============================#
## First, summarize all data
col_main = "gray"

jpeg('../figures/manuscript_figure_estrato_muestral_fit_AR.jpeg', width=6.5,height=2.5, units="in", res = 300)
layout(matrix(1:4,nrow = 1, byrow = T))
##par(mar = c(1,1,3,1), oma = c(1,4,1,1))
par(mar = c(2,1,1,2.5), oma = c(2,2.5,1,0))
pplt = par("plt")
adjx = (0 - pplt[1]) / (pplt[2] - pplt[1])

tmp_fred =  fred_sweep_df %>%
    group_by(state_code, Date, job_id) %>%
    ungroup()%>% dplyr::select(Date, job_id, starts_with('CasesLoc')) %>%
    gather(key = AgeGroup, value = Infections, -c('job_id','Date'))%>%
    mutate(Localidad = as.numeric(str_replace_all(AgeGroup, "CasesLoc_([0-9]+?)_A[0-9]+_[0-9]+","\\1"))) %>%
    mutate(AgeGroup = str_replace_all(AgeGroup, "CasesLoc_[0-9]+_(A[0-9]+_[0-9]+?)","\\1")) %>%
    left_join(sample_id_data %>% dplyr::select(Localidad_ID, Estrato_muestral), by = c("Localidad" = "Localidad_ID")) %>%
    left_join(pop_data, by = "Estrato_muestral") %>%
    group_by(Date, job_id, Estrato_muestral) %>%
    summarize(Infections = sum(Infections, na.rm = T),
              Pop = mean(Pop)) %>%
    mutate(AR = Infections / Pop)

for(ii in 1:4){ 
    print(ii)

    tmp_fred_median = tmp_fred %>%
        filter(Estrato_muestral == ii) %>%
        group_by(Date) %>%
        summarize(AR = medCI(AR)) %>%
        ungroup() %>% mutate(AR = cumsum(AR))
    
    tmp_fred_lowCI = tmp_fred %>%
        filter(Estrato_muestral == ii) %>%
        group_by(Date) %>%
        summarize(AR = lowCI(AR)) %>%
        ungroup() %>% mutate(AR = cumsum(AR))

    tmp_fred_highCI = tmp_fred %>%
        filter(Estrato_muestral == ii) %>%
         group_by(Date) %>%
        summarize(AR = highCI(AR)) %>%
        ungroup() %>% mutate(AR = cumsum(AR))


    ## tmp_data = filter(BOG_data, MunCode == ss, Localidad == localidad_list$Localidad_ID[ll]) %>%
    ##     group_by(Date) %>% summarize(Deaths = sum(Deaths), Cases = sum(Cases)) %>%
    ##     ungroup()%>%
    ##     mutate(CumDeaths = cumsum(Deaths))

    plot(tmp_fred_median$Date, (tmp_fred_median$AR), xaxs = "i", yaxs = "i", type = "l", lwd = 3,
         col = col_main,main = "",
         xlim = c(as.Date("2020-03-01"), fit_date), ylim = c(0,1), xaxt = 'n')
    axis.Date(1, tmp_fred_median$Date, cex.axis = 0.5)
    polygon(x = c(tmp_fred_highCI$Date, rev(tmp_fred_lowCI$Date)),
            y = c(tmp_fred_highCI$AR,
                  rev(tmp_fred_lowCI$AR)),
            border = adjustcolor(col_main, alpha.f = 0.7),
            col = adjustcolor(col_main, alpha.f = 0.2))    
    mtext(sprintf("Estrato muestral %d", ii), side = 3, cex = 0.6)
    ##points(tmp_data$Date, tmp_data$CumDeaths, col = "black", lwd = 0.3, pch = 16, cex = 0.5)
    points(as.Date('2020-11-07'), bog_serological_df$Median[bog_serological_df$Estrato_muestral == ii], col = "black", pch = 19, cex = 0.5)
    arrows(as.Date('2020-11-07'), bog_serological_df$LowCI[bog_serological_df$Estrato_muestral == ii], as.Date('2020-11-07'), bog_serological_df$HighCI[bog_serological_df$Estrato_muestral == ii], col = "black", code = 3, cex = 0.5, length = 0.1, angle = 90)
}
mtext("Attack rate", side = 2, outer = T, line = 1.3, cex = 0.8)
dev.off()



##===============================#
## PLOT Localidad AR --------
##===============================#
dates_fit = c(as.Date('2020-07-07'), as.Date('2020-09-07'), as.Date('2020-11-07'), as.Date('2021-01-07'))
tmp_fred =  fred_sweep_df %>%
    group_by(state_code, Date, job_id) %>%
    ungroup()%>% dplyr::select(Date, job_id, starts_with('CasesLoc')) %>%
    gather(key = AgeGroup, value = Infections, -c('job_id','Date'))%>%
    mutate(Localidad = as.numeric(str_replace_all(AgeGroup, "CasesLoc_([0-9]+?)_A[0-9]+_[0-9]+","\\1"))) %>%
    mutate(AgeGroup = str_replace_all(AgeGroup, "CasesLoc_[0-9]+_(A[0-9]+_[0-9]+?)","\\1")) %>%    
    left_join(pop_localidad, by = "Localidad") %>%
    group_by(Date, job_id, Localidad) %>%
    summarize(Infections = sum(Infections, na.rm = T),
              Pop = mean(Pop)) %>%
    ungroup() %>%
    mutate(AR = Infections / Pop)


## First, summarize all data----------------------
col_main = "gray"
jpeg('../figures/manuscript_figure_localidad_fit_AR.jpeg', width=6.5,height=3.5, units="in", res = 300)
layout(matrix(1:5,nrow = 1, byrow = T), widths = c(3,3,3,3,1.3))
par(mar = c(0.5,1,1,2.5), oma = c(2,2.5,1,0))
pplt = par("plt")
adjx = (0 - pplt[1]) / (pplt[2] - pplt[1])

localidad_shp = rgdal::readOGR('../data/localidades_bogota/poligonos-localidades.shp')
localidad_shp = localidad_shp[localidad_shp$Identificad != 20,]
localidad_shp@data$Localidad = as.numeric(localidad_shp@data$Identificad)

for(dd in 1:length(dates_fit)){
    
    tmp_fred_median = tmp_fred %>%
        arrange(Date, Localidad) %>%
        group_by(Date, Localidad) %>%
        summarize(AR = medCI(AR)) %>%
        ungroup() %>%
        group_by(Localidad) %>%
        mutate(AR = cumsum(AR)) %>%
        filter(Date == dates_fit[dd]) %>%
        dplyr::select(-Date)
    tmp_shp = localidad_shp
    tmp_shp@data = tmp_shp@data %>%
        left_join(tmp_fred_median, by = "Localidad")

    my_colors = brewer.pal(6,"YlOrRd")
    ar_brks = c(0,0.1,0.2,0.3,0.4,0.5)
    my_colors = colorRampPalette(my_colors)(length(ar_brks) -1)
    myColStrip = as.matrix((ar_brks + diff(ar_brks)[1] / 2)[-length(ar_brks)])
    AR_brk <- cut(tmp_shp@data$AR, breaks = ar_brks, include.lowest = T)
    my_colors = my_colors[as.numeric(AR_brk)]
    
    plot(tmp_shp, col = my_colors)


    my_colors = brewer.pal(6,"YlOrRd")
    my_colors = colorRampPalette(my_colors)(length(ar_brks) - 1)
    mtext(dates_fit[dd], side = 1)
}
plot_color_bar(myColStrip_in = myColStrip, 
               myColBreaks_in = ar_brks, myColBar_in = my_colors, 
               myTicks_in = ar_brks, label_in = "AR", 
               cex = 0.6, line = 1.5)

mtext("Estimated attack rate in Bogota", side = 3, outer = T, line = -0.5, cex = 0.8)
dev.off()
