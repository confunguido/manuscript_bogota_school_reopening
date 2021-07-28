#!/usr/bin/env Rscript
##=========================================#
## Author: Guido Espana
## Collect data from param sweep
## Year: 2019
## 
## requires:
##          densimtools library
##=========================================#
## User's input----------------
##=========================================#
library(tidyverse)
library(fredtools)

##========================================#
## Functions -------------
##========================================#
get_loglikelihood <- function(data_in, model_in){
    ll_out = -sum(dpois(data_in, model_in, log = T))
    return(ll_out)
}

##========================================#
## Fix files -------------
##========================================#
check_finished_jobs <- function(params_in){
    job_id = params_in$job_id[1] ## This should be only the job parameters
    
    fred_dir = system(sprintf("fred_find -k %s", job_id), intern = TRUE)
    data_dir = file.path(fred_dir,
                         "DATA", "OUT")
    if(!is.null(attr(fred_dir, "status"))){
        return(FALSE)
    }

    fred_status = system(sprintf("fred_status -k %s", job_id), intern = TRUE)
    if(unlist(strsplit(fred_status, "\\s+"))[1] != "FINISHED"){
        print(sprintf("STATUS is %s\n", unlist(strsplit(fred_status, "\\s+"))[1]))
        return(FALSE)
    }    

    tmp_out = sprintf('tmp_%s.csv', job_id)
    fred_csv = sprintf("fred_csv -k %s > %s", job_id, tmp_out)
    system(fred_csv, intern = T)
    job_df = read_csv(tmp_out)
    unlink(tmp_out)
    
    if(file.exists(file.path(data_dir,'Rt_estimates_daily.csv')) & nrow(job_df) == params_in$days[1]){        
        return(TRUE)
    }else{
        return(FALSE)
    }
}


process_job_data <- function(job_id){
    fred_dir = system(sprintf("fred_find -k %s", job_id), intern = TRUE)
    data_dir = file.path(fred_dir,
                         "DATA", "OUT")
    tmp_out = sprintf('tmp_%s.csv', job_id)
    fred_csv = sprintf("fred_csv -k %s > %s", job_id, tmp_out)
    system(fred_csv, intern = T)
    job_df = read_csv(tmp_out)
    unlink(tmp_out)

    R0_mean = job_df$RR_mean[which(job_df$RR_mean > 0)[1]]
    R0_std = job_df$RR_std[which(job_df$RR_mean > 0)[1]]

    print(sprintf("Processing job %s", job_id))
    Rt_df = read_csv(file.path(data_dir,'Rt_estimates_daily.csv'))    
    job_df$Rt_day_mean = Rt_df$Rt_mean[1]
    job_df$Rt_day_std = Rt_df$Rt_std[1]
    job_df$Rt_daily = Rt_df$Rt_mean

    Rt_week_df = read_csv(file.path(data_dir,'Rt_estimates_weekly.csv'))    
    job_df$Rt_week_mean = Rt_week_df$Rt_mean[1]
    job_df$Rt_week_std = Rt_week_df$Rt_std[1]
        
    job_df$R0_mean = R0_mean
    job_df$R0_std = R0_std

    ##========================================#
    ## Read data and compute LL --------------
    ##========================================#
    job_df$LL = Inf
    job_df$Date = as.Date('2020-01-01') + job_df$Day
        
    tmp_data = filter(COL_data, MunCode == state_input) %>%
        left_join(job_df, by = c("Date" = "Date"))

    if(nrow(job_df) > 0){
        tmp_LL = get_loglikelihood(tmp_data$Deaths, tmp_data$CF_mean)
        if(!is.na(tmp_LL)){
            job_df$LL = tmp_LL
        }
    }                          
    
    return(job_df[,c('Day', 'Week', 'Year', 'Runs', 'N_mean', 'N_std',
                     'C_mean', 'C_std', 'Cs_mean', 'Cs_std','CFR_mean',
                     'CFR_std', 'CF_mean', 'CF_std',
                     'RR_mean','RR_std', 'R0_mean', 'R0_std',
                     'Rt_day_mean','Rt_day_std', 'Rt_week_mean','Rt_week_std', 'AR_mean',
                     'Rt_daily', 'H_sheltering_mean', 'LL')])
}

##========================================#
## Collect for covid --------------------
##========================================#
state_input = 11001
args = (commandArgs(TRUE))
if(length(args) >= 1){
    state_input = as.numeric(args[1])
}
scratch_dir = Sys.getenv('scratch_dir')
## Incidence data
calibration_simdir_name = sprintf('FRED_%d_calibration',state_input)
calibration_simdir = file.path(scratch_dir, calibration_simdir_name)
calibration_dir = file.path(getwd(), 'output','CALIBRATION',sprintf("%s_%s", calibration_simdir_name, "out"))
COL_datafile = file.path(calibration_dir, 'COL_covid_death_data.csv')
if(!file.exists(COL_datafile)){
    stop("Incidence data not found")
}

interventions_df = read_csv('./input_files/interventions_Colombia.csv')

COL_data = read_csv(COL_datafile) %>%
    filter(MunCode %in% interventions_df$State) %>%
    left_join(interventions_df, by = c("MunCode" = "State")) 

simdir_name = sprintf('FRED_%d_interventions',state_input)
simdir = file.path(scratch_dir, simdir_name)
fred_results_dir = file.path(simdir,"FRED_RESULTS")
Sys.setenv(FRED_RESULTS=fred_results_dir)

output_dir = file.path(getwd(), 'output','INTERVENTIONS',sprintf("%s_%s", simdir_name, "out"))
if(!dir.exists(file.path(getwd(), 'output','INTERVENTIONS'))){
    dir.create(file.path(getwd(), 'output','INTERVENTIONS'),recursive = T)
}

fred_outputs = 'fred_output.csv'
params_file = file.path(simdir, "FRED_parameters.csv")

fredtools::fred_gather_data(params=params_file, outdir=output_dir, outfile=fred_outputs,
                            FUN=check_finished_jobs, FUN2=process_job_data, rm_out = TRUE)    



