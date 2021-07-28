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

    fred_csv = sprintf("fred_csv -k %s -v I", job_id)
    fred_csv_st = system(fred_csv, intern = T)
    if(!is.null(attr(fred_csv_st, "status"))){
        ##system(sprintf("rm -rf %s", tmp_out), intern = T)
        return(FALSE)
    }

    job_df = read.csv(text = fred_csv_st, stringsAsFactors = FALSE)
    
    job_df = drop_na(job_df)
    if(!(nrow(job_df) == params_in$days[1] & !is.na(job_df$I_mean[1]))){        
        return(FALSE)
    }
    
    if(!file.exists(file.path(data_dir,'AgeGroupsData_mean.csv')) & nrow(job_df) > 1){
        return(FALSE)
    }
    return(TRUE)
}

process_job_data <- function(job_id){
    fred_dir = system(sprintf("fred_find -k %s", job_id), intern = TRUE)
    data_dir = file.path(fred_dir,
                         "DATA", "OUT")
    fred_csv = sprintf("fred_csv -k %s", job_id)

    fred_csv_st = system(fred_csv, intern = T)
    if(!is.null(attr(fred_csv_st, "status"))){
        ##system(sprintf("rm -rf %s", tmp_out), intern = T)
        return(FALSE)
    }

    job_df = read.csv(text = fred_csv_st, stringsAsFactors = FALSE)
    
    print(sprintf("Processing job %s", job_id))

    ##========================================#
    ## Age stuff-------------
    ##========================================#
    age_df = read.csv(file.path(data_dir,'AgeGroupsData_mean.csv'), stringsAsFactors = F)    
    cols_age = colnames(age_df %>% dplyr::select(-Day))

    job_df = dplyr::left_join(job_df, age_df, by = "Day")        
    job_df$Date = as.Date('2020-01-01') + job_df$Day
    
    cols_output = c('Day', 'Week', 'Year', 'Runs', 'N_mean', 'N_std', 'PrevInf_mean',
                    'C_mean', 'C_std', 'Cs_mean', 'Cs_std','CFR_mean', 'Is_mean', 'I_mean',
                    'CFR_std', 'CF_mean', 'CF_std','Nursing_Home_mean', 
                    'Nursing_Home_CF_mean', 'Wrk_mean', 'Sch_mean', 'H_mean', 'Nbr_mean',
                    'RR_mean', 'RR_std', 'AR_mean', 'AR_std',
                    'H_sheltering_mean', 'N_sheltering_mean')

    cols_output = c(cols_output, cols_age)        
    
    return(list(job_df = job_df[,cols_output]))
    
}

##========================================#
## Collect for covid --------------------
##========================================#
state_input = 11001
asymp_infectivity_in = 1.0
face_mask_transmission_efficacy_in = 0.73
kids_susceptibility_in = 10
args = (commandArgs(TRUE))
if(length(args) >= 1){
    state_input = as.numeric(args[1])
    if(length(args) >= 2){
        asymp_infectivity_in = as.numeric(args[2])
        if(length(args) >= 3){
            face_mask_transmission_efficacy_in = as.numeric(args[3])
            if(length(args) >= 4){
                kids_susceptibility_in = as.numeric(args[4])
            }
        }    
    }
}

scratch_dir = Sys.getenv('scratch_dir')
simdir_name = sprintf('FRED_%d_postcalibration_asymp_%.2f_fm_%.2f_ksus_%.2f_mov',state_input, asymp_infectivity_in,face_mask_transmission_efficacy_in, kids_susceptibility_in)
simdir = file.path(scratch_dir, simdir_name)

if(!dir.exists(file.path(getwd(), 'output','SHORT_FORECAST'))){
    dir.create(file.path(getwd(), 'output','SHORT_FORECAST'), recursive = T)
}

fred_results_dir = file.path(simdir,"FRED_RESULTS")
Sys.setenv(FRED_RESULTS=fred_results_dir)

output_dir = file.path(getwd(), 'output','SHORT_FORECAST',sprintf("%s_%s", simdir_name, "out"))
fred_outputs = 'fred_output.csv'
params_file = sprintf("%s/%s",simdir, "FRED_parameters.csv")

fredtools::fred_gather_data(params=params_file, outdir=output_dir, outfile=fred_outputs,
                            FUN=check_finished_jobs, FUN2=process_job_data, rm_out = TRUE, appendToFile = FALSE)    
