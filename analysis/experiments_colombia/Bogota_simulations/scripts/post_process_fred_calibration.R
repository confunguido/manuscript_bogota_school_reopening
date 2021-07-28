#!/usr/bin/env Rscript
##=======================================#
## Author: Guido Espana
## Post-processes the output of FRED jobs
##=======================================#
## Setup---------------
##=======================================#
library(tidyverse)
library(fredtools)

fred_results_dir = file.path(getwd(), "FRED_RESULTS")
Sys.setenv(FRED_RESULTS=fred_results_dir)

fred_key = "FRED_11001_calibration_1"
fred_n = 1
    
args = (commandArgs(TRUE))
if(length(args) >= 1){
    fred_key = args[1]
    if(length(args) >= 2){
        fred_n = as.numeric(args[2])
    }
}

cols_output = c('Date','WkDay','Day','N','Year','Week','C','Cs','CFR','Is',
                'I', 'CF', 'E', 'PrevInf', 'Nursing_Home', 'Nursing_Home_CF',
                'RR', 'AR', 'H_sheltering', 'N_sheltering')

##========================================#
## Functions----------------
##========================================#
post_process_age <- function(fred_key, brk_ages_ar = c(0,12,40,60,120),
                             hosp_parameters = NULL){
    fred_dir = system(sprintf("fred_find -k %s", fred_key), intern = TRUE)
    data_dir = file.path(fred_dir,
                         "DATA", "OUT")
    fred_csv = sprintf("fred_csv -k %s",fred_key)
    fred_csv_st = system(fred_csv, intern = T)
    if(!is.null(attr(fred_csv_st, "status"))){     
        stop(sprintf("FRED output does not exist %s", fred_key))
    }

    job_df = read.csv(text = fred_csv_st, stringsAsFactors = FALSE)
        
    print(sprintf("Processing job %s", fred_key))

    brk_ages_cs = sort(unique(c(hosp_parameters$MinAge, hosp_parameters$MaxAge)))
    brk_lbls = sprintf("A%d_%d", brk_ages_ar[-length(brk_ages_ar)], brk_ages_ar[-1])        
    brk_lbls_cs = sprintf("ACs%d_%d", brk_ages_cs[-length(brk_ages_cs)], brk_ages_cs[-1])
    
    age_fred_N = job_df %>%
        dplyr::select(Day, matches('^AgeN[0-9]+_mean')) %>%
        gather(key = Age, value = N, -Day) %>%
        mutate(Age = as.numeric(str_replace(Age, "AgeN(.*)_mean", "\\1")))%>%
        mutate(AgeGroup = as.character(cut(Age, brk_ages_ar,brk_lbls, include.lowest = T, right = F))) %>%
        group_by(Day, AgeGroup) %>%
        summarize(N = sum(N, na.rm = T)) %>%
        ungroup()     
    
    age_fred_df = job_df %>%
        dplyr::select(Day, matches('^A[0-9]+_mean')) %>%
        gather(key = Age, value = C, -Day) %>%
        mutate(Age = as.numeric(str_replace(Age, "A(.*)_mean", "\\1"))) %>%
        mutate(AgeGroup = as.character(cut(Age, brk_ages_ar,brk_lbls, include.lowest = T, right = F))) %>%
        group_by(Day, AgeGroup) %>%
        summarize(C = sum(C)) %>%
        ungroup() 
    
    age_fred_ar = job_df %>%
        dplyr::select(Day, matches('^Age[0-9]+_mean')) %>%
        gather(key = Age, value = IAR, -Day) %>%
        mutate(Age = as.numeric(str_replace(Age, "Age(.*)_mean", "\\1")),
               IAR = IAR / 100000) %>%
        mutate(AgeGroup = as.character(cut(Age, brk_ages_ar,brk_lbls, include.lowest = T, right = F))) %>%
        group_by(Day, AgeGroup) %>%
        summarize(IAR = sum(IAR)) %>%
        ungroup()    


    ## IAR stuff
    C_age_df = age_fred_df  %>% 
        group_by(Day) %>% spread(key = AgeGroup, value = C) %>%
        ungroup()
    
    AR_age_df = age_fred_ar  %>%
        mutate(AgeGroup = sprintf("AR%s", AgeGroup)) %>%
        group_by(Day) %>% spread(key = AgeGroup, value = IAR) %>% ungroup()
    
    ## Symptomatics by age and hospitalized by age
    age_fred_df_cs = job_df %>%
        dplyr::select(Day, matches('^ACs[0-9]+_mean')) %>%
        gather(key = Age, value = Cs, -Day) %>%
        mutate(Age = as.numeric(str_replace(Age, "ACs(.*)_mean", "\\1")))%>%
        mutate(AgeGroup = as.character(cut(Age, brk_ages_cs,brk_lbls_cs, include.lowest = T, right = F))) %>%
        group_by(Day, AgeGroup) %>%
        summarize(Cs = sum(Cs)) %>%
        ungroup()    

    Cs_age_df = age_fred_df_cs  %>% 
        group_by(Day) %>% spread(key = AgeGroup, value = Cs) %>%ungroup()
    
    output_age_df = left_join(C_age_df, AR_age_df, by = "Day") %>%
        left_join(Cs_age_df, by = "Day") %>% arrange(Day)
    
    
    if(nrow(hosp_parameters) > 0){
        hosp_parameters$AgeGroup = sprintf("ACs%s", hosp_parameters$Age)
        hosp_age_df = age_fred_df_cs %>%
            left_join(dplyr::select(hosp_parameters, c(Age,SHR, AgeGroup)),
                      by = c("AgeGroup" = "AgeGroup")) %>%
            mutate(AgeGroup = sprintf("AHosp%s", Age))
        hosp_tidy = hosp_age_df %>% dplyr::select(Day,AgeGroup) %>% mutate(Hosp = 0)
        
        age_groups = unique(hosp_age_df$AgeGroup)
        for(ag in 1:length(age_groups)){
            age_group = age_groups[ag]
            hosp_tmp = filter(hosp_age_df, AgeGroup == age_group) 
            num_to_hosp = rbinom(nrow(hosp_tmp), floor(hosp_tmp$Cs+0.5), hosp_tmp$SHR)
            if(sum(num_to_hosp) > 0){
                incidence_days = rep(hosp_tmp$Day, num_to_hosp)
                hosp_days = incidence_days + rpois(length(incidence_days), hosp_parameters$hosp_time[1])
                hosp_freq = table(unlist(lapply(hosp_days, function(x){seq(from=x,by = 1, length.out = rpois(1,hosp_parameters$hosp_stay[1]))})))
                hosp_tmp_df = data.frame(Day = as.numeric(names(hosp_freq)), AgeGroup = age_group, Hosp = as.integer(hosp_freq), stringsAsFactors = F)
                hosp_tmp = hosp_tmp %>% left_join(hosp_tmp_df, by = c("Day", "AgeGroup")) %>%
                    replace_na(list(Hosp = 0))
            }else{
                hosp_tmp$Hosp = 0
            }
            hosp_tidy$Day[(nrow(hosp_tmp) * (ag - 1) + 1):(nrow(hosp_tmp)*ag)] = hosp_tmp$Day
            hosp_tidy$AgeGroup[(nrow(hosp_tmp) * (ag - 1) + 1):(nrow(hosp_tmp)*ag)] = hosp_tmp$AgeGroup
            hosp_tidy$Hosp[(nrow(hosp_tmp) * (ag - 1) + 1):(nrow(hosp_tmp) * ag)] = hosp_tmp$Hosp
            
        }
        
        total_hosp_df = hosp_tidy %>% group_by(Day) %>%
            summarize(Hospitalized_mean = sum(Hosp, na.rm = T)) %>% ungroup()
        output_age_df = output_age_df %>% left_join(total_hosp_df, by = "Day")        
    }
    return(output_age_df)
}

get_infections_df <- function(fred_key, fred_n){
    data_dir = file.path(system(sprintf("fred_find -k %s", fred_key), intern = TRUE),
                         "DATA", "OUT")        
    params_file = file.path(system(sprintf("fred_find -k %s", fred_key), intern = TRUE),
                            "META", "PARAMS")
    print("reading file")
    conn_inf = file(file.path(data_dir, sprintf("infections%d.txt",fred_n)), "r")
    inf_lines = readLines(conn_inf)
    close(conn_inf)

    if(length(inf_lines) == 0){
        return(list(periods = tibble(), intervals = tibble()))
    }
    cols_req = c('day', 'host','age','infector','host_census_tract')
    cols_vec = c('inf','symp')
    names_periods = c('inf1', 'inf2','symp1','symp2')
    
    
    ## Only simulate MSA areas with all counties in pop
    infections = data.frame(stringsAsFactors=F)
    print("parsing file")
    inf_list_tmp = str_split(inf_lines[1], pattern="\\s+")[[1]]
    names_ind = which(inf_list_tmp %in% cols_req) + 1
    names_vec = rep(which(inf_list_tmp %in% cols_vec),each=2) + rep(c(1,2),2)

    infections = sapply(1:length(inf_lines), function(x){
        inf_list = str_split(inf_lines[x], pattern="\\s+")[[1]]
        ##names_ind = which(inf_list %in% cols_req) + 1
        ##names_vec = rep(which(inf_list %in% cols_vec),each=2) + rep(c(1,2),2)
        tmp_df = inf_list[c(names_ind,names_vec)]
        return(tmp_df)
    })
    infections = t(infections)
    infections = as.data.frame(infections, stringsAsFactors = F)
    colnames(infections) = c(cols_req, names_periods)
            
    infections_df = infections[,c(cols_req,names_periods)]
    infections_df$day = as.numeric(infections_df$day)
    infections_df$age = as.numeric(infections_df$age)
    return(infections_df)
}
get_infections_CF_df <- function(fred_key, fred_n){
    ## calculate infectious period
    data_dir = file.path(system(sprintf("fred_find -k %s", fred_key), intern = TRUE),
                         "DATA", "OUT")        
    
    cols_req = c('day', 'host', 'age', 'exp', 'inf', 'symp','dead')

    
    conn_inf = file(file.path(data_dir, sprintf("infectionsCF%d.txt",fred_n)), "r")
    inf_lines = readLines(conn_inf)
    close(conn_inf)

    if(length(inf_lines) == 0){
        return(tibble())
    }
    inf_list_tmp = str_split(inf_lines[1], pattern="\\s+")[[1]]
    names_ind = which(inf_list_tmp %in% cols_req) + 1

    infections_cf = data.frame(stringsAsFactors=F)
    infections_cf = sapply(1:length(inf_lines), function(x){
        inf_list = str_split(inf_lines[x], pattern="\\s+")[[1]]
        tmp_df = inf_list[c(names_ind)]
        return(tmp_df)
    })
    infections_cf = t(infections_cf)
    names(infections_cf) = c(cols_req)
    infections_cf = as.data.frame(infections_cf, stringsAsFactors = F)
    colnames(infections_cf) = c(cols_req)    
    
    infections_df = infections_cf[,cols_req]
    infections_df$day = as.numeric(infections_df$day)
    infections_df$age = floor(as.numeric(infections_df$age))
    infections_df$exp = as.numeric(infections_df$exp)
    infections_df$inf = as.numeric(infections_df$inf)
    infections_df$symp = as.numeric(infections_df$symp)
    infections_df$dead = as.numeric(infections_df$dead)
    
    return(infections_df)
}

process_census_tract <- function(fred_key, fred_n,loc_census_df){
    infections_df = get_infections_df(fred_key, fred_n)
    fatalities_df = get_infections_CF_df(fred_key, fred_n)
    fatalities_df = fatalities_df %>% 
    left_join(infections_df[infections_df$host %in% fatalities_df$host,c('host','host_census_tract')], by = c("host" = "host"))
    infections_localidad = infections_df %>% group_by(day, host_census_tract) %>% summarize(CensusTractC = n()) %>%
        ungroup() %>%
        filter(host_census_tract != "-1") %>%
        left_join(loc_census_df, by = c("host_census_tract" = "census_tract")) %>%
        group_by(day, Localidad) %>%
        summarize(LocalidadCases = sum(CensusTractC, na.rm = T)) %>%
        ungroup() %>%
        right_join(
            expand.grid(day = 1:max(infections_df$day), Localidad = 1:max(loc_census_df$Localidad), stringsAsFactors = F),
            by = c("day","Localidad")) %>%
        replace_na(list(LocalidadCases = 0)) %>%
        mutate(Localidad = sprintf("CasesLoc_%d",Localidad)) %>%
        spread(key = Localidad, value = LocalidadCases)
                   
    fatalities_localidad = fatalities_df %>%
        group_by(day, host_census_tract) %>% summarize(CF = n()) %>%
        ungroup() %>%
        filter(host_census_tract != "-1") %>%
        left_join(loc_census_df, by = c("host_census_tract" = "census_tract")) %>%
        group_by(day, Localidad) %>%
        summarize(LocalidadCF = sum(CF, na.rm = T)) %>%
        ungroup() %>%
        right_join(
            expand.grid(day = 1:max(infections_df$day), Localidad = 1:max(loc_census_df$Localidad), stringsAsFactors = F),
            by = c("day","Localidad")) %>%
        replace_na(list(LocalidadCF = 0)) %>%
        mutate(Localidad = sprintf("CFLoc_%d",Localidad)) %>%
        spread(key = Localidad, value = LocalidadCF)
    census_tract_df = left_join(infections_localidad, fatalities_localidad, by = c("day")) %>%
        rename(Day = day)
    return(census_tract_df)
}
##=======================================#
## calculate age outcomes---------------
##=======================================#
hosp_params = read_csv('infection_hospitalization_risk.csv') %>%
    mutate(SHR = IHR / P_symp, hosp_time = 3.5, hosp_stay = 10) %>%
    separate(Age, c("MinAge", "MaxAge"), sep = '_',remove = F) %>%
    mutate(MinAge = as.numeric(MinAge), MaxAge = as.numeric(MaxAge))

brk_ages = c(0, seq(from=10,by = 10, to = 80), 120)
ages_cf_df = fredtools::calculate_CF_age(fred_key, fred_n, brk_ages_cf_in = brk_ages, save_file = F)
ages_cs_df = post_process_age(fred_key, brk_ages_ar = c(0,12,40,60,120), hosp_parameters = hosp_params)

localidad_census_df = read_csv('./Localidad_Unidad_Catastral.csv') %>%
    mutate(census_tract = sprintf("11001%s", SCACODIGO)) 
census_tract_df = process_census_tract(fred_key, fred_n, localidad_census_df)

data_dir = file.path(system(sprintf("fred_find -k %s", fred_key), intern = TRUE),"DATA")

out_age_file = file.path(system(sprintf("fred_find -k %s", fred_key), intern = TRUE),
                         "DATA", "OUT", "AgeGroupsData_mean.csv")
output_data = left_join(ages_cs_df, ages_cf_df, by = "Day") %>%
    left_join(census_tract_df, by = "Day")
output_data[is.na(output_data)] = 0

write_csv(output_data, path = out_age_file)

## Trim variables and output data for job_Df
table_dir = file.path(data_dir, "TABLES")
vars_file = file.path(table_dir, "VARS")
vars_str = paste(cols_output, collapse="\n")

## Remove unnecessary files
fred_tables = list.files(path=table_dir)
fred_tables_indx = c(which(fred_tables %in% sprintf("%s.txt",cols_output)),which(fred_tables %in% sprintf("Weekly_%s.txt",cols_output)))

for(nn in 1:fred_n){
    fred_tables_indx = c(fred_tables_indx, which(fred_tables %in% sprintf("%s-1.txt",cols_output)))
    fred_tables_indx = c(fred_tables_indx, which(fred_tables %in% sprintf("Weekly_%s-%d.txt",cols_output, nn)))
    fred_tables_indx = c(fred_tables_indx, which(fred_tables %in% sprintf("Weekly-%d.txt", nn)))    
}

unlink(file.path(table_dir,fred_tables[-fred_tables_indx]))

fileConn<-file(vars_file)
writeLines(vars_str, fileConn)
close(fileConn)

for(nn in 1:fred_n){
    fatalities_file = file.path(system(sprintf("fred_find -k %s", fred_key), intern = TRUE),
                                "DATA", "OUT", sprintf("infectionsCF%d.txt",nn))
    infections_file = file.path(system(sprintf("fred_find -k %s", fred_key), intern = TRUE),
                                "DATA", "OUT", sprintf("infections%d.txt",nn))
    unlink(infections_file)
    unlink(fatalities_file)    
}




