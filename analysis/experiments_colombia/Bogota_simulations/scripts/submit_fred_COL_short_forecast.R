##==============================================#
## Author: Guido Espana
## Simulate COVID-19 in WA
## Year: 2019
##==============================================#
## User's input----------------
##==============================================#
set.seed(123456)
library(pomp)
library(lubridate)
library(tidyverse)
library(fredtools)

##==============================================#
## Custom functions-------------
##==============================================#
write_cmd_function <- function(scalars_in, tmpfile){
    ## Delete jobs!!!
    cat("Deleting jobs\n")
    if(dir.exists(file.path(Sys.getenv('FRED_RESULTS'),'JOB'))){
        for(s in 1:nrow(scalars_in)){
            system(sprintf("fred_delete -f -k %s",scalars_in$job_id[s]), intern = T)
        }
        cat("All jobs deleted\n")
    }else{
        cat("FRED RESULTS doesn't exist, nothing to delete",Sys.getenv('FRED_RESULTS'),"\n")
    }
    
    job_cmd_str = sprintf("fred_job -k %s -I %d -p %s -n %.0f",
                          scalars_in$job_id,
                          scalars_in$run_id,
                          scalars_in$params_file,
                          scalars_in$reps
                          )
    fileConn<-file(tmpfile)
    writeLines(job_cmd_str, fileConn)
    close(fileConn)
}

##==============================================#
## Set STATE-------------------
##==============================================#
state_code_input = 11001
reps = 100
reps_per_job = 1
forecast_date = as.Date('2020-08-20')
fit_date = as.Date('2020-05-21')
args = (commandArgs(TRUE))
if(length(args) >= 1){
    state_code_input = as.numeric(args[1])
    if(length(args) >= 2){
        reps = as.numeric(args[2])
        if(length(args) >= 3){
            reps_per_job = as.numeric(args[3])
            if(length(args) >= 4 ){
                forecast_date = as.Date(args[4])
                if(length(args) >= 5 ){
                    fit_date = as.Date(args[5])
                }
            }
        }
    }
}

##==============================================#
## FRED setup-------------
##==============================================#
fred_home = Sys.getenv('FRED_HOME')

fred_defaults = sprintf("%s/input_files/defaults", fred_home)
output.dir = file.path(Sys.getenv('scratch_dir'),sprintf('FRED_%d_short_forecast',state_code_input))

if(file.exists(output.dir)){
    system(paste('rm -rf ', output.dir,sep = ''))
}
system(paste('mkdir -p ', output.dir,sep = ''))

file.copy('./scripts/post_process_fred.R',output.dir)
file.copy('../input_files/params_covid.txt','./input_files/params_covid.txt', overwrite = T)

import_files = list.files('../input_files/','*imports.csv', full.names = T)
file.copy(import_files,'./input_files/', overwrite = T)

##==============================================#
## Sweep fixed parameters-------------------
##==============================================#
start_date = '2020-01-01' # YYYY-MM-DD
numDays = as.numeric(forecast_date - as.Date(start_date)) # We don't need many days yet
track_infection_events_in = 0 # No need to track infections in the calibrations right now
synthetic_population_id = sprintf('synthetic_population_id = colombia_%d', state_code_input)
isolation_rate = 0.05175 # Specific for the US
shelter_in_place_students = 0 # Don't shelter students, use school closure for that
advance_seeding = 'exposed'
school_vacation_end = as.Date('2020-08-10')
shelter_in_place_minimum_value = 0.07 # Assuming 7% will stay sheltered in place

interventions_st_df = read_csv('./input_files/interventions_Colombia.csv')

##==============================================#
## Filter and sample particles--------------
##==============================================#
## 1. Read parameters with LL
calibration_simdir = sprintf('FRED_%d_calibration',state_code_input)
calibration_dir = file.path(getwd(), 'output','CALIBRATION',sprintf("%s_%s", calibration_simdir, "out"))
params_df = read_csv(file.path(calibration_dir, 'FRED_parameters_out.csv'))
fred_sweep_df = read_csv(file.path(calibration_dir, 'fred_output.csv'))
params_sweep_ll =  fred_sweep_df %>%
    group_by(job_id) %>% summarize(LL_total = LL[1]) %>% ungroup() %>%
    left_join(params_df, by = c("job_id" = "job_id")) %>%
    filter(state_code == state_code_input)

## 2. Sample based on their likelihood
particles_w = 0.02

prob_array = exp(-params_sweep_ll$LL_total*particles_w)
indx_sampled = sample.int(n = length(prob_array), size = reps, prob = prob_array, replace = T)
n_sampled = length(unique(indx_sampled))
print(n_sampled)

scalars_sampled = params_sweep_ll[indx_sampled,] %>%
    dplyr::select(imports_factor, influenza_transmissibility, shelter_in_place_compliance, influenza_asymp_infectivity)

scalars_sobol_df = sobolDesign(
    lower = c(seed=1),
    upper = c(seed=as.integer(Sys.time())),
    reps)

scalars_sobol_df = bind_cols(scalars_sobol_df, scalars_sampled)


##==============================================#
## Initial conditions-------------------
##==============================================#
initial_inf_file = sprintf('input_files/%d_imports_alternative.csv', state_code_input)
primary_cases_file_in = file.path(output.dir, sprintf('initial_cases_%d_%d.txt',state_code_input,1:reps))

initial_df = read_csv(initial_inf_file)
initial_df$day = as.numeric(difftime(initial_df$Date, as.Date(start_date), units='days'))

## Sample 'reps' from the initial conditions
## replicate_init = sample.int(n=length(unique(initial_df$Replicate)), size = reps, replace=T)

for(nn in 1:reps){
    ## tmp_df = filter(initial_df, Replicate == replicate_init[nn]) %>%
    ##     arrange(day) %>% dplyr::select(Imports, day)

    ## For now, choose the mean
    tmp_df = initial_df %>% group_by(day) %>% summarize(Imports = round(mean(Imports))) %>% ungroup()
    
    tmp_df$Imports = round(tmp_df$Imports * scalars_sobol_df$imports_factor[nn])
    
    ## Now, let's add one case by week after initial cases            
    extra_rows = numDays - nrow(tmp_df)
    weekly_imports = 1
    if(extra_rows > 0){
        week_imp = data.frame(Imports = rep(weekly_imports, floor(extra_rows/7)), stringsAsFactors = F)
        week_imp$day = seq(from=max(tmp_df$day) + 1, by = 7, length.out = nrow(week_imp))
        tmp_df = bind_rows(tmp_df, week_imp)
    }
    
    init_cases_lines = sprintf('%.0f %.0f %.0f', tmp_df$day, tmp_df$day, tmp_df$Imports)
    
    fileConn<-file(primary_cases_file_in[nn])
    writeLines(init_cases_lines, fileConn)
    close(fileConn)
}


##==============================================#
## Shelter in place-------------------
##==============================================#
## 1. Continue current trend
shelter_trend_file = file.path(output.dir, sprintf('shelter_timeseries_Trend_%s_%d.txt',state_code_input,1:reps))
shelter_all_file = file.path(output.dir, sprintf('shelter_timeseries_All_%s_%d.txt',state_code_input,1:reps))
shelter_none_file = file.path(output.dir, sprintf('shelter_timeseries_None_%s_%d.txt',state_code_input,1:reps))
shelter_periodic_file = file.path(output.dir, sprintf('shelter_timeseries_Periodic_%s_%d.txt',state_code_input,1:reps))

shelter_timeseries_df = read_csv('./input_files/interventions_covid_timevarying_shelter.csv')
shelter_timeseries_df$day = as.numeric(difftime(shelter_timeseries_df$date, as.Date(start_date), units='days'))

for(nn in 1:reps){
    ## For now, choose the mean
    min_shelter = scalars_sobol_df$shelter_in_place_compliance[nn] * shelter_in_place_minimum_value
    tmp_df = shelter_timeseries_df %>% filter(State == state_code_input, replicate == 1) %>%
        mutate(shelter_trend = shelter_trend * scalars_sobol_df$shelter_in_place_compliance[nn])
    
    tmp_df$shelter_trend[tmp_df$shelter_trend < min_shelter & tmp_df$date > fit_date] = min_shelter
    fit_date_shelter = tmp_df %>% filter(date == fit_date) %>% pull(shelter_trend)

    max_shelter = max(tmp_df$shelter_trend[tmp_df$date <= fit_date])
    tmp_trend_df =  tmp_df    
    tmp_all_df = tmp_df
    tmp_all_df$shelter_trend[tmp_all_df$date > fit_date] = fit_date_shelter[1]
                   
    tmp_none_df = tmp_df
    tmp_none_df$shelter_trend[tmp_none_df$date > fit_date] = min_shelter

    tmp_periodic_df = tmp_df
    tmp_periodic_df$shelter_trend[tmp_periodic_df$date > fit_date] = rep(c(rep(1, 14), rep(0,14)), length.out = length(tmp_periodic_df$shelter_trend[tmp_periodic_df$date > fit_date])) * max_shelter
    tmp_periodic_df$shelter_trend[tmp_periodic_df$shelter_trend < min_shelter & tmp_periodic_df$date > fit_date] = min_shelter
    
    shelter_lines_trend = sprintf('%.0f %.0f %.4f', tmp_trend_df$day, tmp_trend_df$day, tmp_trend_df$shelter_trend)
    shelter_lines_all = sprintf('%.0f %.0f %.4f', tmp_all_df$day, tmp_all_df$day, tmp_all_df$shelter_trend)
    shelter_lines_none = sprintf('%.0f %.0f %.4f', tmp_none_df$day, tmp_none_df$day, tmp_none_df$shelter_trend)
    shelter_lines_periodic = sprintf('%.0f %.0f %.4f', tmp_periodic_df$day, tmp_periodic_df$day, tmp_periodic_df$shelter_trend)
    
    fileConn<-file(shelter_trend_file[nn])
    writeLines(shelter_lines_trend, fileConn)
    close(fileConn)

    fileConn<-file(shelter_all_file[nn])
    writeLines(shelter_lines_all, fileConn)
    close(fileConn)
    
    fileConn<-file(shelter_none_file[nn])
    writeLines(shelter_lines_none, fileConn)
    close(fileConn)

    
    fileConn<-file(shelter_periodic_file[nn])
    writeLines(shelter_lines_periodic, fileConn)
    close(fileConn)
}

##==============================================#
## fixed parameters-------------------
##==============================================#
## IMPORTANT: The offset delays the epidemic. Does not include added days of imported cases
epidemic_offset = 0
num_demes = 1

late_school_vacation_end = as.Date('2020-12-31')
late_school_duration = as.integer(late_school_vacation_end - interventions_st_df$School_closure[interventions_st_df$State == state_code_input])

print(sprintf("School late duration: %d\n", late_school_duration))
intervention_base = data.frame(
    enable_shelter_in_place = 1,
    enable_shelter_in_place_timeseries = 1,
    shelter_in_place_delay_mean = as.integer(interventions_st_df$Shelter_in_place[interventions_st_df$State == state_code_input] - as.Date(start_date)),    
    school_closure_policy = 'global',
    school_closure_day = as.integer(interventions_st_df$School_closure[interventions_st_df$State == state_code_input] - as.Date(start_date)),
    school_closure_duration = as.integer(school_vacation_end - interventions_st_df$School_closure[interventions_st_df$State == state_code_input]),
    stringsAsFactors = F)


intervention_df = intervention_base[rep(1,reps),]

## Scenarios
## 1. Continue things as they are: Same trend
## 2. Drop to minimum value of shelter: 7% of baseline?
## 3. Shelter retains last value in the data
## 4. Periodic lockdowns

intervention_df_trend = intervention_df %>%
    mutate(seed = floor(scalars_sobol_df$seed),
           primary_cases_file = primary_cases_file_in,
           imports_factor = scalars_sobol_df$imports_factor,
           shelter_in_place_compliance = scalars_sobol_df$shelter_in_place_compliance,
           influenza_transmissibility = scalars_sobol_df$influenza_transmissibility,
           influenza_asymp_infectivity = scalars_sobol_df$influenza_asymp_infectivity,
           shelter_in_place_file = shelter_trend_file,
           intervention_name = "ShelterTrend"
           )

intervention_df_all = intervention_df %>%
    mutate(seed = floor(scalars_sobol_df$seed),
           primary_cases_file = primary_cases_file_in,
           imports_factor = scalars_sobol_df$imports_factor,
           shelter_in_place_compliance = scalars_sobol_df$shelter_in_place_compliance,
           influenza_transmissibility = scalars_sobol_df$influenza_transmissibility,
           influenza_asymp_infectivity = scalars_sobol_df$influenza_asymp_infectivity,
           shelter_in_place_file = shelter_all_file,
           intervention_name = "ShelterAll"
           )

intervention_df_none = intervention_df %>%
    mutate(seed = floor(scalars_sobol_df$seed),
           primary_cases_file = primary_cases_file_in,
           imports_factor = scalars_sobol_df$imports_factor,
           shelter_in_place_compliance = scalars_sobol_df$shelter_in_place_compliance,
           influenza_transmissibility = scalars_sobol_df$influenza_transmissibility,
           influenza_asymp_infectivity = scalars_sobol_df$influenza_asymp_infectivity,
           shelter_in_place_file = shelter_none_file,
           intervention_name = "ShelterNone"
           )

intervention_df_periodic = intervention_df %>%
    mutate(seed = floor(scalars_sobol_df$seed),
           primary_cases_file = primary_cases_file_in,
           imports_factor = scalars_sobol_df$imports_factor,
           shelter_in_place_compliance = scalars_sobol_df$shelter_in_place_compliance,
           influenza_transmissibility = scalars_sobol_df$influenza_transmissibility,
           influenza_asymp_infectivity = scalars_sobol_df$influenza_asymp_infectivity,
           shelter_in_place_file = shelter_periodic_file,
           intervention_name = "ShelterPeriodic"
           )

## Schools remain closed
intervention_df_trend_closed = intervention_df %>%
    mutate(seed = floor(scalars_sobol_df$seed),
           primary_cases_file = primary_cases_file_in,
           imports_factor = scalars_sobol_df$imports_factor,
           shelter_in_place_compliance = scalars_sobol_df$shelter_in_place_compliance,
           influenza_transmissibility = scalars_sobol_df$influenza_transmissibility,
           influenza_asymp_infectivity = scalars_sobol_df$influenza_asymp_infectivity,
           shelter_in_place_file = shelter_trend_file,
           school_closure_duration = late_school_duration,
           intervention_name = "ShelterTrend_Closed"
           )

intervention_df_all_closed = intervention_df %>%
    mutate(seed = floor(scalars_sobol_df$seed),
           primary_cases_file = primary_cases_file_in,
           imports_factor = scalars_sobol_df$imports_factor,
           shelter_in_place_compliance = scalars_sobol_df$shelter_in_place_compliance,
           influenza_transmissibility = scalars_sobol_df$influenza_transmissibility,
           influenza_asymp_infectivity = scalars_sobol_df$influenza_asymp_infectivity,
           shelter_in_place_file = shelter_all_file,
           school_closure_duration = late_school_duration,
           intervention_name = "ShelterAll_Closed"
           )

intervention_df_none_closed = intervention_df %>%
    mutate(seed = floor(scalars_sobol_df$seed),
           primary_cases_file = primary_cases_file_in,
           imports_factor = scalars_sobol_df$imports_factor,
           shelter_in_place_compliance = scalars_sobol_df$shelter_in_place_compliance,
           influenza_transmissibility = scalars_sobol_df$influenza_transmissibility,
           influenza_asymp_infectivity = scalars_sobol_df$influenza_asymp_infectivity,
           shelter_in_place_file = shelter_none_file,
           school_closure_duration = late_school_duration,
           intervention_name = "ShelterNone_Closed"
           )

intervention_df_periodic_closed = intervention_df %>%
    mutate(seed = floor(scalars_sobol_df$seed),
           primary_cases_file = primary_cases_file_in,
           imports_factor = scalars_sobol_df$imports_factor,
           shelter_in_place_compliance = scalars_sobol_df$shelter_in_place_compliance,
           influenza_transmissibility = scalars_sobol_df$influenza_transmissibility,
           influenza_asymp_infectivity = scalars_sobol_df$influenza_asymp_infectivity,
           shelter_in_place_file = shelter_periodic_file,
           school_closure_duration = late_school_duration,
           intervention_name = "ShelterPeriodic_All"
           )

scalars_intervention = bind_rows(intervention_df_trend, intervention_df_all, intervention_df_none, intervention_df_periodic,
                                 intervention_df_trend_closed, intervention_df_all_closed, intervention_df_none_closed, intervention_df_periodic_closed)

##==============================================#
## Create parameters to sweep-----------------
##==============================================#
## 1. Create DF with parameters
scalars = scalars_intervention %>%
    mutate(days = numDays,
           track_infection_events = track_infection_events_in,
           isolation_rate = isolation_rate,
           shelter_in_place_students = shelter_in_place_students,
           num_demes = num_demes,
           synthetic_population_id = synthetic_population_id,
           advance_seeding = advance_seeding,
           epidemic_offset = epidemic_offset,
           start_date = start_date)
                                                              
##===============================================##
## Write the parameters to files---------------
##===============================================##
defaults_params = './input_files/params_covid.txt'
basename_params = sprintf('covid_%s_params',state_code_input)
basename_jobs = sprintf('FRED_%s_short_forecast',state_code_input)
write_fred_parameters(scalars, defaults_params, output.dir,basename.in=basename_params, fred_defaults = fred_defaults)

## print report scalars parameters file with IDs
report_scalars = dplyr::select(scalars, influenza_transmissibility, influenza_asymp_infectivity, shelter_in_place_compliance, enable_shelter_in_place_timeseries,start_date,
                               imports_factor, days, seed, primary_cases_file, school_closure_policy, intervention_name,
                               school_closure_duration, school_closure_day, shelter_in_place_delay_mean) %>%
    mutate(job_id = sprintf("%s_%d", basename_jobs, row_number()),run_id = row_number(),
           params_file = sprintf('%s_%d.txt',basename_params,row_number()),
           reps = reps_per_job,
           state_code = state_code_input,
           advance_seeding = advance_seeding,
           epidemic_offset = epidemic_offset
           )

write.csv(report_scalars, file.path(output.dir, 'FRED_parameters.csv'), row.names= F, quote = F)

##===============================================##
## submit to CRC---------------
##===============================================##

fred_results_dir = file.path(output.dir,"FRED_RESULTS")
Sys.setenv(FRED_RESULTS=fred_results_dir)
if(!dir.exists(fred_results_dir)){
    dir.create(fred_results_dir)
}

submit_jobs(experiment_supername_in = 'FRED_SHORT_FORECAST',
            experiment_name_in = as.character(state_code_input),
            experiment_dir_in = output.dir,
            params_base = basename_params,
            job_base = basename_jobs,
            reps = reps_per_job,
            scalars = report_scalars,
            FUN = write_cmd_function,cores_in=5,walltime_in = "2:00:00",
            subsys="UGE",
            fred_home_dir_in=fred_home, fred_results_in=fred_results_dir)

