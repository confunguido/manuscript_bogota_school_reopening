##==============================================#
## Author: Guido Espana
## Simulate COVID-19 in WA
## Year: 2019
##==============================================#
## User's input----------------
##==============================================#
library(pomp)
library(lubridate)
library(tidyverse)
library(fredtools)

## TODO:
## [x] Add post-processing
## [x] Add scenarios of school re-opening
## [x] Add facemasks
## [x] Nursing homes
## [x] Susceptibility by age
## [x] Asymptomatic infectivity
## [X] Add 19 simulation scenarios
## - [x] School FM compliance: low, medium, high
## - [x] Capacity: 1.0, 0.75, 0.5
##==============================================#
## Custom functions-------------
##==============================================#
write_cmd_function <- function(scalars_in, tmpfile){
    ## Delte jobs!!!
    cat("Deleting jobs\n")
    if(dir.exists(file.path(Sys.getenv('FRED_RESULTS'),'JOB'))){
        for(s in 1:nrow(scalars_in)){
            system(sprintf("fred_delete -f -k %s",scalars_in$job_id[s]), intern = T)
        }
        cat("All jobs deleted\n")
    }else{
        cat("FRED RESULTS doesn't exist, nothing to delete",Sys.getenv('FRED_RESULTS'),"\n")
    }
    job_cmd_str = sprintf("fred_job -k %s -I %d -p %s -n %.0f; Rscript ./post_process_fred_forecast.R %s %.0f",
                          scalars_in$job_id,
                          scalars_in$run_id,
                          scalars_in$params_file,
                          scalars_in$reps,
                          scalars_in$job_id,
                          scalars_in$reps
                          )
    fileConn<-file(tmpfile)
    writeLines(job_cmd_str, fileConn)
    close(fileConn)
}

##==============================================#
## Set STATE-------------------
##==============================================#
state_code = 11001
reps = 2
reps_per_job = 1
forecast_date = "2020-12-31"
asymp_infectivity_in = 1.0
face_mask_transmission_efficacy_in = 0.3
kids_susceptibility_in = 1.0
args = (commandArgs(TRUE))
if(length(args) >= 1){
    state_name_input = args[1]
    if(length(args) >= 2){
        reps = as.numeric(args[2])
        if(length(args) >= 3){
            reps_per_job = as.numeric(args[3])
            if(length(args) >=4){
                forecast_date = args[4]
                if(length(args) >= 5){
                    asymp_infectivity_in = as.numeric(args[5])
                    if(length(args) >= 6){
                        face_mask_transmission_efficacy_in = as.numeric(args[6])
                        if(length(args) >= 7){
                            kids_susceptibility_in = as.numeric(args[7])
                        }
                    }
                }
            }
        }
    }
}
forecast_date = as.Date(forecast_date)

##==============================================#
## FRED setup-------------
##==============================================#
fred_home = Sys.getenv('FRED_HOME')
scratch_dir = Sys.getenv('scratch_dir')

fred_defaults = sprintf("%s/input_files/defaults", fred_home)

output.dir = file.path(scratch_dir, sprintf('FRED_%.0f_school_forecast_asymp_%.2f_fm_%.2f_ksus_%.2f_mov',state_code, asymp_infectivity_in, face_mask_transmission_efficacy_in, kids_susceptibility_in))

fred_results_dir = file.path(output.dir,"FRED_RESULTS")
Sys.setenv(FRED_RESULTS=fred_results_dir)

if(file.exists(output.dir)){
    system(paste('rm -rf ', output.dir,sep = ''))
}
system(paste('mkdir -p ', output.dir,sep = ''))

file.copy('./scripts/post_process_fred_forecast.R',output.dir)
file.copy('../input_files/params_covid.txt','./input_files/params_covid.txt', overwrite = T)
file.copy("./input_files/infection_hospitalization_risk.csv", output.dir)
file.copy('./input_files/Localidad_Unidad_Catastral.csv',output.dir, overwrite = T)

##==============================================#
## Sweep fixed parameters-------------------
##==============================================#
start_date = '2020-01-01' # YYYY-MM-DD
numDays = as.numeric(forecast_date - as.Date(start_date))
track_infection_events_in = 4 # No need to track infections in the calibrations right now
isolation_rate = 0.05175 # Specific for the US
report_place_of_infection_in = 1 # We might want to track place of infection
track_fatality_events_in = 1 
report_age_of_infection_in = 4 # Track individual ages of infections
report_incidence_by_county_in = 0 # We don't need this for the calibration either
shelter_in_place_students = 0 # Don't shelter students, use school closure for that
report_place_of_infection_in = 1 # We might want to track place of infection
synthetic_population_id = sprintf('synthetic_population_id = colombia_%d', state_code)

advance_seeding = 'exposed'
school_vacation_end = as.Date('2020-12-31')

## Shelter in place
interventions_st_df = read_csv('./input_files/interventions_Colombia.csv')
enable_shelter_in_place_in = 1

enable_shelter_in_place_timeseries_in = 1
shelter_in_place_delay_mean_in = as.integer(interventions_st_df$Shelter_in_place[interventions_st_df$State == state_code] - as.Date(start_date)) ## Just for book-keeping. The parameter should not have an effect in shelter timeseries

## School closure
early_school_vacation_end = as.Date('2020-09-28')
early_closure_duration = as.integer(early_school_vacation_end - interventions_st_df$School_closure[interventions_st_df$State == state_code])
early_school_reopening_day = as.numeric(early_school_vacation_end - as.Date(start_date))

school_closure_policy_in = 'global'
school_closure_day_in = as.integer(interventions_st_df$School_closure[interventions_st_df$State == state_code] - as.Date(start_date))
school_closure_duration_in = as.integer(school_vacation_end - interventions_st_df$School_closure[interventions_st_df$State == state_code])

## Nursing home importations
enable_nursing_homes_importations_in = 0

## Age specific susceptibility
susceptibility_params = read_csv('./input_files/age_susceptibility_fit.csv')
enable_age_specific_susceptibility_in = 1
influenza_susceptibility_by_age_rate_in = susceptibility_params$rate_in[1]
influenza_susceptibility_by_age_cutoff_in = susceptibility_params$cutoff[1]

## Facemask usage
enable_face_mask_timeseries_in = 1
enable_face_mask_usage_in = 1


## Reduced capacity
enable_school_reduced_capacity_in = 1
school_reduced_capacity_in = 1.0
school_reduced_capacity_day_in =  as.integer(early_school_vacation_end - as.Date(start_date))

##==============================================#
## Filter and sample particles--------------
##==============================================#
state_code_in = state_code
## 1. Read parameters with LL
calibration_simdir = sprintf('FRED_%.0f_calibration_asymp_%.2f_fm_%.2f_ksus_%.2f_mov',state_code, asymp_infectivity_in,face_mask_transmission_efficacy_in,kids_susceptibility_in)

calibration_dir = file.path(getwd(), 'output','CALIBRATION',sprintf("%s_%s", calibration_simdir, "out"))
params_df = read_csv(file.path(calibration_dir, 'FRED_parameters_out.csv'))
fred_sweep_df = read_csv(file.path(calibration_dir, 'fred_output.csv'))
params_sweep_ll =  fred_sweep_df %>%
    group_by(job_id) %>% summarize(LL_total = LL[1]) %>% ungroup() %>%
    left_join(params_df, by = c("job_id" = "job_id")) %>%
    filter(state_code == state_code_in)

## 2. Sample based on their likelihood to ensure 10% sampled
particles_w = 0.001
prob_array = exp(-params_sweep_ll$LL_total*particles_w)
if(sum(prob_array) > 0){
    indx_sampled = sample.int(n = length(prob_array), size = reps, prob = prob_array, replace = T)
    n_sampled = length(unique(indx_sampled))
}
print(n_sampled)
scalars_sampled = params_sweep_ll[indx_sampled,] %>%
    dplyr::select(imports_factor, influenza_transmissibility,
                  shelter_in_place_compliance,shelter_in_place_age_compliance, facemask_compliance,                  
                  influenza_susceptibility_by_age_offset,influenza_susceptibility_by_age_rate,
                  influenza_susceptibility_by_age_cutoff,influenza_susceptibility_by_age_high)

scalars_sobol_df = sobolDesign(
    lower = c(seed=1),
    upper = c(seed=as.integer(Sys.time())),
    reps)

scalars_sobol_df = bind_cols(scalars_sobol_df, scalars_sampled)

##==============================================#
## Initial conditions-------------------
##==============================================#
initial_inf_file = sprintf('input_files/%d_imports_alternative.csv', state_code)
primary_cases_file_in = file.path(output.dir, sprintf('initial_cases_%d_%d.txt',state_code,1:reps))

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
post_lockdown_mobility_in = 0.1
shelter_time_file = file.path(output.dir, sprintf('shelter_timeseries_%s_%d.txt',state_code,1:reps))

## Moving away from google's data to Grandata census-tract specific
## shelter_timeseries_df = read_csv('./input_files/interventions_covid_timevarying_shelter.csv')
shelter_timeseries_df = read_csv('./input_files/11001_mobility_trends.csv')
shelter_timeseries_df$day = as.numeric(difftime(shelter_timeseries_df$date, as.Date(start_date), units='days'))

for(nn in 1:reps){
    ## For now, choose the mean
    tmp_df = shelter_timeseries_df %>% filter(State == state_code, replicate == 1) %>%
        mutate(shelter_trend = shelter_trend * scalars_sobol_df$shelter_in_place_compliance[nn])
    max_tmp_day = max(tmp_df$day)
    tmp_df_tail = tmp_df[tmp_df$day == max_tmp_day,]
    tmp_df_tail$day = max_tmp_day + 1
    tmp_df_tail$shelter_trend = post_lockdown_mobility_in * tmp_df_tail$shelter_trend
        
    tmp_df$shelter_trend[tmp_df$shelter_trend < 0.0001] = 0.0
    tmp_df$shelter_trend[tmp_df$shelter_trend > 1.0] = 1.0
    
    shelter_time_lines = sprintf('%.0f %.0f %.4f 0 120 %d%s', tmp_df$day, tmp_df$day, tmp_df$shelter_trend, tmp_df$State, tmp_df$SCACODIGO)

    shelter_time_tail_lines = sprintf('%.0f %.0f %.4f 0 120 %d%s', tmp_df_tail$day, numDays, tmp_df_tail$shelter_trend, tmp_df_tail$State, tmp_df_tail$SCACODIGO)

    ## Add shelter by age
    shelter_age_lines = sprintf("82 212 %.4f 60 120", scalars_sobol_df$shelter_in_place_age_compliance[nn])
    fileConn<-file(shelter_time_file[nn])
    writeLines(c(shelter_time_lines, shelter_time_tail_lines, shelter_age_lines), fileConn)
    close(fileConn)
}
##==============================================#
## Facemask compliance-------------------
##==============================================#
facemask_high_file = file.path(output.dir, sprintf('facemask_compliance_high_%d_%d.txt',state_code,1:reps))
facemask_med_file = file.path(output.dir, sprintf('facemask_compliance_med_%d_%d.txt',state_code,1:reps))
facemask_low_file = file.path(output.dir, sprintf('facemask_compliance_low_%d_%d.txt',state_code,1:reps))
facemask_school_null_file = file.path(output.dir, sprintf('facemask_compliance_school_null_%d_%d.txt',state_code,1:reps))

facemask_timeseries_df = read_csv('./input_files/facemask_timeseries_compliance.csv') %>%
    dplyr::select(-Day)
facemask_timeseries_df$day = as.numeric(difftime(facemask_timeseries_df$Date, as.Date(start_date), units='days'))

for(nn in 1:reps){
    ## FM compliance stays at the last value in the data
    tmp_df = facemask_timeseries_df %>% filter(State == tolower(interventions_st_df$state_name[interventions_st_df$State == state_code])) %>%
        mutate(FacemaskTrends = FacemaskTrends * scalars_sobol_df$facemask_compliance[nn]) %>%
        dplyr::select(day, FacemaskTrends)
    
    if(max(tmp_df$day) < numDays){
        tmp_df2 = data.frame(day = seq(from=max(tmp_df$day) + 1, to = numDays),
                             FacemaskTrends = tail(tmp_df$FacemaskTrends,1), stringsAsFactors = F)
        tmp_df = bind_rows(tmp_df, tmp_df2)
    }
    
    compliance_time_lines = c()
    for(loc in c("workplace", "office", "other")){
        compliance_time_lines = c(compliance_time_lines,
                                  sprintf('%.0f %.0f %.4f %s', tmp_df$day, tmp_df$day, tmp_df$FacemaskTrends, loc)
                                  )
    }

    ## Schools start facemask compliance when they open
    facemask_school_day = as.integer(early_school_vacation_end - as.Date(start_date))
    school_med_lines = c()
    school_low_lines = c()
    school_high_lines = c()
    
    for(loc in c("school", "classroom")){
        school_med_lines = c(school_med_lines, sprintf('%.0f %.0f %.4f %s', facemask_school_day, numDays, 0.75, loc))
        school_high_lines = c(school_high_lines, sprintf('%.0f %.0f %.4f %s', facemask_school_day, numDays, 1.0, loc))
        school_low_lines = c(school_low_lines, sprintf('%.0f %.0f %.4f %s', facemask_school_day, numDays, 0.5, loc))
    }

    school_med_lines = c(compliance_time_lines, school_med_lines)
    school_low_lines = c(compliance_time_lines, school_low_lines)
    school_high_lines = c(compliance_time_lines, school_high_lines)
    school_null_lines = compliance_time_lines
    
    fileConn<-file(facemask_med_file[nn])
    writeLines(school_med_lines, fileConn)
    close(fileConn)

    fileConn<-file(facemask_low_file[nn])
    writeLines(school_low_lines, fileConn)
    close(fileConn)

    fileConn<-file(facemask_high_file[nn])
    writeLines(school_high_lines, fileConn)
    close(fileConn)

    fileConn<-file(facemask_school_null_file[nn])
    writeLines(school_null_lines, fileConn)
    close(fileConn)
}

##==============================================#
## Intervention parameters-------------------
##==============================================#
epidemic_offset = 0
num_demes_in = 1
intervention_base = data.frame(
    enable_shelter_in_place = 1,
    enable_shelter_in_place_timeseries = 1,
    shelter_in_place_delay_mean = as.integer(interventions_st_df$Shelter_in_place[interventions_st_df$State == state_code] - as.Date(start_date)),    
    school_closure_policy = 'global',
    school_closure_day = as.integer(interventions_st_df$School_closure[interventions_st_df$State == state_code] - as.Date(start_date)),
    school_closure_duration = as.integer(school_vacation_end - interventions_st_df$School_closure[interventions_st_df$State == state_code]),
    stringsAsFactors = F)


## ONLY 10 SCENARIOS:
## 1. [x] Keep schools closed
## 3. [x] Re-open in august 17, FM med,low, high
## 3. [x] Re-open in august 17, Cap med, low, high

intervention_df = intervention_base[rep(1,reps),]

intervention_df_closed = intervention_df %>%    
    mutate(seed = floor(scalars_sobol_df$seed),
           primary_cases_file = primary_cases_file_in,
           imports_factor = scalars_sobol_df$imports_factor,
           shelter_in_place_file = shelter_time_file,
           shelter_in_place_compliance = scalars_sobol_df$shelter_in_place_compliance,
           influenza_transmissibility = scalars_sobol_df$influenza_transmissibility,
           school_closure_duration = school_closure_duration_in,
           influenza_susceptibility_by_age_offset = scalars_sobol_df$influenza_susceptibility_by_age_offset,
           influenza_susceptibility_by_age_rate  = scalars_sobol_df$influenza_susceptibility_by_age_rate,
           influenza_susceptibility_by_age_cutoff = scalars_sobol_df$influenza_susceptibility_by_age_cutoff,
           influenza_susceptibility_by_age_high = scalars_sobol_df$influenza_susceptibility_by_age_high,
           face_mask_timeseries_file = facemask_med_file,
           facemask_compliance = scalars_sobol_df$facemask_compliance,
           enable_school_reduced_capacity = enable_school_reduced_capacity_in,
           school_reduced_capacity = school_reduced_capacity_in,
           school_reduced_capacity_day = school_reduced_capacity_day_in,
           enable_face_mask_usage = enable_face_mask_usage_in,
           intervention_name = "SchoolsClosed")

intervention_df_closed_nfm = intervention_df %>%    
    mutate(seed = floor(scalars_sobol_df$seed),
           primary_cases_file = primary_cases_file_in,
           imports_factor = scalars_sobol_df$imports_factor,
           shelter_in_place_file = shelter_time_file,
           shelter_in_place_compliance = scalars_sobol_df$shelter_in_place_compliance,
           influenza_transmissibility = scalars_sobol_df$influenza_transmissibility,
           school_closure_duration = school_closure_duration_in,
           influenza_susceptibility_by_age_offset = scalars_sobol_df$influenza_susceptibility_by_age_offset,
           influenza_susceptibility_by_age_rate  = scalars_sobol_df$influenza_susceptibility_by_age_rate,
           influenza_susceptibility_by_age_cutoff = scalars_sobol_df$influenza_susceptibility_by_age_cutoff,
           influenza_susceptibility_by_age_high = scalars_sobol_df$influenza_susceptibility_by_age_high,
           face_mask_timeseries_file = facemask_med_file,
           facemask_compliance = scalars_sobol_df$facemask_compliance,
           enable_school_reduced_capacity = enable_school_reduced_capacity_in,
           school_reduced_capacity = school_reduced_capacity_in,
           school_reduced_capacity_day = school_reduced_capacity_day_in,
           enable_face_mask_usage = 0,
           intervention_name = "SchoolsClosedNFM")

facemask_files = data.frame(low = facemask_low_file, med = facemask_med_file, high = facemask_high_file, stringsAsFactors = F)
school_capacity_list = c(0.5, 0.75, 1.0)

intervention_df_open_nfm = intervention_df %>%    
            mutate(seed = floor(scalars_sobol_df$seed),
                   primary_cases_file = primary_cases_file_in,
                   imports_factor = scalars_sobol_df$imports_factor,
                   shelter_in_place_file = shelter_time_file,
                   shelter_in_place_compliance = scalars_sobol_df$shelter_in_place_compliance,
                   influenza_transmissibility = scalars_sobol_df$influenza_transmissibility,           
                   school_closure_duration = early_closure_duration,
                   influenza_susceptibility_by_age_offset = scalars_sobol_df$influenza_susceptibility_by_age_offset,
                   influenza_susceptibility_by_age_rate  = scalars_sobol_df$influenza_susceptibility_by_age_rate,
                   influenza_susceptibility_by_age_cutoff = scalars_sobol_df$influenza_susceptibility_by_age_cutoff,
                   influenza_susceptibility_by_age_high = scalars_sobol_df$influenza_susceptibility_by_age_high,
                   face_mask_timeseries_file = facemask_school_null_file,
                   facemask_compliance = scalars_sobol_df$facemask_compliance,
                   enable_school_reduced_capacity = enable_school_reduced_capacity_in,
                   school_reduced_capacity = 1.0,
                   school_reduced_capacity_day = school_reduced_capacity_day_in,
                   enable_face_mask_usage = enable_face_mask_usage_in,
                   intervention_name = "SchoolsOpen_FM0.0_Cap1.0")

scalars_intervention = bind_rows(intervention_df_closed, intervention_df_closed_nfm, intervention_df_open_nfm)

for(ff in 1:ncol(facemask_files)){
    for(cc in 1:length(school_capacity_list)){
        intervention_df_tmp = intervention_df %>%    
            mutate(seed = floor(scalars_sobol_df$seed),
                   primary_cases_file = primary_cases_file_in,
                   imports_factor = scalars_sobol_df$imports_factor,
                   shelter_in_place_file = shelter_time_file,
                   shelter_in_place_compliance = scalars_sobol_df$shelter_in_place_compliance,
                   influenza_transmissibility = scalars_sobol_df$influenza_transmissibility,           
                   school_closure_duration = early_closure_duration,
                   influenza_susceptibility_by_age_offset = scalars_sobol_df$influenza_susceptibility_by_age_offset,
                   influenza_susceptibility_by_age_rate  = scalars_sobol_df$influenza_susceptibility_by_age_rate,
                   influenza_susceptibility_by_age_cutoff = scalars_sobol_df$influenza_susceptibility_by_age_cutoff,
                   influenza_susceptibility_by_age_high = scalars_sobol_df$influenza_susceptibility_by_age_high,
                   face_mask_timeseries_file = facemask_files[,ff],
                   facemask_compliance = scalars_sobol_df$facemask_compliance,
                   enable_school_reduced_capacity = enable_school_reduced_capacity_in,
                   school_reduced_capacity = school_capacity_list[cc],
                   school_reduced_capacity_day = school_reduced_capacity_day_in,
                   enable_face_mask_usage = enable_face_mask_usage_in,
                   intervention_name = sprintf("SchoolsOpen_FM%s_Cap%.2f",colnames(facemask_files)[ff], school_capacity_list[cc]))
        scalars_intervention = bind_rows(scalars_intervention, intervention_df_tmp)
    }
}

##==============================================#
## Create parameters to sweep-----------------
##==============================================#
## 1. Create DF with parameters
enable_age_specific_susceptibility_min_in = 0
influenza_susceptibility_by_age_minage_in = 10
influenza_susceptibility_by_age_minvalue_in = kids_susceptibility_in
if(kids_susceptibility_in < 1.0){
    enable_age_specific_susceptibility_min_in = 1
}

scalars = scalars_intervention %>%
    mutate(days = numDays,
           track_infection_events = track_infection_events_in,
           enable_age_specific_susceptibility = enable_age_specific_susceptibility_in,
           enable_age_specific_susceptibility_min = enable_age_specific_susceptibility_min_in,
           influenza_susceptibility_by_age_minage = influenza_susceptibility_by_age_minage_in,
           influenza_susceptibility_by_age_minvalue = influenza_susceptibility_by_age_minvalue_in,
           influenza_asymp_infectivity = asymp_infectivity_in,
           influenza_face_mask_transmission_efficacy = face_mask_transmission_efficacy_in,
           enable_nursing_homes_importations = enable_nursing_homes_importations_in,
           track_fatality_events = track_fatality_events_in,
           report_age_of_infection = report_age_of_infection_in,
           report_incidence_by_county = report_incidence_by_county_in,
           report_place_of_infection = report_place_of_infection_in,
           isolation_rate = isolation_rate,
           shelter_in_place_students = shelter_in_place_students,
           num_demes = num_demes_in,
           synthetic_population_id = synthetic_population_id,
           advance_seeding = advance_seeding,
           epidemic_offset = epidemic_offset,
           enable_shelter_in_place = enable_shelter_in_place,
           enable_shelter_in_place_timeseries = enable_shelter_in_place_timeseries,
           shelter_in_place_delay_mean = shelter_in_place_delay_mean,           
           enable_face_mask_timeseries = enable_face_mask_timeseries_in,
           start_date = start_date)
                                                              
##===============================================##
## Write the parameters to files---------------
##===============================================##
defaults_params = './input_files/params_covid.txt'
basename_params = sprintf('covid_%d_params',state_code)
basename_jobs = sprintf('FRED_%d_school_forecast_asymp',state_code)
write_fred_parameters(scalars, defaults_params, output.dir,basename.in=basename_params, fred_defaults = fred_defaults)

## print report scalars parameters file with IDs
report_scalars = dplyr::select(
                            scalars, influenza_transmissibility, influenza_asymp_infectivity,
                            influenza_face_mask_transmission_efficacy,
                            enable_age_specific_susceptibility_min,
                            influenza_susceptibility_by_age_minage,
                            influenza_susceptibility_by_age_minvalue,
                            shelter_in_place_compliance, start_date,
                            imports_factor, days, seed, primary_cases_file, school_closure_policy,
                            intervention_name,
                            enable_face_mask_usage, enable_face_mask_timeseries, facemask_compliance,
                            enable_school_reduced_capacity, school_reduced_capacity, school_reduced_capacity_day,
                            influenza_susceptibility_by_age_rate,
                            influenza_susceptibility_by_age_offset,
                            influenza_susceptibility_by_age_cutoff,
                            influenza_susceptibility_by_age_high,
                            school_closure_duration, school_closure_day, shelter_in_place_delay_mean) %>%
    mutate(job_id = sprintf("%s_%d", basename_jobs, row_number()), run_id = row_number(),
           params_file = sprintf('%s_%d.txt',basename_params,row_number()),
           reps = reps_per_job,
           state_code = state_code,
           advance_seeding = advance_seeding,
           epidemic_offset = epidemic_offset
           )

write.csv(report_scalars, file.path(output.dir, 'FRED_parameters.csv'), row.names= F, quote = F)

##===============================================##
## submit to CRC---------------
##===============================================##
## unlink(fred_results_dir,recursive = TRUE)
if(!dir.exists(fred_results_dir)){
    dir.create(fred_results_dir)
}

submit_jobs(experiment_supername_in = sprintf('FRED_BOG_SCHOOL_FORECAST_asymp_%.2f_FM_%.2f_KSUS_%.2f', asymp_infectivity_in, face_mask_transmission_efficacy_in, kids_susceptibility_in),
            experiment_name_in = as.character(state_code),
            experiment_dir_in = output.dir,
            params_base = basename_params,
            job_base = basename_jobs,
            reps = reps_per_job,
            scalars = report_scalars,
            FUN = write_cmd_function,cores_in=6,walltime_in = "1:00:00",
            subsys="UGE",
            fred_home_dir_in=fred_home, fred_results_in=fred_results_dir)

