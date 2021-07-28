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

## TODO:
## [x] Add post-processing
## [x] Add scenarios of school re-opening
## [x] Add facemasks
## [x] Nursing homes
## [x] Susceptibility by age
## [x] Asymptomatic infectivity
## [x] School reopen by grade
## [x] School reopen by income of school
## [x] Winter vacation
## [ ] Distinction of private vs public
## [ ] Change scenarios
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
forecast_date = "2021-07-31"
asymp_infectivity_in = 1.0
face_mask_transmission_efficacy_in = 0.73
kids_susceptibility_age_in = 10
mov_factor_in = 1.0

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
                            kids_susceptibility_age_in = as.numeric(args[7])
                            if(length(args) >= 8){
                                mov_factor_in = as.numeric(args[8])
                            }
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

output.dir = file.path(scratch_dir, sprintf('FRED_%.0f_school_psa_asymp_%.2f_fm_%.2f_ksus_%.2f_mov_%.2f',state_code, asymp_infectivity_in, face_mask_transmission_efficacy_in, kids_susceptibility_age_in,mov_factor_in))

fred_results_dir = file.path(output.dir,"FRED_RESULTS")
Sys.setenv(FRED_RESULTS=fred_results_dir)

if(file.exists(output.dir)){
    system(paste('rm -rf ', output.dir,sep = ''))
}
system(paste('mkdir -p ', output.dir,sep = ''))

file.copy('./scripts/post_process_fred_forecast.R',output.dir)
file.copy('../input_files/params_covid.txt','./input_files/params_covid.txt', overwrite = T)
file.copy('../input_files/11001_schools_open_gps.csv','./input_files/11001_schools_open_gps.csv', overwrite = T)
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
school_vacation_end = as.Date('2021-01-25')

## Shelter in place
interventions_st_df = read_csv('./input_files/interventions_Colombia.csv')
enable_shelter_in_place_in = 1

enable_shelter_in_place_timeseries_in = 1
shelter_in_place_delay_mean_in = as.integer(interventions_st_df$Shelter_in_place[interventions_st_df$State == state_code] - as.Date(start_date)) ## Just for book-keeping. The parameter should not have an effect in shelter timeseries

## School closure: Open in january 25
early_school_vacation_end = as.Date('2021-01-25')
early_closure_duration = as.integer(early_school_vacation_end - interventions_st_df$School_closure[interventions_st_df$State == state_code])
early_school_reopening_day = as.numeric(early_school_vacation_end - as.Date(start_date))

school_closure_policy_in = 'global_schedule'
school_closure_day_in = as.integer(interventions_st_df$School_closure[interventions_st_df$State == state_code] - as.Date(start_date))
school_closure_duration_in = as.integer(school_vacation_end - interventions_st_df$School_closure[interventions_st_df$State == state_code])
school_student_teacher_ratio_in =  18

## Nursing home importations
enable_nursing_homes_importations_in = 1

## Age specific susceptibility
susceptibility_params = read_csv('./input_files/age_susceptibility_fit.csv')
enable_age_specific_susceptibility_in = 1
influenza_susceptibility_by_age_rate_in = susceptibility_params$rate_in[1]
influenza_susceptibility_by_age_cutoff_in = susceptibility_params$cutoff[1]

## Facemask usage
enable_face_mask_timeseries_in = 1
enable_face_mask_usage_in = 1
min_age_face_masks_in = 8

## Reduced capacity
enable_school_reduced_capacity_in = 0
school_reduced_capacity_in = 0.35 ## Current capacity for Bogotá
school_reduced_capacity_day_in =  as.integer(early_school_vacation_end - as.Date(start_date))

## contacts
neighborhood_contacts_in = 0.7883
workplace_contacts_in = 0.0686
office_contacts_in = 0.1372

## School contacts
school_contact_factor_in = 0.9
school_contacts_in = 0.6295
classroom_contacts_in = 1.2590

## Holiday contacts
enable_holiday_contacts_in = 1
holiday_start_in = "11-16"
holiday_end_in = "01-07"

##==============================================#
## Filter and sample particles--------------
##==============================================#
state_code_in = state_code
## 1. Read parameters with LL
calibration_simdir = sprintf('FRED_%.0f_calibration_asymp_%.2f_fm_%.2f_ksus_%.2f_mov',state_code, asymp_infectivity_in,face_mask_transmission_efficacy_in,kids_susceptibility_age_in)

calibration_dir = file.path(getwd(), 'output','CALIBRATION',sprintf("%s_%s", calibration_simdir, "out"))
params_df = read_csv(file.path(calibration_dir, 'FRED_parameters_out.csv'))
fred_sweep_df = read_csv(file.path(calibration_dir, 'fred_output.csv'))
params_sweep_ll =  fred_sweep_df %>%
    group_by(job_id) %>% summarize(LL_total = LL[1]) %>% ungroup() %>%
    left_join(params_df, by = c("job_id" = "job_id")) %>%
    filter(state_code == state_code_in)

## 2. Sample based on their likelihood to ensure 10% sampled
particles_w = 0.02
prob_array = exp(-params_sweep_ll$LL_total*particles_w)
if(sum(prob_array) > 0){
    indx_sampled = sample.int(n = length(prob_array), size = reps, prob = prob_array, replace = T)
    n_sampled = length(unique(indx_sampled))
}
print(n_sampled)
scalars_sampled = params_sweep_ll[indx_sampled,] %>%
    dplyr::select(imports_factor, influenza_transmissibility,
                  shelter_in_place_compliance, facemask_compliance,                  
                  influenza_susceptibility_by_age_offset,influenza_susceptibility_by_age_rate,
                  influenza_susceptibility_by_age_cutoff,influenza_susceptibility_by_age_high,
                  nursing_home_incidence_importations_factor,
                  workplace_contacts, office_contacts, workplace_contact_factor,
                  neighborhood_contacts, neighborhood_contact_factor,holiday_contact_rate,
                  school_contacts, classroom_contacts,school_contact_factor)


scalars_sobol_df = sobolDesign(
    lower = c(seed=1),
    upper = c(seed=as.integer(Sys.time())),
    reps)

scalars_sobol_df = bind_cols(scalars_sobol_df, scalars_sampled)
## scalars_sobol_df$school_contacts = school_contact_factor_in * school_contacts_in
## scalars_sobol_df$classroom_contacts = school_contact_factor_in * classroom_contacts_in
## scalars_sobol_df$school_contact_factor = school_contact_factor_in

##==============================================#
## Initial conditions-------------------
##==============================================#
initial_inf_file = sprintf('input_files/%d_imports_combined.csv', state_code)
primary_cases_file_in = file.path(output.dir, sprintf('initial_cases_%d_%d.txt',state_code,1:reps))

initial_df = read_csv(initial_inf_file)
initial_df$day = as.numeric(difftime(initial_df$Date, as.Date(start_date), units='days'))

## Sample 'reps' from the initial conditions
## replicate_init = sample.int(n=length(unique(initial_df$Replicate)), size = reps, replace=T)

for(nn in 1:reps){
    ## tmp_df = filter(initial_df, Replicate == replicate_init[nn]) %>%
    ##     arrange(day) %>% dplyr::select(Imports, day)

    ## For now, choose the mean
    tmp_df = initial_df %>% group_by(day) %>% summarize(Imports = ceiling(mean(Imports))) %>% ungroup()
    
    tmp_df$Imports = round(tmp_df$Imports * scalars_sobol_df$imports_factor[nn])
    
    ## Now, let's add one case by week after initial cases            
    extra_rows = numDays - nrow(tmp_df)
    weekly_imports = 7
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
post_lockdown_mobility_in = 0.5
shelter_time_file = file.path(output.dir, sprintf('shelter_timeseries_%d_%d.txt',state_code,1:reps))

## Moving away from google's data to Grandata census-tract specific
## shelter_timeseries_df = read_csv('./input_files/interventions_covid_timevarying_shelter.csv')
shelter_timeseries_df = read_csv('./input_files/11001_mobility_trends.csv')
shelter_timeseries_df$day = as.numeric(difftime(shelter_timeseries_df$date, as.Date(start_date), units='days'))
data_mov_day = as.integer(as.Date('2021-02-01') - as.Date(start_date))
for(nn in 1:reps){
    ## For now, choose the mean
    tmp_df = shelter_timeseries_df %>% filter(State == state_code, replicate == 1) %>%
        mutate(shelter_trend = shelter_trend * scalars_sobol_df$shelter_in_place_compliance[nn]) %>%
        dplyr::filter(day <= data_mov_day)

    ## Adjust mobility for in-person school
    ## tmp_df$shelter_trend[tmp_df$day > school_reopen_numday] = school_mov * mov_factor_in
    
    max_tmp_day = max(tmp_df$day)
    tmp_df_tail = tmp_df[tmp_df$day == max_tmp_day,]
    tmp_df_tail$day = max_tmp_day + 1
    tmp_df_tail$shelter_trend = mov_factor_in * tmp_df_tail$shelter_trend
        
    tmp_df$shelter_trend[tmp_df$shelter_trend < 0.0001] = 0.0
    tmp_df$shelter_trend[tmp_df$shelter_trend > 1.0] = 1.0
    
    shelter_time_lines = sprintf('%.0f %.0f %.4f 0 120 %d%s', tmp_df$day, tmp_df$day, tmp_df$shelter_trend, tmp_df$State, tmp_df$SCACODIGO)

    shelter_time_tail_lines = sprintf('%.0f %.0f %.4f 0 120 %d%s', tmp_df_tail$day, numDays, tmp_df_tail$shelter_trend, tmp_df_tail$State, tmp_df_tail$SCACODIGO)

    ## Add shelter by age
    ## shelter_age_lines = sprintf("82 212 %.4f 60 120", scalars_sobol_df$shelter_in_place_age_compliance[nn])
    fileConn<-file(shelter_time_file[nn])
    writeLines(c(shelter_time_lines, shelter_time_tail_lines), fileConn)
    close(fileConn)
}
##==============================================#
## Facemask compliance-------------------
##==============================================#
school_facemask_compliance = 0.75
facemask_community_df  =  read_csv('./input_files/facemask_timeseries_compliance_community.csv')  %>%
    mutate(state_name = tolower(state_name))
community_scaling = 1.0

facemask_high_file = file.path(output.dir, sprintf('facemask_compliance_high_%d_%d.txt',state_code,1:reps))
facemask_school_null_file = file.path(output.dir, sprintf('facemask_compliance_school_null_%d_%d.txt',state_code,1:reps))

facemask_timeseries_df = read_csv('./input_files/facemask_timeseries_compliance.csv') %>%
    dplyr::select(-Day)
facemask_timeseries_df$day = as.numeric(difftime(facemask_timeseries_df$Date, as.Date(start_date), units='days'))
facemask_community_df$day = as.numeric(difftime(facemask_community_df$Date, as.Date(start_date), units='days'))

for(nn in 1:reps){
    ## FM compliance stays at the last value in the data
    tmp_df = facemask_timeseries_df %>% filter(State == tolower(interventions_st_df$state_name[interventions_st_df$State == state_code])) %>%
        mutate(FacemaskTrends = FacemaskTrends * scalars_sobol_df$facemask_compliance[nn]) %>%
        dplyr::select(day, FacemaskTrends)

    tmp_comm_df = filter(facemask_community_df, state_name == tolower(interventions_st_df$state_name[interventions_st_df$State == state_code])) %>%
        mutate(FacemaskTrends = FacemaskTrends * community_scaling) %>%
        dplyr::select(day, FacemaskTrends) %>%
        filter(day > max(tmp_df$day)) %>%
        mutate(FacemaskTrends = ifelse(FacemaskTrends < 1.0,FacemaskTrends, 1.0))

    tmp_comm_df = bind_rows(tmp_df, tmp_comm_df)

    if(max(tmp_df$day) < numDays){
        tmp_df2 = data.frame(day = seq(from=max(tmp_df$day) + 1, to = numDays),
                             FacemaskTrends = tail(tmp_df$FacemaskTrends,1), stringsAsFactors = F)
        tmp_df = bind_rows(tmp_df, tmp_df2)
    }
    
    compliance_time_lines = c()
    for(loc in c("workplace", "office", "other")){
        if(!(loc %in% c("other_community"))){ ## Says other community just to ignore the community facemask for now go with other
            compliance_time_lines = c(compliance_time_lines,
                                      sprintf('%.0f %.0f %.4f %s', tmp_df$day, tmp_df$day,  tmp_df$FacemaskTrends, loc)
                                      )
        }else{
            compliance_time_lines = c(compliance_time_lines,
                                      sprintf('%.0f %.0f %.4f %s', tmp_comm_df$day, tmp_comm_df$day,  tmp_comm_df$FacemaskTrends, loc)
                                      )
        }        
    }

    ## Schools start facemask compliance when they open
    facemask_school_day = as.integer(early_school_vacation_end - as.Date(start_date))
    school_high_lines = c()
    
    for(loc in c("school", "classroom")){
        school_high_lines = c(school_high_lines, sprintf('%.0f %.0f %.4f %s', facemask_school_day, numDays, school_facemask_compliance, loc))
    }


    school_high_lines = c(compliance_time_lines, school_high_lines)
    school_null_lines = compliance_time_lines
    
    fileConn<-file(facemask_high_file[nn])
    writeLines(school_high_lines, fileConn)
    close(fileConn)

    fileConn<-file(facemask_school_null_file[nn])
    writeLines(school_null_lines, fileConn)
    close(fileConn)
}

##==============================================#
## School closure-------------------
##==============================================#
schools_open_list = read_csv('./input_files/11001_schools_open_gps.csv')
schools_open_list$PropOpen[schools_open_list$PropOpen > 1] = 1.0
current_open_date = as.Date('2020-10-15')

localidad_esc = read_csv('./input_files/Localidad_Unidad_Catastral.csv')
localidad_list = 1:19
school_capacity_list = c(0.35, 0.5,0.75,0.9,1.0)
school_capacity_short = c(0.35, 0.75)
school_schedule_income1_file = file.path(output.dir, sprintf('school_schedule_income1_%d_cap_%d.txt',state_code, 1:length(school_capacity_list)))
school_schedule_income2_file = file.path(output.dir, sprintf('school_schedule_income2_%d_cap_%d.txt',state_code, 1:length(school_capacity_list)))
school_schedule_income3_file = file.path(output.dir, sprintf('school_schedule_income3_%d_cap_%d.txt',state_code, 1:length(school_capacity_list)))
school_schedule_income4_file = file.path(output.dir, sprintf('school_schedule_income4_%d_cap_%d.txt',state_code, 1:length(school_capacity_list)))


school_schedule_prek_file = file.path(output.dir, sprintf('school_schedule_prek_%d_cap_%d.txt',state_code, 1:length(school_capacity_list)))
school_schedule_prekprimary_file = file.path(output.dir, sprintf('school_schedule_prekprimary_%d_cap_%d.txt',state_code, 1:length(school_capacity_list)))
school_schedule_primary_file = file.path(output.dir, sprintf('school_schedule_primary_%d_cap_%d.txt',state_code, 1:length(school_capacity_list)))
school_schedule_secondary_file = file.path(output.dir, sprintf('school_schedule_secondary_%d_cap_%d.txt',state_code, 1:length(school_capacity_list)))
school_schedule_prekprimarysecondary_file = file.path(output.dir, sprintf('school_schedule_prekprimarysecondary_%d_cap_%d.txt',state_code, 1:length(school_capacity_list)))

school_schedule_closed_file = file.path(output.dir, sprintf('school_schedule_closed_%d.txt',state_code))
school_schedule_open_file = file.path(output.dir, sprintf('school_schedule_open_%d.txt',state_code))
school_schedule_open_delay_file = file.path(output.dir, sprintf('school_schedule_open_delay_%d_%d.txt',1:2,state_code))

school_schedule_open_cap_file = file.path(output.dir, sprintf('school_schedule_open_%d_cap_%d.txt',state_code, 1:length(school_capacity_short)))
school_schedule_open_cap_delay_file = list()
for(cc in 1:length(school_capacity_short)){
    school_schedule_open_cap_delay_file[[cc]] = file.path(output.dir, sprintf('school_schedule_open_cap_%d_delay_%d_%d.txt',cc,1:2,state_code))
}
school_schedule_open_universities_file = file.path(output.dir, sprintf('school_schedule_open_universities_%d.txt',state_code))


## Write files
{
    initial_closure = sprintf("%d %d 1 20 0", school_closure_day_in,  as.integer(early_school_vacation_end - as.Date(start_date)))
    ## Make sure this is working properly
    vacation_closure = c(sprintf("%d %d 1 20 0", as.integer(as.Date('2020-12-15') - as.Date(start_date)),  as.integer(as.Date(school_vacation_end) - as.Date(start_date))),
                         sprintf("%d %d 1 20 0", as.integer(as.Date('2021-06-22') - as.Date(start_date)),  as.integer(as.Date('2021-07-12') - as.Date(start_date))))
    school_baseline_reopen_date = as.integer(early_school_vacation_end - as.Date(start_date)) + 1
    school_delayed_reopen_dates = list(as.integer(early_school_vacation_end %m+% months(1) - as.Date(start_date)) + 1,
                                       as.integer(early_school_vacation_end %m+% months(2) - as.Date(start_date)) + 1)
    school_current_early_open_date = as.integer(current_open_date - as.Date(start_date)) + 1
    
    ## Schools already open
    current_open_school_lines = c(initial_closure, vacation_closure)
    current_delayed_school_lines = list(c(initial_closure, vacation_closure),c(initial_closure, vacation_closure))
    
    for(ll in 1:length(localidad_list)){
        localidad_tmp = filter(schools_open_list, Localidad_ID == localidad_list[ll])        
        esc_in_loc = sprintf("11001%s", localidad_esc$SCACODIGO[localidad_esc$Localidad == localidad_list[ll]])
        
        current_open_school_lines = c(current_open_school_lines,
                                   sprintf("%d %d 18 20 %0.2f 0 %s",  school_current_early_open_date, school_baseline_reopen_date, localidad_tmp[localidad_tmp$Grade == 'university','PropOpen'] * school_reduced_capacity_in, esc_in_loc),
                                   sprintf("%d %d 1 17 %.2f 0 %s", school_current_early_open_date, school_baseline_reopen_date, localidad_tmp[localidad_tmp$Grade == 'basic_ed','PropOpen'] * school_reduced_capacity_in, esc_in_loc))
        
        for(dd in 1:length(current_delayed_school_lines)){
            current_delayed_school_lines[[dd]] = c(
                current_delayed_school_lines[[dd]],
                sprintf("%d %d 18 20 %0.2f 0 %s",  school_current_early_open_date, school_delayed_reopen_dates[[dd]], localidad_tmp[localidad_tmp$Grade == 'university','PropOpen'] * school_reduced_capacity_in, esc_in_loc),
                sprintf("%d %d 1 17 %.2f 0 %s", school_current_early_open_date, school_delayed_reopen_dates[[dd]], localidad_tmp[localidad_tmp$Grade == 'basic_ed','PropOpen'] * school_reduced_capacity_in, esc_in_loc))
        }
    }
    
    for(cc in 1:length(school_capacity_list)){
        ## Income scenarios
        school_income1_lines = c(current_open_school_lines,
                                 sprintf("%d %d 18 20 0.0", school_baseline_reopen_date , numDays),
                                 sprintf("%d %d 1 17 %.2f 1",  school_baseline_reopen_date, numDays, school_capacity_list[cc]),
                                 sprintf("%d %d 1 20 0.0 2",  school_baseline_reopen_date, numDays),
                                 sprintf("%d %d 1 20 0.0 3",  school_baseline_reopen_date, numDays),
                                 sprintf("%d %d 1 20 0.0 4",  school_baseline_reopen_date, numDays)                  
                                 )
        school_income2_lines = c(current_open_school_lines,
                                 sprintf("%d %d 18 20 0.0",  school_baseline_reopen_date, numDays),
                                 sprintf("%d %d 1 20 0.0 1",  school_baseline_reopen_date, numDays),
                                 sprintf("%d %d 1 17 %.2f 2",  school_baseline_reopen_date, numDays, school_capacity_list[cc]),
                                 sprintf("%d %d 1 20 0.0 3",  school_baseline_reopen_date, numDays),
                                 sprintf("%d %d 1 20 0.0 4",  school_baseline_reopen_date, numDays)
                                 )
        school_income3_lines = c(current_open_school_lines,
                                 sprintf("%d %d 18 20 0.0",  school_baseline_reopen_date, numDays),
                                 sprintf("%d %d 1 20 0.0 1",  school_baseline_reopen_date, numDays),
                                 sprintf("%d %d 1 20 0.0 2",  school_baseline_reopen_date, numDays),
                                 sprintf("%d %d 1 17 %.2f 3",  school_baseline_reopen_date, numDays, school_capacity_list[cc]),
                                 sprintf("%d %d 1 20 0.0 4",  school_baseline_reopen_date, numDays)
                                 )
        school_income4_lines = c(current_open_school_lines,
                                 sprintf("%d %d 18 20 0.0",  school_baseline_reopen_date, numDays),
                                 sprintf("%d %d 1 20 0.0 1",  school_baseline_reopen_date, numDays),
                                 sprintf("%d %d 1 20 0.0 2",  school_baseline_reopen_date, numDays),
                                 sprintf("%d %d 1 20 0.0 3",  school_baseline_reopen_date, numDays),
                                 sprintf("%d %d 1 17 %.2f 4",  school_baseline_reopen_date, numDays,  school_capacity_list[cc])
                                 )

        ## Grades scenarios
        school_prek_lines = c(current_open_school_lines,
                              sprintf("%d %d 6 20 0",  school_baseline_reopen_date, numDays),
                              sprintf("%d %d 1 5 %.2f",  school_baseline_reopen_date, numDays, school_capacity_list[cc]))
        
        school_prekprimary_lines = c(current_open_school_lines,
                                     sprintf("%d %d 11 20 0",  school_baseline_reopen_date, numDays),
                                     sprintf("%d %d 1 10 %.2f",  school_baseline_reopen_date, numDays, school_capacity_list[cc]))

        school_prekprimarysecondary_lines = c(current_open_school_lines,
                                     sprintf("%d %d 18 20 0",  school_baseline_reopen_date, numDays),
                                     sprintf("%d %d 1 5 0.75",  school_baseline_reopen_date, numDays),
                                     sprintf("%d %d 11 17 0.35",  school_baseline_reopen_date, numDays),
                                     sprintf("%d %d 6 10 %.2f",  school_baseline_reopen_date, numDays, school_capacity_list[cc]))
        
        school_primary_lines = c(current_open_school_lines,
                                 sprintf("%d %d 1 5 0",  school_baseline_reopen_date, numDays),
                                 sprintf("%d %d 11 20 0",  school_baseline_reopen_date, numDays),
                                 sprintf("%d %d 6 10 %.2f",  school_baseline_reopen_date, numDays, school_capacity_list[cc]))
        
        school_secondary_lines = c(current_open_school_lines,
                                   sprintf("%d %d 1 10 0",  school_baseline_reopen_date, numDays),
                                   sprintf("%d %d 18 20 0",  school_baseline_reopen_date, numDays),
                                   sprintf("%d %d 11 17 %.2f",  school_baseline_reopen_date, numDays, school_capacity_list[cc]))
        ## Write reopening files
        fileConn<-file(school_schedule_income1_file[cc])
        writeLines(school_income1_lines, fileConn)
        close(fileConn)

        fileConn<-file(school_schedule_income2_file[cc])
        writeLines(school_income2_lines, fileConn)
        close(fileConn)

        fileConn<-file(school_schedule_income3_file[cc])
        writeLines(school_income3_lines, fileConn)
        close(fileConn)

        fileConn<-file(school_schedule_income4_file[cc])
        writeLines(school_income4_lines, fileConn)
        close(fileConn)

        
        fileConn<-file(school_schedule_prek_file[cc])
        writeLines(school_prek_lines, fileConn)
        close(fileConn)

        fileConn<-file(school_schedule_primary_file[cc])
        writeLines(school_primary_lines, fileConn)
        close(fileConn)

        fileConn<-file(school_schedule_prekprimary_file[cc])
        writeLines(school_prekprimary_lines, fileConn)
        close(fileConn)

        fileConn<-file(school_schedule_prekprimarysecondary_file[cc])
        writeLines(school_prekprimarysecondary_lines, fileConn)
        close(fileConn)

        fileConn<-file(school_schedule_secondary_file[cc])
        writeLines(school_secondary_lines, fileConn)
        close(fileConn)
    }  

    ## Delayed scenarios
    for(dd in 1:length(current_delayed_school_lines)){
        school_open_delayed_lines = c(
            current_delayed_school_lines[[dd]],                                      
            sprintf("%d %d 1 17 1.0",  school_delayed_reopen_dates[[dd]],numDays),
            sprintf("%d %d 18 20 0",  school_delayed_reopen_dates[[dd]], numDays))
        
        fileConn<-file(school_schedule_open_delay_file[dd])
        writeLines(school_open_delayed_lines, fileConn)
        close(fileConn)

        for(cc in 1:length(school_capacity_short)){
            school_open_cap_delayed_lines = c(
                current_delayed_school_lines[[dd]],
                sprintf("%d %d 1 17 %.2f",  school_delayed_reopen_dates[[dd]], numDays,school_capacity_short[cc]),
                sprintf("%d %d 18 20 0",  school_delayed_reopen_dates[[dd]],numDays))
            
            
            fileConn<-file(school_schedule_open_cap_delay_file[[cc]][dd])
            writeLines(school_open_cap_delayed_lines, fileConn)
            close(fileConn)
        }
    }
    
    ## Extreme scenarios
    school_closed_lines = c(current_open_school_lines,
                            sprintf("%d %d 1 20 0",  as.integer(early_school_vacation_end - as.Date(start_date)) + 1, numDays))
    
    school_open_lines = c(current_open_school_lines,
                          sprintf("%d %d 1 17 1.0",  as.integer(early_school_vacation_end - as.Date(start_date)) + 1, numDays),
                          sprintf("%d %d 18 20 0",  as.integer(early_school_vacation_end - as.Date(start_date)) + 1, numDays))
    
    
    school_open_universities_lines = c(current_open_school_lines,
                                       sprintf("%d %d 1 17 1.0",  as.integer(early_school_vacation_end - as.Date(start_date)) + 1, numDays),
                                       sprintf("%d %d 18 20 1.0",  as.integer(early_school_vacation_end - as.Date(start_date)) + 1, numDays))

    fileConn<-file(school_schedule_open_universities_file)
    writeLines(school_open_universities_lines, fileConn)
    close(fileConn)
    
    fileConn<-file(school_schedule_open_file)
    writeLines(school_open_lines, fileConn)
    close(fileConn)

    for(cc in 1:length(school_capacity_short)){
        school_open_cap_lines = c(
            current_open_school_lines,
            sprintf("%d %d 1 17 %.2f",  as.integer(early_school_vacation_end - as.Date(start_date)) + 1, numDays,school_capacity_short[cc]),
            sprintf("%d %d 18 20 0",  as.integer(early_school_vacation_end - as.Date(start_date)) + 1, numDays))
        
        fileConn<-file(school_schedule_open_cap_file[cc])
        writeLines(school_open_cap_lines, fileConn)
        close(fileConn)
    }
    
    fileConn<-file(school_schedule_closed_file)
    writeLines(school_closed_lines, fileConn)
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
    school_closure_policy = school_closure_policy_in,
    school_closure_day = as.integer(interventions_st_df$School_closure[interventions_st_df$State == state_code] - as.Date(start_date)),
    school_closure_duration = as.integer(school_vacation_end - interventions_st_df$School_closure[interventions_st_df$State == state_code]),
    school_student_teacher_ratio = school_student_teacher_ratio_in,
    stringsAsFactors = F)

intervention_df = intervention_base[rep(1,reps),]

## School contacts
intervention_df$school_contacts = scalars_sobol_df$school_contacts
intervention_df$classroom_contacts = scalars_sobol_df$classroom_contacts
intervention_df$school_contact_factor = scalars_sobol_df$school_contact_factor
intervention_df$neighborhood_contacts = scalars_sobol_df$neighborhood_contacts
intervention_df$neighborhood_contact_factor = scalars_sobol_df$neighborhood_contact_factor
intervention_df$holiday_contact_rate = scalars_sobol_df$holiday_contact_rate
intervention_df$workplace_contacts = scalars_sobol_df$workplace_contacts
intervention_df$office_contacts = scalars_sobol_df$office_contacts
intervention_df$workplace_contact_factor = scalars_sobol_df$workplace_contact_factor
intervention_df$enable_holiday_contacts = enable_holiday_contacts_in
intervention_df$holiday_start = holiday_start_in
intervention_df$holiday_end = holiday_end_in

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
           face_mask_timeseries_file = facemask_high_file,
           facemask_compliance = scalars_sobol_df$facemask_compliance,
           enable_school_reduced_capacity = enable_school_reduced_capacity_in,
           school_reduced_capacity = school_reduced_capacity_in,
           school_reduced_capacity_day = school_reduced_capacity_day_in,
           enable_face_mask_usage = enable_face_mask_usage_in,
           school_global_schedule_file = school_schedule_closed_file,
           nursing_home_incidence_importations_factor = scalars_sobol_df$nursing_home_incidence_importations_factor, 
           intervention_name = "Schools_Closed")

intervention_df_open = intervention_df %>%    
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
           face_mask_timeseries_file = facemask_high_file,
           facemask_compliance = scalars_sobol_df$facemask_compliance,
           enable_school_reduced_capacity = enable_school_reduced_capacity_in,
           school_reduced_capacity = school_reduced_capacity_in,
           school_reduced_capacity_day = school_reduced_capacity_day_in,
           enable_face_mask_usage = enable_face_mask_usage_in,
           school_global_schedule_file = school_schedule_open_file,
           nursing_home_incidence_importations_factor = scalars_sobol_df$nursing_home_incidence_importations_factor, 
           intervention_name = "Schools_Open")

intervention_df_open_noholiday = intervention_df_open %>%
mutate(enable_holiday_contacts = 0,
       intervention_name = "Schools_OpenNoHoliday")


intervention_df_open_cap = intervention_df %>%    
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
           face_mask_timeseries_file = facemask_high_file,
           facemask_compliance = scalars_sobol_df$facemask_compliance,
           enable_school_reduced_capacity = enable_school_reduced_capacity_in,
           school_reduced_capacity = school_reduced_capacity_in,
           school_reduced_capacity_day = school_reduced_capacity_day_in,
           enable_face_mask_usage = enable_face_mask_usage_in,
           school_global_schedule_file = school_schedule_open_cap_file[1],
           nursing_home_incidence_importations_factor = scalars_sobol_df$nursing_home_incidence_importations_factor, 
           intervention_name = sprintf("Schools_OpenCap_%.2f", school_capacity_short[1]))

intervention_df_open_cap_noholiday = intervention_df_open_cap %>%
    mutate(enable_holiday_contacts = 0,
           intervention_name = "Schools_OpenCapNoHoliday")

intervention_df_open_cap_2 = intervention_df_open_cap %>%
    mutate(school_global_schedule_file = school_schedule_open_cap_file[2],
           school_reduced_capacity = school_capacity_short[2],
           intervention_name = sprintf("Schools_OpenCap_%.2f", school_capacity_short[2]))


intervention_df_open_all = intervention_df %>%    
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
           face_mask_timeseries_file = facemask_high_file,
           facemask_compliance = scalars_sobol_df$facemask_compliance,
           enable_school_reduced_capacity = enable_school_reduced_capacity_in,
           school_reduced_capacity = school_reduced_capacity_in,
           school_reduced_capacity_day = school_reduced_capacity_day_in,
           enable_face_mask_usage = enable_face_mask_usage_in,
           school_global_schedule_file = school_schedule_open_universities_file,
           nursing_home_incidence_importations_factor = scalars_sobol_df$nursing_home_incidence_importations_factor, 
           intervention_name = "Schools_OpenUniversities")

## This is the scenario with no interventions... aka the counterfactual
## No facemasks, no shelter in place, no school closure
intervention_df_open_nfm = intervention_df %>%    
    mutate(seed = floor(scalars_sobol_df$seed),
           primary_cases_file = primary_cases_file_in,
           imports_factor = scalars_sobol_df$imports_factor,
           enable_shelter_in_place = 0,
           enable_shelter_in_place_timeseries = 0,
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
           school_closure_policy = 'none',
           school_reduced_capacity = school_reduced_capacity_in,
           school_reduced_capacity_day = school_reduced_capacity_day_in,
           enable_face_mask_usage = 0,
           school_global_schedule_file = school_schedule_open_file,
           nursing_home_incidence_importations_factor = scalars_sobol_df$nursing_home_incidence_importations_factor, 
           intervention_name = "Schools_OpenNFM")


## School reopen files
school_schedule_files = list(
    Schools_prek = school_schedule_prek_file, Schools_prekprimary = school_schedule_prekprimary_file,
    Schools_prekprimarysecondary = school_schedule_prekprimarysecondary_file,
    Schools_primary = school_schedule_primary_file, Schools_secondary = school_schedule_secondary_file,
    Schools_income1 = school_schedule_income1_file,
    Schools_income2 = school_schedule_income2_file,
    Schools_income3 = school_schedule_income3_file,
    Schools_income4 = school_schedule_income4_file)

scalars_intervention = bind_rows(intervention_df_closed, intervention_df_open,intervention_df_open_cap, intervention_df_open_cap_2, intervention_df_open_nfm,
                                 intervention_df_open_all)


school_contact_list = c(0.9,0.75,0.5)
tmp_capacity_ind = 1
for(ff in 1:length(school_schedule_files)){
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
                   nursing_home_incidence_importations_factor = scalars_sobol_df$nursing_home_incidence_importations_factor, 
                   face_mask_timeseries_file = facemask_high_file,
                   facemask_compliance = scalars_sobol_df$facemask_compliance,
                   enable_school_reduced_capacity = enable_school_reduced_capacity_in,
                   school_reduced_capacity = school_capacity_list[cc],
                   school_reduced_capacity_day = school_reduced_capacity_day_in,
                   enable_face_mask_usage = enable_face_mask_usage_in,
                   school_global_schedule_file = school_schedule_files[[ff]][cc],
                   intervention_name = sprintf('%s-cap%.2f', names(school_schedule_files)[ff], school_capacity_list[cc]))
        scalars_intervention = bind_rows(scalars_intervention, intervention_df_tmp)
    }

    ## for(ss in 1:length(school_contact_list)){
    ##     intervention_df_tmp = intervention_df %>%    
    ##         mutate(seed = floor(scalars_sobol_df$seed),
    ##                primary_cases_file = primary_cases_file_in,
    ##                imports_factor = scalars_sobol_df$imports_factor,
    ##                shelter_in_place_file = shelter_time_file,
    ##                shelter_in_place_compliance = scalars_sobol_df$shelter_in_place_compliance,
    ##                influenza_transmissibility = scalars_sobol_df$influenza_transmissibility,           
    ##                school_closure_duration = early_closure_duration,
    ##                influenza_susceptibility_by_age_offset = scalars_sobol_df$influenza_susceptibility_by_age_offset,
    ##                influenza_susceptibility_by_age_rate  = scalars_sobol_df$influenza_susceptibility_by_age_rate,
    ##                influenza_susceptibility_by_age_cutoff = scalars_sobol_df$influenza_susceptibility_by_age_cutoff,
    ##                influenza_susceptibility_by_age_high = scalars_sobol_df$influenza_susceptibility_by_age_high,
    ##                nursing_home_incidence_importations_factor = scalars_sobol_df$nursing_home_incidence_importations_factor, 
    ##                face_mask_timeseries_file = facemask_high_file,
    ##                facemask_compliance = scalars_sobol_df$facemask_compliance,
    ##                enable_school_reduced_capacity = enable_school_reduced_capacity_in,
    ##                school_reduced_capacity = school_capacity_list[tmp_capacity_ind],
    ##                school_reduced_capacity_day = school_reduced_capacity_day_in,
    ##                enable_face_mask_usage = enable_face_mask_usage_in,
    ##                school_global_schedule_file = school_schedule_files[[ff]][tmp_capacity_ind],
    ##                school_contacts = school_contacts_in * school_contact_factor_in,
    ##                classroom_contacts = classroom_contacts_in * school_contact_list[ss],
    ##                school_contact_factor = school_contact_list[ss],
    ##                intervention_name = sprintf('%s-cap%.2f-class%.2f', names(school_schedule_files)[ff], school_capacity_list[tmp_capacity_ind], school_contact_list[ss])
    ##                )
    ##     scalars_intervention = bind_rows(scalars_intervention, intervention_df_tmp)
    ## }
}

## Reopen files with delays

for(dd in 1:length(school_schedule_open_delay_file)){
    intervention_df_tmp = intervention_df_open %>%
        mutate(school_global_schedule_file = school_schedule_open_delay_file[dd],
               intervention_name = sprintf("Schools_Open_Delay_%d", dd))
    for(cc in 1:length(school_capacity_short)){
        intervention_df_tmp_cap = intervention_df_open_cap %>%
            mutate(school_global_schedule_file = school_schedule_open_cap_delay_file[[cc]][dd],
                   intervention_name = sprintf("Schools_OpenCap_%.2f_Delay_%d", school_capacity_short[cc],dd))
        
        scalars_intervention = bind_rows(scalars_intervention, intervention_df_tmp, intervention_df_tmp_cap)
    }
}

scalars_intervention = bind_rows(scalars_intervention, intervention_df_open_noholiday, intervention_df_open_cap_noholiday)
##==============================================#
## Create parameters to sweep-----------------
##==============================================#
## 1. Create DF with parameters
enable_age_specific_susceptibility_min_in = 0
influenza_susceptibility_by_age_minage_in = 10
influenza_susceptibility_by_age_minvalue_in = 1.0


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
           min_age_face_masks = min_age_face_masks_in,
           start_date = start_date)
                                                              
##===============================================##
## Write the parameters to files---------------
##===============================================##
defaults_params = './input_files/params_covid.txt'
basename_params = sprintf('covid_%d_params',state_code)
basename_jobs = sprintf('FRED_%d_school_psa_asymp',state_code)
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
                            enable_face_mask_usage, enable_face_mask_timeseries,
                            facemask_compliance, min_age_face_masks,
                            enable_school_reduced_capacity, school_reduced_capacity, school_reduced_capacity_day,
                            school_student_teacher_ratio,
                            influenza_susceptibility_by_age_rate,
                            influenza_susceptibility_by_age_offset,
                            influenza_susceptibility_by_age_cutoff,
                            influenza_susceptibility_by_age_high,
                            school_contacts,school_contact_factor,
                            classroom_contacts,
                            neighborhood_contacts, neighborhood_contact_factor,
                            workplace_contacts, office_contacts, workplace_contact_factor,
                            enable_holiday_contacts,
                            holiday_contact_rate,
                            holiday_start,
                            holiday_end,
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

submit_jobs(experiment_supername_in = sprintf('FRED_BOG_SCHOOL_PSA_asymp_%.2f_FM_%.2f_KSUS_%.2f_MV_%.2f', asymp_infectivity_in, face_mask_transmission_efficacy_in, kids_susceptibility_age_in, mov_factor_in),
            experiment_name_in = as.character(state_code),
            experiment_dir_in = output.dir,
            params_base = basename_params,
            job_base = basename_jobs,
            reps = reps_per_job,
            scalars = report_scalars,
            FUN = write_cmd_function,cores_in=6,walltime_in = "1:00:00",
            subsys="UGE",
            fred_home_dir_in=fred_home, fred_results_in=fred_results_dir)

