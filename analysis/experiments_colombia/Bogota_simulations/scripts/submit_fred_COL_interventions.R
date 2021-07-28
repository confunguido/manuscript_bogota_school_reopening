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

##==============================================#
## Custom functions-------------
##==============================================#
write_cmd_function <- function(scalars_in, tmpfile){
    ## Delte jobs!!!
    cat("Deleting jobs\n")
    if(dir.exists(file.path(Sys.getenv('FRED_RESULTS'), 'JOB'))){
        for(s in 1:nrow(scalars_in)){
            system(sprintf("fred_delete -f -k %s",scalars_in$job_id[s]), intern = T)
        }
        cat("All jobs deleted\n")
    }else{
        cat("FRED RESULTS: ", Sys.getenv('FRED_RESULTS'), "not found, nothing to delete\n")
    }
    job_cmd_str = sprintf("fred_job -k %s -p %s -n %.0f;Rscript ./post_process_fred.R %s %.0f",
                          scalars_in$job_id,
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
state_code_in = 11001
reps = 10
reps_per_job = 1
args = (commandArgs(TRUE))
if(length(args) >= 1){
    state_code_in = as.numeric(args[1])
    if(length(args) >= 2){
        reps = as.numeric(args[2])
        if(length(args) >= 3){
            reps_per_job = as.numeric(args[3])
        }
    }

}

##==============================================#
## FRED setup-------------
##==============================================#
fred_home = Sys.getenv('FRED_HOME')
scratch_dir = Sys.getenv('scratch_dir')
fred_defaults = sprintf("%s/input_files/defaults", fred_home)
output.dir = file.path(scratch_dir, sprintf('FRED_%d_interventions',state_code_in))

if(file.exists(output.dir)){
    system(paste('rm -rf ', output.dir,sep = ''))
}
system(paste('mkdir -p ', output.dir,sep = ''))

file.copy('./scripts/post_process_fred.R',output.dir)
file.copy('../input_files/params_covid.txt','./input_files/params_covid.txt', overwrite = T)



##==============================================#
## Sweep fixed parameters-------------------
##==============================================#
numDays = 365 # One year of simulations 
synthetic_population_id = sprintf('synthetic_population_id = colombia_%d', state_code_in)
isolation_rate = 0.05175 # Specific for the US
shelter_in_place_students = 0 # Don't shelter students, use school closure for that

advance_seeding = 'exposed'
start_date = '2020-01-01' # YYYY-MM-DD
school_vacation_end = as.Date('2020-07-03')

##==============================================#
## Filter and sample particles--------------
##==============================================#
## 1. Read parameters with LL
calibration_simdir = sprintf('FRED_%d_calibration',state_code_in)
calibration_dir = file.path('output','CALIBRATION',sprintf("%s_%s", calibration_simdir, "out"))
params_df = read_csv(file.path(calibration_dir, 'FRED_parameters_out.csv'))
fred_sweep_df = read_csv(file.path(calibration_dir, 'fred_output.csv'))
params_sweep_ll =  fred_sweep_df %>%
    group_by(job_id) %>% summarize(LL = LL[1]) %>% ungroup() %>%
    left_join(params_df, by = c("job_id" = "job_id")) %>%
    filter(state_code == state_code_in)

## 2. Sample based on their likelihood
prob_array = exp(-params_sweep_ll$LL*0.05)
indx_sampled = sample.int(n = length(prob_array), size = reps, prob = prob_array, replace = T)

scalars_sampled = params_sweep_ll[indx_sampled,] %>%
    dplyr::select(imports_factor, influenza_transmissibility, shelter_in_place_compliance)

## scalars_sobol_df = sobolDesign(
##     lower = c(seed = 1),
##     upper = c(seed = as.integer(Sys.time())),
##     reps)
scalars_sobol_df = sobol(
    vars = list(
        seed = c(1,as.integer(Sys.time()))),reps
)
scalars_sobol_df = bind_cols(scalars_sobol_df, scalars_sampled)

##==============================================#
## Initial conditions-------------------
##==============================================#
initial_inf_file = sprintf('input_files/%d_imports.csv', state_code_in)
primary_cases_file_in = file.path(output.dir, sprintf('initial_cases_%d_%d.txt',state_code_in,1:reps))

initial_df = read_csv(initial_inf_file)
initial_df$day = as.numeric(difftime(initial_df$Date, as.Date(start_date), units='days'))

## Sample 'reps' from the initial conditions
replicate_init = sample.int(n=length(unique(initial_df$Replicate)), size = reps, replace=F)

for(nn in 1:reps){
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
## Intervention parameters-------------------
##==============================================#
epidemic_offset = 0
num_demes = 1

interventions_st_df = read_csv('./input_files/interventions_Colombia.csv')

intervention_base = data.frame(
    enable_shelter_in_place = 1,
    shelter_in_place_enable_fixed_end = 1,
    shelter_in_place_enable_linear_early_rate = 1,
    shelter_in_place_delay_mean = as.integer(interventions_st_df$Shelter_in_place[interventions_st_df$State == state_code_in] - as.Date(start_date)),
    shelter_in_place_early_rate = interventions_st_df$early_rate[interventions_st_df$State == state_code_in],
    school_closure_policy = 'global',
    school_closure_day = as.integer(interventions_st_df$School_closure[interventions_st_df$State == state_code_in] - as.Date(start_date)),
    school_summer_schedule = 1,
    school_summer_start = '06-16',
    school_summer_end = '07-03',
    stringsAsFactors = F)


## 4 scenarios:
## 1. keep shelter forever
## 2. shelter to 50% of baseline on may 1st
## 3. stop shelter on may 1
## 4. gradual shelter reduction

school_closure_duration = as.integer(school_vacation_end - interventions_st_df$School_closure[interventions_st_df$State == state_code_in])

int_day = 132 ## May 11
min_duration = int_day - intervention_base$shelter_in_place_delay_mean[1]
stepwise_compliance = seq(from=1.0,to=0.3,by=-0.1)
stepwise_duration = seq(from=min_duration,by=7,length.out=8)

shelter_int_df = data.frame(
    shelter_in_place_duration_mean = c(numDays, int_day, int_day, int_day) - intervention_base$shelter_in_place_delay_mean[1],
    shelter_in_place_compliance_factor = c(1.0, 0.5, 0.0,0.75),
    shelter_in_place_enable_stepwise = c(0,0,0,1),
    school_closure_duration = rep(school_closure_duration,4),    
    shelter_in_place_enable_workplace_release_by_size = rep(0,4),
    shelter_in_place_stepwise_compliance = "2 0.9 0.8",
    shelter_in_place_stepwise_duration = sprintf("%d %s", length(stepwise_duration), paste(stepwise_duration,collapse = " ")),
    shelter_in_place_workplace_sizes = "2 100 1000",
    shelter_in_place_workplace_release_days = "2 10 20",
    shelter_in_place_workplace_release_prop = "2 0.9 0.9",
    intervention_name = c("ShelterAll","Shelter50","Shelter0","ShelterGradual"),
    stringsAsFactors = F
    )

intervention_df = shelter_int_df
intervention_df = bind_cols(intervention_df, intervention_base[rep(1,nrow(intervention_df)),])

scalars_intervention = data.frame(stringsAsFactors=F)
for(s in 1:reps){
    intervention_df = intervention_df %>%
        mutate(seed = floor(scalars_sobol_df$seed[s]),
               primary_cases_file = primary_cases_file_in[s],
               imports_factor = scalars_sobol_df$imports_factor[s],
               shelter_in_place_compliance_end = scalars_sobol_df$shelter_in_place_compliance[s] * shelter_in_place_compliance_factor,
               shelter_in_place_compliance = scalars_sobol_df$shelter_in_place_compliance[s],
               influenza_transmissibility = scalars_sobol_df$influenza_transmissibility[s])

    stepwise_indx = which(intervention_df$shelter_in_place_enable_stepwise == 1)
    intervention_df$shelter_in_place_compliance_end[stepwise_indx] = 0
    intervention_df$shelter_in_place_stepwise_compliance[stepwise_indx] = sprintf("%d %s", length(stepwise_compliance), paste(stepwise_compliance * scalars_sobol_df$shelter_in_place_compliance[s], collapse = " "))
    
    scalars_intervention = bind_rows(scalars_intervention, intervention_df)
}

##==============================================#
## Create parameters to sweep-----------------
##==============================================#
## 1. Create DF with parameters
scalars = scalars_intervention %>%
    mutate(days = numDays,
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
basename_params = sprintf('covid_%d_params', state_code_in)
basename_jobs = sprintf('FRED_%d_interventions', state_code_in)
write_fred_parameters(scalars, defaults_params, output.dir,basename.in=basename_params, fred_defaults = fred_defaults)

## print report scalars parameters file with IDs
report_scalars = dplyr::select(scalars, influenza_transmissibility, shelter_in_place_compliance, start_date,
                               imports_factor, days, seed, primary_cases_file, school_closure_policy,
                               shelter_in_place_compliance_end,intervention_name, shelter_in_place_enable_workplace_release_by_size, shelter_in_place_enable_stepwise,
                               school_closure_duration, school_closure_day, shelter_in_place_duration_mean, shelter_in_place_delay_mean) %>%
    mutate(job_id = sprintf("%s_%d", basename_jobs, row_number()),
           params_file = sprintf('%s_%d.txt',basename_params,row_number()),
           reps = reps_per_job,
           state_code = state_code_in,
           advance_seeding = advance_seeding,
           epidemic_offset = epidemic_offset
           )

write.csv(report_scalars, file.path(output.dir, 'FRED_parameters.csv'), row.names= F, quote = F)

##===============================================##
## submit to CRC---------------
##===============================================##
## unlink(fred_results_dir,recursive = TRUE)
fred_results_dir = file.path(output.dir,"FRED_RESULTS")
Sys.setenv(FRED_RESULTS=fred_results_dir)

if(!dir.exists(fred_results_dir)){
    dir.create(fred_results_dir)
}

submit_jobs(experiment_supername_in = 'FRED_INTERVENTIONS',
            experiment_name_in = state_code_in,
            experiment_dir_in = output.dir,
            params_base = basename_params,
            job_base = basename_jobs,
            reps = reps_per_job,
            scalars = report_scalars,
            FUN = write_cmd_function,cores_in=5,
            subsys="UGE",
            fred_home_dir_in=fred_home, fred_results_in=fred_results_dir)

