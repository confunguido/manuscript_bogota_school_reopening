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
    for(s in 1:nrow(scalars_in)){
        system(sprintf("fred_delete -f -k %s",scalars_in$job_id[s]), intern = T)
    }
    cat("All jobs deleted\n")
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
state_code = 63001
reps = 10
args = (commandArgs(TRUE))
if(length(args) >= 1){
    state_code = as.numeric(args[1])
    if(length(args) == 2){
        reps = as.numeric(args[2])
    }
}

##==============================================#
## FRED setup-------------
##==============================================#
fred_home = Sys.getenv('FRED_HOME')
fred_results_dir = Sys.getenv('FRED_RESULTS')

fred_defaults = sprintf("%s/input_files/defaults", fred_home)
output.dir = sprintf('FRED_%d_baseline',state_code)

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
reps_per_job = 1

numDays = 300 # We don't need many days yet

synthetic_population_id = sprintf('synthetic_population_id = colombia_%d', state_code)

isolation_rate = 0.05175 # Specific for the US
shelter_in_place_students = 0 # Don't shelter students, use school closure for that

advance_seeding = 'exposed'
start_date = '2020-01-01' # YYYY-MM-DD
school_vacation_end = as.Date('2020-08-02')


enable_shelter_in_place = 1
shelter_in_place_duration_mean = numDays
shelter_in_place_delay_mean = as.integer(as.Date("2020-03-21")- as.Date(start_date))

school_closure_policy = 'global'
school_closure_day = as.integer(as.Date("2020-03-21") - as.Date(start_date))
school_closure_duration = as.integer(school_vacation_end - as.Date("2020-03-21"))

## A different seed for each set of initial conditions for calibration
## imports_factor = c(1.0,4.0)
## influenza_transmissibility = c(0.7,2.0) # calibrated to R0 from Singapore
## shelter_in_place_compliance = c(0.5,0.9) # Just some baseline

## scalars_sobol_df = sobol(
##     vars = list(
##         seed = c(1,as.integer(Sys.time())),
##         imports_factor = imports_factor,
##         influenza_transmissibility = influenza_transmissibility,
##         shelter_in_place_compliance = shelter_in_place_compliance
##     ), reps)


## Just one simulation, not calibrating
scalars_sobol_df = sobol(
    vars = list(
        seed = c(1,as.integer(Sys.time()))
    ), reps)

scalars_sobol_df$imports_factor = 1.0
scalars_sobol_df$influenza_transmissibility = 0.95
scalars_sobol_df$shelter_in_place_compliance = 0.7

##==============================================#
## Initial conditions-------------------
##==============================================#
initial_inf_file = sprintf('input_files/%d_imports.csv', state_code)
primary_cases_file = file.path(getwd(), output.dir, sprintf('initial_cases_%d_%d.txt',state_code,1:reps))

initial_df = read_csv(initial_inf_file)
initial_df$day = as.numeric(difftime(initial_df$Date, as.Date(start_date), units='days'))

## Sample 'reps' from the initial conditions
## replicate_init = sample.int(n=length(unique(initial_df$Replicate)), size = reps, replace=F)

for(nn in 1:reps){
    ## tmp_df = filter(initial_df, Replicate == replicate_init[nn]) %>%
    ##     arrange(day) %>% dplyr::select(Imports, day)

    ## For now, choose the mean
    tmp_df = initial_df %>% group_by(day) %>% summarize(Imports = round(mean(Imports))) %>% ungroup()
    
    tmp_df$Imports = round(tmp_df$Imports * scalars_sobol_df$imports_factor[nn])
    
    ## Now, let's add one case by week after initial cases            
    extra_rows = numDays - nrow(tmp_df)
    if(extra_rows > 0){
        week_imp = data.frame(Imports = rep(1, floor(extra_rows/7)), stringsAsFactors = F)
        week_imp$day = seq(from=max(tmp_df$day) + 1, by = 7, length.out = nrow(week_imp))
        tmp_df = bind_rows(tmp_df, week_imp)
    }
    
    init_cases_lines = sprintf('%.0f %.0f %.0f', tmp_df$day, tmp_df$day, tmp_df$Imports)
    
    fileConn<-file(primary_cases_file[nn])
    writeLines(init_cases_lines, fileConn)
    close(fileConn)
}

##==============================================#
## fixed parameters-------------------
##==============================================#
## IMPORTANT: The offset delays the epidemic. Does not include added days of imported cases
epidemic_offset = 0
num_demes = 1

scalars_intervention = data.frame(stringsAsFactors=F)
for(s in 1:reps){
    intervention_df = data.frame(seed = floor(scalars_sobol_df$seed[s]),
                                 primary_cases_file = primary_cases_file[s],
                                 imports_factor = scalars_sobol_df$imports_factor[s],
                                 shelter_in_place_compliance = scalars_sobol_df$shelter_in_place_compliance[s],
                                 influenza_transmissibility = scalars_sobol_df$influenza_transmissibility[s],
                                 stringsAsFactors = F)
    
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
           enable_shelter_in_place = enable_shelter_in_place,
           shelter_in_place_delay_mean = shelter_in_place_delay_mean,
           shelter_in_place_duration_mean = shelter_in_place_duration_mean,
           school_closure_policy = school_closure_policy,
           school_closure_day = school_closure_day,
           school_closure_duration = school_closure_duration,
           start_date = start_date)
                                                              
##===============================================##
## Write the parameters to files---------------
##===============================================##
defaults_params = './input_files/params_covid.txt'
basename_params = sprintf('covid_%d_params',state_code)
basename_jobs = sprintf('FRED_%d_baseline',state_code)
write_fred_parameters(scalars, defaults_params, output.dir,basename.in=basename_params, fred_defaults = fred_defaults)

## print report scalars parameters file with IDs
report_scalars = dplyr::select(scalars, influenza_transmissibility, shelter_in_place_compliance, start_date,
                               imports_factor, days, seed, primary_cases_file, school_closure_policy,
                               school_closure_duration, school_closure_day, shelter_in_place_duration_mean, shelter_in_place_delay_mean) %>%
    mutate(job_id = sprintf("%s_%d", basename_jobs, row_number()),
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

submit_jobs(experiment_supername_in = 'FRED_Colombia_BASELINE',
            experiment_name_in =  as.character(state_code),
            experiment_dir_in = output.dir,
            params_base = basename_params,
            job_base = basename_jobs,
            reps = reps_per_job,
            scalars = report_scalars,
            FUN = write_cmd_function,cores_in=2)

