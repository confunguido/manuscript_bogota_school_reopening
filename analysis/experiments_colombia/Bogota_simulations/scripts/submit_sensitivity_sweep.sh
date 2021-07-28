#!/bin/csh -f
set reps = 30
set reps_per_job = 1
set forecast_date = 2021-08-31
set asymp = (0.5 0.75 1.0)
set fm_efficacy = (0.3 0.12 0.73)
set kids_susc_age = (10 1 5)
set mov_list = (1.0 0.0)

set in_as = 3
set in_fm = 3
set in_ks = 1
set in_mov = 1

Rscript ./scripts/process_google_mobility_data_timeseries.R $forecast_date $reps

## Bogota
Rscript ./scripts/submit_fred_school_psa.R 11001 $reps $reps_per_job $forecast_date $asymp[$in_as] $fm_efficacy[$in_fm] $kids_susc_age[$in_ks] $mov_list[$in_mov]
