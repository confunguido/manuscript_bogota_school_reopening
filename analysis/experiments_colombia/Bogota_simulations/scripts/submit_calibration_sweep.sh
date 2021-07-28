#!/bin/csh -f
set reps = 2000
set reps_per_job = 1
set forecast_date = 2021-08-31
set fit_date = 2021-02-23
set asymp = (0.5 0.75 1.0)
set fm_efficacy = (0.3 0.12 0.73)
set kids_susc_age = (10 1 5)

set in_as = 2
set in_fm = 3
set in_ks = 1

Rscript ./scripts/process_google_mobility_data_timeseries.R $forecast_date 1
Rscript ./scripts/process_facemask_community_trends.R $forecast_date 1
Rscript ./scripts/process_susceptibility_data.R

## Bogota
Rscript ./scripts/submit_fred_COL_calibration.R 11001 $reps $reps_per_job $fit_date $asymp[$in_as] $fm_efficacy[$in_fm] $kids_susc_age[$in_ks]
