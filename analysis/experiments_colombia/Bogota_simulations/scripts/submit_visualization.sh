#!/bin/csh -f
set reps = 4
set reps_per_job = 1
set forecast_date = 2021-06-30
set asymp = (0.5 1.0)
set fm_efficacy = (0.3 0.12 0.73)
set kids_susc_age = (10 20 5)

set in_as = 2
set in_fm = 3
set in_ks = 1


## Bogota
Rscript ./scripts/submit_fred_school_visualization.R 11001 $reps $reps_per_job $forecast_date $asymp[$in_as] $fm_efficacy[$in_fm] $kids_susc_age[$in_ks]
