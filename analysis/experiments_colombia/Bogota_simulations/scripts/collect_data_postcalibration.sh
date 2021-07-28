#!/bin/csh -f
## NOTE: Change this with a loop :)
set asymp = (0.5 0.75 1.0)
set fm_efficacy = (0.3 0.12 0.73)
set kids_susc_age = (10 1 5)

set in_as = 3
set in_fm = 3
set in_ks = 1

## Bogota
Rscript ./scripts/collect_data_COL_postcalibration.R 11001 $asymp[$in_as] $fm_efficacy[$in_fm] $kids_susc_age[$in_ks]
