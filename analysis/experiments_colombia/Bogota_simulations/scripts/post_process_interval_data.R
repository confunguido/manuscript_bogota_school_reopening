#!/usr/bin/env Rscript
##=======================================#
## Author: Guido Espana
## Post-processes the output of FRED jobs
##=======================================#
## Setup---------------
##=======================================#
library(tidyverse)
library(fredtools)

fred_key = "test_serial"
fred_n = 1
    
args = (commandArgs(TRUE))
if(length(args) >= 1){
    fred_key = args[1]
    if(length(args) >= 2){
        fred_n = as.numeric(args[2])
    }
}

##=======================================#
## calculate R0---------------
##=======================================#
a = fredtools::calculate_fred_R0(fred_key, fred_n)

b = fredtools::calculate_intervals(fred_key, fred_n)
write_csv(x=b$periods,path='./output/fred_test_periods.csv')
write_csv(x=b$intervals,path='./output/fred_test_intervals.csv')
