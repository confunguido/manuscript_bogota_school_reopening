##===============================#
## Generate importations for
## COVID-19 Colombia
## Author: Guido España
## 2020
##===============================#
## Setup-------------
##===============================#
library(dplyr)
library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(RSocrata)
date_format = '20210511'

args = (commandArgs(TRUE))

if(length(args) >= 1){
    date_format = args[1]
}

##===============================#
## Read epidata-------------
##===============================#
## Read API
##date_format = format(lubridate::today() - 1, "%Y%m%d")
print(date_format)
##base_covid_df = read.socrata('https://www.datos.gov.co/resource/gt2j-8ykr.json')
##write_csv(base_covid_df, '../epidata/Covid_Colombia_cases_%s.csv',date_format)

covid_file = sprintf('../epidata/Covid_Colombia_cases_%s.csv',date_format)
reports_covid_df = read_csv(covid_file) %>%
    rename(DateDiagnosis = "Fecha de diagnóstico",
           SymptomOnset = "Fecha de inicio de síntomas",
           MunCode = "Código DIVIPOLA municipio",
           CaseType = "Tipo de contagio",
           DateDeath = "Fecha de muerte") %>%
    dplyr::select(MunCode, CaseType, DateDeath, DateDiagnosis, SymptomOnset) %>%
    mutate(DateDeath = str_replace(DateDeath, "-   -", "")) %>%
    filter(DateDiagnosis != as.Date('1899-12-31')) %>%
    mutate(DateDiagnosis = str_replace_all(DateDiagnosis, "0+:.*", ""),
           DateDeath = str_replace_all(DateDeath, "0+:.*", "")) %>%
    mutate(DateDiagnosis = parse_date_time(DateDiagnosis, orders = c("dmy", "dmy HMS")))%>%
    mutate(SymptomOnset = parse_date_time(SymptomOnset, orders = c("dmy", "dmy HMS")))%>%
    mutate(DateDeath = parse_date_time(DateDeath, orders = c("dmy", "dmy HMS")))%>%
    mutate(DateDiagnosis = lubridate::ymd(DateDiagnosis))%>%
    mutate(SymptomOnset = lubridate::ymd(SymptomOnset))%>%
    mutate(DateDeath = lubridate::ymd(DateDeath)) 

diagnose_df = reports_covid_df %>%
    group_by(MunCode, DateDiagnosis) %>%
    summarize(Cases = n()) %>%
    ungroup() 

deaths_df = reports_covid_df %>%
    group_by(MunCode, DateDeath) %>%
    summarize(Deaths = n()) %>%
    ungroup() %>%
    drop_na()

covid_format = left_join(
    diagnose_df,
    deaths_df,
    by = c("MunCode" = "MunCode", "DateDiagnosis" = "DateDeath")) %>%
    replace_na(list(Cases = 0, Deaths = 0)) %>%
    mutate(Date = DateDiagnosis) %>%
    dplyr::select(-DateDiagnosis) %>% drop_na()

covid_format_dpto = covid_format %>%
    mutate(DptoCode = substr(MunCode, 1, 2)) %>%
    group_by(DptoCode, Date) %>%
    summarize(Cases = sum(Cases, na.rm = T),
              Deaths = sum(Deaths, na.rm = T)) %>%
    ungroup()

write_csv(covid_format, '../experiments_colombia/input_files/COL_covid_death_data.csv')
write_csv(covid_format, '../output/COL_covid_death_data.csv')


##===================================#
## Age format----------
##===================================#
brk_ages = c(0, seq(from=10,by = 10, to = 80), 120)
brk_lbls = sprintf("ACF%d_%d", brk_ages[-length(brk_ages)], brk_ages[-1]) 
age_covid_df = read_csv(covid_file) %>%
    rename(DateDiagnosis = "Fecha de diagnóstico",
           SymptomOnset = "Fecha de inicio de síntomas",
           MunCode = "Código DIVIPOLA municipio",
           CaseType = "Tipo de contagio",
           DateDeath = "Fecha de muerte",
           Age = "Edad") %>%    
    dplyr::select(MunCode, CaseType, DateDeath, DateDiagnosis, SymptomOnset, Age) %>%    
    mutate(DateDeath = str_replace(DateDeath, "-   -", "")) %>%
    mutate(DateDiagnosis = str_replace_all(DateDiagnosis, "0+:.*", ""),
           DateDeath = str_replace_all(DateDeath, "0+:.*", "")) %>%
    mutate(DateDeath = parse_date_time(DateDeath, orders = c("dmy", "dmy HMS")))%>%
    mutate(DateDeath = lubridate::ymd(DateDeath)) %>%
    filter(!is.na(DateDeath)) %>%
    mutate(AgeGroup = as.character(cut(as.numeric(Age), brk_ages, brk_lbls,include.lowest = T, right = F))) %>%
    group_by(MunCode, DateDeath, AgeGroup) %>%
    summarize(Deaths = n()) %>%
    ungroup() %>%
    rename(Date = "DateDeath")
write_csv(age_covid_df, '../output/Age_COL_covid_data.csv')
write_csv(age_covid_df, '../experiments_colombia/input_files/Age_COL_covid_data.csv')

##===================================#
## Imports----------
##===================================#
imports_df = read_csv('../experiments_colombia/input_files/11001_imports_alternative.csv') %>%
    group_by(Date) %>%
    summarize(Imports = mean(Imports)) %>%
    ungroup() %>%
    mutate(Replicate = 1)

data_imports_df = reports_covid_df %>%
    filter(MunCode == 11001, CaseType == "Importado") %>%
    group_by(DateDiagnosis) %>%
    summarize(Imports = n()) %>%
    ungroup() %>%
    rename(Date = DateDiagnosis) %>%
    mutate(Date = Date - 3,
           Imports = Imports * 3.3,
           Replicate = 1)

combined_imports_df =  bind_rows(imports_df, data_imports_df) %>%
    group_by(Date) %>%
    summarize(Imports = max(Imports), Replicate = 1) %>%
    ungroup()

write_csv(combined_imports_df, '../experiments_colombia/input_files/11001_imports_combined.csv')
