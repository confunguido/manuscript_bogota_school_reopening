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

date_format = '20200916'

args = (commandArgs(TRUE))

if(length(args) >= 1){
    date_format = args[1]
}

##===============================#
## Read epidata-------------
##===============================#
lubridate::day(as.Date(date_format, format = "%Y%m%d"))
covid_file = sprintf('../epidata/positivos_%02d_%02d.xlsx',
                     lubridate::day(as.Date(date_format, format = "%Y%m%d")),
                     lubridate::month(as.Date(date_format, format = "%Y%m%d")))

reports_covid_df =   readxl::read_xlsx(covid_file, skip = 0, sheet = 'Hoja1') %>%
    rename(Age = edad, SymptomOnset = "Fecha de Inicio de Síntomas",
           DateDeath = "Fecha de Muerte",
           Diagnosis = "Estado",
           DateDiagnosis = "Fecha de Consulta",
           Localidad = "Numero de localidad") %>%
    dplyr::select(Age, SymptomOnset, DateDeath, DateDiagnosis,Localidad, Diagnosis) %>%
    mutate(MunCode = "11001",
           SymptomOnset = as.Date(SymptomOnset, format = "%Y-%m-%d"),
           DateDeath =  as.Date(DateDeath, format = "%Y-%m-%d"),
           DateDiagnosis =  as.Date(DateDiagnosis, format = "%Y-%m-%d"),
           Localidad = as.numeric(Localidad)) %>%
    filter(Localidad < 20)

reports_covid_df$SymptomOnset[is.na(reports_covid_df$SymptomOnset)] = reports_covid_df$DateDiagnosis[is.na(reports_covid_df$SymptomOnset)]

diagnose_df = reports_covid_df %>%
    group_by(MunCode, Localidad, SymptomOnset) %>%
    summarize(Cases = n()) %>%
    ungroup() 

deaths_df = reports_covid_df %>%
    filter(Diagnosis == "Fallecido") %>%
    group_by(MunCode, Localidad, DateDeath) %>%
    summarize(Deaths = n()) %>%
    ungroup() %>%
    mutate(DateDeath = as.Date(DateDeath, format = "%Y-%m-%d")) %>%
    drop_na()

covid_format = left_join(
    diagnose_df,
    deaths_df,
    by = c("MunCode" = "MunCode", "SymptomOnset" = "DateDeath", "Localidad" = "Localidad")) %>%
    replace_na(list(Cases = 0, Deaths = 0)) %>%
    rename(Date = SymptomOnset) %>%
    drop_na()

write_csv(covid_format, '../experiments_colombia/input_files/BOG_covid_death_data.csv')
write_csv(covid_format, '../output/BOG_covid_death_data.csv')

##===================================#
## Age format----------
##===================================#
brk_ages = c(0, seq(from=10,by = 10, to = 80), 120)
brk_lbls = sprintf("ACF%d_%d", brk_ages[-length(brk_ages)], brk_ages[-1])
age_covid_df = reports_covid_df %>%
    filter(!is.na(DateDeath), Diagnosis == "Fallecido") %>%
    mutate(AgeGroup = as.character(cut(as.numeric(Age), brk_ages, brk_lbls,include.lowest = T, right = F)),
           Date = DateDeath) %>% 
    group_by(MunCode, Date, AgeGroup) %>%
    summarize(Deaths = n()) %>%
    ungroup()

write_csv(age_covid_df, '../output/Age_BOG_covid_data.csv')
write_csv(age_covid_df, '../experiments_colombia/input_files/Age_BOG_covid_data.csv')

brk_lbls = sprintf("ACs%d_%d", brk_ages[-length(brk_ages)], brk_ages[-1])
age_cases_df = reports_covid_df %>%
    filter(!is.na(SymptomOnset)) %>%
    mutate(AgeGroup = as.character(cut(as.numeric(Age), brk_ages, brk_lbls,include.lowest = T, right = F)),
           Date = SymptomOnset) %>% 
    group_by(MunCode, Date, AgeGroup) %>%
    summarize(Cs = n()) %>%
    ungroup() 
write_csv(age_cases_df, '../output/Age_BOG_covid_cases_data.csv')
write_csv(age_cases_df, '../experiments_colombia/input_files/Age_BOG_covid_cases_data.csv')


##===================================#
## Save imports----------
##===================================#
reports_covid_df = readxl::read_xlsx(covid_file, skip = 0, sheet = 'Hoja1')%>%
    dplyr::rename(Origen = "fuente o tipo de contagio") %>%
    rename(Age = edad, SymptomOnset = "Fecha de Inicio de Síntomas",
           DateDeath = "Fecha de Muerte",
           DateDiagnosis = "Fecha de Consulta",
           Localidad = "Numero de localidad") %>%
    dplyr::select(Age, Origen, SymptomOnset, DateDeath, DateDiagnosis,Localidad) %>%
    mutate(MunCode = "11001",
           SymptomOnset = as.Date(SymptomOnset, format = "%Y-%m-%d"),
           DateDeath =  as.Date(DateDeath, format = "%Y-%m-%d"),
           DateDiagnosis =  as.Date(DateDiagnosis, format = "%Y-%m-%d"),
           Localidad = as.numeric(Localidad)) 

imports_df = read_csv('../experiments_colombia/input_files/11001_imports_alternative.csv') %>%
    group_by(Date) %>%
    summarize(Imports = mean(Imports)) %>%
    ungroup() %>%
    mutate(Replicate = 1)

data_imports_df = reports_covid_df %>%
    filter(Origen == "Importado") %>%
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

##===================================#
## Age/localidad format----------
##===================================#
brk_ages = c(0, seq(from=10,by = 10, to = 80), 120)
brk_lbls = sprintf("A%d_%d", brk_ages[-length(brk_ages)], brk_ages[-1])

age_covid_df = reports_covid_df %>%
    filter(!is.na(DateDeath), Diagnosis == "Fallecido") %>%
    mutate(AgeGroup = as.character(cut(as.numeric(Age), brk_ages, brk_lbls,include.lowest = T, right = F)),
           Date = DateDeath) %>% 
    group_by(MunCode, Date, AgeGroup, Localidad) %>%
    summarize(Deaths = n()) %>%
    ungroup()

age_cases_df = reports_covid_df %>%
    filter(!is.na(SymptomOnset)) %>%
    mutate(AgeGroup = as.character(cut(as.numeric(Age), brk_ages, brk_lbls,include.lowest = T, right = F)),
           Date = SymptomOnset) %>% 
    group_by(MunCode, Date, AgeGroup, Localidad) %>%
    summarize(Cs = n()) %>%
    ungroup() %>%
    left_join(age_covid_df, by = c("MunCode","Date","AgeGroup","Localidad")) %>%
    replace_na(list(Cs =  0, Deaths = 0))

write_csv(age_cases_df, '../output/Age_Localidad_BOG_covid_cases_data.csv')
write_csv(age_cases_df, '../experiments_colombia/input_files/Age_Localidad_BOG_covid_cases_data.csv')
