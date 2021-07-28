##===============================================================#
## Create report of Synthetic Population for Bogotá
## Author: Guido España
## Date: 2020/09/08
##===============================================================#
## Setup-------------
##===============================================================#
library(dplyr)
library(tidyverse)
library(rjson)

library(sf)
library(raster)
library(rgdal)
library(maptools)
library(osmdata)
library(RColorBrewer)
library(osmplotr)
library(RCurl)
library(data.table)
options(digits = 22,scipen = 999)


##===============================================================#
## 0. Data--------------
##===============================================================#
## Data
age_data = read_csv('../synthetic_populations/data/processed_data/popdata/bogota_population_data_sec.csv')
household_comp = read_csv('../synthetic_populations/data/processed_data/popdata/bogota_household_composition_sec.csv')
synth_pop = read_csv('../synthetic_populations/output/formatted_populations/colombia_11001/colombia_11001_synth_people.txt', col_types = cols(.default = "c"))
synth_houses = read_csv('../synthetic_populations/output/formatted_populations/colombia_11001/colombia_11001_synth_households.txt', col_types = cols(.default = "c"))
schools_df = read_csv('../synthetic_populations/output/formatted_populations/colombia_11001/colombia_11001_schools.txt')
workplaces_df = read_csv('../synthetic_populations/output/formatted_populations/colombia_11001/colombia_11001_workplaces.txt')

esc_shp = rgdal::readOGR('../synthetic_populations/data/raw_data/geodata/scat_shp/scat_shp.shp')
localidad_shp = rgdal::readOGR('../synthetic_populations/data/raw_data/geodata/localidades_bogota/poligonos-localidades.shp')
block_shp = rgdal::readOGR('../synthetic_populations/data/raw_data/geodata/vulnrb_data/VULNRB_IPMxMZ.shp')
upz_shp = rgdal::readOGR('../synthetic_populations/data/raw_data/geodata/UPZ_Bogota/UPla.shp')

##===============================================================#
## Process geo data--------------
##===============================================================#
house_coor = coordinates(synth_houses %>%
                         dplyr::select(latitude, longitude) %>%
                         mutate(latitude = as.numeric(latitude),longitude = as.numeric(longitude))) 
colnames(house_coor) = c("latitude", "longitude")
house_coor = as.data.frame(house_coor)
coordinates(house_coor) = ~  longitude + latitude
##house_coor = spTransform(house_coor, CRS(upz_shp))
##proj4string(house_coor) = proj4string(upz_shp)
crs(house_coor) = crs(upz_shp)

schools_coor = coordinates(schools_df %>%
                         dplyr::select(latitude, longitude) %>%
                         mutate(latitude = as.numeric(latitude),longitude = as.numeric(longitude))) 
colnames(schools_coor) = c("latitude", "longitude")
schools_coor = as.data.frame(schools_coor)
coordinates(schools_coor) = ~  longitude + latitude
##schools_coor = spTransform(schools_coor, crs(upz_shp))
##proj4string(schools_coor) = proj4string(upz_shp) 
crs(schools_coor) = crs(upz_shp)

workplace_coor = coordinates(workplaces_df %>%
                         dplyr::select(latitude, longitude) %>%
                         mutate(latitude = as.numeric(latitude),longitude = as.numeric(longitude))) 
colnames(workplace_coor) = c("latitude", "longitude")
workplace_coor = as.data.frame(workplace_coor)
coordinates(workplace_coor) = ~  longitude + latitude
##workplace_coor = spTransform(workplace_coor, crs(upz_shp))
##proj4string(workplace_coor) = proj4string(upz_shp)
crs(workplace_coor) = crs(upz_shp)

##===============================================================#
## 1. Population pyramid and household composition--------------
##===============================================================#
pop_data = age_data %>% group_by(AgeGroup, Gender) %>%
    summarize(Pop = sum(Pop)) %>%
    ungroup() %>%
    spread(key = Gender, value = Pop) %>%
    separate(AgeGroup, into = c('MinAge','MaxAge'), remove = F) %>%
    mutate(MinAge = as.numeric(MinAge)) %>%
    arrange(MinAge)

## Read synth population
brks = c(sort(pop_data$MinAge), 200)
lbls = sprintf("%d-%d",brks[1:(length(brks) - 1)], brks[2:length(brks)] - 1)
lbls[length(lbls)] = sprintf("%d-above", brks[length(brks) -1])


synth_age_df = synth_pop %>%
    mutate(AgeGroup = cut(as.numeric(age), breaks =brks, labels = lbls, include.lowest = T,right = F)) %>%
    group_by(AgeGroup) %>%
    summarize(SynthPop = n()) %>%
    ungroup()

work_pop  = synth_pop %>% filter(sp_work_id != "") %>%
    mutate(AgeGroup = cut(as.numeric(age), breaks =brks, labels = lbls, include.lowest = T,right = F)) %>%
    group_by(AgeGroup) %>%
    summarize(N = n()) %>%
    ungroup() %>%
    left_join(synth_age_df, by = 'AgeGroup') %>%
    mutate(PropWork = 100*(N / SynthPop ))

synth_pop_df = synth_pop %>%
    mutate(AgeGroup = cut(as.numeric(age), breaks =brks, labels = lbls, include.lowest = T,right = F)) %>%
    group_by(AgeGroup, sex) %>%
    summarize(SynthPop = n()) %>%
    ungroup() %>%
    mutate(AgeGroup = str_replace_all(AgeGroup, "_","-"),
           sex = ifelse(sex == "2", "SynthFemale", "SynthMale")) %>%
    spread(key = sex, value = SynthPop)
    
pop_data = pop_data %>% left_join(synth_pop_df, by = c("AgeGroup" = "AgeGroup"))


col_palette = c('gray','white')
jpeg('../figures/manuscript_figure_synth_pop_fit.jpeg', width=6.5,height=6.5, units="in", res = 300)
layout(
    rbind(c(1,2,3),c(4,4,5)), widths = c(rep(4,2),6), heights = rep(1,1)
)
par(mar = c(0.5,0.0,3.5,0.0), oma = c(1,1.0,1.0,1.5))
pplt = par("plt")
adjx = (0 - pplt[1]) / (pplt[2] - pplt[1])
bb = barplot(height = -pop_data$Female, horiz = T, col = col_palette[1], xlim = c(-max(pop_data$Female)*1.4,0), xaxt = "n")
axis(1, at = seq(from= 0, by = -100000, length.out = 5), labels =  seq(from= 0, by = -100000, length.out = 5), cex.axis = 1.0)
points( -pop_data$SynthFemale, bb, pch = 18)

text(cex=1.0, x=-max(pop_data$Female)*1.2, y=bb,pop_data$AgeGroup , xpd=TRUE)
mtext("A", side = 3, line = 0.5, outer = F, cex = 1.0, adj = adjx)

barplot(height = pop_data$Male, horiz = T, col = col_palette[2], xlim = c(0,max(pop_data$Male)*1.4), xaxt = "n")
axis(1, at = seq(from= 0, by = 100000, length.out = 5), labels =  str_replace(as.character(seq(from= 0, by = 100000, length.out = 5)), "^0", ""))
points(pop_data$SynthMale, bb, pch = 18)

##plot(0,0, lwd = 0,axes = F)
legend('top',legend=c("F","M"), fill = col_palette, cex = 1)

## Household composition
house_df = household_comp %>% group_by(PersonsHousehold) %>% summarize(NumHouses = sum(NumHouses)) %>%
    full_join(data.frame(PersonsHousehold = 1:max(household_comp$PersonsHousehold, NumHouses = 0)), by = 'PersonsHousehold') %>%
    replace_na(list(NumHouses = 0)) %>%
    arrange(PersonsHousehold)

par(mar = c(1.0,1.5,3.5,1.5))
bb = barplot(height = house_df$NumHouses, horiz = F, col = col_palette[1])
axis(1, at = bb, labels = house_df$PersonsHousehold )

synth_houses_df = synth_houses %>% group_by(hh_size) %>%
    summarize(SynthHouses = n()) %>%
    ungroup() %>%
    mutate(PersonsHousehold = as.numeric(hh_size)) %>%
    full_join(data.frame(PersonsHousehold = 1:max(household_comp$PersonsHousehold, NumHouses = 0)), by = 'PersonsHousehold') %>%
    replace_na(list(NumHouses = 0)) %>%
    arrange(PersonsHousehold)

points(bb, synth_houses_df$SynthHouses, pch = 18)
mtext("B", side = 3, line = 0.5, outer = F, cex = 1.0, adj = adjx)
mtext(text = "Number of houses", side = 2, line = 2, cex = 0.6)
mtext(text = "Persons per house", side = 1, line = 2, cex = 0.6)


## Total population
synth_pop$Zone = substr(synth_pop$stcotrbg, 6, 13)
zone_pop = synth_pop %>% group_by(Zone) %>% summarize(Pop = n()) %>%ungroup()
pop_shp = esc_shp
pop_shp@data = left_join(pop_shp@data, zone_pop, by = c("SCACODIGO"= "Zone"))

my_colors = brewer.pal(6,"YlGnBu")
my_colors = colorRampPalette(my_colors)(6)
pop_brk <- cut(pop_shp@data$Pop, breaks = c(0,1000,2000,5000,10000,50000,10000000))
my_colors = my_colors[as.numeric(pop_brk)]

plot(upz_shp[upz_shp@data$UPlTipo == 1,], lwd = 0.001)
plot(pop_shp, col = my_colors, bg = "gray", lwd = 0.1, add = T)
plot(localidad_shp, add = T, lwd = 0.5)
mtext("C", side = 3, line = 0.5,  outer = F, cex = 1.0, adj = adjx)

## HOUSEHOLDS LOCATION
plot(upz_shp[upz_shp@data$UPlTipo == 1,], lwd = 0.001)
plot(house_coor, col = "#bdbdbd10", pch = 15, cex = 0.05, add = T)

plot(workplace_coor, col = "#7570b320", pch = 16, cex = 0.1, add = T)
plot(schools_coor, col = "#1b9e7780", pch = 18, cex = 0.3, add = T)
plot(localidad_shp, add = T, lwd = 0.5)
mtext("D", side = 3, line = 0.5, outer = F, cex = 1.0, adj = adjx)
dev.off()


