## ---------------------------
##
## Script name: Data_prep
##
## Purpose of script: 
##
## Author: Gerrit Stahn
##
## Date Created: 2024-08-22
## Last Update: 2025-05-09
##
## Copyright (c) Gerrit Stahn, 2024
## Email: gerrit.stahn93.gs4@gmail.com
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

## -----------------------------------------------------------------------------
## Start
## -----------------------------------------------------------------------------

### Install packages (uncomment as required) ###
# install.packages("tidyverse")
# install.packages("gstat")
# install.packages("raster")
# install.packages("sf")
# install.packages("sp")
# install.packages("ggplot2")
# install.packages("readxl")

### Load add-on packages ### 
library(tidyverse)
library(sf)
library(raster)
library(gstat)
library(sp)
library(ggplot2)
library(readxl)

### clean start ###
rm(list = ls())

### set working directory and paths ###
setwd("/Users/apxww/Library/Mobile Documents/com~apple~CloudDocs/GitHub/shiny_ranking_german_districts")      
path_data <- "/Users/apxww/Library/Mobile Documents/com~apple~CloudDocs/GitHub/shiny_ranking_german_districts/Data/Data_2020orearlier"

## -----------------------------------------------------------------------------
## Data Air Quality
## -----------------------------------------------------------------------------

### Load Meta-Data for stations and districts ###
meta_stations <- read_delim(
  file=paste0(path_data, "/Umweltbundesamt/Bericht_EU_Meta_Stationen.csv"), 
  delim=";", locale=locale(encoding = "windows-1252"), skip=1) %>%
  mutate(station_start_date=ymd(station_start_date)) %>%
  mutate(station_end_date=ymd(station_end_date))  

long_lat <- meta_stations %>%
  dplyr::select(station_latitude_d, station_longitude_d, station_code)

aoi <- st_read(paste0(path_data, "/Kreis_data/VG250_KRS.shp")) %>%
  st_transform(4326)

aoi_sp <- as(aoi, "Spatial")

### NO2 data ###################################################################
no2 <- read_delim(
  file=paste0(path_data, "/Umweltbundesamt/Jahresbilanzen_Stickstoffdioxid_2020.csv"), 
  delim=";", locale=locale(encoding = "windows-1252"), name_repair="universal") %>%
  rename(station_code = Stationscode) %>%
  rename(NO2 = Jahresmittelwert.in.Âµg.mÂ.) %>%
  left_join(long_lat, by=join_by(station_code)) %>% 
  as.data.frame()

### NO2 data to sp###
no2_sf <- st_as_sf(no2, 
                   coords = c("station_longitude_d", "station_latitude_d"), 
                   crs = 4326) 
no2_sp <-  as(no2_sf, 'Spatial')

### Create grid ###
extent_aoi<- extent(aoi_sp)
extent_aoi

grid_aoi <- expand.grid(x = seq(from = round(extent_aoi@xmin),
                                to = round(extent_aoi@xmax),
                                by = 0.10000),
                        y = seq(from = round(extent_aoi@ymin),
                                to = round(extent_aoi@ymax),
                                by = 0.10000))

plot(grid_aoi)

coordinates(grid_aoi) <- ~x + y
proj4string(grid_aoi) <- proj4string(no2_sp)
head(grid_aoi)

gridded(grid_aoi) <- TRUE

# # Plot grid and borders and stations
# plot(grid_aoi,
#      main = paste("Air Stations in Germany\n and Interpolation Grid"),
#      col = "grey",
#      cex.main = 0.9)
# plot(aoi_sp, add = TRUE, border = "red")
# plot(no2_sp, add = TRUE, pch = 19, cex = 0.5, col = "blue")

idw_result <- gstat::idw(NO2 ~ 1, locations = no2_sp, newdata = grid_aoi, idp = 2)

### Create raster ### 
idw_raster <- raster(idw_result)

### Create final dataset ###
no2_kreis <- raster::extract(idw_raster, aoi_sp, fun = mean, na.rm = TRUE, df = TRUE)
no2_kreis$ID_K <- as.numeric(aoi$ARS)
no2_kreis$GF <- aoi$GF

no2_kreis <- no2_kreis %>%
  as_tibble() %>%
  filter(GF==4) %>%
  dplyr::select(!GF) %>% dplyr::select(!ID) %>%
  rename(no2_avg=var1.pred)

write_rds(no2_kreis, file = paste0(path_data, "/Manipulated/Umweltbundesamt/no2_kreis_2020.rds"))

### Particulates pm2.5 data ###################################################################
pm25 <- read_delim(
  file=paste0(path_data, "/Umweltbundesamt/Jahresbilanzen_Feinstaub_2020_PM2,5.csv"), 
  delim=";", locale=locale(encoding = "windows-1252"), name_repair="universal") %>%
  rename(station_code = Stationscode) %>%
  rename(pm25 = Jahresmittelwert.in.Âµg.mÂ.) %>%
  left_join(long_lat, by=join_by(station_code)) %>% 
  filter(!is.na(station_latitude_d)) %>%
  as.data.frame()

### pm25 data to sp###
pm25_sf <- st_as_sf(pm25, 
                   coords = c("station_longitude_d", "station_latitude_d"), 
                   crs = 4326) 
pm25_sp <-  as(pm25_sf, 'Spatial')

idw_result <- gstat::idw(pm25 ~ 1, locations = pm25_sp, newdata = grid_aoi, idp = 2)

### Create raster ### 
idw_raster <- raster(idw_result)

### Create final dataset ###
pm25_kreis <- raster::extract(idw_raster, aoi_sp, fun = mean, na.rm = TRUE, df = TRUE)
pm25_kreis$ID_K <- as.numeric(aoi$ARS)
pm25_kreis$GF <- aoi$GF

pm25_kreis <- pm25_kreis %>%
  as_tibble() %>%
  filter(GF==4) %>%
  dplyr::select(!GF) %>% dplyr::select(!ID) %>%
  rename(pm25_avg=var1.pred)

write_rds(pm25_kreis, file = paste0(path_data, "/Manipulated/Umweltbundesamt/pm25_kreis_2020.rds"))

### Particulates pm10 data ###################################################################
pm10 <- read_delim(
  file=paste0(path_data, "/Umweltbundesamt/Jahresbilanzen_Feinstaub_2020.csv"), 
  delim=";", locale=locale(encoding = "windows-1252"), name_repair="universal") %>%
  rename(station_code = Stationscode) %>%
  rename(pm10 = Jahresmittelwert.in.Âµg.mÂ.) %>%
  left_join(long_lat, by=join_by(station_code)) %>% 
  filter(!is.na(station_latitude_d)) %>%
  as.data.frame()

### pm10 data to sp###
pm10_sf <- st_as_sf(pm10, 
                    coords = c("station_longitude_d", "station_latitude_d"), 
                    crs = 4326) 
pm10_sp <-  as(pm10_sf, 'Spatial')

idw_result <- gstat::idw(pm10 ~ 1, locations = pm10_sp, newdata = grid_aoi, idp = 2)

### Create raster ### 
idw_raster <- raster(idw_result)

### Create final dataset ###
pm10_kreis <- raster::extract(idw_raster, aoi_sp, fun = mean, na.rm = TRUE, df = TRUE)
pm10_kreis$ID_K <- as.numeric(aoi$ARS)
pm10_kreis$GF <- aoi$GF

pm10_kreis <- pm10_kreis %>%
  as_tibble() %>%
  filter(GF==4) %>%
  dplyr::select(!GF) %>% dplyr::select(!ID) %>%
  rename(pm10_avg=var1.pred)

write_rds(pm10_kreis, file = paste0(path_data, "/Manipulated/Umweltbundesamt/pm10_kreis_2020.rds"))

### CO data ###################################################################
co <- read_csv2(
  file=paste0(path_data, "/Umweltbundesamt/Jahresbilanzen_Kohlenmonoxid_2020.csv"), 
  locale=locale(encoding = "windows-1252"), name_repair="universal") %>%
  rename(station_code = Stationscode) %>%
  rename(co = Jahresmittelwert.in.mg.mÂ.) %>%
  left_join(long_lat, by=join_by(station_code)) %>% 
  filter(!is.na(station_latitude_d)) %>%
  as.data.frame()

### co data to sp###
co_sf <- st_as_sf(co, 
                    coords = c("station_longitude_d", "station_latitude_d"), 
                    crs = 4326) 
co_sp <-  as(co_sf, 'Spatial')

idw_result <- gstat::idw(co ~ 1, locations = co_sp, newdata = grid_aoi, idp = 2)

### Create raster ### 
idw_raster <- raster(idw_result)

### Create final dataset ###
co_kreis <- raster::extract(idw_raster, aoi_sp, fun = mean, na.rm = TRUE, df = TRUE)
co_kreis$ID_K <- as.numeric(aoi$ARS)
co_kreis$GF <- aoi$GF

co_kreis <- co_kreis %>%
  as_tibble() %>%
  filter(GF==4) %>%
  dplyr::select(!GF) %>% dplyr::select(!ID) %>%
  rename(co_avg=var1.pred)

write_rds(co_kreis, file = paste0(path_data, "/Manipulated/Umweltbundesamt/co_kreis_2020.rds"))

### SO2 data ###################################################################
so2 <- read_csv2(
  file=paste0(path_data, "/Umweltbundesamt/Jahresbilanzen_Schwefeldioxid_2020.csv"), 
  locale=locale(encoding = "windows-1252"), name_repair="universal") %>%
  rename(station_code = Stationscode) %>%
  rename(so2 = Jahresmittelwert.in.Âµg.mÂ.) %>%
  left_join(long_lat, by=join_by(station_code)) %>% 
  filter(!is.na(station_latitude_d)) %>%
  as.data.frame()

### so2 data to sp###
so2_sf <- st_as_sf(so2, 
                  coords = c("station_longitude_d", "station_latitude_d"), 
                  crs = 4326) 
so2_sp <-  as(so2_sf, 'Spatial')

idw_result <- gstat::idw(so2 ~ 1, locations = so2_sp, newdata = grid_aoi, idp = 2)

### Create raster ### 
idw_raster <- raster(idw_result)

### Create final dataset ###
so2_kreis <- raster::extract(idw_raster, aoi_sp, fun = mean, na.rm = TRUE, df = TRUE)
so2_kreis$ID_K <- as.numeric(aoi$ARS)
so2_kreis$GF <- aoi$GF

so2_kreis <- so2_kreis %>%
  as_tibble() %>%
  filter(GF==4) %>%
  dplyr::select(!GF) %>% dplyr::select(!ID) %>%
  rename(so2_avg=var1.pred)

write_rds(so2_kreis, file = paste0(path_data, "/Manipulated/Umweltbundesamt/so2_kreis_2020.rds"))

### Pb data ###################################################################
pb <- read_csv2(
  file=paste0(path_data, "/Umweltbundesamt/Jahresbilanzen_Blei im Feinstaub_2020.csv"), 
  locale=locale(encoding = "windows-1252"), name_repair="universal") %>%
  rename(station_code = Stationscode) %>%
  rename(pb = Jahresmittelwert.in.ng.mÂ.) %>%
  left_join(long_lat, by=join_by(station_code)) %>% 
  filter(!is.na(station_latitude_d)) %>%
  as.data.frame()

### pb data to sp###
pb_sf <- st_as_sf(pb, 
                   coords = c("station_longitude_d", "station_latitude_d"), 
                   crs = 4326) 
pb_sp <-  as(pb_sf, 'Spatial')

idw_result <- gstat::idw(pb ~ 1, locations = pb_sp, newdata = grid_aoi, idp = 2)

### Create raster ### 
idw_raster <- raster(idw_result)

### Create final dataset ###
pb_kreis <- raster::extract(idw_raster, aoi_sp, fun = mean, na.rm = TRUE, df = TRUE)
pb_kreis$ID_K <- as.numeric(aoi$ARS)
pb_kreis$GF <- aoi$GF

pb_kreis <- pb_kreis %>%
  as_tibble() %>%
  filter(GF==4) %>%
  dplyr::select(!GF) %>% dplyr::select(!ID) %>%
  rename(pb_avg=var1.pred)

write_rds(pb_kreis, file = paste0(path_data, "/Manipulated/Umweltbundesamt/pb_kreis_2020.rds"))

### Merge #####################################################################
no2_kreis <- read_rds(paste0(path_data, "/Manipulated/Umweltbundesamt/no2_kreis_2020.rds"))
pm25_kreis <- read_rds(paste0(path_data, "/Manipulated/Umweltbundesamt/pm25_kreis_2020.rds"))
pm10_kreis <- read_rds(paste0(path_data, "/Manipulated/Umweltbundesamt/pm10_kreis_2020.rds"))
co_kreis <- read_rds(paste0(path_data, "/Manipulated/Umweltbundesamt/co_kreis_2020.rds"))
so2_kreis <- read_rds(paste0(path_data, "/Manipulated/Umweltbundesamt/so2_kreis_2020.rds"))
pb_kreis <- read_rds(paste0(path_data, "/Manipulated/Umweltbundesamt/pb_kreis_2020.rds"))

pollution <- no2_kreis %>%
  left_join(pm25_kreis, join_by(ID_K)) %>% 
  left_join(pm10_kreis, join_by(ID_K)) %>% 
  left_join(co_kreis, join_by(ID_K)) %>% 
  left_join(so2_kreis, join_by(ID_K)) %>% 
  left_join(pb_kreis, join_by(ID_K)) %>%
  mutate(Year=2020)

# Average for ID_K = 16056 and ID_K= 16063, since both districts merged in 2022
poll_merge <- pollution %>%
  filter(ID_K==16056 | ID_K==16063) %>%
  summarise_all(mean) %>%
  mutate(ID_K=16063)

pollution_400 <- pollution %>%
  filter(!(ID_K==16056 | ID_K==16063)) %>%
  bind_rows(.,poll_merge)

write_rds(pollution_400, file = paste0(path_data, "/Manipulated/Umweltbundesamt/pollution_400_2020.rds"))

## -----------------------------------------------------------------------------
## Data INKAR
## -----------------------------------------------------------------------------

# clean start 
rm(list=setdiff(ls(), c("path_data", "pollution_400", lsf.str())))

### Load data ###

# Get col names
col_names <- names(read_csv(paste0(path_data, "/INKAR/Tabelle Abfrage%3A%20Indikator%20WO%202024.csv"), n_max = 0)) 
col_names <- data.frame(col_names) %>%
  separate_rows(col_names, sep=";") %>%
  pull()

inkar_ref <- read_excel(paste0(path_data, "/INKAR/inkar_ref_2020.xlsx"), skip=1) %>%
  rename(Kennziffer= "Kreise 31.12.2017") %>% rename(LK_ID= "Kreisfreie Stadt=1/Landkreis=2 - Ebene Kreise") %>%
  mutate(Kennziffer= Kennziffer/1000) %>%
  dplyr::select(Kennziffer, LK_ID) %>%
  filter(!Kennziffer==16056)

inkar<-read_csv2(
  paste0(path_data, "/INKAR/Tabelle Abfrage%3A%20Indikator%20WO%202024.csv"), 
  col_names = col_names, skip = 2, name_repair = "universal") %>%
  mutate(Kennziffer=as.numeric(Kennziffer)) %>%
  left_join(inkar_ref, by="Kennziffer") %>%
  relocate(Kennziffer, LK_ID)

# Change col names 
colnames(inkar) <- c('ID_K', "LK_ID", 'Region', 'Aggregate', 'Population', 'Workers', 'GDP.per.Capita', 
                    'Expert.Jobs', 'Rent.Prices', 'New.Housing.per.Capita', 
                    'Tertiary.Employees', 'Creative.Industry.Employees', 
                    'Elderly.Population', 'Migration.Balance', 'Median.Income', 
                    'Purchasing.Power', 'Recreation.Area.per.Capita', 'Forest.Area', 
                    'Water.Area', 'Daycare.Staff', 'Investment.Allocations', 
                    'Population.Density', 'Highway.Access', 'Airport.Access', 
                    'Rail.Access', 'Supermarket.Access', 'Pharmacy.Access', 
                    'Broadband.100Mbps', 'EV.Cars', 'Large.Companies', 
                    'Academic.Workers', 'Renewable.Buildings', 'Renewable.Housing', 
                    'Public.Transport.Access', 'EV.Charging.Stations', 
                    'Traffic.Accidents', 'Construction.Backlog', 'Child.Poverty', 
                    'Daycare', 'Inclusive.Daycare', 'Women.Council.Members', 
                    'Apprentices', 'Students.18.to.25', 'Foreign.Worker.Rate', 
                    'Doctors', 'Broadband.1000Mbps')

write_rds(inkar, file = paste0(path_data, "/Manipulated/INKAR/inkar_400_2020.rds"))

## -----------------------------------------------------------------------------
## Data IOER
## -----------------------------------------------------------------------------

# clean start 
rm(list=setdiff(ls(), c("path_data", "pollution_400", "inkar", lsf.str())))

### Load data ###

# Settlement Area in Flood Zone
R05RT <- read_csv2(paste0(path_data, "/IOER_Monitor/R05RT__2020.csv"), skip=6,
                   trim_ws = T, col_names = c("Lfd", "ID_K", "K", 
                                              "Settlement.Area.in.Flood.Zone")) %>%
  filter(!(K=="Deutschland")) %>%
  mutate(ID_K=as.numeric(ID_K)) %>%
  dplyr::select(ID_K, Settlement.Area.in.Flood.Zone)


# Sealed Area per Capita
B22MT <- read_csv2(paste0(path_data, "/IOER_Monitor/B22MT__2018.csv"), skip=6,
                   trim_ws = T, col_names = c("Lfd", "ID_K", "K", 
                                              "Sealed.Area.per.Capita")) %>%
  filter(!(K=="Deutschland")) %>%
  mutate(ID_K=as.numeric(ID_K)) %>%
  dplyr::select(ID_K, Sealed.Area.per.Capita)

ground <- R05RT %>%
  left_join(B22MT, by="ID_K")

write_rds(ground, file = paste0(path_data, "/Manipulated/IOER_Monitor/ground_400_2020.rds"))

## -----------------------------------------------------------------------------
## Merge all
## -----------------------------------------------------------------------------

rm(list=setdiff(ls(), c("path_data", "pollution_400", "inkar", "ground", lsf.str())))

# Uncomment of necessary
pollution_400 <- read_rds(file = paste0(path_data, "/Manipulated/Umweltbundesamt/pollution_400_2020.rds"))
inkar <- read_rds(file = paste0(path_data, "/Manipulated/INKAR/inkar_400_2020.rds"))
ground <- read_rds(file = paste0(path_data, "/Manipulated/IOER_Monitor/ground_400_2020.rds"))

data <- pollution_400 %>%
  left_join(inkar, by="ID_K") %>%
  left_join(ground, by="ID_K") %>%
  relocate(Year, ID_K, LK_ID, Aggregate, Region)

write_rds(data, file = paste0(path_data, "/Manipulated/data_2020.rds"))

### Normalize data ###
n <- length(names(data))
normalized_data <- data %>%
  mutate(across(6:55, ~ (. - min(.)) / (max(.) - min(.))))

write_rds(normalized_data, file = paste0(path_data, "/Manipulated/normalized_data_2020.rds"))
write_csv(normalized_data, file = paste0(path_data, "/Manipulated/normalized_data_2020.csv"))

## -----------------------------------------------------------------------------
## Split LK and SK
## -----------------------------------------------------------------------------

### SK ###
data_SK <- data %>%
  filter(LK_ID==1)

write_rds(data_SK , file = paste0(path_data, "/Manipulated/data_SK_2020.rds"))
write_csv(data_SK , file = paste0(path_data, "/Manipulated/data_SK_2020.csv"))

n <- length(names(data_SK))
normalized_data_SK <- data_SK %>%
  mutate(across(6:n, ~ (. - min(.)) / (max(.) - min(.))))

write_rds(normalized_data_SK, file = paste0(path_data, "/Manipulated/normalized_data_SK_2020.rds"))
write_csv(normalized_data_SK, file = paste0(path_data, "/Manipulated/normalized_data_SK_2020.csv"))

### LK ###
data_LK <- data %>%
  filter(LK_ID==2)

write_rds(data_LK , file = paste0(path_data, "/Manipulated/data_LK_2020.rds"))
write_csv(data_LK , file = paste0(path_data, "/Manipulated/data_LK_2020.csv"))

n <- length(names(data_LK))

normalized_data_LK <- data_LK %>%
  mutate(across(6:n, ~ (. - min(.)) / (max(.) - min(.))))

write_rds(normalized_data_LK, file = paste0(path_data, "/Manipulated/normalized_data_LK_2020.rds"))
write_csv(normalized_data_LK, file = paste0(path_data, "/Manipulated/normalized_data_LK_2020.csv"))

## -----------------------------------------------------------------------------