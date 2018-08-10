library(tidyverse)
library(readxl)

# bio data
biodat <- read_excel('raw/WOAC_allData _DS_forNina_nb.xlsx', sheet = 'PteropodData') %>% 
  select(-`Sample size for size measurement`, -`sample size for SEM imaging`, -`# of type I`, -`# of type II`, -`# of type III`, -`# of type II&III scores`) %>% 
  rename(
    year = Year, 
    mo = Month, 
    station = Station, 
    abu = `Abundance (# individuals/m3)`,
    avesz = `Average size`,
    stdsz = `Standard deviation size`, 
    typ1 = `% type I`, 
    typ2 = `% type II`,
    typ3 = `% type III`,
    typ23 = `% type II&III`
  ) %>% 
  mutate(
    station = gsub('^P', '', station), 
    station = as.numeric(station)
  )

save(biodat, file = 'data/biodat.RData', compress = 'xz')

# chemdat
chmdat <- read_excel('raw/WOAC_allData _DS_forNina_nb.xlsx', sheet = 'ChemData_100m', na = c('', '-999')) %>% 
  select(-`record no`, -CRUISE_ID, -Time_collected, -LONGITUDE_DEC, -LATITUDE_DEC, -stn_niskin, -CTDPRS_DBAR, -`DEPTH (M)`) %>% 
  rename(
    date = Date_collected, 
    station = STATION_NO,
    niskin = NISKIN_NO,
    oxy = CTDOXY_UMOL_KG_ADJ,
    nitrate = `NITRATE umol_kg`,
    nitrite = `NITRITE umol_kg`, 
    ammonia = `AMMONIA umol_kg`,
    phosphate = `PHOSPHATE umol_kg`, 
    silicate = `SILICATE umol_kg`, 
    ph = `Ph Total in situ`, 
    pco2 = `pCO2 uatm`, 
    co3 = `CO3-- umol/kg`,
    ara = `Omega Ar`,
    revelle = Revelle
  ) %>% 
  mutate(
    date = as.Date(date)
  ) %>% 
  gather('var', 'val', oxy:revelle) %>% 
  group_by(date, station, var) %>% 
  summarise(
    avev = mean(val, na.rm = T),
    minv = min(val, na.rm = T),
    maxv = max(val, na.rm = T)
  ) 

save(chmdat, file = 'data/chmdat.RData', compress = 'xz')
