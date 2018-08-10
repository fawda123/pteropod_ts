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
  )

# chemdat
chmdat <- read_excel('raw/WOAC_allData _DS_forNina_nb.xlsx', sheet = 'ChemData_100m', na = '-999') %>% 
  select(-`record no`, -CRUISE_ID, -Time_collected, -stn_niskin, -CTDPRS_DBAR) %>% 
  rename(
    date = Date_collected, 
    lon = LONGITUDE_DEC,
    lat = LATITUDE_DEC,
    station = STATION_NO,
    niskin = NISKIN_NO,
    depth = `DEPTH (M)`,
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
  )

save(chmdat, file = 'data/chmdat.RData', compress = 'xz')
