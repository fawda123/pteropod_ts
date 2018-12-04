library(tidyverse)
library(lubridate)
library(readxl)

##
# bio data
biodat <- read_excel('raw/WOAC_allData _DS_forNina_nb.xlsx', sheet = 'PteropodData') %>% 
  select(-`Sample size for size measurement`, -`sample size for SEM imaging`, -`# of type I`, -`# of type II`, -`# of type III`, -`# of type II&III scores`) %>% 
  rename(
    yr = Year, 
    mo = Month, 
    station = Station, 
    abu = `Abundance (# individuals/m3)`,
    avesz = `Average size`,
    stdsz = `Standard deviation size`, 
    typ1 = `% type I`, 
    typ2 = `% type II`,
    typ3 = `% type III`
  ) %>% 
  mutate(
    station = gsub('^P', '', station), 
    station = as.numeric(station),
    mo = month(mo, label = T),
    mo = factor(mo, levels = c('Jul', 'Sep', 'Apr'), ordered = T),
    dy = 15,
    cohortyr = ifelse(mo %in% 'Apr', yr - 1, yr)
  ) %>% 
  unite('date', yr, mo, dy, sep = '-', remove = F) %>% 
  mutate(date = ymd(date)) %>% 
  select(-dy) %>%
  select(date, yr, cohortyr, everything())

save(biodat, file = 'data/biodat.RData', compress = 'xz')

##
# in situ chemdat
chmdat <- read_excel('raw/WOAC_data_5-1-2018_for_Nina.xlsx', sheet = 'ALL_DATA', na = c('', '-999')) %>% 
  select(Date_collected, STATION_NO, LATITUDE_DEC, LONGITUDE_DEC, NISKIN_NO, CTDTMP_DEG_C_ITS90, CTDSAL_PSS78,
         CTDOXY_UMOL_KG_ADJ, `NITRATE umol_kg`, `NITRITE umol_kg`, `AMMONIA umol_kg`, `PHOSPHATE umol_kg`, 
         `SILICATE umol_kg`, `Ph Total in situ`, `pCO2 uatm`, `CO3-- umol/kg`, `Omega Ar`, Revelle) %>% 
  rename(
    date = Date_collected, 
    station = STATION_NO,
    lat = LATITUDE_DEC,
    lon = LONGITUDE_DEC,
    niskin = NISKIN_NO,
    temp = CTDTMP_DEG_C_ITS90,
    sal = CTDSAL_PSS78,
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
  gather('var', 'val', temp:revelle) %>% 
  group_by(station) %>% 
  mutate(
    lat = mean(lat), 
    lon = mean(lon)
  ) 

# fix dates to month, year events to match with bio
# October in 2014 was averaged with september because of missing sep data at some stations
chmdat <- chmdat %>% 
  mutate(
    yr = year(date), 
    mo = month(date)
    ) %>% 
  filter(mo %in% c(4, 7, 9, 10)) %>%
  mutate(
    mo = case_when(
      yr == 2014 & mo == 10 ~ 9,
      TRUE ~ mo
    ),
    mo = month(mo, label = T), 
    mo = factor(mo, levels = c('Jul', 'Sep', 'Apr'), ordered = T)
  ) %>% 
  group_by(yr, mo, station, lon, lat, var) %>% 
  summarise(
    ave = mean(val, na.rm = T),
    min = min(val, na.rm = T),
    max = max(val, na.rm = T),
    std = sd(val, na.rm = T)
  ) %>% 
  ungroup %>% 
  mutate(dy = 15) %>% 
  unite('date', yr, mo, dy, sep = '-', remove = F) %>% 
  mutate(
    date = ymd(date),
    cohortyr = ifelse(mo %in% 'Apr', yr - 1, yr)
    ) %>% 
  select(-dy) %>% 
  select(date, yr, cohortyr, mo, station, lon, lat, everything())

save(chmdat, file = 'data/chmdat.RData', compress = 'xz')

##
# simulated chemistry, 2008 
# only aragonite is selected, other vars avaialable
# sims run from Jan. 6 12am to Dec. 30 6pm, six hour time step
simdat <- list.files('raw/', '^Time', full.names = T) %>%
  enframe %>%
  mutate(station = gsub('^.*TimeSeries_P([0-9]+)_.*$', '\\1', value)) %>%
  select(-name) %>%
  mutate(
    simdat = map(value, read_excel)
  ) %>%
  unnest %>%
  rename(
    datetime = DateTime,
    ara = OmegaAr
  ) %>%
  select(station, datetime, ara) %>%
  na.omit # empty values at end of time series for each station must be removed

save(simdat, file = 'data/simdat.RData', compress = 'xz')

##
# processing mooring data from NOAA
# https://www.nodc.noaa.gov/ocads/oceans/Moorings/Dabob.html
# https://www.nodc.noaa.gov/ocads/oceans/Moorings/Twanoh.html

mordat <- list.files('raw/', pattern = '^Dabob|^Twanoh', full.names = T) %>% 
  tibble(fl = .) %>%
  mutate(
    dat = map(fl, function(x){

      out <- x %>% 
        read_csv %>%
        select(matches('CO2.*SW.*wet|Latitude|Longitude|Date|Time')) %>% 
        rename(CO2 = matches('CO2'))
      
      return(out)
      
    })
  ) %>% 
  unnest %>% 
  unite('datetime', Date, Time, sep = ' ') %>% 
  mutate(
    fl = gsub('^raw/|_.*$', '', fl),
    datetime = dmy_hms(datetime, tz = 'UTC'),
    CO2 = ifelse(CO2 < 0, NA, CO2)
  ) %>% 
  arrange(fl, datetime)

# ggplot(mordat, aes(x = datetime, y = CO2, colour = fl)) + 
#   geom_line() +
#   geom_point()
