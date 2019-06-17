library(tidyverse)
library(lubridate)
library(readxl)
library(mgcv)
library(readxl)

##
# bio data
biodat <- read_excel('raw/WOAC_allData _DS_forNina_nb.xlsx', sheet = 'PteropodData') %>% 
  select(-`Average size`, -`Standard deviation size`, -`Sample size for size measurement`, -`sample size for SEM imaging`, -`# of type I`, -`# of type II`, -`# of type III`, -`# of type II&III scores`) %>% 
  rename(
    yr = Year, 
    mo = Month, 
    station = Station, 
    abu = `Abundance (# individuals/m3)`,
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

# additional abundance data (inds/m3, same as above)
abuadd <- read_excel('raw/pteropod abundances 2014.xlsx') %>% 
  rename(
    date = Season, 
    station = Station,
    abu = `Pteropods/m^3`
  ) %>% 
  mutate(
    station = gsub('^P', '', station), 
    station = as.numeric(station),
    date = as.Date(date), 
    yr = year(date), 
    mo = month(date),
    mo = case_when(
      yr == 2014 & mo == 10 ~ 9,
      TRUE ~ mo
    ),
    dy = 15,
    mo = month(mo, label = T),
    mo = factor(mo, levels = c('Jul', 'Sep', 'Apr'), ordered = T),
    cohortyr = ifelse(mo %in% 'Apr', yr - 1, yr)
  ) %>% 
  unite('date', yr, mo, dy, sep = '-', remove = F) %>% 
  mutate(date = ymd(date)) %>% 
  select(-dy) %>%
  select(date, yr, cohortyr, mo, station, abu) %>% 
  na.omit %>% 
  group_by(date, yr, cohortyr, mo, station) %>% 
  summarise(abu = mean(abu))

# join with old abudat
abuadd <- biodat %>% 
  select(date, yr, cohortyr, mo, station, abu) %>% 
  na.omit %>% 
  bind_rows(abuadd, .)

# join with complete biodat, add abu pa
biodat <- biodat %>% 
  select(-abu) %>% 
  left_join(abuadd, by = c('date', 'yr', 'cohortyr', 'mo', 'station'))

# length data, to replace old lenght data in biodat
lendat <- read_excel('raw/length data Puget SOund.xlsx') %>% 
  select(-Whorls, -PT, -`Individual #`) %>% 
  rename(
    station = `P#`, 
    yr = Year, 
    mo = Month, 
    len = `Length (um)`
  ) %>% 
  mutate(
    station = case_when(
      station %in% '381' ~ '38',
      station %in% '38wrongname' ~ '38',
      station %in% '4O2' ~ '402',
      station %in% '28extra' ~ '28',
      station %in% '28resample' ~ '28',
      station %in% '28RS' ~ '28',
      T ~ station
    ), 
    mo = case_when(
      mo %in% 'May' ~ 'April',
      mo %in% c('November', 'October') ~ 'September',
      T ~ mo
    )
  ) %>% 
  filter(!station %in% 'RS') %>% 
  group_by(station, mo, yr) %>% 
  summarise(len = mean(len, na.rm = T)) %>% 
  ungroup %>% 
  mutate(
    mo = factor(mo, levels = c('July', 'September', 'April'), labels = c('Jul', 'Sep', 'Apr'), ordered = T),
    cohortyr = ifelse(mo %in% 'Apr', yr - 1, yr),
    dy = 15,
    station = as.numeric(station)
  ) %>% 
  unite('date', yr, mo, dy, remove = F, sep = '-') %>% 
  mutate(date = ymd(date)) %>% 
  dplyr::select(date, yr, cohortyr, mo, station, len)

# join lendat to biodat
biodat <- biodat %>% 
  left_join(lendat, by = c('date', 'yr', 'cohortyr', 'mo', 'station'))

# identify winter, summer cohorts by length in each season
biodat <- biodat %>% 
  mutate(
    cohortseas = case_when(
      mo %in% 'Jul' ~ 'summer',
      mo %in% 'Sep' & len >= 1200 ~ 'summer', 
      mo %in% 'Sep' & len < 1200 ~ 'winter',
      mo %in% 'Apr' & len >= 600 ~ 'winter',
      mo %in% 'Apr' & len < 600 ~ 'summer'
    )
  )

# join addl abundance data wiht new biodat
save(biodat, file = 'data/biodat.RData', compress = 'xz')

##
# in situ chemdat
# fix dates to month, year events to match with bio
# October in 2014 was averaged with september because of missing sep data at some stations
chmdatall <- read_excel('raw/WOAC_data_5-1-2018_for_Nina.xlsx', sheet = 'ALL_DATA', na = c('', '-999')) %>% 
  select(Date_collected, STATION_NO, LATITUDE_DEC, LONGITUDE_DEC, NISKIN_NO, `DEPTH (M)`, CTDTMP_DEG_C_ITS90, CTDSAL_PSS78,
         CTDOXY_UMOL_KG_ADJ, `NITRATE umol_kg`, `NITRITE umol_kg`, `AMMONIA umol_kg`, `PHOSPHATE umol_kg`, 
         `SILICATE umol_kg`, `Ph Total in situ`, `pCO2 uatm`, `CO3-- umol/kg`, `Omega Ar`, Revelle, `CHLA (ug/l)`) %>% 
  rename(
    date = Date_collected, 
    station = STATION_NO,
    lat = LATITUDE_DEC,
    lon = LONGITUDE_DEC,
    depth = `DEPTH (M)`,
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
    revelle = Revelle,
    chla = `CHLA (ug/l)`
  ) %>% 
  mutate(
    date = as.Date(date)
  ) %>% 
  gather('var', 'val', temp:chla) %>% 
  group_by(station) %>% 
  mutate(
    lat = mean(lat), 
    lon = mean(lon),
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
  )

# raw chemistry data for profile plots
chmdatraw <- chmdatall %>% 
  mutate(dy = 15) %>% 
  unite('date', yr, mo, dy, sep = '-', remove = F) %>% 
  mutate(
    date = ymd(date),
    cohortyr = ifelse(mo %in% 'Apr', yr - 1, yr)
  ) %>% 
  select(-dy) %>% 
  select(date, yr, cohortyr, mo, station, lon, lat, everything())

save(chmdatraw, file = 'data/chmdatraw.RData', compress = 'xz')

# summarized chemistry data for analyses
# fix dates to month, year events to match with bio
# October in 2014 was averaged with september because of missing sep data at some stations
chmdatsum <- chmdatall %>% 
  group_by(yr, mo, station, lon, lat, var) %>% 
  summarise(
    ave = mean(val, na.rm = T),
    min = min(val, na.rm = T),
    max = max(val, na.rm = T),
    std = sd(val, na.rm = T), 
    rng = diff(range(val, na.rm = T))
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

# percent depths undersaturated for aragonite, this is diff from ave, min, max so must do separately
# its gotta look the same as chemdatsum to join
perund <- chmdatall %>% 
  filter(var %in% 'ara') %>% 
  group_by(yr, mo, station, lon, lat, var) %>% 
  summarise(
    ave = sum(val < 1, na.rm = T) / length(val),
    min = ave,
    max = ave,
    std = ave,
    rng = ave,
    dlt = ave
  ) %>% 
  ungroup %>% 
  mutate(dy = 15) %>% 
  unite('date', yr, mo, dy, sep = '-', remove = F) %>% 
  mutate(
    date = ymd(date),
    cohortyr = ifelse(mo %in% 'Apr', yr - 1, yr),
    var = 'araund'
  ) %>% 
  select(-dy) %>% 
  select(date, yr, cohortyr, mo, station, lon, lat, everything())

# get delta values from surface to 40m
delt40 <- chmdatall %>% 
  group_by(yr, mo, station, var) %>% 
  nest %>% 
  mutate(
    dlt = purrr::map(data, function(x){
      
      # extrapolate env variable at zero depth
      mod <- loess(val ~ depth, x)
      rng <- range(x$depth, na.rm = T)
      est <- predict(mod, newdata = data.frame(depth = c(40, rng[1])))
      return(diff(est))
      
      # depdat <- seq(min(x$depth, na.rm = T), max(x$depth, na.rm = T), length = 100)
      # prddat <- predict(mod, newdata = data.frame(depth = depdat))
      # prddat <- data.frame(val = prddat, depth = depdat)
      # plot(-depth ~ val, x)
      # lines(prddat$val, -prddat$depth)
      
    })
  ) %>% 
  select(-data) %>% 
  unnest

chmdatsum <- chmdatsum %>% 
  full_join(delt40, by = c('yr' , 'mo', 'station', 'var')) %>% 
  bind_rows(perund) %>% 
  arrange(date, -station, var)

save(chmdatsum, file = 'data/chmdatsum.RData', compress = 'xz')

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
