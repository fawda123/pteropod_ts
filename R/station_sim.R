library(tidyverse)
library(sf)
library(ggmap)
library(mapview)
library(lubridate)

data(biodat)
data(chmdat)
data(simdat)

locs <- chmdat %>% 
  select(station, lon, lat) %>% 
  unique 

# get the extent
dat_ext <- make_bbox(locs$lon, locs$lat, f = 0.05)

# get the base map using the extent
bsmap <-  get_map(location = dat_ext, maptype = 'satellite', zoom = 9)


# plot the basemap
ggmap(bsmap) + 
  geom_point(data = locs, aes(x = lon, y = lat), colour = 'tomato1', size = 8) +
  geom_text(data = locs, aes(x = lon, y = lat, label = station))
  

simdatts <- simdat %>%
  group_by(station) %>% 
  nest %>% 
  mutate(
    datts = map(data, function(x){
      
      ts(x$ara, start = 2008, frequency = nrow(x))
      
    })
  )
  spread(station, ara)
  
###
# in situ chemdat
toplo <- read_excel('raw/WOAC_data_5-1-2018_for_Nina.xlsx', sheet = 'ALL_DATA', na = c('', '-999')) %>% 
  select(Date_collected, STATION_NO, LATITUDE_DEC, LONGITUDE_DEC, NISKIN_NO, CTDTMP_DEG_C_ITS90, 
         CTDSAL_PSS78, `Omega Ar`) %>% 
  rename(
    date = Date_collected, 
    station = STATION_NO,
    lat = LATITUDE_DEC,
    lon = LONGITUDE_DEC,
    niskin = NISKIN_NO,
    temp = CTDTMP_DEG_C_ITS90,
    sal = CTDSAL_PSS78,
    ara = `Omega Ar`
  ) %>% 
  mutate(
    date = as.Date(date)
  ) %>% 
  gather('var', 'val', temp:ara) %>% 
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
    mo = month(mo, label = T)
  )

ggplot(toplo, aes(x = factor(station), y = val, fill = factor(yr))) + 
  geom_boxplot(position = 'dodge') +
  facet_grid(var ~ mo, scales = 'free_y')

# average across years by month
toplo2 <- toplo %>% 
  # filter(mo == 'Jul') %>% 
  group_by(station, var, mo, lat, lon) %>% 
  summarise(
    val = mean(val, na.rm = T),
    valsd = sd(val, na.rm = T),
    valsdrs = scales::rescale(valsd, to = c(0, 1)) # use this as plot aesthetic map to alpha
  )

salplo <- toplo2 %>% 
  filter(var %in% 'sal')
ggplot(salplo, aes(x = lon, y = lat)) + 
  geom_point(aes(size = val, colour = val)) + 
  coord_map() + 
  scale_colour_gradient(low="lightgreen", high="tomato1")+
  facet_wrap(~mo) + 
  guides(color=guide_legend(), size = guide_legend())

tempplo <- toplo2 %>% 
  filter(var %in% 'temp')
ggplot(tempplo, aes(x = lon, y = lat)) + 
  geom_point(aes(size = val, colour = val)) + 
  coord_map() + 
  scale_colour_gradient(low="lightgreen", high="tomato1")+
  facet_wrap(~mo) + 
  guides(color=guide_legend(), size = guide_legend())

araplo <- toplo2 %>% 
  filter(var %in% 'ara')
ggplot(araplo, aes(x = lon, y = lat)) + 
  geom_point(aes(size = val, colour = val)) + 
  coord_map() + 
  scale_colour_gradient(low="lightgreen", high="tomato1")+
  facet_wrap(~mo) + 
  guides(color=guide_legend(), size = guide_legend())

# do some kind of cluster analysis on stations by parameters - optimal clusters?  