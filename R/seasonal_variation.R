library(tidyverse)
data(chmdatsum)

# check variation between (chmosum) and within (chmyrsum) years for aragonite min. 

chmmosum <- chmdatsum %>% 
  select(station, yr, mo, var, min) %>% 
  filter(var %in% 'ara') %>% 
  group_by(mo, station) %>% 
  summarise(variance = var(min, na.rm = T)) %>% 
  group_by(mo) %>% 
  summarise(variance = mean(variance, na.rm = T))

chmyrsum <- chmdatsum %>% 
  select(station, yr, mo, var, min) %>% 
  filter(var %in% 'ara') %>% 
  group_by(yr, station) %>% 
  summarise(variance = var(min, na.rm = T)) %>% 
  group_by(yr) %>% 
  summarise(variance = mean(variance, na.rm = T))
