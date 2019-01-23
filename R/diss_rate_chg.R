# estimate dissolution rate of change

library(tidyverse)

data(biodat)

disrat <- biodat %>% 
  select(date, cohortyr, mo, station, typ3) %>% 
  arrange(station, date) %>% 
  group_by(station) %>% 
  mutate(
    disavg = mean(typ3, na.rm = T),
    disdel = c(NA, diff(typ3)) / disavg,
    dtsdel = c(NA, (as.numeric(diff(date)) / 31)),
    disrat = 100 * disdel/dtsdel
  )