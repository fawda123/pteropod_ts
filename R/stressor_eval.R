######
# plots of discrete sampling scenarios

library(tidyverse)
library(readxl)
library(arrangements)

# data setup for example categories of exposure
# for eval of in situ data
mos <- c('July', 'Sep', 'Apr')
prms <- permutations(c(0.25, 0.75), k = 3, replace = T) %>% 
  as.tibble %>%
  rename_at(vars(everything()), ~ mos) %>% 
  rownames_to_column('id') %>% 
  gather('mo', 'ara', -id) %>% 
  mutate(
    arac = 0.5,
    mo = factor(mo, levels = mos),
    cts = case_when(
      ara < arac ~ T,
      T ~ F
    )
  )

ggplot(prms, aes(x = mo, y = ara, fill = cts, group = 1)) + 
  stat_summary(fun.y = sum, colour = 'grey', geom = 'line') + 
  geom_point(stat = 'summary', pch = 21, size = 5, fun.y = sum) +
  geom_hline(aes(yintercept = arac), linetype = 'dashed', colour = 'grey') + 
  facet_grid(id ~.) +
  scale_fill_manual(values = c('lightgreen', 'tomato1')) +
  theme(
    axis.title.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(), 
    panel.background = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=1), 
    legend.position = 'none', 
    strip.background = element_blank()
  ) + 
  ylab('Aragonite saturation') +
  scale_y_continuous(limits = c(0, 1))

######
# cumulative stress days from simulated data

data(simdat)
data(biodat)

# pteropod birthday
strt <- '2008-06-01' %>% 
  as.Date

# aragonite thresholds to eval
thrshvls <- seq(0.5, 1, by = 0.1) %>% 
  rev

# get cumulative stress measures of days < aragonite thresh
# from simulated data
simdat <- simdat %>% 
  crossing(thrsh = thrshvls) %>% 
  group_by(station, thrsh) %>% 
  arrange(station, thrsh, datetime) %>% 
  mutate( # setup index
    dys =  6 / 24, 
    dys = cumsum(dys),
    crr = dys[datetime == strt],
    dys = case_when(
      datetime >= strt ~ dys - crr,
      datetime < strt ~ dys + max(dys) - crr
    )
  ) %>%
  arrange(station, thrsh, dys) %>% 
  mutate( # setup cumumulative stress by threshold
    strdys = ifelse(ara < thrsh, 6 / 24, 0),
    strdys = cumsum(strdys)
  ) %>% 
  select(-crr)

ggplot(simdat, aes(x = dys, y = strdys, colour = factor(thrsh))) + 
  geom_abline(intercept = 0, slope = 1) +
  geom_line() + 
  facet_wrap(~station) 

# aggregated biological data to match with sim data
# 2014 and 2015 averaged by month
# merged with simulation data
biodatagg <- biodat %>% 
  filter(cohortyr %in% c(2014, 2015)) %>% 
  select(station, mo, typ1, typ2, typ3) %>% 
  gather('var', 'val', -station, -mo) %>% 
  group_by(station, mo, var) %>% 
  summarise(val = mean(val, na.rm = T)) %>% 
  spread(var, val) %>% 
  ungroup %>% 
  mutate(
    datetime = paste('2008', as.numeric(mo), '15 00:00', sep = '-'),
    datetime = as.POSIXct(datetime, tz = 'UTC'),
    mo = factor(mo, levels = c('Jul', 'Sep', 'Apr')), 
    station = as.character(station)
  ) %>% 
  left_join(simdat, by = c('station', 'datetime'))

ggplot(biodatagg, aes(x = strdys, y = typ1)) + 
  geom_point() + 
  facet_wrap(~thrsh) + 
  stat_smooth(method = 'lm')
