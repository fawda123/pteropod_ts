library(tidyverse)
library(gridExtra)
library(glue)
library(lubridate)
library(ggord)
library(patchwork)

source('R/funcs.R')
data(chmdatsum)
data(chmdatraw)
data(biodat)

# cluster
clsts <- tibble(
  station = c(402, 38, 28, 22, 12, 8, 4),
  clst = c(3, 2, 2, 1, 3, 2, 3)
)

# pteropod birthday
strt <- '2008-06-01' %>% 
  as.Date

# make cohortyr an ordered factor
biodat <- biodat %>% 
  mutate(cohortyr = factor(cohortyr, ordered = T))
chmdatsum <- chmdatsum %>% 
  mutate(cohortyr = factor(cohortyr, ordered = T))

# combine data for pca
biosub <- biodat %>% 
  select(cohortyr, mo, station, typ3)

chmsub <- chmdatsum %>% 
  filter(var %in% c('ara', 'sal', 'temp', 'oxy')) %>%
  select(-date, -yr, -lon, -lat, -max, -min, -std, -rng, -dlt) %>% 
  spread(var, ave)

tomod <- chmsub %>% 
  left_join(biosub, by = c('cohortyr', 'mo', 'station')) %>% 
  left_join(clsts, by = 'station') %>% 
  unite('stat_mo', station, mo, sep = ', ', remove = F) %>% 
  unite(stat_moyr, stat_mo, cohortyr, sep = ' ', remove = F) %>% 
  filter(!is.na(typ3)) %>% 
  as.data.frame(stringsAsFactors = F) %>% 
  column_to_rownames('stat_moyr')

# pc mod
mod <- prcomp(tomod[ , c('ara', 'oxy', 'sal', 'temp')], scale. = T, center = T)

# plots
cols <- c('lightgreen', 'lightblue', 'tomato1')

# no labels
p1 <- ggord(mod, grp_in = as.character(tomod$clst), vec_ext = 4, size = tomod$typ3, coord_fix = F) + 
  scale_size(range = c(2, 8)) +
  scale_colour_manual(values = cols) + 
  scale_fill_manual(values = cols) + 
  guides(size = guide_legend(title = '% type 3')) + 
  theme(legend.position = 'top')
pleg <- g_legend(p1)
p1 <- p1 + theme(legend.position = 'none')
 
# with labels
p2 <- ggord(mod, obslab = T, vec_ext = 4, size = 2, coord_fix = F) 

pdf('figs/pcastat.pdf', family = 'serif', height = 5, width = 10)
grid.arrange(
  pleg,
  arrangeGrob(p1, p2, ncol = 2, widths = c(1, 1)),
  ncol = 1, heights = c(0.1, 1)
)
dev.off()

#  
# # station, date, variable, averaged
# tomod <- chmsub %>% 
#   left_join(biosub, by = c('cohortyr', 'mo', 'station')) %>% 
#   gather('var', 'val', ara:typ3) %>% 
#   group_by(station, var) %>% 
#   summarise(
#     val = mean(val, na.rm = T)
#   ) %>% 
#   spread(var, val) %>%
#   left_join(clsts, by = 'station')
# 
# mod <- prcomp(tomod[ , c('ara', 'oxy', 'sal', 'temp')], scale. = T, center = T)
# ggord(mod, grp_in = as.character(tomod$clst), vec_ext = 4, size = tomod$typ3) + 
#   scale_size(range = c(1, 6)) +
#   ggtitle('All dates') +
#   scale_colour_manual(values = cols) + 
#   scale_fill_manual(values = cols) + 
#   guides(size = guide_legend(title = '% type 3'))
