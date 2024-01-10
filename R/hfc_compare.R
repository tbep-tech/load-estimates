# updated HFC (City of Tampa) DPS 
# raw data at T:\03_BOARDS_COMMITTEES\05_TBNMC\2022_RA_Update\01_FUNDING_OUT\DELIVERABLES\Copy.of.TBNMC.Point.Source.Data.Reporting.Tool.HFC.2012-22.REVISED.xlsx
# raw data here is tabular form of data sheet in the workbook

library(tidyverse)
library(here)
library(haven)

source(here('R/funcs.R'))

# hfc/city of tampa updated data
load(file = here('data/dpsupdate.RData'))

##
# monthly diff

# get old and new data combined, load as tons/mo, except hydro as 10e3 m3/mo
toplo <- dpsdiff_fun(dpsupdate, annual = F) %>% 
  mutate(
    dt = ymd(paste(Year, Month, '01', sep = '-'))
  ) %>% 
  select(-diffv) %>% 
  pivot_longer(values_to = 'val', names_to = 'type', cols = c('old', 'new'))

p1 <- ggplot(toplo, aes(x = dt, y = val, color = type)) +
  geom_line() +
  facet_wrap(var~source, scales = 'free_y', ncol = 2) +
  theme_minimal() +
  theme(
    legend.position = 'bottom'
  ) + 
  labs(
    x = NULL, 
    y = 'Load', 
    color = NULL,
    title = 'Howard F. Curren AWTP (City of Tampa)',
  )

# png('~/Desktop/HFC_updatemo.png', width = 8, height = 4, units = 'in', res = 300)
print(p1)
# dev.off()

##
# annual diff

# get old and new data combined, load as tons/yr, except hydro as 10e3 m3/yr
toplo <- dpsdiff_fun(dpsupdate, annual = T) %>% 
  select(-diffv) %>% 
  pivot_longer(values_to = 'val', names_to = 'type', cols = c('old', 'new'))

p2 <- ggplot(toplo, aes(x = year, y = val, color = type)) +
  geom_line() +
  facet_wrap(var~source, scales = 'free_y', ncol = 2) +
  theme_minimal() +
  theme(
    legend.position = 'bottom'
  ) + 
  labs(
    x = NULL, 
    y = 'Load', 
    color = NULL,
    title = 'Howard F. Curren AWTP (City of Tampa)',
  )

# png('~/Desktop/HFC_updateyr.png', width = 8, height = 4, units = 'in', res = 300)
print(p2)
# dev.off()
