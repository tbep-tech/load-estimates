# updated HFC (City of Tampa) DPS 
# raw data at T:\03_BOARDS_COMMITTEES\05_TBNMC\2022_RA_Update\01_FUNDING_OUT\DELIVERABLES\Copy.of.TBNMC.Point.Source.Data.Reporting.Tool.HFC.2012-22.REVISED.xlsx
# raw data here is tabular form of data sheet in the workbook

library(tidyverse)
library(here)
library(haven)

source(here('R/funcs.R'))

# hfc/city of tampa corrected data
load(file = here('data/dpscorr.RData'))

# original data -------------------------------------------------------------------------------

# domestic point source prior to 2017-2021 RA 
dpsmosdat1 <- read_sas(here('data/raw/dps0420monthentbas.sas7bdat')) %>% 
  filter(Year < 2017)
dpsmosdat2 <- read_sas(here('data/raw/dps1721monthentbas.sas7bdat')) 

olddat <- bind_rows(dpsmosdat1, dpsmosdat2) %>% 
  rename(
    year = Year, 
    month = Month
  ) %>% 
  mutate(
    source = case_when(
      grepl('REUSE$', source2) ~ 'DPS - reuse', 
      grepl('SW$', source2) ~ 'DPS - end of pipe'
    )
  ) %>% 
  group_by(entity, source, year, month) %>% 
  summarise(
    tn_load = sum(tnloadtons, na.rm = T), 
    .groups = 'drop'
  ) %>% 
  filter(entity == 'Tampa') %>% 
  filter(year > 2011) # earliest year in updated data is 2012

# updated data --------------------------------------------------------------------------------

# hfc/city of tampa corrected data
newdat <- dpscorr %>% 
  filter(Year < 2022) %>% 
  select(year = Year, month = Month, entity, source, tn_load = tn_load)

# compare -------------------------------------------------------------------------------------

# prep for plot
toplo1 <- full_join(olddat, newdat, by = c('year', 'month', 'entity', 'source')) %>% 
  rename(
    original = tn_load.x,
    update = tn_load.y
  ) %>% 
  pivot_longer(names_to = 'type', values_to = 'tn_load', -c(year, month, entity, source)) %>%
  summarise(
    tn_load = sum(tn_load), 
    .by = c(year, source, type)
  )

toplo2 <- toplo1 %>% 
  summarise(
    tn_load = sum(tn_load), 
    .by = c(year, type)
  ) %>% 
  mutate(
    source = 'DPS - total'
  )

toplo <- bind_rows(toplo1, toplo2)

p1 <- ggplot(toplo, aes(x = year, y = tn_load, color = type)) +
  geom_line() +
  facet_wrap(~source, scales = 'free_y') +
  theme_minimal() +
  theme(
    legend.position = 'bottom'
  ) + 
  labs(
    x = NULL, 
    y = 'TN load (tons)', 
    color = NULL,
    title = 'Howard F. Curren AWTP (City of Tampa)',
  )

png('~/Desktop/HFC_update.png', width = 8, height = 4, units = 'in', res = 300)
print(p1)
dev.off()

