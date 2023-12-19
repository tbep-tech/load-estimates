# updated HFC (City of Tampa) DPS 
# raw data at T:\03_BOARDS_COMMITTEES\05_TBNMC\2022_RA_Update\01_FUNDING_OUT\DELIVERABLES\Copy.of.TBNMC.Point.Source.Data.Reporting.Tool.HFC.2012-22.REVISED.xlsx
# raw data here is tabular form of data sheet in the workbook

library(tidyverse)
library(here)
library(haven)

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

datraw <- read_csv(here('data/raw/HFC_update.csv'))

# R-002 and R-003 are not counted (see T:\03_BOARDS_COMMITTEES\05_TBNMC\2022_RA_Update\01_FUNDING_OUT\DELIVERABLES\TO-9\datastick_deliverables\LoadingCodes&Datasets\2021\PointSource2021\Domestic2021\1_DPS_2021a_20221025.sas)
# flow in million gallons per day
# multiply flow by day in month to get million gallons per month
# multiply flow by 3785.412 to get cubic meters per month
# multiply N by flow and divide by 1000 to get kg N per month 
#   multiply m3 by 1000 to get L, then divide by 1e6 to convert mg to kg)
#   same as dividing by 1000
newdat <- datraw %>% 
  select(Year, Month, matches('D-001|R-001'), `Total N`) %>% 
  rename(
    `DPS - end of pipe` = matches('D-001'), 
    `DPS - reuse` = matches('R-001')
  ) %>% 
  pivot_longer(names_to = 'source', values_to = 'flow_mgd', -c(Year, Month, `Total N`)) %>% 
  mutate(
    dys = days_in_month(ymd(paste(Year, Month, '01', sep = '-'))), 
    flow_mgm = flow_mgd * dys, # million gallons per month
    flow_m3m = flow_mgm * 3785.412, # cubic meters per month
    tn_load_kg = `Total N` * flow_m3m / 1000, # kg N per month
    tn_load_tons = tn_load_kg / 907.1847, 
    entity = 'Tampa'
  ) %>% 
  filter(Year < 2022) %>% 
  select(year = Year, month = Month, entity, source, tn_load = tn_load_tons)

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

