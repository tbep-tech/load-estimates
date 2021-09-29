
library(tidyverse)
library(lubridate)
library(haven)
library(here)

# segment id, annuals
segidann <- tibble(
  BAY_SEG = c(1, 2, 3, 4, 5567),
  bay_segment = c('Old Tampa Bay', 'Hillsborough Bay', 'Middle Tampa Bay', 'Lower Tampa Bay', 'Remainder Lower Tampa Bay')
)

# segment id, monthly
# 5 is all of BCB, 6 is Terra Ceia Bay, 7 is Manatee River, 55 is BCB south
# RA reports only BCB south so 5 is excluded here
segidmos <- tibble(
  bayseg = c(1, 2, 3, 4, 6, 7, 55),
  bay_segment = c('Old Tampa Bay', 'Hillsborough Bay', 'Middle Tampa Bay', 'Lower Tampa Bay', 'Remainder Lower Tampa Bay', 'Remainder Lower Tampa Bay', 'Remainder Lower Tampa Bay')
)

# coastal land use code lookup
clucs_lkup <- read.csv('data/raw/CLUCSID_lookup.csv') %>% 
  select(CLUCSID, DESCRIPTION) %>% 
  unique

# annual tn estimates -----------------------------------------------------

# 85 - 20
# original data from here T:/03_BOARDS_COMMITTEES/05_TBNMC/2022_RA_Update/01_FUNDING_OUT/DELIVERABLES/TO-8/LoadingCodes&Datasets2020/TotalLoads2020'
ad8520 <- read_sas(here('data/raw/ad_8520.sas7bdat'))
dps8520 <- read_sas(here('data/raw/dps_8520.sas7bdat'))
ips8520 <- read_sas(here('data/raw/ips_ml_8520.sas7bdat'))
nps8520 <- read_sas(here('data/raw/nps_8520.sas7bdat'))
gws8520 <- read_sas(here('data/raw/gws_8520.sas7bdat'))

# h2o load data prior to 2012, no source info
hyolddat <- read_sas(here('data/raw/h2oannseg8511.sas7bdat'))

# TN is in tons / yr, h2o is in million m3 / yr
dat <- bind_rows(ad8520, dps8520, ips8520, nps8520, gws8520) %>% 
  rename(
    hy_load = h2oload10e6m3, 
    tn_load = TN_tons
  ) %>% 
  left_join(segidann, by = 'BAY_SEG') 

# totals across all segments
tots <- dat %>% 
  group_by(YEAR, SOURCE) %>% 
  summarise(
    tn_load = sum(tn_load),
    hy_load = sum(hy_load),
    .groups = 'drop'
  ) %>% 
  mutate(bay_segment = 'All Segments (- N. BCB)')

dat <- bind_rows(tots, dat)

# tn data only
tnanndat <- dat %>% 
  select(SOURCE, YEAR, tn_load, bay_segment)

save(tnanndat, file = 'data/tnanndat.RData', compress = 'xz')

# annual totals -----------------------------------------------------------

# tn load, sum source within bay segment
tntots <- tnanndat %>% 
  group_by(bay_segment, YEAR) %>% 
  summarise(tn_load = sum(tn_load, na.rm = T), .groups = 'drop')

# hy dat only, 2012 to 2020, sum across source
hytots <- dat %>% 
  select(SOURCE, YEAR, hy_load, bay_segment) %>% 
  filter(!is.na(hy_load)) %>% 
  group_by(bay_segment, YEAR) %>% 
  summarise(hy_load = sum(hy_load, na.rm = T), .groups = 'drop')

# hy dat prior to 2012
hyoldtots <- hyolddat %>% 
  rename(BAY_SEG = bay_seg) %>% 
  left_join(segidann, by = 'BAY_SEG') %>% 
  rename(
    hy_load = h2oload10e6m3, 
    YEAR = year
  ) %>% 
  select(-BAY_SEG)

# hy dat prior to 2012, sum by segments
hyoldallseg <- hyoldtots %>% 
  group_by(YEAR) %>% 
  summarise(
    hy_load = sum(hy_load, na.rm = T),
    .groups = 'drop'
  ) %>% 
  mutate(bay_segment = 'All Segments (- N. BCB)')

hyoldtots <- hyoldtots %>%
  bind_rows(hyoldallseg)

hytots <- hytots %>% 
  bind_rows(hyoldtots) %>% 
  arrange(bay_segment, YEAR)

# combine tn, hy 
totanndat <- tntots %>% 
  full_join(hytots, by = c('bay_segment', 'YEAR')) %>% 
  mutate(tnhy =  tn_load / hy_load)

save(totanndat, file = 'data/totanndat.RData', compress = 'xz')

# all monthly tn estimates ------------------------------------------------

# source here: T:\03_BOARDS_COMMITTEES\05_TBNMC\2022_RA_Update\01_FUNDING_OUT\DELIVERABLES\TO-8\2017-2020Annual&MonthlyLoadDatasets
mosdat <- read_sas(here('data/raw/monthly1720entityloaddataset.sas7bdat')) %>% 
  select(bayseg, YEAR, MONTH, source, tnloadtons) %>% 
  mutate(
    source = case_when(
      source == 'Atmospheric Deposition' ~ 'AD', 
      source %in% c('Springs', 'Ground Water') ~ 'GWS', 
      source %in% c('PS - Domestic - REUSE', 'PS - Domestic - SW') ~ 'DPS', 
      source %in% c('PS - Industrial', 'Material Losses') ~ 'IPS', 
      source == 'Non-Point Source' ~ 'NPS'
    )
  ) %>% 
  group_by(bayseg, YEAR, MONTH, source) %>% 
  summarise(
    tnload = sum(tnloadtons), 
    .groups = 'drop'
  ) %>% 
  left_join(segidmos, by = 'bayseg') %>% 
  select(
    SOURCE = source, 
    YEAR, 
    MONTH, 
    tn_load = tnload, 
    bay_segment
  )

totsmo <- mosdat %>% 
  group_by(YEAR, MONTH, SOURCE) %>% 
  summarise(
    tn_load = sum(tn_load),
    .groups = 'drop'
  ) %>% 
  mutate(bay_segment = 'All Segments (- N. BCB)')

tnmosdat <- bind_rows(mosdat, totsmo) %>% 
  mutate(dy = 1) %>% 
  unite('date', YEAR, MONTH, dy, sep = '-', remove = T) %>% 
  mutate(
    date = ymd(date)
  )

save(tnmosdat, file = here('data/tnmosdat.RData'))


# all monthly tn estimates by entity --------------------------------------

# source here: T:\03_BOARDS_COMMITTEES\05_TBNMC\2022_RA_Update\01_FUNDING_OUT\DELIVERABLES\TO-8\2017-2020Annual&MonthlyLoadDatasets
tnmosentdat <- read_sas(here('data/raw/monthly1720entityloaddataset.sas7bdat')) %>% 
  select(entity, YEAR, MONTH, source, tnloadtons) %>% 
  mutate(
    source = case_when(
      source == 'Atmospheric Deposition' ~ 'AD', 
      source %in% c('Springs', 'Ground Water') ~ 'GWS', 
      source %in% c('PS - Domestic - REUSE', 'PS - Domestic - SW') ~ 'DPS', 
      source %in% c('PS - Industrial', 'Material Losses') ~ 'IPS', 
      source == 'Non-Point Source' ~ 'NPS'
    )
  ) %>% 
  group_by(entity, YEAR, MONTH, source) %>% 
  summarise(
    tnload = sum(tnloadtons), 
    .groups = 'drop'
  ) %>% 
  select(
    entity,
    SOURCE = source, 
    YEAR, 
    MONTH, 
    tn_load = tnload
  )

save(tnmosentdat, file = here('data/tnmosentdat.RData'))

# monthly ips, dps, nps ---------------------------------------------------

# non-point source
npsmosdat <- read_sas(here('data/raw/nps0420monthentbaslu.sas7bdat')) %>% 
  inner_join(segidmos, by = 'bayseg') %>% 
  left_join(clucs_lkup, by = 'CLUCSID') %>% 
  mutate(dy = 1) %>% 
  unite('date', year, month, dy, sep = '-', remove = T) %>% 
  mutate(
    date = ymd(date), 
    source = 'NPS'
  ) %>% 
  select(date, bay_segment, basin, entity, lu = DESCRIPTION, source, tn_load = tnloadtons)

# industrial point source
ipsmosdat <- read_sas(here('data/raw/ips0420monthentbas.sas7bdat')) %>% 
  inner_join(segidmos, by = 'bayseg') %>% 
  mutate(dy = 1) %>% 
  unite('date', Year, Month, dy, sep = '-', remove = T) %>% 
  mutate(
    date = ymd(date), 
    source = 'IPS'
  ) %>% 
  select(date, bay_segment, basin = BASIN, facility = facname, source, tn_load = tnloadtons)

# domestic point source  
dpsmosdat <- read_sas(here('data/raw/dps0420monthentbas.sas7bdat')) %>% 
  inner_join(segidmos, by = 'bayseg') %>% 
  mutate(dy = 1) %>% 
  unite('date', Year, Month, dy, sep = '-', remove = T) %>% 
  mutate(
    date = ymd(date), 
    source = case_when(
      grepl('REUSE$', source2) ~ 'DPS - reuse', 
      grepl('SW$', source2) ~ 'DPS - stormwater'
    )
  ) %>% 
  select(date, bay_segment, basin, entity, facility = facname, source, tn_load = tnloadtons)

save(npsmosdat, file = here('data/npsmosdat.RData'))
save(ipsmosdat, file = here('data/ipsmosdat.RData'))
save(dpsmosdat, file = here('data/dpsmosdat.RData'))

# get tn by bay segment only
npsdpsips <- list(npsmosdat, ipsmosdat, dpsmosdat) %>% 
  enframe() %>% 
  mutate(
    value = purrr::map(value, function(x) select(x, date, bay_segment, source, tn_load))
  ) %>% 
  unnest('value') %>% 
  group_by(date, bay_segment, source) %>% 
  summarise(
    tn_load = sum(tn_load), 
    .groups = 'drop'
  ) 

npsdpsipsall <- npsdpsips %>% 
  group_by(date, source) %>% 
  summarise(
    tn_load = sum(tn_load), 
    .groups = 'drop'
  ) %>% 
  mutate(bay_segment = 'All Segments (- N. BCB)')
npsdpsips <- bind_rows(npsdpsips, npsdpsipsall) %>% 
  rename(SOURCE = source)

save(npsdpsips, file = here('data/npsdpsips.RData'))

# nps, ips, dps by entity -------------------------------------------------

# non-point source
npsmosdat <- read_sas(here('data/raw/nps0420monthentbaslu.sas7bdat')) %>% 
  group_by(entity, year, month) %>% 
  summarise(
    tn_load = sum(tnloadtons, na.rm = T), 
    .groups = 'drop'
  ) %>% 
  mutate(
    source = 'NPS'
  ) %>% 
  select(entity, year, month, source, tn_load)

# industrial point source
ipsmosdat <- read_sas(here('data/raw/ips0420monthentbas.sas7bdat')) %>% 
  rename(
    year = Year, 
    month = Month
  ) %>% 
  group_by(entity, year, month) %>% 
  summarise(
    tn_load = sum(tnloadtons, na.rm = T), 
    .groups = 'drop'
  ) %>% 
  mutate(
    source = 'PS'
  ) %>% 
  select(entity, year, month, source, tn_load)

# domestic point source  
dpsmosdat <- read_sas(here('data/raw/dps0420monthentbas.sas7bdat')) %>% 
  rename(
    year = Year, 
    month = Month
  ) %>% 
  mutate(
    source = case_when(
      grepl('REUSE$', source2) ~ 'DPS - reuse', 
      grepl('SW$', source2) ~ 'DPS - stormwater'
    )
  ) %>% 
  group_by(entity, source, year, month) %>% 
  summarise(
    tn_load = sum(tnloadtons, na.rm = T), 
    .groups = 'drop'
  ) %>% 
  select(entity, year, month, source, tn_load)

npsdpsipsent <- bind_rows(npsmosdat, ipsmosdat, dpsmosdat)
  
save(npsdpsipsent, file = here('data/npsdpsipsent.RData'))
