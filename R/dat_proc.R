
library(tidyverse)
library(lubridate)
library(haven)
library(readxl)
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
    tn_load = TN_tons, 
    year = YEAR,
    source = SOURCE
  ) %>% 
  left_join(segidann, by = 'BAY_SEG') 

# totals across all segments
tots <- dat %>% 
  group_by(year, source) %>% 
  summarise(
    tn_load = sum(tn_load),
    hy_load = sum(hy_load),
    .groups = 'drop'
  ) %>% 
  mutate(bay_segment = 'All Segments (- N. BCB)')

dat <- bind_rows(tots, dat)

# tn data only
tnanndat <- dat %>% 
  select(year, bay_segment, source, tn_load)

save(tnanndat, file = 'data/tnanndat.RData', version = 2)

# annual totals -----------------------------------------------------------

# tn load, sum source within bay segment
tntots <- tnanndat %>% 
  group_by(bay_segment, year) %>% 
  summarise(tn_load = sum(tn_load, na.rm = T), .groups = 'drop')

# hy dat only, 2012 to 2020, sum across source
hytots <- dat %>% 
  select(source, year, hy_load, bay_segment) %>% 
  filter(!is.na(hy_load)) %>% 
  group_by(bay_segment, year) %>% 
  summarise(hy_load = sum(hy_load, na.rm = T), .groups = 'drop')

# hy dat prior to 2012
hyoldtots <- hyolddat %>% 
  rename(BAY_SEG = bay_seg) %>% 
  left_join(segidann, by = 'BAY_SEG') %>% 
  rename(
    hy_load = h2oload10e6m3
  ) %>% 
  select(-BAY_SEG)

# hy dat prior to 2012, sum by segments
hyoldallseg <- hyoldtots %>% 
  group_by(year) %>% 
  summarise(
    hy_load = sum(hy_load, na.rm = T),
    .groups = 'drop'
  ) %>% 
  mutate(bay_segment = 'All Segments (- N. BCB)')

hyoldtots <- hyoldtots %>%
  bind_rows(hyoldallseg)

hytots <- hytots %>% 
  bind_rows(hyoldtots) %>% 
  arrange(bay_segment, year)

# combine tn, hy 
totanndat <- tntots %>% 
  full_join(hytots, by = c('bay_segment', 'year')) %>% 
  mutate(tnhy =  tn_load / hy_load) %>% 
  select(year, bay_segment, tn_load, hy_load, tnhy)

save(totanndat, file = 'data/totanndat.RData', version = 2)

# all monthly tn, tp, tss, bod estimates ----------------------------------

# source here: T:\03_BOARDS_COMMITTEES\05_TBNMC\2022_RA_Update\01_FUNDING_OUT\DELIVERABLES\TO-8\2017-2020Annual&MonthlyLoadDatasets
mosdat <- read_sas(here('data/raw/monthly1720entityloaddataset.sas7bdat')) %>% 
  select(bayseg, year = YEAR, month = MONTH, source, tnloadtons, tploadtons, tssloadtons, bodloadtons) %>% 
  mutate(
    source = case_when(
      source == 'Atmospheric Deposition' ~ 'AD', 
      source %in% c('Springs', 'Ground Water') ~ 'GWS', 
      source %in% c('PS - Domestic - REUSE', 'PS - Domestic - SW') ~ 'DPS', 
      source %in% c('PS - Industrial', 'Material Losses') ~ 'IPS', 
      source == 'Non-Point Source' ~ 'NPS'
    )
  ) %>% 
  group_by(bayseg, year, month, source) %>% 
  summarise(
    tnload = sum(tnloadtons), 
    tpload = sum(tploadtons), 
    tssload = sum(tssloadtons), 
    bodload = sum(bodloadtons),
    .groups = 'drop'
  ) %>% 
  left_join(segidmos, by = 'bayseg') %>% 
  select(
    source, 
    year, 
    month, 
    tn_load = tnload, 
    tp_load = tpload,
    tss_load = tssload, 
    bod_load = bodload,
    bay_segment
  )

totsmo <- mosdat %>% 
  group_by(year, month, source) %>% 
  summarise(
    tn_load = sum(tn_load),
    tp_load = sum(tp_load), 
    tss_load = sum(tss_load), 
    bod_load = sum(bod_load),
    .groups = 'drop'
  ) %>% 
  mutate(bay_segment = 'All Segments (- N. BCB)')

mosdat <- bind_rows(mosdat, totsmo) %>% 
  select(year, month, bay_segment, source, tn_load, tp_load, tss_load, bod_load)

save(mosdat, file = here('data/mosdat.RData'), version = 2)

# all monthly tn, tp, tss, bod estimates by entity ------------------------

# source here: T:\03_BOARDS_COMMITTEES\05_TBNMC\2022_RA_Update\01_FUNDING_OUT\DELIVERABLES\TO-8\2017-2020Annual&MonthlyLoadDatasets
mosentdat <- read_sas(here('data/raw/monthly1720entityloaddataset.sas7bdat')) %>% 
  select(entity, year = YEAR, month = MONTH, source, tnloadtons, tploadtons, tssloadtons, bodloadtons) %>% 
  mutate(
    source = case_when(
      source == 'Atmospheric Deposition' ~ 'AD', 
      source %in% c('Springs', 'Ground Water') ~ 'GWS', 
      source %in% c('PS - Domestic - REUSE', 'PS - Domestic - SW') ~ 'DPS', 
      source %in% c('PS - Industrial', 'Material Losses') ~ 'IPS', 
      source == 'Non-Point Source' ~ 'NPS'
    )
  ) %>% 
  group_by(entity, year, month, source) %>% 
  summarise(
    tnload = sum(tnloadtons), 
    tpload = sum(tploadtons), 
    tssload = sum(tssloadtons), 
    bodload = sum(bodloadtons),
    .groups = 'drop'
  ) %>% 
  select(year, month, entity, source, tn_load = tnload)

save(mosentdat, file = here('data/mosentdat.RData'), version = 2)

# all monthly hydro load --------------------------------------------------

dat1 <- read_excel(here('data/raw/TotH2O_2020_Monthly4Seg.xlsx')) %>% 
  mutate(
    bay_segment = factor(Segment, levels = c('1', '2', '3', '4'), labels = c('OTB', 'HB', 'MTB', 'LTB'))
  ) %>% 
  select(year = Year, month = Month, bay_segment, hy_load = `H2O Load (106 m3/yr)`)
dat2 <- read_excel(here('data/raw/RALTB_H2O_Monthly_1720.xlsx')) %>% 
  mutate(bay_segment = 'RLTB') %>% 
  select(year = Year, month = Month, bay_segment, hy_load = `H2O Load (106 m3)`)
dat3 <- read_excel(here('data/raw/H2OMonthlySeg1719.xlsx')) %>% 
  mutate(
    bay_segment = factor(Segment, levels = c('OTB', 'HB', 'MTB', 'LTB'))
  ) %>% 
  select(year = Year, month = Month, bay_segment, hy_load = `H2O Load (10e6 m3/yr)`)

mohydat <- bind_rows(dat1, dat2, dat3) %>% 
  mutate(
    bay_segment = factor(
      bay_segment, 
      levels = c('OTB', 'HB', 'MTB', 'LTB', 'RLTB'), 
      labels = c('Old Tampa Bay', 'Hillsborough Bay', 'Middle Tampa Bay', 'Lower Tampa Bay', 'Remainder Lower Tampa Bay'))
  ) %>% 
  arrange(bay_segment, year, month) %>% 
  mutate(
    bay_segment = as.character(bay_segment)
  ) %>% 
  rename(
    hy_load_106_m3_mo = hy_load
  )

allmohydat <- mohydat %>% 
  group_by(year, month) %>% 
  summarise(
    hy_load_106_m3_mo = sum(hy_load_106_m3_mo), 
    .groups = 'drop'
  ) %>% 
  mutate(bay_segment = 'All Segments (- N. BCB)') %>% 
  select(year, month, bay_segment, hy_load_106_m3_mo)

mohydat <- bind_rows(mohydat, allmohydat)

save(mohydat, file = here('data/mohydat.RData'))

write.csv(mohydat, '~/Desktop/mohydat.csv', quote = F, row.names = F)

# monthly ips, dps, nps ---------------------------------------------------

# non-point source
npsmosdat <- read_sas(here('data/raw/nps0420monthentbaslu.sas7bdat')) %>% 
  inner_join(segidmos, by = 'bayseg') %>% 
  left_join(clucs_lkup, by = 'CLUCSID') %>% 
  mutate(
    source = 'NPS'
  ) %>% 
  select(year, month, bay_segment, basin, entity, lu = DESCRIPTION, source, tn_load = tnloadtons)

# industrial point source
ipsmosdat <- read_sas(here('data/raw/ips0420monthentbas.sas7bdat')) %>% 
  inner_join(segidmos, by = 'bayseg') %>% 
  mutate(
    source = 'IPS'
  ) %>% 
  select(year = Year, month = Month, bay_segment, basin = BASIN, facility = facname, source, tn_load = tnloadtons)

# domestic point source  
dpsmosdat <- read_sas(here('data/raw/dps0420monthentbas.sas7bdat')) %>% 
  inner_join(segidmos, by = 'bayseg') %>%
  mutate(
    source = case_when(
      grepl('REUSE$', source2) ~ 'DPS - reuse', 
      grepl('SW$', source2) ~ 'DPS - end of pipe'
    )
  ) %>% 
  select(year = Year, month = Month, bay_segment, basin, entity, facility = facname, source, tn_load = tnloadtons)

save(npsmosdat, file = here('data/npsmosdat.RData'), version = 2)
save(ipsmosdat, file = here('data/ipsmosdat.RData'), version = 2)
save(dpsmosdat, file = here('data/dpsmosdat.RData'), version = 2)

# get tn by bay segment only
npsdpsips <- list(npsmosdat, ipsmosdat, dpsmosdat) %>% 
  enframe() %>% 
  mutate(
    value = purrr::map(value, function(x) select(x, year, month, bay_segment, source, tn_load))
  ) %>% 
  unnest('value') %>% 
  group_by(year, month, bay_segment, source) %>% 
  summarise(
    tn_load = sum(tn_load), 
    .groups = 'drop'
  ) 

npsdpsipsall <- npsdpsips %>% 
  group_by(year, month, source) %>% 
  summarise(
    tn_load = sum(tn_load), 
    .groups = 'drop'
  ) %>% 
  mutate(bay_segment = 'All Segments (- N. BCB)')
npsdpsips <- bind_rows(npsdpsips, npsdpsipsall) %>% 
  select(year, month, bay_segment, source, tn_load)

save(npsdpsips, file = here('data/npsdpsips.RData'), version = 2)

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
  )

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
  )

# domestic point source  
dpsmosdat <- read_sas(here('data/raw/dps0420monthentbas.sas7bdat')) %>% 
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
  )

npsdpsipsent <- bind_rows(npsmosdat, ipsmosdat, dpsmosdat) %>% 
  select(year, month, entity, source, tn_load)
  
save(npsdpsipsent, file = here('data/npsdpsipsent.RData'), version = 2)

# nps tn by land use ------------------------------------------------------

# non-point source
npsmosludat <- read_sas(here('data/raw/nps0420monthentbaslu.sas7bdat')) %>% 
  left_join(clucs_lkup, by = 'CLUCSID') %>% 
  inner_join(segidmos, by = 'bayseg') %>% 
  group_by(DESCRIPTION, bay_segment, year, month) %>% 
  summarise(
    tn_load = sum(tnloadtons, na.rm = T), 
    .groups = 'drop'
  ) %>% 
  mutate(
    source = 'NPS'
  ) %>% 
  select(year, month, bay_segment, `land use` = DESCRIPTION, source, tn_load)

save(npsmosludat, file = here('data/npsmosludat.RData'), version = 2)
