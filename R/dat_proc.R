
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


# tn load by source for major bay segments ----------------------------------------------------

# 85 - 20data
# original data from here T:/03_BOARDS_COMMITTEES/05_TBNMC/2022_RA_Update/01_FUNDING_OUT/DELIVERABLES/TO-8/LoadingCodes&Datasets2020/TotalLoads2020'
ad8520 <- read_sas(here('data/raw/ad_8520.sas7bdat'))
dps8520 <- read_sas(here('data/raw/dps_8520.sas7bdat'))
ips8520 <- read_sas(here('data/raw/ips_ml_8520.sas7bdat'))
nps8520 <- read_sas(here('data/raw/nps_8520.sas7bdat'))
gws8520 <- read_sas(here('data/raw/gws_8520.sas7bdat'))

# TN is in tons / yr
dat <- bind_rows(ad8520, dps8520, ips8520, nps8520, gws8520) %>% 
  select(
    BAY_SEG,
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
    .groups = 'drop'
  ) %>% 
  mutate(bay_segment = 'All Segments (- N. BCB)')

dat <- bind_rows(tots, dat)

# tn data only, up to 2016
tnanndat <- dat %>% 
  select(year, bay_segment, source, tn_load) %>% 
  filter(year <= 2016)

# current load data by sourc ra period 2017 to 2021 (from RP email 11/4/22)
# source is here T:\03_BOARDS_COMMITTEES\05_TBNMC\2022_RA_Update\01_FUNDING_OUT\DELIVERABLES\TO-9\Loads1721_DocTables\Loads1721_DocTables
loadra1721 <- read.csv(here('data/raw/totn1721_segsource.csv')) %>% 
  select(
    bay_segment = BAY_SEG, 
    year = Year, 
    source, 
    tn_load = tnload
    ) %>% 
  na.omit() %>% 
  filter(bay_segment %in% c(1, 2, 3, 4, 5567)) %>%
  filter(!source %in% c('POR')) %>% # fertilizer handling losses, sometimes as ML, see RP email 2/9/23
  mutate(
    source = as.character(factor(source, 
                    levels = c('AD', 'DPS', 'GW', 'IPS', 'NPS', 'SPR'),
                    labels = c('AD', 'DPS', 'GWS', 'IPS', 'NPS', 'GWS')
    )),
    bay_segment = as.character(factor(bay_segment, 
                         levels = as.character(c(1, 2, 3, 4, 5567)), 
                         labels = c('Old Tampa Bay', 'Hillsborough Bay', 'Middle Tampa Bay', 'Lower Tampa Bay', 'Remainder Lower Tampa Bay')))
  ) %>% 
  group_by(year, bay_segment, source) %>% 
  summarise(
    tn_load = sum(tn_load), 
    .groups = 'drop'
  )
  
loadra1721tots <- loadra1721 %>% 
  group_by(year, source) %>% 
  summarise(
    tn_load = sum(tn_load),
    .groups = 'drop'
  ) %>% 
  mutate(bay_segment = 'All Segments (- N. BCB)')

tnanndat <- bind_rows(tnanndat, loadra1721, loadra1721tots) %>%
  tidyr::complete(bay_segment, source, year, fill = list(tn_load = 0)) %>% 
  arrange(year, bay_segment, source)

save(tnanndat, file = 'data/tnanndat.RData', version = 2)

# annual totals -----------------------------------------------------------

totanndatpre <- read_sas('data/raw/tb_rasegsanntntph2o_8521.sas7bdat') %>% 
  mutate(
    bay_segment = case_when(
      BAY_SEG == 1 ~ 'Old Tampa Bay', 
      BAY_SEG == 2 ~ 'Hillsborough Bay', 
      BAY_SEG == 3 ~ 'Middle Tampa Bay', 
      BAY_SEG == 4 ~ 'Lower Tampa Bay', 
      BAY_SEG %in% c(6, 7, 55) ~ 'Remainder Lower Tampa Bay'
    )
  ) %>% 
  rename(
    year = YEAR, 
    tn_load = TN_tons, 
    tp_load = TP_tons, 
    hy_load = h2oload10e6m3
  ) %>% 
  group_by(year, bay_segment) %>% 
  summarise(
    tn_load = sum(tn_load, na.rm = T), 
    tp_load = sum(tp_load, na.rm = T), 
    hy_load = sum(hy_load, na.rm = T),
    .groups = 'drop'
  ) %>% 
  mutate(
    tnhy = tn_load / hy_load, 
    tphy = tp_load / hy_load
  ) %>% 
  filter(year < 2017)

# 2017 - 2022 RA period (from updated file)
# source is here T:\03_BOARDS_COMMITTEES\05_TBNMC\2022_RA_Update\01_FUNDING_OUT\DELIVERABLES\TO-9\Loads1721_DocTables\Loads1721_DocTables
totanndatpos <- read.csv(here('data/raw/totn1721_segsource.csv')) %>% 
  select(
    bay_segment = BAY_SEG, 
    year = Year, 
    source, 
    tn_load = tnload, 
    tp_load = tpload, 
    hy_load = h2oload10e6m3
  ) %>% 
  filter(bay_segment %in% c(1, 2, 3, 4, 6, 7, 55)) %>%
  mutate(
    bay_segment = as.character(factor(bay_segment, 
                                      levels = as.character(c(1, 2, 3, 4, 6, 7, 55)), 
                                      labels = c('Old Tampa Bay', 'Hillsborough Bay', 'Middle Tampa Bay', 'Lower Tampa Bay', 'Remainder Lower Tampa Bay', 'Remainder Lower Tampa Bay', 'Remainder Lower Tampa Bay')))
  ) %>% 
  group_by(year, bay_segment) %>% 
  summarise(
    tn_load = sum(tn_load, na.rm = T), 
    tp_load = sum(tp_load, na.rm = T), 
    hy_load = sum(hy_load, na.rm = T),
    .groups = 'drop'
  ) %>% 
  mutate(
    tnhy = tn_load / hy_load,      
    tphy = tp_load / hy_load
  )

totanndat <- bind_rows(totanndatpre, totanndatpos)

# hy dat prior to 2012, sum by segments
allseg <- totanndat %>% 
  group_by(year) %>% 
  summarise(
    tn_load = sum(tn_load, na.rm = T),
    tp_load = sum(tp_load, na.rm = T),
    hy_load = sum(hy_load, na.rm = T),
    .groups = 'drop'
  ) %>% 
  mutate(
    tnhy = tn_load / hy_load,
    tphy = tp_load / hy_load
  ) %>% 
  mutate(bay_segment = 'All Segments (- N. BCB)')

totanndat <- totanndat %>% 
  bind_rows(allseg) %>% 
  arrange(bay_segment, year)

save(totanndat, file = 'data/totanndat.RData', version = 2)

# all monthly tn, tp, tss, bod estimates ----------------------------------

# original at T:/03_BOARDS_COMMITTEES/05_TBNMC/2022_RA_Update/01_FUNDING_OUT/DELIVERABLES/TO-9/datastick_deliverables/2017-2021Annual&MonthlyLoadDatasets/MakeMonthAnnDatasets/Monthly/monthly1721entityloaddataset.sas7bdat
mosdat <- read_sas(here('data/raw/monthly1721entityloaddataset.sas7bdat')) %>% 
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

# original at T:/03_BOARDS_COMMITTEES/05_TBNMC/2022_RA_Update/01_FUNDING_OUT/DELIVERABLES/TO-9/datastick_deliverables/2017-2021Annual&MonthlyLoadDatasets/MakeMonthAnnDatasets/Monthly/monthly1721entityloaddataset.sas7bdat
mosentdat <- read_sas(here('data/raw/monthly1721entityloaddataset.sas7bdat')) %>% 
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

# non-point source prior to 2017-2021 RA
npsmosdat <- read_sas(here('data/raw/nps0420monthentbaslu.sas7bdat')) %>% 
  inner_join(segidmos, by = 'bayseg') %>% 
  left_join(clucs_lkup, by = 'CLUCSID') %>% 
  mutate(
    source = 'NPS'
  ) %>% 
  select(year, month, bay_segment, entity, lu = DESCRIPTION, source, tn_load = tnloadtons) %>% filter(year < 2017)

# industrial point source prior to 2017-2021 RA
ipsmosdat <- read_sas(here('data/raw/ips0420monthentbas.sas7bdat')) %>% 
  inner_join(segidmos, by = 'bayseg') %>% 
  mutate(
    source = 'IPS'
  ) %>% 
  select(year = Year, month = Month, bay_segment, facility = facname, source, tn_load = tnloadtons) %>% filter(year < 2017)

# domestic point source prior to 2017-2021 RA 
dpsmosdat <- read_sas(here('data/raw/dps0420monthentbas.sas7bdat')) %>% 
  inner_join(segidmos, by = 'bayseg') %>%
  mutate(
    source = case_when(
      grepl('REUSE$', source2) ~ 'DPS - reuse', 
      grepl('SW$', source2) ~ 'DPS - end of pipe'
    )
  ) %>% 
  select(year = Year, month = Month, bay_segment, entity, facility = facname, source, tn_load = tnloadtons) %>% filter(year < 2017)

# non-point source 2017-2021 RA
# source at T:\03_BOARDS_COMMITTEES\05_TBNMC\2022_RA_Update\01_FUNDING_OUT\DELIVERABLES\TO-9\datastick_deliverables\2017-2021LUEntityLoads
npsmosdat2 <- read_sas(here('data/raw/nps1721monthenbaslu.sas7bdat')) %>% 
  inner_join(segidmos, by = 'bayseg') %>% 
  left_join(clucs_lkup, by = 'CLUCSID') %>% 
  mutate(
    source = 'NPS'
  ) %>% 
  select(year, month, bay_segment, entity, lu = DESCRIPTION, source, tn_load = tnloadtons)

# industrial point source 2017-2021 RA
# source at T:\03_BOARDS_COMMITTEES\05_TBNMC\2022_RA_Update\01_FUNDING_OUT\DELIVERABLES\TO-9\datastick_deliverables\2017-2021LUEntityLoads
ipsmosdat2 <- read_sas(here('data/raw/ips1721monthentbas.sas7bdat')) %>% 
  inner_join(segidmos, by = 'bayseg') %>% 
  mutate(
    source = 'IPS'
  ) %>% 
  select(year = Year, month = Month, bay_segment, facility = facname, source, tn_load = tnloadtons)

# domestic point source 2017-2021 RA
# source at T:\03_BOARDS_COMMITTEES\05_TBNMC\2022_RA_Update\01_FUNDING_OUT\DELIVERABLES\TO-9\datastick_deliverables\2017-2021LUEntityLoads
dpsmosdat2 <- read_sas(here('data/raw/dps1721monthentbas.sas7bdat')) %>% 
  inner_join(segidmos, by = 'bayseg') %>%
  mutate(
    source = case_when(
      grepl('REUSE$', source2) ~ 'DPS - reuse', 
      grepl('SW$', source2) ~ 'DPS - end of pipe'
    )
  ) %>% 
  select(year = Year, month = Month, bay_segment, entity, facility = facname, source, tn_load = tnloadtons) 

npsmosdat <- bind_rows(npsmosdat, npsmosdat2)
ipsmosdat <- bind_rows(ipsmosdat, ipsmosdat2)
dpsmosdat <- bind_rows(dpsmosdat, dpsmosdat2)

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

# non-point source prior to 2017-2021 RA
npsmosdat <- read_sas(here('data/raw/nps0420monthentbaslu.sas7bdat')) %>% 
  group_by(entity, year, month) %>% 
  summarise(
    tn_load = sum(tnloadtons, na.rm = T), 
    .groups = 'drop'
  ) %>% 
  mutate(
    source = 'NPS'
  ) %>% 
  filter(year < 2017)

# industrial point source prior to 2017-2021 RA
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
  filter(year < 2017)

# domestic point source prior to 2017-2021 RA 
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
  ) %>% 
  filter(year < 2017)

# non-point source 2017-2021 RA
# source at T:\03_BOARDS_COMMITTEES\05_TBNMC\2022_RA_Update\01_FUNDING_OUT\DELIVERABLES\TO-9\datastick_deliverables\2017-2021LUEntityLoads
npsmosdat2 <- read_sas(here('data/raw/nps1721monthenbaslu.sas7bdat')) %>% 
  group_by(entity, year, month) %>% 
  summarise(
    tn_load = sum(tnloadtons, na.rm = T), 
    .groups = 'drop'
  ) %>% 
  mutate(
    source = 'NPS'
  )

# industrial point source 2017-2021 RA
# source at T:\03_BOARDS_COMMITTEES\05_TBNMC\2022_RA_Update\01_FUNDING_OUT\DELIVERABLES\TO-9\datastick_deliverables\2017-2021LUEntityLoads
ipsmosdat2 <- read_sas(here('data/raw/ips1721monthentbas.sas7bdat')) %>% 
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

# domestic point source 2017-2021 RA
# source at T:\03_BOARDS_COMMITTEES\05_TBNMC\2022_RA_Update\01_FUNDING_OUT\DELIVERABLES\TO-9\datastick_deliverables\2017-2021LUEntityLoads
dpsmosdat2 <- read_sas(here('data/raw/dps1721monthentbas.sas7bdat')) %>% 
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

npsmosdat <- bind_rows(npsmosdat, npsmosdat2)
ipsmosdat <- bind_rows(ipsmosdat, ipsmosdat2)
dpsmosdat <- bind_rows(dpsmosdat, dpsmosdat2)

npsdpsipsent <- bind_rows(npsmosdat, ipsmosdat, dpsmosdat) %>% 
  select(year, month, entity, source, tn_load)
  
save(npsdpsipsent, file = here('data/npsdpsipsent.RData'), version = 2)

# nps tn by land use ------------------------------------------------------

# non-point source prior to 2017 - 2022 RA
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
  select(year, month, bay_segment, `land use` = DESCRIPTION, source, tn_load) %>% 
  filter(year < 2017)

# non-point source 2017 - 2022 RA
# source at T:\03_BOARDS_COMMITTEES\05_TBNMC\2022_RA_Update\01_FUNDING_OUT\DELIVERABLES\TO-9\datastick_deliverables\2017-2021LUEntityLoads
npsmosludat2 <- read_sas(here('data/raw/nps1721monthenbaslu.sas7bdat')) %>% 
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

npsmosludat <- bind_rows(npsmosludat, npsmosludat2)

save(npsmosludat, file = here('data/npsmosludat.RData'), version = 2)
