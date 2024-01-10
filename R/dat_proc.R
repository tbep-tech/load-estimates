library(tidyverse)
library(lubridate)
library(haven)
library(readxl)
library(here)

source('R/funcs.R')

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

segidall <- tibble(
  bayseg = c(1, 2, 3, 4, 5, 6, 7, 55),
  bay_segment = c('Old Tampa Bay', 'Hillsborough Bay', 'Middle Tampa Bay', 'Lower Tampa Bay', 'Boca Ciega Bay', 'Terra Ceia Bay', 'Manatee River', 'Boca Ciega Bay South')
)

# coastal land use code lookup
clucs_lkup <- read.csv('data/raw/CLUCSID_lookup.csv') %>% 
  select(CLUCSID, DESCRIPTION) %>% 
  unique

# updated hfc/city of tampa data --------------------------------------------------------------

# see email from JH, 10/4/23
# dps_est function includes methods from RP SAS code specific to city of tampa data
# these data are applied to many below for the correction to DPS load data
dpsupdate <- dps_est(here('data/raw/Copy of TBNMC Point Source Data Reporting Tool HFC 2012-22 REVISED.xls'))

save(dpsupdate, file = here('data/dpsupdate.RData'))

# tn load by source for major bay segments ----------------------------------------------------

# hfc/city of tampa updated data
load(file = here('data/dpsupdate.RData'))

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

# correction to dat from hfc update
dpscorr <- dpsdiff_fun(dpsupdate, annual = T, total = T, varsel = 'tn_load') %>% 
  filter(year < 2021) %>% 
  select(-entity) %>% 
  mutate(source = 'DPS')

dat <- dat %>% 
  left_join(dpscorr, by = c('year', 'source', 'bay_segment')) %>% 
  mutate(
    tn_load = ifelse(is.na(tn_load_diffv), tn_load, tn_load + tn_load_diffv)
  ) %>% 
  select(-tn_load_diffv)

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

# correction to loadra1721 from hfc update
dpscorr <- dpsdiff_fun(dpsupdate, annual = T, total = T, varsel = 'tn_load') %>% 
  filter(year > 2016 & year < 2022) %>% 
  select(-entity) %>% 
  mutate(source = 'DPS')

loadra1721 <- loadra1721 %>% 
  left_join(dpscorr, by = c('year', 'source', 'bay_segment')) %>% 
  mutate(
    tn_load = ifelse(is.na(tn_load_diffv), tn_load, tn_load + tn_load_diffv)
  ) %>% 
  select(-tn_load_diffv)

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

# hfc/city of tampa updated data
load(file = here('data/dpsupdate.RData'))

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
  filter(year < 2017)

# correction to totanndatpre from hfc update
dpscorr <- dpsdiff_fun(dpsupdate, annual = T, total = T, varsel = c('tn_load', 'tp_load', 'hy_load')) %>% 
  filter(year < 2017) %>% 
  select(-entity, -source)

totanndatpre <- totanndatpre %>% 
  left_join(dpscorr, by = c('year', 'bay_segment')) %>% 
  mutate(
    tn_load = ifelse(is.na(tn_load_diffv), tn_load, tn_load + tn_load_diffv),
    tp_load = ifelse(is.na(tp_load_diffv), tp_load, tp_load + tp_load_diffv),
    hy_load = ifelse(is.na(hy_load_diffv), hy_load, hy_load + hy_load_diffv)
  ) %>% 
  select(-tn_load_diffv, -tp_load_diffv, -hy_load_diffv) %>% 
  mutate(
    tnhy = tn_load / hy_load, 
    tphy = tp_load / hy_load
  )  

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
  )

# correction to totanndatos from hfc update
dpscorr <- dpsdiff_fun(dpsupdate, annual = T, total = T, varsel = c('tn_load', 'tp_load', 'hy_load')) %>% 
  filter(year > 2016 & year < 2022) %>% 
  select(-entity, -source)

totanndatpos <- totanndatpos %>% 
  left_join(dpscorr, by = c('year', 'bay_segment')) %>% 
  mutate(
    tn_load = ifelse(is.na(tn_load_diffv), tn_load, tn_load + tn_load_diffv),
    tp_load = ifelse(is.na(tp_load_diffv), tp_load, tp_load + tp_load_diffv),
    hy_load = ifelse(is.na(hy_load_diffv), hy_load, hy_load + hy_load_diffv)
  ) %>% 
  select(-tn_load_diffv, -tp_load_diffv, -hy_load_diffv) %>% 
  mutate(
    tnhy = tn_load / hy_load, 
    tphy = tp_load / hy_load
  )  

totanndat <- bind_rows(totanndatpre, totanndatpos)

# totals sum by segments
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

# hfc/city of tampa updated data
load(file = here('data/dpsupdate.RData'))

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

# correction to mosdat from hfc update
dpscorr <- dpsdiff_fun(dpsupdate, annual = F, total = T, varsel = c('tn_load', 'tp_load', 'tss_load', 'bod_load')) %>% 
  filter(year > 2016 & year < 2022) %>% 
  select(-entity, -source) %>% 
  mutate(source = 'DPS')

# add correction
mosdat <- mosdat %>% 
  left_join(dpscorr, by = c('source', 'year', 'month', 'bay_segment')) %>% 
  mutate(
    tn_load = ifelse(is.na(tn_load_diffv), tn_load, tn_load + tn_load_diffv),
    tp_load = ifelse(is.na(tp_load_diffv), tp_load, tp_load + tp_load_diffv),
    tss_load = ifelse(is.na(tss_load_diffv), tss_load, tss_load + tss_load_diffv),
    bod_load = ifelse(is.na(bod_load_diffv), bod_load, bod_load + bod_load_diffv)
  ) %>% 
  select(-tn_load_diffv, -tp_load_diffv, -tss_load_diffv, -bod_load_diffv)

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

# hfc/city of tampa updated data
load(file = here('data/dpsupdate.RData'))

# original at T:/03_BOARDS_COMMITTEES/05_TBNMC/2022_RA_Update/01_FUNDING_OUT/DELIVERABLES/TO-9/datastick_deliverables/2017-2021Annual&MonthlyLoadDatasets/MakeMonthAnnDatasets/Monthly/monthly1721entityloaddataset.sas7bdat
mosentdat <- read_sas(here('data/raw/monthly1721entityloaddataset.sas7bdat')) %>% 
  select(bayseg, entity, year = YEAR, month = MONTH, source, tnloadtons, tploadtons, tssloadtons, bodloadtons) %>% 
  mutate(
    source = case_when(
      source == 'Atmospheric Deposition' ~ 'AD', 
      source %in% c('Springs', 'Ground Water') ~ 'GWS', 
      source %in% c('PS - Domestic - REUSE', 'PS - Domestic - SW') ~ 'DPS', 
      source %in% c('PS - Industrial', 'Material Losses') ~ 'IPS', 
      source == 'Non-Point Source' ~ 'NPS'
    )
  ) %>% 
  group_by(entity, year, month, source, bayseg) %>% 
  summarise(
    tnload = sum(tnloadtons), 
    tpload = sum(tploadtons), 
    tssload = sum(tssloadtons), 
    bodload = sum(bodloadtons),
    .groups = 'drop'
  ) %>% 
  select(year, month, bayseg, entity, source, tn_load = tnload)

# format corrected HFC/City of Tampa DPS

# hfc/city of tampa updated data
newdat <- dpsupdate %>% 
  filter(Year > 2016 & Year < 2022) %>% 
  summarise(
    tn_load = sum(tn_load), 
    .by = c('Year', 'Month', 'entity', 'bayseg')
  ) %>%
  mutate(source = 'DPS') %>% 
  select(year = Year, month = Month, bayseg, entity, source, tn_load)

# swap out old hfc/city of tampa with new
mosentdat[mosentdat$entity == 'Tampa' & mosentdat$source == 'DPS', ] <- newdat

mosentdat <- mosentdat %>% 
  mutate(
    bayseg = factor(bayseg, levels = segidmos$bayseg, labels = segidmos$bay_segment), 
    bayseg = as.character(bayseg)
  )

save(mosentdat, file = here('data/mosentdat.RData'))

# all monthly hydro load --------------------------------------------------

# hfc/city of tampa updated data
load(file = here('data/dpsupdate.RData'))

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
dat4 <- read_excel(here('data/raw/TotH2O_2021_Monthly4Seg.xlsx')) %>% 
  mutate(
    bay_segment = factor(Segment, levels = c('1', '2', '3', '4'), labels = c('OTB', 'HB', 'MTB', 'LTB'))
  ) %>% 
  select(year = Year, month = Month, bay_segment, hy_load = `H2O Load (106 m3/yr)`)

# rltb 2021, sent via email from RP 3/10/23
dat5 <- read_excel(here('data/raw/RLTB21MnthH2O.xlsx')) %>% 
  mutate(
    bay_segment = factor(`BaySeg (RLTB)`, levels = '5567', labels = 'RLTB')
  ) %>% 
  select(year = Year, month = Month, bay_segment, hy_load = `H2O Load (million m3/month)`)

mohydat <- bind_rows(dat1, dat2, dat3, dat4, dat5) %>% 
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

# correction to mohydat from hfc update
dpscorr <- dpsdiff_fun(dpsupdate, annual = F, total = T, varsel = 'hy_load') %>% 
  filter(year > 2016 & year < 2022) %>% 
  select(-entity, -source)

# add correction
mohydat <- mohydat %>% 
  left_join(dpscorr, by = c('year', 'month', 'bay_segment')) %>% 
  mutate(
    hy_load_106_m3_mo = ifelse(is.na(hy_load_diffv), hy_load_106_m3_mo, hy_load_106_m3_mo + hy_load_diffv)
  ) %>% 
  select(-hy_load_diffv)
  
allmohydat <- mohydat %>% 
  group_by(year, month) %>% 
  summarise(
    hy_load_106_m3_mo = sum(hy_load_106_m3_mo), 
    .groups = 'drop'
  ) %>% 
  mutate(bay_segment = 'All Segments (- N. BCB)') %>% 
  select(year, month, bay_segment, hy_load_106_m3_mo)

# 1985 to 2016
oldmohydat <- read_excel(here('data/raw/Tampa Bay Loadings 1985-2016.xlsx'), sheet = 'Monthly H2O Loads') %>% 
  mutate(
    bay_segment = case_when(
      Month == 1 ~ 'Old Tampa Bay', 
      Month == 2 ~ 'Hillsborough Bay', 
      Month == 3 ~ 'Middle Tampa Bay', 
      Month == 4 ~ 'Lower Tampa Bay', 
      Month == 5 ~ 'Boca Ciega Bay', 
      Month == 6 ~ 'Terra Ceia Bay', 
      Month == 7 ~ 'Manatee River'
    )
  ) %>% 
  select(
    year = YEAR, 
    month = MONTH, 
    bay_segment, 
    hy_load_106_m3_mo = `H2O Load (million m3/month)`
  ) %>% 
  mutate(
    bay_segment = case_when(
      bay_segment %in% c('Boca Ciega Bay', 'Terra Ceia Bay', 'Manatee River') ~ 'Remainder Lower Tampa Bay', 
      T ~ bay_segment
    )
  ) %>% 
  summarise(
    hy_load_106_m3_mo = sum(hy_load_106_m3_mo), 
    .by = c(year, month, bay_segment)
  )

# correction to oldmohydat from hfc update
dpscorr <- dpsdiff_fun(dpsupdate, annual = F, total = T, varsel = 'hy_load') %>% 
  filter(year < 2017) %>% 
  select(-entity, -source)

# add correction
oldmohydat <- oldmohydat %>% 
  left_join(dpscorr, by = c('year', 'month', 'bay_segment')) %>% 
  mutate(
    hy_load_106_m3_mo = ifelse(is.na(hy_load_diffv), hy_load_106_m3_mo, hy_load_106_m3_mo + hy_load_diffv)
  ) %>% 
  select(-hy_load_diffv)

alloldmohydat <- oldmohydat %>% 
  summarise(
    hy_load_106_m3_mo = sum(hy_load_106_m3_mo), 
    .by = c(year, month)
  ) %>% 
  mutate(
    bay_segment = 'All Segments (- N. BCB)'
  )

mohydat <- bind_rows(oldmohydat, alloldmohydat, mohydat, allmohydat) %>% 
  arrange(bay_segment, year, month)

save(mohydat, file = here('data/mohydat.RData'))

# write.csv(mohydat, '~/Desktop/mohydat.csv', quote = F, row.names = F)

# monthly ips, dps, nps ---------------------------------------------------

# hfc/city of tampa updated data
load(file = here('data/dpsupdate.RData'))

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

# correction to dpsmosdat from hfc update
dpscorr <- dpsdiff_fun(dpsupdate, annual = F, total = F, varsel = 'tn_load')

dpsmosdat <- dpsmosdat %>% 
  left_join(dpscorr, by = c('year', 'month', 'bay_segment', 'entity', 'source')) %>% 
  mutate(
    tn_load = ifelse(is.na(tn_load_diffv), tn_load, tn_load + tn_load_diffv)
  ) %>% 
  select(-tn_load_diffv)

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

# hfc/city of tampa updated data
load(file = here('data/dpsupdate.RData'))

##
# non-point source prior to 2017-2021 RA
npsmosdat1 <- read_sas(here('data/raw/nps0420monthentbaslu.sas7bdat')) %>% 
  filter(year < 2017)

# non-point source 2017-2021 RA
# source at T:\03_BOARDS_COMMITTEES\05_TBNMC\2022_RA_Update\01_FUNDING_OUT\DELIVERABLES\TO-9\datastick_deliverables\2017-2021LUEntityLoads
npsmosdat2 <- read_sas(here('data/raw/nps1721monthenbaslu.sas7bdat')) 

npsmosdat <- bind_rows(npsmosdat1, npsmosdat2) %>% 
  group_by(entity, year, month, bayseg) %>% 
  summarise(
    tn_load = sum(tnloadtons, na.rm = T), 
    .groups = 'drop'
  ) %>% 
  mutate(
    source = 'NPS'
  )

##
# industrial point source prior to 2017-2021 RA
ipsmosdat1 <- read_sas(here('data/raw/ips0420monthentbas.sas7bdat')) %>% 
  filter(Year < 2017)

# industrial point source 2017-2021 RA
# source at T:\03_BOARDS_COMMITTEES\05_TBNMC\2022_RA_Update\01_FUNDING_OUT\DELIVERABLES\TO-9\datastick_deliverables\2017-2021LUEntityLoads
ipsmosdat2 <- read_sas(here('data/raw/ips1721monthentbas.sas7bdat')) 

ipsmosdat <- bind_rows(ipsmosdat1, ipsmosdat2) %>%
  rename(
    year = Year, 
    month = Month
  ) %>% 
  group_by(entity, year, month, bayseg) %>% 
  summarise(
    tn_load = sum(tnloadtons, na.rm = T), 
    .groups = 'drop'
  ) %>% 
  mutate(
    source = 'PS'
  )

##
# domestic point source prior to 2017-2021 RA 
dpsmosdat1 <- read_sas(here('data/raw/dps0420monthentbas.sas7bdat')) %>% 
  filter(Year < 2017)

# domestic point source 2017-2021 RA
# source at T:\03_BOARDS_COMMITTEES\05_TBNMC\2022_RA_Update\01_FUNDING_OUT\DELIVERABLES\TO-9\datastick_deliverables\2017-2021LUEntityLoads
dpsmosdat2 <- read_sas(here('data/raw/dps1721monthentbas.sas7bdat'))

dpsmosdat <- bind_rows(dpsmosdat1, dpsmosdat2) %>% 
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
  group_by(entity, source, year, month, bayseg) %>% 
  summarise(
    tn_load = sum(tnloadtons, na.rm = T), 
    .groups = 'drop'
  )

# format corrected HFC/City of Tampa DPS

# calculate dps load data from raw
newdat <- dpsupdate %>% 
  filter(Year < 2022) %>% 
  select(entity, source, year = Year, month = Month, bayseg, tn_load) %>% 
  arrange(source)

# swap out old hfc/city of tampa with new
dpsmosdat[dpsmosdat$year > 2011 & dpsmosdat$entity == 'Tampa', ] <- newdat

##
# combine all
npsdpsipsent <- bind_rows(npsmosdat, ipsmosdat, dpsmosdat) %>% 
  select(year, month, bayseg, entity, source, tn_load) %>% 
  mutate(
    bayseg = factor(bayseg, levels = segidall$bayseg, labels = segidall$bay_segment),
    bayseg = as.character(bayseg)
  )
  
save(npsdpsipsent, file = here('data/npsdpsipsent.RData'))

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
