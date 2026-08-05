# setup ---------------------------------------------------------------------------------------

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

# TBEP-derived (tbeploads/tbeploadsproc) 2022-2025 estimates, replacing RP's 2022-2024
# deliverables and extending coverage through 2025 (RP has not delivered 2025 data).
# See tbeploadsproc/R/09_export_2225.R for full provenance and how to regenerate this
# bundle whenever tbeploads/tbeploadsproc inputs change (e.g. a future 2026 refresh).
load(here('data/raw/tbep2225.RData'))

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
    tn_load = sum(tn_load, na.rm = T),
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
    tn_load = sum(tn_load, na.rm = T), 
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
    tn_load = sum(tn_load, na.rm = T),
    .groups = 'drop'
  ) %>% 
  mutate(bay_segment = 'All Segments (- N. BCB)')

# 2022 - 2025, TBEP-derived (replaces RP's SrcSegAnnLoad2224.csv deliverable; see
# tbeploadsproc/R/09_export_2225.R)
loadratbep2225 <- bind_rows(ad2225seg_yr, dps2225seg_yr, gw2225seg_yr, ips2225seg_yr, ml2225seg_yr, nps2225seg_yr, spr2225seg_yr) %>%
  select(Year, source, segment, tn_load) %>%
  left_join(segidall, by = c('segment' = 'bay_segment')) %>%
  left_join(segidmos, by = 'bayseg') %>%
  filter(!is.na(bay_segment)) %>%   # drops Boca Ciega Bay (bayseg 5), matches historical N. BCB exclusion
  mutate(source = recode_src5(source)) %>%
  group_by(year = Year, bay_segment, source) %>%
  summarise(tn_load = sum(tn_load, na.rm = T), .groups = 'drop')

loadratbep2225tots <- loadratbep2225 %>%
  group_by(year, source) %>%
  summarise(tn_load = sum(tn_load, na.rm = T), .groups = 'drop') %>%
  mutate(bay_segment = 'All Segments (- N. BCB)')

tnanndat <- bind_rows(tnanndat, loadra1721, loadra1721tots, loadratbep2225, loadratbep2225tots) %>%
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

# 2022 - 2025, TBEP-derived (replaces RP's TOTLoadsRASeg2224.csv deliverable; see
# tbeploadsproc/R/09_export_2225.R)
totanndattbep2225 <- bind_rows(ad2225seg_yr, dps2225seg_yr, gw2225seg_yr, ips2225seg_yr, ml2225seg_yr, nps2225seg_yr, spr2225seg_yr) %>%
  left_join(segidall, by = c('segment' = 'bay_segment')) %>%
  left_join(segidmos, by = 'bayseg') %>%
  filter(!is.na(bay_segment)) %>%
  group_by(year = Year, bay_segment) %>%
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

totanndat <- bind_rows(totanndatpre, totanndatpos, totanndattbep2225)

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
    tnload = sum(tnloadtons, na.rm = T), 
    tpload = sum(tploadtons, na.rm = T), 
    tssload = sum(tssloadtons, na.rm = T), 
    bodload = sum(bodloadtons, na.rm = T),
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

##
# 1995 to 2016 load by month, source, segment
# see RP email 1/31/24
#
# notes from RP
# OTB (1): no IPS for 2008-2014, no records for IPS for those years.
# LTB (4): no IPS for 1999-7/2002, no records for IPS for those years.
# MR (7): no IPS for 2008-2012, no records for IPS for those years.
# BCBS (55): no IPS ever, no records for IPS.
# For materials handling losses (PO for Ports), only HB (2) and LTB (4) have records.
# For springs, only records are for HB (2).
pastmosdat <- read_sas(here('data/raw/tbloadmonthsrcseg9516.sas7bdat')) %>% 
  select(
    year = YEAR, 
    month = MONTH, 
    bay_segment = bay_seg, 
    source, 
    tn_load = tnloadkg, 
    tp_load = tploadkg, 
    tss_load = tssloadkg, 
    bod_load = bodloadkg
  ) %>% 
  filter(bay_segment != 5) %>% # remove boca ciega bay, there's a separate segment (55) for bcb south
  mutate_at(vars(tn_load, tp_load, tss_load, bod_load), ~ . / 907.2) %>% # convert to tons
  mutate(
    bay_segment = factor(bay_segment, 
                     levels = segidmos$bayseg,
                     labels = segidmos$bay_segment
                       ), 
    bay_segment = as.character(bay_segment),
    source = case_when(
      source %in% c('GW', 'SPR') ~ 'GWS', 
      source %in% c('PO') ~ 'IPS', # PO ports or material losses, grouped with IPS for original mosdat (2017-2022)
      T ~ source
    )
  ) %>% 
  summarise(
    tn_load = sum(tn_load, na.rm = T), 
    tp_load = sum(tp_load, na.rm = T), 
    tss_load = sum(tss_load, na.rm = T), 
    bod_load = sum(bod_load, na.rm = T),
    .by = c('year', 'month', 'bay_segment', 'source')
  ) %>% 
  complete(year, month, bay_segment, source, fill = list(tn_load = 0, tp_load = 0, tss_load = 0, bod_load = 0))

##
# combine all for 1995 to 2021

mosdat <- bind_rows(mosdat, pastmosdat) %>% 
  arrange(year, bay_segment, month, source)

# correction to mosdat from hfc update
dpscorr <- dpsdiff_fun(dpsupdate, annual = F, total = T, varsel = c('tn_load', 'tp_load', 'tss_load', 'bod_load')) %>% 
  filter(year < 2022) %>% 
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
    tn_load = sum(tn_load, na.rm = T),
    tp_load = sum(tp_load, na.rm = T), 
    tss_load = sum(tss_load, na.rm = T), 
    bod_load = sum(bod_load, na.rm = T),
    .groups = 'drop'
  ) %>% 
  mutate(bay_segment = 'All Segments (- N. BCB)')

# 2022 - 2025, TBEP-derived (replaces RP's monthly2224entityloaddataset.csv deliverable;
# see tbeploadsproc/R/09_export_2225.R). AD and GW have no TSS/BOD load estimates in
# tbeploads (not modeled -- atmospheric deposition and groundwater have no established
# TSS/BOD methodology). These contribute NA for tss_load/bod_load and are summed with
# na.rm = TRUE, equivalent to a 0 contribution, consistent with how AD/GWS have always
# been handled in this dataset.
mosdattbep2225 <- bind_rows(ad2225seg_mo, dps2225seg_mo, gw2225seg_mo, ips2225seg_mo, ml2225seg_mo, nps2225seg_mo, spr2225seg_mo) %>%
  left_join(segidall, by = c('segment' = 'bay_segment')) %>%
  left_join(segidmos, by = 'bayseg') %>%
  filter(!is.na(bay_segment)) %>%
  mutate(source = recode_src5(source)) %>%
  group_by(year = Year, month = Month, bay_segment, source) %>%
  summarise(
    tn_load = sum(tn_load, na.rm = T),
    tp_load = sum(tp_load, na.rm = T),
    tss_load = sum(tss_load, na.rm = T),
    bod_load = sum(bod_load, na.rm = T),
    .groups = 'drop'
  )

totsmotbep2225 <- mosdattbep2225 %>%
  group_by(year, month, source) %>%
  summarise(
    tn_load = sum(tn_load, na.rm = T),
    tp_load = sum(tp_load, na.rm = T),
    tss_load = sum(tss_load, na.rm = T),
    bod_load = sum(bod_load, na.rm = T),
    .groups = 'drop'
  ) %>%
  mutate(bay_segment = 'All Segments (- N. BCB)')

mosdat <- bind_rows(mosdat, totsmo, mosdattbep2225, totsmotbep2225) %>%
  arrange(year, bay_segment, month, source) %>%
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
    tnload = sum(tnloadtons, na.rm = T), 
    tpload = sum(tploadtons, na.rm = T), 
    tssload = sum(tssloadtons, na.rm = T), 
    bodload = sum(bodloadtons, na.rm = T),
    .groups = 'drop'
  ) %>% 
  select(year, month, bayseg, entity, source, tn_load = tnload)

# format corrected HFC/City of Tampa DPS

# hfc/city of tampa updated data
newdat <- dpsupdate %>% 
  filter(Year > 2016 & Year < 2022) %>% 
  summarise(
    tn_load = sum(tn_load, na.rm = T), 
    .by = c('Year', 'Month', 'entity', 'bay_segment')
  ) %>%
  mutate(
    source = 'DPS', 
    bayseg = 2
  ) %>% 
  select(year = Year, month = Month, bayseg, entity, source, tn_load)

# swap out old hfc/city of tampa with new
mosentdat[mosentdat$entity == 'Tampa' & mosentdat$source == 'DPS', ] <- newdat

# 2022 - 2025, TBEP-derived (replaces RP's monthly2224entityloaddataset.csv deliverable;
# see tbeploadsproc/R/09_export_2225.R). DPS/IPS/ML come directly from tbeploads'
# entity+segment monthly summaries (anlz_dps/anlz_ips/anlz_ml with summ = 'entity'), which
# already resolve entity, segment, and source correctly -- no crosswalk join needed here.
# NPS/MS4 entities require nps_entmo_fun() (see R/funcs.R): no tbeploads function
# produces monthly, entity-resolved NPS loads directly (anlz_nps has no entity grouping
# option, and anlz_aa, the only NPS -> entity allocator, is annual-only), so this is a
# modeled approximation -- see the function's own documentation for the full algorithm.
dpsmosenttbep2225 <- dps2225entseg_mo %>%
  mutate(source = recode_src5(source), bayseg = segidall$bayseg[match(segment, segidall$bay_segment)]) %>%
  filter(bayseg %in% segidmos$bayseg) %>%   # drops Boca Ciega Bay (bayseg 5), matches historical N. BCB exclusion
  group_by(year = Year, month = Month, bayseg, entity, source) %>%
  summarise(tn_load = sum(tn_load, na.rm = T), .groups = 'drop')

ipsmosenttbep2225 <- bind_rows(ips2225entseg_mo, ml2225entseg_mo) %>%
  mutate(source = recode_src5(source), bayseg = segidall$bayseg[match(segment, segidall$bay_segment)]) %>%
  filter(bayseg %in% segidmos$bayseg) %>%   # drops Boca Ciega Bay (bayseg 5), matches historical N. BCB exclusion
  group_by(year = Year, month = Month, bayseg, entity, source) %>%
  summarise(tn_load = sum(tn_load, na.rm = T), .groups = 'drop')

npsmosenttbep2225 <- nps_entmo_fun(npsfactors, nps2225bas_mo, aa2225_yr)

mosentdattbep2225 <- bind_rows(dpsmosenttbep2225, ipsmosenttbep2225, npsmosenttbep2225)

mosentdat <- bind_rows(mosentdat, mosentdattbep2225) %>%
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
    hy_load_106_m3_mo = sum(hy_load_106_m3_mo, na.rm = T), 
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
    hy_load_106_m3_mo = sum(hy_load_106_m3_mo, na.rm = T), 
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
    hy_load_106_m3_mo = sum(hy_load_106_m3_mo, na.rm = T), 
    .by = c(year, month)
  ) %>% 
  mutate(
    bay_segment = 'All Segments (- N. BCB)'
  )

# 2022 - 2025, TBEP-derived (replaces RP's segmonthlyh2o_2224_allsources_allsegs.csv
# deliverable; see tbeploadsproc/R/09_export_2225.R). ml2225seg_mo is excluded --
# Material Loss has no outfall/hydro concept, hy_load is always NA there.
mohydattbep2225 <- bind_rows(ad2225seg_mo, dps2225seg_mo, gw2225seg_mo, ips2225seg_mo, nps2225seg_mo, spr2225seg_mo) %>%
  left_join(segidall, by = c('segment' = 'bay_segment')) %>%
  left_join(segidmos, by = 'bayseg') %>%
  filter(!is.na(bay_segment)) %>%
  group_by(year = Year, month = Month, bay_segment) %>%
  summarise(hy_load_106_m3_mo = sum(hy_load, na.rm = T), .groups = 'drop')

allmohydattbep2225 <- mohydattbep2225 %>%
  summarise(
    hy_load_106_m3_mo = sum(hy_load_106_m3_mo, na.rm = T),
    .by = c(year, month)
  ) %>%
  mutate(
    bay_segment = 'All Segments (- N. BCB)'
  )

mohydat <- bind_rows(oldmohydat, alloldmohydat, mohydat, allmohydat, mohydattbep2225, allmohydattbep2225) %>%
  arrange(bay_segment, year, month)

save(mohydat, file = here('data/mohydat.RData'))

# write.csv(mohydat, '~/Desktop/mohydat.csv', quote = F, row.names = F)

# monthly ips, dps, nps ---------------------------------------------------

# hfc/city of tampa updated data
load(file = here('data/dpsupdate.RData'))

# non-point source prior to 2017-2021 RA
# T:\09_TECHNICAL_PROJECTS\TECHNICAL_SUPPORT\00_CONTRACTS\02_FUNDING_OUT\ESA-Janicki\04_DELIVERABLES\TO5
npsmosdat <- read_sas(here('data/raw/nps0420monthentbaslu.sas7bdat')) %>% 
  inner_join(segidmos, by = 'bayseg') %>% 
  left_join(clucs_lkup, by = 'CLUCSID') %>% 
  mutate(
    source = 'NPS'
  ) %>% 
  select(year, month, bay_segment, entity, lu = DESCRIPTION, source, tn_load = tnloadtons) %>% filter(year < 2017)

# industrial point source prior to 2017-2021 RA
# T:\09_TECHNICAL_PROJECTS\TECHNICAL_SUPPORT\00_CONTRACTS\02_FUNDING_OUT\ESA-Janicki\04_DELIVERABLES\TO5
ipsmosdat <- read_sas(here('data/raw/ips0420monthentbas.sas7bdat')) %>% 
  inner_join(segidmos, by = 'bayseg') %>% 
  mutate(
    source = 'IPS'
  ) %>% 
  select(year = Year, month = Month, bay_segment, facility = facname, source, tn_load = tnloadtons) %>% filter(year < 2017)

# domestic point source prior to 2017-2021 RA 
# T:\09_TECHNICAL_PROJECTS\TECHNICAL_SUPPORT\00_CONTRACTS\02_FUNDING_OUT\ESA-Janicki\04_DELIVERABLES\TO5
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

# 2022 - 2025, TBEP-derived (replaces RP's NPS2224MonthEnBasLU.csv/IPS2224MonthEntBas.csv/
# DPS2224MonthEntBas.csv deliverables; see tbeploadsproc/R/09_export_2225.R).
#
# npsmosdat: nps2225lu_mo (aslu = TRUE) has no entity column -- tbeploads' land-use
# breakdown decomposes by segment/basin/land-use only, not jurisdiction, and the only
# consumer of npsmosdat's entity column (index.Rmd) sums over entity before use anyway,
# so this is not a loss of information that matters downstream. aslu = TRUE also only
# decomposes ungaged-basin loads by land use; gaged-basin loads are gauge-measured, with
# nothing to decompose -- an inherent modeling limitation shared by RP's historical data,
# not a new gap.
npsmosdattbep2225 <- nps2225lu_mo %>%
  left_join(segidall, by = c('segment' = 'bay_segment')) %>%
  left_join(segidmos, by = 'bayseg') %>%
  filter(!is.na(bay_segment)) %>%
  mutate(source = 'NPS') %>%
  group_by(year = Year, month = Month, bay_segment, lu, source) %>%
  summarise(tn_load = sum(tn_load, na.rm = T), .groups = 'drop')

# ips by mo, facility 2022 - 2025 (ML folded into IPS per existing convention)
ipsmosdattbep2225 <- bind_rows(ips2225facseg_mo, ml2225facseg_mo) %>%
  left_join(segidall, by = c('segment' = 'bay_segment')) %>%
  left_join(segidmos, by = 'bayseg') %>%
  filter(!is.na(bay_segment)) %>%
  mutate(source = 'IPS') %>%
  group_by(year = Year, month = Month, bay_segment, facility, source) %>%
  summarise(tn_load = sum(tn_load, na.rm = T), .groups = 'drop')

# dps by mo, facility 2022 - 2025 -- anlz_dps() already resolves source to "DPS - end of
# pipe"/"DPS - reuse" internally, no recoding needed
dpsmosdattbep2225 <- dps2225facseg_mo %>%
  left_join(segidall, by = c('segment' = 'bay_segment')) %>%
  left_join(segidmos, by = 'bayseg') %>%
  filter(!is.na(bay_segment)) %>%
  group_by(year = Year, month = Month, bay_segment, entity, facility, source) %>%
  summarise(tn_load = sum(tn_load, na.rm = T), .groups = 'drop')

npsmosdat <- bind_rows(npsmosdat, npsmosdat2, npsmosdattbep2225)
ipsmosdat <- bind_rows(ipsmosdat, ipsmosdat2, ipsmosdattbep2225)
dpsmosdat <- bind_rows(dpsmosdat, dpsmosdat2, dpsmosdattbep2225)

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

# 2022 - 2025, TBEP-derived (replaces RP's NPS2224MonthEnBasLU.csv deliverable; see
# tbeploadsproc/R/09_export_2225.R) -- same aslu = TRUE source as npsmosdattbep2225
# above, just without entity (npsmosludat was already entity-less historically)
npsmosludattbep2225 <- npsmosdattbep2225 %>%
  rename(`land use` = lu)

npsmosludat <- bind_rows(npsmosludat, npsmosludat2, npsmosludattbep2225)

save(npsmosludat, file = here('data/npsmosludat.RData'), version = 2)
