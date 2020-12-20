
library(tidyverse)
library(haven)

# segment id
segid <- tibble(
  BAY_SEG = c(1, 2, 3, 4, 5567),
  bay_segment = c('Old Tampa Bay', 'Hillsborough Bay', 'Middle Tampa Bay', 'Lower Tampa Bay', 'Remainder Lower Tampa Bay')
)

# sas files ---------------------------------------------------------------

# 85 - 19
ad8519 <- read_sas('~/Desktop/TBEP/LoadingCodes&Datasets1719_toTBEP_20201117/TotalLoads1719/TotalLoads2019/ad_8519.sas7bdat')
dps8519 <- read_sas('~/Desktop/TBEP/LoadingCodes&Datasets1719_toTBEP_20201117/TotalLoads1719/TotalLoads2019/dps_8519.sas7bdat')
ips8519 <- read_sas('~/Desktop/TBEP/LoadingCodes&Datasets1719_toTBEP_20201117/TotalLoads1719/TotalLoads2019/ips_ml_8519.sas7bdat')
nps8519 <- read_sas('~/Desktop/TBEP/LoadingCodes&Datasets1719_toTBEP_20201117/TotalLoads1719/TotalLoads2019/nps_8519.sas7bdat')
gws8519 <- read_sas('~/Desktop/TBEP/LoadingCodes&Datasets1719_toTBEP_20201117/TotalLoads1719/TotalLoads2019/gws_8519.sas7bdat')

# h2o load data prior to 2012, no source info
hyolddat <- read_sas('~/Desktop/TBEP/LoadingCodes&Datasets1719_toTBEP_20201117/TotalLoads1719/TotalLoads2019/h2oannseg8511.sas7bdat')

# TN is in tons / yr, h2o is in million m3 / yr
dat <- bind_rows(ad8519, dps8519, ips8519, nps8519, gws8519) %>% 
  left_join(segid, by = 'BAY_SEG') %>% 
  rename(
    hy_load = h2oload10e6m3, 
    tn_load = TN_tons
  )  

# totals
tots <- dat %>% 
  group_by(YEAR, SOURCE) %>% 
  summarise(
    tn_load = sum(tn_load),
    hy_load = sum(hy_load),
    .groups = 'drop'
    ) %>% 
  mutate(bay_segment = 'All Segments (- N. BCB)')

dat <- bind_rows(tots, dat)

# TN data by source, entire record ----------------------------------------

# tn data only
tndat <- dat %>% 
  select(SOURCE, YEAR, tn_load, bay_segment)

save(tndat, file = 'data/tndat.RData', compress = 'xz')

# totals ------------------------------------------------------------------

# I could not find hy load by source for the entire record
# estimates existed for some (e.g., groundwater), but not all (e.g., atmospheric deposition)

# tn load, sum source within bay segment
tntots <- tndat %>% 
  group_by(bay_segment, YEAR) %>% 
  summarise(tn_load = sum(tn_load), .groups = 'drop')

# hy dat only, 2012 to 2019, sum across source
hytots <- dat %>% 
  select(SOURCE, YEAR, hy_load, bay_segment) %>% 
  filter(!is.na(hy_load)) %>% 
  group_by(bay_segment, YEAR) %>% 
  summarise(hy_load = sum(hy_load), .groups = 'drop')

# hy dat prior to 2012
hyoldtots <- hyolddat %>% 
  rename(BAY_SEG = bay_seg) %>% 
  left_join(segid, by = 'BAY_SEG') %>% 
  rename(
    hy_load = h2oload10e6m3, 
    YEAR = year
  ) %>% 
  select(-BAY_SEG)

# hy dat prior to 2012, sum by segments
hyoldallseg <- hyoldtots %>% 
  group_by(YEAR) %>% 
  summarise(
    hy_load = sum(hy_load),
    .groups = 'drop'
  ) %>% 
  mutate(bay_segment = 'All Segments (- N. BCB)')

hyoldtots <- hyoldtots %>%
  bind_rows(hyoldallseg)

hytots <- hytots %>% 
  bind_rows(hyoldtots) %>% 
  arrange(bay_segment, YEAR)
  
# combine tn, hy 
totdat <- tntots %>% 
  full_join(hytots, by = c('bay_segment', 'YEAR')) %>% 
  mutate(tnhy =  tn_load / hy_load)

save(totdat, file = 'data/totdat.RData', compress = 'xz')
