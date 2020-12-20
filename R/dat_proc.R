
# process data ------------------------------------------------------------

library(tidyverse)
library(haven)

# segment id
segid <- tibble(
  BAY_SEG = c(1, 2, 3, 4, 5567),
  bay_segment = c('Old Tampa Bay', 'Hillsborough Bay', 'Middle Tampa Bay', 'Lower Tampa Bay', 'Remainder Lower Tampa Bay')
)

# 85 - 19
ad8519 <- read_sas('~/Desktop/TBEP/LoadingCodes&Datasets1719_toTBEP_20201117/TotalLoads1719/TotalLoads2019/ad_8519.sas7bdat')
dps8519 <- read_sas('~/Desktop/TBEP/LoadingCodes&Datasets1719_toTBEP_20201117/TotalLoads1719/TotalLoads2019/dps_8519.sas7bdat')
ips8519 <- read_sas('~/Desktop/TBEP/LoadingCodes&Datasets1719_toTBEP_20201117/TotalLoads1719/TotalLoads2019/ips_ml_8519.sas7bdat')
nps8519 <- read_sas('~/Desktop/TBEP/LoadingCodes&Datasets1719_toTBEP_20201117/TotalLoads1719/TotalLoads2019/nps_8519.sas7bdat')
gws8519 <- read_sas('~/Desktop/TBEP/LoadingCodes&Datasets1719_toTBEP_20201117/TotalLoads1719/TotalLoads2019/gws_8519.sas7bdat')

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

# tn data only
tndat <- dat %>% 
  select(SOURCE, YEAR, tn_load, bay_segment)

# hy dat only
hydat <- dat %>% 
  select(SOURCE, YEAR, hy_load, bay_segment) %>% 
  filter(!is.na(hy_load))

save(tndat, file = 'data/tndat.RData', compress = 'xz')
save(hydat, file = 'data/hydat.RData', compress = 'xz')
