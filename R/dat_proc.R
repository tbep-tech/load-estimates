
# process data ------------------------------------------------------------

library(tidyverse)
library(haven)

# segment id
levs <- c('Old Tampa Bay', 'Hillsborough Bay', 'Middle Tampa Bay', 'Lower Tampa Bay', 'Remainder Lower Tampa Bay')
segid <- tibble(
  BAY_SEG = c(1, 2, 3, 4, 5567),
  bay_segment = levs
)

# 85 - 19
ad8519 <- read_sas('~/Desktop/TBEP/LoadingCodes&Datasets1719_toTBEP_20201117/TotalLoads1719/TotalLoads2019/ad_8519.sas7bdat')
dps8519 <- read_sas('~/Desktop/TBEP/LoadingCodes&Datasets1719_toTBEP_20201117/TotalLoads1719/TotalLoads2019/dps_8519.sas7bdat')
ips8519 <- read_sas('~/Desktop/TBEP/LoadingCodes&Datasets1719_toTBEP_20201117/TotalLoads1719/TotalLoads2019/ips_ml_8519.sas7bdat')
nps8519 <- read_sas('~/Desktop/TBEP/LoadingCodes&Datasets1719_toTBEP_20201117/TotalLoads1719/TotalLoads2019/nps_8519.sas7bdat')
gws8519 <- read_sas('~/Desktop/TBEP/LoadingCodes&Datasets1719_toTBEP_20201117/TotalLoads1719/TotalLoads2019/gws_8519.sas7bdat')

all8519 <- bind_rows(ad8519, dps8519, ips8519, nps8519, gws8519) %>% 
  left_join(segid, by = 'BAY_SEG') %>% 
  mutate(
    bay_segment = factor(bay_segment, levels = levs)
  )

p <- ggplot(all8519, aes(x = YEAR, y = TN_tons, fill = SOURCE)) + 
  geom_area() +
  theme_minimal() + 
  theme(
    axis.title.x = element_blank(), 
    axis.text.x = element_text(angle = 45, size = 7, hjust = 1), 
    axis.ticks.x = element_line(), 
    legend.title = element_blank()
  ) +
  scale_y_continuous(expand = c(0, 0)) + 
  scale_x_continuous(expand = c(0, 0), breaks = seq(1985, 2019)) + 
  facet_wrap(~ bay_segment, ncol = 1, scales = 'free_y') +
  labs(
    y  = 'Total Nitrogen (tons/yr)'
  )
