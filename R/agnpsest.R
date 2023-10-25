library(tidyverse)

data(tnanndat)
data(npsmosludat)

aglu <- c("Feedlots", "Groves", "Nursery", "Pasture", "Range Lands", "Row and Field Crops")

# 2010 - 2014 period --------------------------------------------------------------------------

yr1 <- 2010
yr2 <- 2014

agyr <- npsmosludat %>% 
  filter(year >= yr1 & year <= yr2) %>% 
  filter(`land use` %in% aglu) %>%
  summarise(
    tn_load = sum(tn_load), 
    .by = c(year)
  )
agyrave <- mean(agyr$tn_load)

npsyr <- tnanndat %>% 
  filter(year >= yr1 & year <= yr2) %>% 
  filter(source == 'NPS') %>% 
  summarise(
    tn_load = sum(tn_load), 
    .by = c(year)
  )
npsyrave <- mean(npsyr$tn_load)

agyrave
npsyrave
agyrave / npsyrave

# 2017-2021 period ----------------------------------------------------------------------------

yr1 <- 2017
yr2 <- 2021

agyr <- npsmosludat %>% 
  filter(year >= yr1 & year <= yr2) %>% 
  filter(`land use` %in% aglu) %>%
  summarise(
    tn_load = sum(tn_load), 
    .by = c(year)
  )
agyrave <- mean(agyr$tn_load)

npsyr <- tnanndat %>% 
  filter(year >= yr1 & year <= yr2) %>% 
  filter(source == 'NPS') %>% 
  summarise(
    tn_load = sum(tn_load), 
    .by = c(year)
  )
npsyrave <- mean(npsyr$tn_load)

agyrave
npsyrave
agyrave / npsyrave

