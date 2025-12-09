library(tidyverse)
library(ggridges)
library(here)
library(sf)
library(mapview)
library(tbeploads)

data(dpsmosdat)

ptsrc <- dpsmosdat |> 
  arrange(facility, year, month) |> 
  filter(source == 'DPS - end of pipe') |> 
  mutate(
    date = lubridate::make_date(year, month, 1)
  )

toplo <- dpsmosdat |> 
  filter(source == 'DPS - end of pipe') |>
  summarise(
    tn_load = sum(tn_load, na.rm = T), 
    .by = c('year', 'facility')
  ) |> 
  mutate(
    facility = gsub('\\sAWWTP$|\\sAWWTF$|\\sWRF$|\\sAWTP$|\\sWWTP$', '', facility)
  ) |> 
  filter(!is.na(tn_load)) |> 
  mutate(
    tnave = mean(tn_load, na.rm = T), 
    .by = 'facility'
  ) 

alph <- 0.8
p1 <- ggplot(toplo, aes(y = reorder(facility, tn_load), height = tn_load, x = year, fill = tnave)) +
  geom_ridgeline(scale = 0.05, show.legend = F, alpha = 1) + 
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_discrete(expand = c(0, 0)) + 
  scale_fill_distiller(palette = 'Greens', direction = 1) + 
  theme_minimal() + 
  theme(
    panel.grid.major.y = element_blank(), 
    panel.grid.minor.y = element_blank(), 
    panel.grid.minor.x = element_blank()
  ) +
  labs(
    y = "Relative loads by facility", 
    x = NULL, 
    # subtitle = "By facility", 
    caption = 'Data from Janicki Environmental, Inc. and Tampa Bay Estuary Program',
    title = 'Relative WWTP loadings to Tampa Bay', 
  )

toplo2 <- toplo |> 
  filter(facility != 'Howard F. Curren')

p2 <- ggplot(toplo2, aes(y = reorder(facility, tn_load), height = tn_load, x = year, fill = tnave)) +
  geom_ridgeline(scale = 0.05, show.legend = F, alpha = 1) + 
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_discrete(expand = c(0, 0)) + 
  scale_fill_distiller(palette = 'Greens', direction = 1) + 
  theme_minimal() + 
  theme(
    panel.grid.major.y = element_blank(), 
    panel.grid.minor.y = element_blank(), 
    panel.grid.minor.x = element_blank()
  ) +
  labs(
    y = "Relative loads by facility", 
    x = NULL, 
    # subtitle = "By facility", 
    caption = 'Data from Janicki Environmental, Inc. and Tampa Bay Estuary Program',
    title = 'Relative WWTP loadings to Tampa Bay', 
  )

png(here('figs/wwtpall.png'), width = 6, height = 8, units = 'in', res = 300)
print(p1)
dev.off()

png(here('figs/wwtpnohfc.png'), width = 6, height = 8, units = 'in', res = 300)
print(p2)
dev.off()

# get wwtp facility locatios ---------------------------------------------

data(dpsmosdat)

facs <- dpsmosdat |> 
  filter(source == 'DPS - end of pipe') |> 
  pull(facility) |> 
  unique()
perms <- facilities |> 
  filter(facname %in% facs) |> 
  pull(permit) |> 
  unique()

locsall <- st_read(here('data/raw/domptsrc.shp'))

ptsrclocs <- locsall |> 
  mutate(
    FACILITY_I = case_when(
      FACILITY_I == 'FL0028061' ~ 'FL0028061SW', # pinellas county south county regional
      FACILITY_I == 'FLA021888' ~ 'FL0128937', # city of clearwater northeast
      T ~ FACILITY_I
    )
  ) |> 
  filter(FACILITY_I %in% perms) |> 
  left_join(facilities, by = c('FACILITY_I' = 'permit')) |> 
  select(entity, facname, longitude = LONDECD, latitude = LATDECD) |> 
  st_set_geometry(NULL) |> 
  distinct()

# from raw data on T drive
lklnd <- tibble(
  entity = 'Lakeland', 
  facname = 'City of Lakeland', 
  longitude = -81.943333, 
  latitude = 27.900000
)

ptsrclocs <- bind_rows(ptsrclocs, lklnd) |> 
  arrange(facname)

write.csv(locswwtp, '~/Desktop/ptsrclocs.csv', row.names = F)

mis <- facs[!(facs %in% ptsrclocs$facname)]

tomap <- st_as_sf(ptsrclocs, coords = c('longitude', 'latitude'), crs = 4326)
mapview(tomap)
