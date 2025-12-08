library(tidyverse)
library(ggridges)
library(here)

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
    tn_load = sum(tn_load), 
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