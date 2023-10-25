library(tidyverse)
library(lubridate)
library(here)

load(file = here('data/mosentdat.RData'))

dpsdat <- mosentdat %>% 
  filter(source == 'DPS')

write.csv(dpsdat, '~/Desktop/dpsdat.csv', row.names = F)

toplo <- dpsdat %>% 
  mutate(
    cat = case_when(
      entity == 'Lakeland' ~ 'Lakeland', 
      T ~ 'Other DPS'
    ),
    cat = factor(cat),
    dy = 15
  ) %>% 
  summarise(
    tn_load = sum(tn_load), 
    .by = c('year', 'cat')
  )

p <- ggplot(toplo, aes(x = year, y = tn_load, group = cat, fill = cat)) + 
  geom_col(position = position_dodge(), alpha = 0.8) + 
  geom_text(aes(label = round(tn_load, 0)), position = position_dodge(width = 0.9), vjust = -0.5) + 
  theme_minimal() + 
  scale_fill_manual(values = c('tomato1', 'grey')) +
  theme(
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(), 
    legend.position = 'top',
    axis.text.x = element_text(size = 12)
  ) + 
  labs(
    x = NULL,
    y = 'TN load (tons / yr)', 
    fill = NULL, 
    subtitle = 'Domestic point source loads for 2017 - 2021 RA period'
  )

png('~/Desktop/lakelanddat.png', width = 6, height = 5, units = 'in', res = 300)
print(p)
dev.off()