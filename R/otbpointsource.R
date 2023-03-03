library(tidyverse)
library(plotly)
library(ggfx)
library(ggridges)
library(patchwork)
library(here)

data(dpsmosdat)
data(ipsmosdat)

toplo1 <- bind_rows(ipsmosdat, dpsmosdat) %>% 
  summarise(
    tn_load = sum(tn_load), 
    .by = c('year', 'facility', 'bay_segment')
  ) %>% 
  filter(bay_segment == 'Old Tampa Bay') %>% 
  mutate(
    facility = gsub('\\sAWWTP$|\\sAWWTF$|\\sWRF$|\\sAWTP$|\\sWWTP$', '', facility)
  ) %>% 
  mutate(
    tnave = mean(tn_load), 
    .by = 'facility'
  ) #%>% 
  # filter(!facility %in% c('Dale Mabry', 'Bridgeway Acres'))

alph <- 0.8

p1a <- ggplot(toplo1, aes(y = tn_load, x = year, group = reorder(facility, -tn_load))) +
  geom_area(aes(fill = tnave), alpha = alph, show.legend = F, color = 'black') + 
  # scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) +
  # geom_hline(yintercept = 90, linetype = 'dotted') +
  scale_fill_distiller(palette = 'Greens', direction = 1) +
  theme_minimal() + 
  theme(
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(), 
    panel.grid.minor.y = element_blank()
  ) +
  labs(
    y = 'TN load (tons / yr)', 
    x = NULL, 
    title = 'Total Nitrogen to Old Tampa Bay', 
    subtitle = 'Includes end of pipe, reuse, and industrial sources'
  )

p1b <- ggplot(toplo1, aes(y = reorder(facility, tn_load), height = tn_load, x = year, fill = tnave)) +
  geom_ridgeline(scale = 0.1, show.legend = F, alpha = alph) + 
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_discrete(expand = c(0, 0), position = 'right') + 
  scale_fill_distiller(palette = 'Greens', direction = 1) + 
  theme_minimal() + 
  theme(
    panel.grid.major.y = element_blank(), 
    panel.grid.minor.y = element_blank(), 
    panel.grid.minor.x = element_blank()
  ) +
  labs(
    y = NULL, 
    x = NULL, 
    # subtitle = "By facility", 
    caption = 'Data from Janicki Environmental, Inc. and Tampa Bay Estuary Program'
  )

p1 <- p1a + p1b + plot_layout(ncol = 2, widths = c(1, 0.6))

png(here('figs/tnridges.png'), height = 5, width = 9, units = 'in', res = 300)
print(p1)
dev.off()

toplo2 <- toplo1 %>% 
  summarise(
    tnave = mean(tn_load),
    tnhi = t.test(tn_load)$conf.int[2],
    tnlo = t.test(tn_load)$conf.int[1],
    .by = 'facility'
  ) %>% 
  mutate(
    tnavetxt = round(tnave, 1)
  )

p2 <- ggplot(toplo2, aes(y = reorder(facility, tnave), x = tnave)) + 
  with_shadow(geom_col(color = 'black', aes(fill = tnave), show.legend = F, alpha = 1, width = 0.6), x_offset = 0, y_offset = 0, sigma = 3) + 
  # geom_errorbar(aes(xmin = tnlo, xmax = tnhi), width = 0) + 
  geom_text(aes(x = tnave, label = tnavetxt), nudge_x = 0.9) + 
  scale_x_continuous(expand = c(0, 0)) + 
  scale_fill_distiller(palette = 'Greens', direction = 1) + 
  coord_cartesian(xlim = c(0, max(toplo2$tnave * 1.2))) + 
  theme_minimal() + 
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(), 
    axis.text.x = element_text(size = rel(1.3))
  ) + 
  labs(
    x = 'Mean TN load (tons / yr)',
    y = NULL, 
    title = paste0('Total Nitrogen to Old Tampa Bay: ', sum(toplo2$tnavetxt), ' tons / yr'),
    subtitle = 'Includes end of pipe, reuse, and industrial sources' ,
    caption = 'Data from Janicki Environmental, Inc. and Tampa Bay Estuary Program'
  )

png(here('figs/tnloadbar.png'), height = 4, width = 7, units = 'in', res = 300)
print(p2)
dev.off()
