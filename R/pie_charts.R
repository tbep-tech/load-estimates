library(tidyverse)
library(colorspace)
library(plotrix)
library(here)

data(tnanndat)

# ggplot ------------------------------------------------------------------

cols <- qualitative_hcl(length(unique(tnanndat$source)), palette = "Dynamic")

toplo <- tnanndat %>% 
  filter(bay_segment %in% 'All Segments (- N. BCB)') %>% 
  select(source, year, tn_load) %>% 
  group_by(year) %>% 
  mutate(
    tot = sum(tn_load)
  )

p <- ggplot(toplo, aes(x = tot/2, y=tn_load, fill=source, width=tot)) +
  geom_bar(position = "fill", stat="identity", color = 'black') +
  facet_wrap(~ year) + 
  coord_polar("y") +
  scale_fill_manual(values = cols) + 
  theme_void() +
  theme(legend.title = element_blank())

png(here('figs/load_pies.png'), height = 8, width = 8, units = 'in', res= 300)
print(p)
dev.off()

# plotrix 3d --------------------------------------------------------------

srcs <- c('AD', 'DPS', 'GWS', 'IPS', 'NPS')
labs <- c('Atmospheric Deposition', 'Domestic Point Source', 'Groundwater & Springs', 'Industrial Point Sources', 'Nonpoint Sources')
cols <- c('AD' = I('#33CC33'), 'DPS' = I('#00B0F0'), 'GWS' = I('#EB641B'), 'IPS' = I('#C0504D'), 'NPS' = I('#FFFF99'))
names(cols) <- labs

toplo <- tnanndat %>% 
  filter(bay_segment %in% 'All Segments (- N. BCB)') %>% 
  mutate(
    yearcat = cut(year, 
                  breaks = c(-Inf, 1990, 2000, 2010, Inf), 
                  labels = c('1985-1989', '1990s', '2000s', '2010s'), 
                  right = F) ,
    source = factor(source, levels = srcs, labels = labs)
  ) %>% 
  select(source, year, yearcat, tn_load) %>% 
  group_by(source, yearcat) %>% 
  summarise(
    tot = sum(tn_load), 
    nyr = length(unique(year)), 
    .groups = 'drop'
  ) %>% 
  group_by(yearcat) %>% 
  mutate(
    per = 100 * tot / sum(tot), 
    kgyr = 907.185 * sum(tot) / nyr, 
    kgyr = kgyr / 1e6
  ) %>% 
  ungroup %>% 
  mutate(
    rad = rescale(kgyr, newrange = c(0.5, 1.4)), 
    lab = gsub('0', '< 1', round(per, 0)), 
    lab = paste0(lab, '%'), 
    ttl = paste0(yearcat, ': ', round(kgyr, 1), ' mill kg/yr')
  )

png(here('figs/3dload_pies.png'), height = 4.75, width = 8, res = 300, units = 'in')

par(mfrow = c(2, 2), mar = c(1, 0, 1, 0))

for(i in levels(toplo$yearcat)){
  
  tmp <- toplo %>% 
    filter(yearcat == i)
  
  rad <- unique(tmp$rad)
  ttl <- unique(tmp$ttl)
  
  p <- pie3D(tmp$tot, mar = c(0, 6, 0, 6),
             col = cols,
             radius = rad,
             theta = 0.5,
             explode = 0.2,
             height = 0.1,
             shade = 0.8)
  pie3D.labels(p, labels = tmp$lab, labelrad = rad + 0.5, labelcex = 1)
  title(ttl)
  
}

legend(x = -4.5, -0.75, ncol = 3, legend = labs, xpd = NA, fill = cols, box.col = NA)

dev.off()


