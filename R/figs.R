library(tidyverse)
library(colorspace)
library(plotrix)
library(here)
library(showtext)

# get font
font_add_google("Roboto", "roboto")#, regular = 'C:/Windows/Fonts/Roboto.ttf')
fml <- "roboto"

showtext_auto()
showtext_opts(dpi = 500)

data(tnanndat)

# ggplot ------------------------------------------------------------------

# cols <- qualitative_hcl(length(unique(tnanndat$source)), palette = "Dynamic")
# 
# toplo <- tnanndat %>% 
#   filter(bay_segment %in% 'All Segments (- N. BCB)') %>% 
#   select(source, year, tn_load) %>% 
#   group_by(year) %>% 
#   mutate(
#     tot = sum(tn_load)
#   )
# 
# p <- ggplot(toplo, aes(x = tot/2, y=tn_load, fill=source, width=tot)) +
#   geom_bar(position = "fill", stat="identity", color = 'black') +
#   facet_wrap(~ year) + 
#   coord_polar("y") +
#   scale_fill_manual(values = cols) + 
#   theme_void() +
#   theme(legend.title = element_blank())
# 
# png(here('figs/load_pies.png'), height = 8, width = 8, units = 'in', res= 300)
# print(p)
# dev.off()

# plotrix 3d --------------------------------------------------------------

##
# historic estimates
labs <- c('Atmospheric Deposition', 'Fertilizer Losses', 'Point Sources', 'Nonpoint Sources', 'Groundwater & Springs')
cols <- c('#727272', '#3d7993', '#17506f', '#1f806e', '#4e7f0d')
names(cols) <- labs
yrlabs <- c('1970s', '1985-1989', '1990s', '2000s', '2010s')

# from TBEP #04-94
# totals are kg/yr, conveted to tons/yr
historic <- tibble(
  source = labs,
  yearcat = '1970s',
  tot = c(967059.21, 443613.465, 5418616.005, 2150028.45, 5443.11), 
  nyr = 1
) %>% 
  mutate(
    yearcat = factor(yearcat, levels = yrlabs),
    tot = tot / 907.185,
    per = 100 * tot / sum(tot), 
    tonyr = sum(tot) / nyr, 
    cols
  )

##
# current estimates, adding historical

srcs <- c('AD', 'DPS', 'IPS', 'NPS', 'GWS')
labs <- c('Atmospheric Deposition', 'Domestic Point Sources', 'Industrial Point Sources', 'Nonpoint Sources', 'Groundwater & Springs')
cols <- c('#727272', '#826992', '#657389', '#1f806e', '#4e7f0d')

# load in tons, convert to kg
toplo <- tnanndat %>% 
  filter(bay_segment %in% 'All Segments (- N. BCB)') %>% 
  mutate(
    yearcat = cut(year, 
                  breaks = c(-Inf, 1970, 1990, 2000, 2010, Inf), 
                  labels = yrlabs, 
                  right = F) ,
    source = factor(source, levels = srcs, labels = labs), 
    cols = as.character(factor(source, levels = labs, labels = cols))
  ) %>% 
  select(source, year, yearcat, tn_load, cols) %>% 
  group_by(source, yearcat, cols) %>% 
  summarise(
    tot = sum(tn_load), 
    nyr = length(unique(year)), 
    .groups = 'drop'
  ) %>% 
  group_by(yearcat) %>% 
  mutate(
    per = 100 * tot / sum(tot), 
    tonyr = sum(tot) / nyr
  ) %>% 
  ungroup %>% 
  bind_rows(historic) %>% 
  mutate(
    rad = rescale(tonyr, newrange = c(0.5, 1)), 
    lab = gsub('^0$', '< 1', round(per, 0)), 
    lab = paste0(lab, '%'), 
    ttl = paste0(yearcat, ': ', formatC(round(tonyr, 0), big.mark = ',', format = 'd'), ' tons/yr')
  )

png(here('figs/3dload_pies.png'), height = 8, width = 8, units = 'in', res = 500, family = fml)

par(mfrow = c(2, 1), mar = c(1, 0, 1, 0), xpd = NA)

for(i in c('1970s', '2010s')){
  
  tmp <- toplo %>%
    filter(yearcat == i)
  
  rad <- unique(tmp$rad)
  ttl <- unique(tmp$ttl)
  cols <- tmp$cols
  src <- tmp$source
  
  p <- pie3D(tmp$tot, mar = c(0, 6, 0, 6),
             col = cols,
             radius = rad,
             theta = 0.9,
             explode = 0.1,
             height = 0.06,
             shade = 0.8)
  pie3D.labels(p, labels = tmp$lab, theta = 0.9, labelrad = rad + 0.25, labelcex = 1.2)
  title(ttl, line = -1.4, cex.main = 1.5)
  
  ord <- matrix(1:6, nrow = 2, ncol = 3, byrow = T)
  legend(x = -1.65, y = -0.63, ncol = 3, legend = na.omit(rev(src)[ord]), xpd = NA, col = na.omit(rev(cols)[ord]), box.col = NA, pch = 15, pt.cex = 3.6, y.intersp = 1.5, 
         bg = 'transparent')
  
}

dev.off()

# hydro load scaled -------------------------------------------------------

load(file = here('data/totanndat.RData'))

levs <- c('Old Tampa Bay', 'Hillsborough Bay', 'Middle Tampa Bay', 'Lower Tampa Bay')

hydf <- data.frame(
  bay_segment = levs, 
  ln = c(449, 896, 645, 361)
)

ttl <- 'Total Hydro Load (mill m3 / yr)'

toplo <- totanndat %>% 
  select(bay_segment, year, hy_load) %>% 
  filter(bay_segment %in% levs) %>% 
  left_join(hydf, by = 'bay_segment') %>% 
  mutate(
    bay_segment = factor(bay_segment, levels = levs)#, 
    # colv = ifelse(hy_load >= ln, 'above', 'below'), 
    # perdiff = (hy_load - ln )/ ln
  )

toplo <- seq(min(toplo$year), max(toplo$year), by = 0.1) %>% 
  crossing(
    year = .,
    bay_segment = levs) %>% 
  left_join(., totanndat, by = c('year', 'bay_segment'), multiple = 'all')  %>% 
  select(bay_segment, year, hy_load) %>% 
  left_join(., hydf, by = 'bay_segment') %>% 
  mutate(
    bay_segment = factor(bay_segment, levels = levs)
  ) %>% 
  arrange(bay_segment, year) %>% 
  mutate(
    hy_load = zoo::na.approx(hy_load, na.rm = FALSE), 
    .by = 'bay_segment'
  ) %>% 
  mutate(
    colv = ifelse(hy_load >= ln, 'above', 'below'),
    perdiff = (hy_load - ln )/ ln
  )

p <- ggplot(toplo, aes(x = year, group = bay_segment, col = colv, fill = colv)) + 
  geom_area(aes(y = ifelse(perdiff < 0, perdiff, 0), fill = 'Below target', col = 'Below target'), alpha = 0.7) +
  geom_area(aes(y = ifelse(perdiff > 0, perdiff, 0), fill = 'Above target', col = 'Above target'), alpha = 0.7) +
  geom_hline(aes(yintercept = 0)) +
  scale_color_manual(values = c('#CC3231', '#2DC938')) + 
  scale_fill_manual(values = c('#CC3231', '#2DC938')) + 
  facet_wrap(~bay_segment, ncol = 1) + 
  scale_x_continuous(breaks = seq(min(toplo$year), max(toplo$year), by = 5), expand = c(0, 0)) + 
  scale_y_continuous(labels = scales::percent_format(), limits = c(-1, 1)) +
  theme_minimal() + 
  theme(
    strip.background = element_blank(), 
    strip.text = element_text(size = 12), 
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(), 
    panel.grid.minor.y = element_blank(), 
    legend.position = 'none', 
    axis.title.y = element_text(size = 12), 
    axis.text.x = element_text(size = 12)
  ) +
  labs(
    y = '% above/below', 
    x = NULL, 
    title = 'Annual hydrologic load above or below bay segment target'
  )

png(here('figs/hydroscaled.png'), height = 10, width = 10, units = 'in', res = 300, family = fml)
print(p)
dev.off()
