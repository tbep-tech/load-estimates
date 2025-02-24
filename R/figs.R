library(tidyverse)
library(colorspace)
library(plotrix)
library(here)
library(showtext)
# library(patchwork)

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
# 
# # load pie ------------------------------------------------------------------------------------
# 
# ##
# # historic estimates
# hlabs <- c('Atmospheric Deposition', 'Fertilizer Losses', 'Point Sources', 'Nonpoint Sources', 'Groundwater & Springs')
# cols <- c('#727272', '#3d7993', '#17506f', '#1f806e', '#4e7f0d')
# names(cols) <- hlabs
# yrlabs <- c('1970s', '1985-1989', '1990s', '2000s', '2010s')
# 
# # from TBEP #04-94
# # totals are kg/yr, conveted to tons/yr
# historic <- tibble(
#   source = hlabs,
#   yearcat = '1970s',
#   tot = c(967059.21, 443613.465, 5418616.005, 2150028.45, 5443.11), 
#   nyr = 1
# ) %>% 
#   mutate(
#     yearcat = factor(yearcat, levels = yrlabs),
#     tot = tot / 907.185,
#     per = 100 * tot / sum(tot), 
#     tonyr = sum(tot) / nyr, 
#     cols
#   )
# 
# ##
# # current estimates, adding historical
# 
# srcs <- c('AD', 'PS', 'NPS', 'GWS')
# labs <- c('Atmospheric Deposition', 'Point Sources', 'Nonpoint Sources', 'Groundwater & Springs')
# cols <- c('#727272', '#17506f', '#1f806e', '#4e7f0d')
# 
# # load in tons, convert to kg
# toplo <- tnanndat %>% 
#   filter(bay_segment %in% 'All Segments (- N. BCB)') %>% 
#   mutate(
#     yearcat = cut(year, 
#                   breaks = c(-Inf, 1970, 1990, 2000, 2010, Inf), 
#                   labels = yrlabs, 
#                   right = F) ,
#     source = gsub('^IPS$|^DPS$', 'PS', source),
#     source = factor(source, levels = srcs, labels = labs), 
#     cols = as.character(factor(source, levels = labs, labels = cols))
#   ) %>% 
#   select(source, year, yearcat, tn_load, cols) %>% 
#   group_by(source, yearcat, cols) %>% 
#   summarise(
#     tot = sum(tn_load), 
#     nyr = length(unique(year)), 
#     .groups = 'drop'
#   ) %>% 
#   group_by(yearcat) %>% 
#   mutate(
#     per = 100 * tot / sum(tot), 
#     tonyr = sum(tot) / nyr
#   ) %>% 
#   ungroup %>% 
#   bind_rows(historic) %>% 
#   mutate(
#     prp = rescale(tonyr, newrange = c(min(tonyr) / max(tonyr), 1)), 
#     lab = gsub('^0$', '< 1', round(per, 0)), 
#     lab = paste0(lab, '%'), 
#     ttl = paste0(yearcat, ': ', formatC(round(tonyr, 0), big.mark = ',', format = 'd'), ' tons/yr')
#   )
# 
# toplo1 <- toplo %>%
#   filter(yearcat == '1970s') %>% 
#   arrange(-per) %>% 
#   mutate(
#     source = factor(source, levels = source),
#     fraction = tot / sum(tot),
#     ymax = cumsum(fraction),
#     ymin = c(0, head(ymax, n = -1))
#   )
# 
# prp <- unique(toplo1$prp)
# ttl <- unique(toplo1$ttl)
# cols <- toplo1$cols
# names(cols) <- toplo1$source
# 
# maxrad <- 4
# maxare <- pi * maxrad ^ 2
# area1 <- maxare * prp
# rad1 <- sqrt(area1 / pi)
# 
# # Make the plot
# p1 <- ggplot(toplo1, aes(ymax = ymax, ymin = ymin, xmin = 0, xmax = rad1, fill = source)) +
#   geom_rect(color = 'black') +
#   coord_polar(theta = "y") +
#   xlim(c(0, maxrad)) +
#   theme_void() +
#   ggrepel::geom_text_repel(
#     aes(label = lab, x = 0.75 * rad1, y = (ymin + ymax) / 2),
#     size = 4,
#     point.size = NA, 
#     colour = 'white'
#   ) +
#   # guides(fill = guide_legend(ncol = 3)) +
#   theme(
#     plot.title = element_text(size = 16, hjust = 0.5)
#   ) +
#   labs(
#     x = NULL,
#     y = NULL,
#     title = ttl,
#     fill = NULL
#   ) +
#   scale_fill_manual(values = cols)
#   
# toplo2 <- toplo %>%
#   filter(yearcat == '2010s') %>% 
#   arrange(-per) %>% 
#   mutate(
#     source = factor(source, levels = source),
#     fraction = tot / sum(tot),
#     ymax = cumsum(fraction),
#     ymin = c(0, head(ymax, n = -1))
#   )
# 
# prp <- unique(toplo2$prp)
# ttl <- unique(toplo2$ttl)
# cols <- toplo2$cols
# names(cols) <- toplo2$source
# 
# maxrad <- 4
# maxare <- pi * maxrad ^ 2
# area2 <- maxare * prp
# rad2 <- sqrt(area2 / pi)
# 
# # Make the plot
# p2 <- ggplot(toplo2, aes(ymax = ymax, ymin = ymin, xmin = 0, xmax = rad2, fill = source)) +
#   geom_rect(color = 'black') +
#   coord_polar(theta = "y") +
#   xlim(c(0, maxrad)) +
#   theme_void() +
#   ggrepel::geom_text_repel(
#     aes(label = lab, x = 0.75 * rad2, y = (ymin + ymax) / 2),
#     size = 4,
#     point.size = NA, 
#     colour = 'white'
#   ) +
#   # guides(fill = guide_legend(ncol = 3)) +
#   theme(
#     plot.title = element_text(size = 16, hjust = 0.5), 
#     legend.position = 'none'
#   ) +
#   labs(
#     x = NULL,
#     y = NULL,
#     title = ttl,
#     fill = NULL
#   ) +
#   scale_fill_manual(values = cols)
# 
# p <- p1 + p2 + plot_layout(ncol = 1, guides ='collect')
# png(here('figs/load_pie.png'), height = 7, width = 6, units = 'in', res = 500)#, family = fml)
# print(p)
# dev.off()
# 
# # load bar ------------------------------------------------------------------------------------
# 
# ##
# # historic estimates
# labs <- c('Atmospheric Deposition', 'Fertilizer Losses', 'Point Sources', 'Nonpoint Sources', 'Groundwater & Springs')
# cols <- c('#727272', '#3d7993', '#db5b25', '#1f806e', '#4e7f0d')
# names(cols) <- labs
# yrlabs <- c('1970s', '1985-1989', '1990s', '2000s', '2010s')
# 
# # from TBEP #04-94
# # totals are kg/yr, conveted to tons/yr
# historic <- tibble(
#   source = labs,
#   yearcat = '1970s',
#   tot = c(967059.21, 443613.465, 5418616.005, 2150028.45, 5443.11), 
#   nyr = 1
# ) %>% 
#   mutate(
#     yearcat = factor(yearcat, levels = yrlabs),
#     tot = tot / 907.185,
#     per = 100 * tot / sum(tot), 
#     tonyr = sum(tot) / nyr, 
#     cols
#   )
# 
# ##
# # current estimates, adding historical
# 
# srcs <- c('AD', 'PS', 'NPS', 'GWS')
# labs <- c('Atmospheric Deposition', 'Point Sources', 'Nonpoint Sources', 'Groundwater & Springs')
# cols <- c('#727272', '#db5b25', '#1f806e', '#4e7f0d')
# 
# # load in tons, convert to kg
# toplo <- tnanndat %>% 
#   filter(bay_segment %in% 'All Segments (- N. BCB)') %>% 
#   mutate(
#     yearcat = cut(year, 
#                   breaks = c(-Inf, 1970, 1990, 2000, 2010, Inf), 
#                   labels = yrlabs, 
#                   right = F) ,
#     source = gsub('^IPS$|^DPS$', 'PS', source),
#     source = factor(source, levels = srcs, labels = labs), 
#     cols = as.character(factor(source, levels = labs, labels = cols))
#   ) %>% 
#   select(source, year, yearcat, tn_load, cols) %>% 
#   group_by(source, yearcat, cols) %>% 
#   summarise(
#     tot = sum(tn_load), 
#     nyr = length(unique(year)), 
#     .groups = 'drop'
#   ) %>% 
#   group_by(yearcat) %>% 
#   mutate(
#     per = 100 * tot / sum(tot), 
#     tonyr = sum(tot) / nyr
#   ) %>% 
#   ungroup %>% 
#   bind_rows(historic) %>% 
#   mutate(
#     rad = rescale(tonyr, newrange = c(0.8, 1)), 
#     lab = gsub('^0$', '< 1', round(per, 0)), 
#     lab = paste0(lab, '%'), 
#     ttl = paste0(yearcat, ': ', formatC(round(tonyr, 0), big.mark = ',', format = 'd'), ' tons/yr')
#   ) %>% 
#   mutate(
#     tonyr = case_when(
#       yearcat == '1970s' ~ tot, 
#       T ~ tot / nyr
#     ), 
#     .by = c(yearcat, source)
#   )
# 
# cols <- toplo %>% 
#   select(source, cols) %>%
#   distinct() %>% 
#   deframe()
# 
# # Make the plot
# p <- ggplot(toplo, aes(x = yearcat, y = tonyr, fill = source)) +
#   geom_bar(color = 'black', stat = 'identity', position = 'stack', alpha = 0.7) +
#   theme_minimal(base_size = 14) +
#   # guides(fill = guide_legend(ncol = 3)) +
#   theme(
#     plot.title = element_text(size = 16, hjust = 0.5), 
#     panel.grid.major.x = element_blank(), 
#     panel.grid.minor.y = element_blank()
#   ) +
#   labs(
#     x = NULL,
#     y = 'tons / yr',
#     title = 'TN loading over time',
#     fill = NULL
#   ) +
#   scale_fill_manual(values = cols)
# 
# png(here('figs/load_bar.png'), height = 7, width = 7, units = 'in', res = 500, family = fml)
# print(p)
# dev.off()

# plotrix 3d --------------------------------------------------------------

##
# historic estimates
labs <- c('Atmospheric Deposition', 'Fertilizer Losses', 'Point Sources', 'Nonpoint Sources', 'Groundwater & Springs')
cols <- c('#727272', '#3d7993', '#17506f', '#1f806e', '#4e7f0d')
names(cols) <- labs
yrlabs <- c('1970s', '1985-1989', '1990s', '2000s', '2010s', '2020')

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

srcs <- c('AD', 'PS', 'NPS', 'GWS')
labs <- c('Atmospheric Deposition', 'Point Sources', 'Nonpoint Sources', 'Groundwater & Springs')
cols <- c('#727272', '#17506f', '#1f806e', '#4e7f0d')

# load in tons, convert to kg
toplo <- tnanndat %>% 
  filter(bay_segment %in% 'All Segments (- N. BCB)') %>% 
  mutate(
    yearcat = cut(year, 
                  breaks = c(-Inf, 1970, 1990, 2000, 2010, 2020, Inf), 
                  labels = yrlabs, 
                  right = F) ,
    source = gsub('^IPS$|^DPS$', 'PS', source),
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
    prp = rescale(tonyr, newrange = c(min(tonyr) / max(tonyr), 1)), 
    lab = gsub('^0$', '< 1', round(per, 0)), 
    lab = paste0(lab, '%'), 
    ttl = paste0(yearcat, ': ', formatC(round(tonyr, 0), big.mark = ',', format = 'd'), ' tons/yr')
  ) %>% 
  mutate(
    tonyr = case_when(
      yearcat == '1970s' ~ tot, 
      T ~ tot / nyr
    ), 
    .by = c(yearcat, source)
  )

png(here('figs/3dload_pies.png'), height = 8, width = 8, units = 'in', res = 500)#, family = fml)

par(mfrow = c(2, 1), mar = c(1, 0, 1, 0), xpd = NA)

tmp <- toplo %>%
  filter(yearcat == '1970s') %>% 
  arrange(-per)

prp <- unique(tmp$prp)
ttl <- unique(tmp$ttl)
cols1 <- tmp$cols
src1 <- tmp$source

maxrad <- 0.5
maxare <- pi * maxrad ^ 2
area1 <- maxare * prp
rad1 <- sqrt(area1 / pi)

p <- pie3D(tmp$tot, mar = c(1, 0, 5, 0),
           col = cols1,
           radius = rad1,
           theta = 2.5,
           explode = 0.05,
           height = 0.1,
           shade = 0.8, 
           pty = 'm')
pie3D.labels(p, labels = tmp$lab, theta = 2.5, labelrad = (rad1) + 0.15, labelcex = 1.2)
title(ttl, line = -1.4, cex.main = 1.5)

tmp <- toplo %>%
  filter(yearcat == '2020') %>% 
  arrange(-per)

prp <- unique(tmp$prp)
ttl <- unique(tmp$ttl)
cols <- tmp$cols
src <- tmp$source

maxrad <- 0.5
maxare <- pi * maxrad ^ 2
area2 <- maxare * prp
rad2 <- sqrt(area2 / pi)

p <- pie3D(tmp$tot, mar = c(5, 0, 5, 0),
           col = cols,
           radius = rad2,
           theta = 3.5,
           explode = 0.05,
           height = 0.1,
           shade = 0.8, 
           pty = 'm')
pie3D.labels(p, labels = tmp$lab, theta = 3.5, labelrad = (rad2) + 0.15, labelcex = 1.2)
title(ttl, line = -1.4, cex.main = 1.5)

ord <- matrix(1:6, nrow = 2, ncol = 3, byrow = T)
legend(x = -0.8, y = -1.35, ncol = 3, legend = na.omit(src1[ord]), xpd = NA, col = na.omit(cols1[ord]), box.col = NA, pch = 15, pt.cex = 3.6, y.intersp = 2.5, 
       bg = 'transparent')
  
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
