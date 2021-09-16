library(tidyverse)
library(colorspace)

data(tnanndat)

cols <- qualitative_hcl(length(unique(tnanndat$SOURCE)), palette = "Dynamic")

toplo <- tnanndat %>% 
  filter(bay_segment %in% 'All Segments (- N. BCB)') %>% 
  select(SOURCE, YEAR, tn_load) %>% 
  group_by(YEAR) %>% 
  mutate(
    tot = sum(tn_load)
  )

p <- ggplot(toplo, aes(x = tot/2, y=tn_load, fill=SOURCE, width=tot)) +
  geom_bar(position = "fill", stat="identity", color = 'black') +
  facet_wrap(~ YEAR) + 
  coord_polar("y") +
  scale_fill_manual(values = cols) + 
  theme_void() +
  theme(legend.title = element_blank())

png('~/Desktop/load_pies.png', height = 8, width = 8, units = 'in', res= 300)
print(p)
dev.off()

