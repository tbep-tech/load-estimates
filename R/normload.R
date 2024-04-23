library(tidyverse)
library(here)

load(file = here('data/totanndat.RData'))

normdat <- totanndat |> 
  filter(!grepl('^All', bay_segment)) |> 
  mutate(
    refhydro = case_when(
      bay_segment == 'Old Tampa Bay' ~ 449, 
      bay_segment == 'Hillsborough Bay' ~ 896,
      bay_segment  == 'Middle Tampa Bay' ~ 645, 
      bay_segment == 'Lower Tampa Bay' ~ 361, 
      bay_segment == 'Remainder Lower Tampa Bay' ~ 423
    ),
    alloc = case_when(
      bay_segment == 'Old Tampa Bay' ~ 486, 
      bay_segment == 'Hillsborough Bay' ~ 1451,
      bay_segment  == 'Middle Tampa Bay' ~ 799, 
      bay_segment == 'Lower Tampa Bay' ~ 349, 
      bay_segment == 'Remainder Lower Tampa Bay' ~ 629
    ),
    normtn_load = tn_load * refhydro / hy_load
  ) |> 
  select(bay_segment, year, tn_load, normtn_load, alloc) |> 
  pivot_longer(-c(bay_segment, alloc, year)) |> 
  mutate(
    name = factor(name, levels = c('tn_load', 'normtn_load'),
                  labels = c('Observed', 'Normalized'))
  )

ggplot(normdat, aes(x = year, y = value, color = name)) + 
  geom_line() + 
  geom_point() +
  geom_hline(aes(yintercept = alloc), linetype = 'dashed') +
  theme_minimal() + 
  theme(
    legend.position = 'top',
    panel.grid.minor = element_blank()
  ) + 
  facet_wrap(~bay_segment, scales = 'free_y') +
  labs(
    x = NULL, 
    y = 'tons / yr', 
    color = NULL,
    title = 'Tampa Bay TN loads',
    subtitle = 'Dashed line is segment annual target'
  )

