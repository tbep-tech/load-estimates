library(plotly)
library(tidyverse)
library(haven)

source('R/funcs.R')

# this code attempts to recreate the summaries in mosdat from R/datproc.R but keeping RLTB segments separate
# original files at T:\03_BOARDS_COMMITTEES\05_TBNMC\2022_RA_Update\01_FUNDING_OUT\DELIVERABLES\TO-9\datastick_deliverables\2017-2021Annual&MonthlyLoadDatasets\MakeMonthAnnDatasets\Monthly 
# NPS at T:\03_BOARDS_COMMITTEES\05_TBNMC\2022_RA_Update\01_FUNDING_OUT\DELIVERABLES\TO-9\datastick_deliverables\2017-2021Annual&MonthlyLoadDatasets\NPS1721\Monthly (files in first path had RLTB aggregated)

allmo <- list(
    ad = 'ad1721monthsegloads20221026.sas7bdat',
    dps = 'dps1721monthloads20221026.sas7bdat',
    fe = 'fe1721monthsegloads20230123.sas7bdat',
    ips = 'ips1721monthloads20221025.sas7bdat',
    gw = 'gw1721monthsegloads20221026.sas7bdat',
    spr = 'spr1721monthsegloads20221026.sas7bdat',
    npsbod = 'npsmonth1721_bod7segs_byentity.sas7bdat',
    npstn = 'npsmonth1721_tn7segs_byentity.sas7bdat',
    npstp = 'npsmonth1721_tp7segs_byentity.sas7bdat',
    npstss = 'npsmonth1721_tss7segs_byentity.sas7bdat'
  ) %>% 
  purrr::map(function(x) read_sas(paste0('data/raw/', x))) %>% 
  purrr::map(function(x){
    names(x) <- tolower(names(x)) 
    rename(x, 
           bay_segment = matches('bayseg|bay_seg'), 
           bod_load = matches('bod_tons|bodloadtons'),
           tn_load = matches('tn_tons|tnloadtons'),
           tp_load = matches('tp_tons|tploadtons'),
           tss_load = matches('tss_tons|tssloadtons')
    )
  }) %>% 
  enframe() %>% 
  unnest('value') %>% 
  mutate(
    source = case_when(
      name %in% 'ad' ~ 'AD', 
      name %in% 'dps' ~ 'DPS', 
      name %in% c('fe', 'ips') ~ 'IPS', 
      name %in% c('gw', 'spr') ~ 'GW', 
      name %in% c('npsbod', 'npstn', 'npstp', 'npstss') ~ 'NPS'
    ), 
    bay_segment = case_when(
      bay_segment %in% 1 ~ 'Old Tampa Bay', 
      bay_segment %in% 2 ~ 'Hillsborough Bay',
      bay_segment %in% 3 ~ 'Middle Tampa Bay',
      bay_segment %in% 4 ~ 'Lower Tampa Bay',
      bay_segment %in% 5 ~ 'Boca Ciega Bay',
      bay_segment %in% 55 ~ 'Boca Ciega Bay-South',
      bay_segment %in% 6 ~ 'Terra Ceia Bay', 
      bay_segment %in% 7 ~ 'Manatee River'
    )
  ) %>%
  mutate_at(vars(tn_load, tp_load, tss_load, bod_load), function(x){
    pmax(0, ifelse(is.na(x), 0, x))
  }) %>% 
  summarise(
    tn_load = sum(tn_load),
    tp_load = sum(tp_load),
    tss_load = sum(tss_load),
    bod_load = sum(bod_load),
    .by = c('bay_segment', 'year', 'month', 'source')
  )

tnsrc_plo2 <- function(datin, yval = 'tn_load'){
  
  srcs <- c('AD', 'DPS', 'GWS', 'IPS', 'NPS')
  cols <- c('AD' = '#33CC33', 'DPS' = '#00B0F0', 'GWS' = '#EB641B', 'IPS' = '#C0504D', 'NPS' = '#FFFF99')
  
  levs <- c('Old Tampa Bay', 'Hillsborough Bay', 'Middle Tampa Bay', 'Lower Tampa Bay', 'Boca Ciega Bay', 'Terra Ceia Bay', 'Manatee River')
  
  ttl <- 'Total Nitrogen (tons / yr)'

  ttls <- c('tn_load' = 'Total Nitrogen (tons / mo)', 
            'tp_load' = 'Total Phosphorus (tons / mo)',
            'tss_load' = 'Total Suspended Solids (tons / mo)', 
            'bod_load' = 'Biological Oxygen Demands (tons / mo)')
  
  ttl <- ttls[[yval]]
  
  datin <- datin %>% 
    mutate(dy = 1) %>% 
    unite('date', year, month, dy, sep = '-', remove = T) %>% 
    mutate(
      date = ymd(date)
    ) %>% 
    select(date, bay_segment, source, !!yval)
  
  for(lev in seq_along(levs)){
    
    toplo <- datin %>% 
      select(matches('year|date'), bay_segment, source, !!yval) %>% 
      filter(bay_segment %in% !!levs[lev]) %>% 
      rename(dt = 'date') %>% 
      mutate(source = factor(source, levels = srcs)) %>%
      spread(source, !!yval, fill = 0, drop = F)
    
    showleg <- F
    if(lev == 1)
      showleg <- T
    
    p <- plot_ly(toplo, alpha = 1, fill = 'tonexty')  %>% 
      add_markers(x = ~dt, y = ~NPS, color = I(cols[['NPS']]), stackgroup = 'one', mode = 'none', marker = list(opacity = 0, size = 0), 
                  showlegend = showleg, legendgroup = 'grp5', name = 'NPS') %>%   
      add_markers(x = ~dt, y = ~IPS, color = I(cols[['IPS']]), stackgroup = 'one', mode = 'none', marker = list(opacity = 0, size = 0), 
                  showlegend = showleg, legendgroup = 'grp4', name = 'IPS') %>% 
      add_markers(x = ~dt, y = ~GWS, color = I(cols[['GWS']]), stackgroup = 'one', mode = 'none', marker = list(opacity = 0, size = 0), 
                  showlegend = showleg, legendgroup = 'grp3', name = 'GWS') %>% 
      add_markers(x = ~dt, y = ~DPS, color = I(cols[['DPS']]), stackgroup = 'one', mode = 'none', marker = list(opacity = 0, size = 0), 
                  showlegend = showleg, legendgroup = 'grp2', name = 'DPS') %>% 
      add_markers(x = ~dt, y = ~AD, color = I(cols[['AD']]), stackgroup = 'one', mode = 'none', marker = list(opacity = 0, size = 0), 
                  showlegend = showleg, legendgroup = 'grp1', name = 'AD') 
      
    p <- p %>% 
      add_annotations(
        text = ~unique(bay_segment),
        x = 0.5,
        y = 1.2,
        yref = "paper",
        xref = "paper",
        xanchor = "middle",
        yanchor = "top",
        showarrow = FALSE,
        font = list(size = 15)
      )
    
    if(lev == 2)
      p <- p %>% 
      layout(
        yaxis = list(title = ttl)
      )
    
    if(lev != 2)
      p <- p %>% 
      layout(
        yaxis = list(title = NA)
      )
    
    nm <- paste0('p', lev)
    
    assign(nm, p)
    
  }
  
  out <- subplot(p1, p2, p3, p4, p5, p6, p7, shareX = F, nrows = length(levs), shareY = F, titleY = T) %>%
    layout(
      xaxis = list(title = NA, gridcolor = '#FFFFFF'),
      barmode = 'stack',
      legend = list(title = list(text = 'Source'), traceorder = 'reversed')
    ) %>% 
    config(
      toImageButtonOptions = list(
        format = "svg",
        filename = "myplot"
      )
    )
  
  return(out)
  
}

toplo <- allmo %>% 
  filter(!bay_segment %in% 'Boca Ciega Bay') %>% 
  mutate(
    bay_segment = case_when(
      bay_segment == 'Boca Ciega Bay-South' ~ 'Boca Ciega Bay', 
      T ~ bay_segment
    )
  )
tnsrc_plo2(toplo)

toplo <- allmo %>% 
  filter(!bay_segment %in% 'Boca Ciega Bay') %>%
  mutate(
    bay_segment = case_when(
      bay_segment %in% c('Boca Ciega Bay-South', 'Manatee River', 'Terra Ceia Bay') ~ 'Remainder Lower Tampa Bay', 
      T ~ bay_segment
    )
  ) %>% 
  summarise(
    tn_load = sum(tn_load),
    tp_load = sum(tp_load),
    tss_load = sum(tss_load),
    bod_load = sum(bod_load),
    .by = c('bay_segment', 'year', 'month', 'source')
  )

totsmo <- toplo %>% 
  group_by(year, month, source) %>% 
  summarise(
    tn_load = sum(tn_load),
    tp_load = sum(tp_load), 
    tss_load = sum(tss_load), 
    bod_load = sum(bod_load),
    .groups = 'drop'
  ) %>% 
  mutate(bay_segment = 'All Segments (- N. BCB)')

toplo <- bind_rows(toplo, totsmo) %>% 
  select(year, month, bay_segment, source, tn_load, tp_load, tss_load, bod_load)

tnsrc_plo(toplo, xval = 'date', src = 'all')
