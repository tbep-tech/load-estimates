#' plot tn load by source, annual or monthly
tnsrc_plo <- function(datin, xval = c('year', 'date'), src = c('all', 'select'), yval = 'tn_load', addtnlns = F){

  xval <- match.arg(xval)

  srcs <- c('AD', 'DPS', 'GWS', 'IPS', 'NPS')
  cols <- c('AD' = '#33CC33', 'DPS' = '#00B0F0', 'GWS' = '#EB641B', 'IPS' = '#C0504D', 'NPS' = '#FFFF99')
  if(src == 'select'){
    srcs <- c('DPS - reuse', 'DPS - end of pipe', 'IPS', 'NPS')
    cols <- c('DPS - reuse' = '#1F497D', 'DPS - end of pipe' = '#00B0F0','IPS' = '#C0504D', 'NPS' = '#FFFF99')
  }
  
  levs <- c('All Segments (- N. BCB)', 'Old Tampa Bay', 'Hillsborough Bay', 'Middle Tampa Bay', 'Lower Tampa Bay', 'Remainder Lower Tampa Bay')
  
  lntndf <- data.frame(
    bay_segment = levs[-1], 
    ln = c(486, 1451, 799, 349, 629)
  )

  ttl <- 'Total Nitrogen (tons / yr)'
  if(xval == 'date'){
    
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
    
  }
  
  for(lev in seq_along(levs)){

    toplo <- datin %>% 
      select(matches('year|date'), bay_segment, source, !!yval) %>% 
      filter(bay_segment %in% !!levs[lev]) %>% 
      rename(dt = !!xval) %>% 
      mutate(source = factor(source, levels = srcs)) %>%
      spread(source, !!yval, fill = 0, drop = F)
    
    showleg <- F
    if(lev == 1)
      showleg <- T

    if(src == 'all'){
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
    
      # horizontal ref tn line
      if(lev != 1 & addtnlns){
        
        ln <- lntndf[lntndf$bay_segment %in% levs[lev], 'ln']
        
        p <- p %>%  
          add_segments(x = min(toplo$dt), xend = max(toplo$dt), y = ln, yend = ln, line = list(color = 'grey', dash = 3), showlegend = F, alpha = 0)
        
      }
        
    }
    
    if(src == 'select')
      p <- plot_ly(toplo, alpha = 1, fill = 'tonexty')  %>% 
        add_markers(x = ~dt, y = ~`DPS - reuse`, color = I(cols[['DPS - reuse']]), stackgroup = 'one', mode = 'none', marker = list(opacity = 0, size = 0), 
                    showlegend = showleg, legendgroup = 'grp4', name = 'DPS - reuse') %>%   
        add_markers(x = ~dt, y = ~`DPS - end of pipe`, color = I(cols[['DPS - end of pipe']]), stackgroup = 'one', mode = 'none', marker = list(opacity = 0, size = 0), 
                    showlegend = showleg, legendgroup = 'grp3', name = 'DPS - end of pipe') %>% 
        add_markers(x = ~dt, y = ~IPS, color = I(cols[['IPS']]), stackgroup = 'one', mode = 'none', marker = list(opacity = 0, size = 0), 
                    showlegend = showleg, legendgroup = 'grp2', name = 'IPS') %>% 
        add_markers(x = ~dt, y = ~NPS, color = I(cols[['NPS']]), stackgroup = 'one', mode = 'none', marker = list(opacity = 0, size = 0), 
                    showlegend = showleg, legendgroup = 'grp1', name = 'NPS') 
    
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
  
  out <- subplot(p1, p2, p3, p4, p5, p6, shareX = F, nrows = length(levs), shareY = F, titleY = T) %>%
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

#' plot hydrologic load by month
hy_plo <- function(datin){

  levs <- c('All Segments (- N. BCB)', 'Old Tampa Bay', 'Hillsborough Bay', 'Middle Tampa Bay', 'Lower Tampa Bay', 'Remainder Lower Tampa Bay')
  
  ttl <- 'Total Hydrologic load (10e6 m3/mo)'

  datin <- datin %>% 
    mutate(dy = 1) %>% 
    unite('date', year, month, dy, sep = '-', remove = T) %>% 
    mutate(
      date = ymd(date)
    ) %>% 
    select(date, bay_segment, hy_load_106_m3_mo)
    
  for(lev in seq_along(levs)){
    
    toplo <- datin %>% 
      filter(bay_segment %in% !!levs[lev]) %>% 
      rename(dt = 'date')
    
    p <- plot_ly(toplo, alpha = 1, fill = 'tonexty')  %>% 
      add_markers(x = ~dt, y = ~hy_load_106_m3_mo, color = I('lightblue'), mode = 'none', marker = list(opacity = 0, size = 0), showlegend = FALSE)
      
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
  
  out <- subplot(p1, p2, p3, p4, p5, p6, shareX = T, nrows = length(levs), shareY = F, titleY = T, titleX = T) %>%
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

#' plot total load as tn, hyd, or ratio, annual or monthly
ldtot_plo <- function(datin, yval = c('tn_load', 'tp_load', 'hy_load', 'tnhy', 'tphy')){
  
  levs <- c('All Segments (- N. BCB)', 'Old Tampa Bay', 'Hillsborough Bay', 'Middle Tampa Bay', 'Lower Tampa Bay', 'Remainder Lower Tampa Bay')
  
  # ref lines, all from 2009 Reasonal Assurance Addendum (except tphy)
  # https://drive.google.com/file/d/10IjJAfcGFf007a5VdPXAUtUi4dx-cmsA/view
  tnhydf <- data.frame(
    bay_segment = levs[-1], 
    ln = c(1.08, 1.62, 1.24, 0.97, 1.59)
  )
  
  tphydf <- data.frame(
    bay_segment = levs[-1], 
    ln = c(0.23, 1.28, 0.24, 0.14, 0.52) # from Ray Pribble email 11/4/22
  )
  
  lntndf <- data.frame(
    bay_segment = levs[-1], 
    ln = c(486, 1451, 799, 349, 629)
  )
  
  hydf <- data.frame(
    bay_segment = levs[-1], 
    ln = c(449, 896, 645, 361, 423)
  )
  
  ylbs <- tibble(
    yval = c('tn_load', 'tp_load', 'hy_load', 'tnhy', 'tphy'), 
    ttl = c('Total Nitrogen (tons / yr)', 'Total Phosphorus (tons/ yr)', 'Total Hydro Load (mill m3 / yr)', 'TN vs Hydrology ratio', 'TP vs Hydrology ratio')
  ) 
    
  yval <- match.arg(yval)
  
  ttl <- ylbs %>% 
    filter(yval == !!yval) %>% 
    pull(ttl)
  
  for(lev in seq_along(levs)){
    
    toplo <- datin %>% 
      filter(bay_segment %in% !!levs[lev]) %>% 
      rename(
        dt = year, 
        yv = !!yval
      )
    
    p <- plot_ly(toplo)  %>% 
      add_trace(x = ~dt, y = ~yv, color = I('blue'), mode = 'lines+markers', type = 'scatter', showlegend = F) %>% #, marker = list(opacity = 1, size = 4)) %>% 
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
    
    # horizontal ref line
    if(lev != 1 & yval == 'tnhy'){
      
      ln <- tnhydf[tnhydf$bay_segment %in% levs[lev], 'ln']
      
      p <- p %>%  
        add_segments(x = min(toplo$dt), xend = max(toplo$dt), y = ln, yend = ln, line = list(color = 'grey', dash = 3), showlegend = F)
      
    }
    
    # horizontal ref line
    if(lev != 1 & yval == 'tphy'){
      
      ln <- tphydf[tphydf$bay_segment %in% levs[lev], 'ln']
      
      p <- p %>%  
        add_segments(x = min(toplo$dt), xend = max(toplo$dt), y = ln, yend = ln, line = list(color = 'grey', dash = 3), showlegend = F)
      
    }
    
    # horizontal ref tn line
    if(lev != 1 & yval == 'tn_load'){
      
      ln <- lntndf[lntndf$bay_segment %in% levs[lev], 'ln']
      
      p <- p %>%  
        add_segments(x = min(toplo$dt), xend = max(toplo$dt), y = ln, yend = ln, line = list(color = 'grey', dash = 3), showlegend = F)
      
    }
    
    # horizontal ref line
    if(lev != 1 & yval == 'hy_load'){
      
      ln <- hydf[hydf$bay_segment %in% levs[lev], 'ln']
      
      p <- p %>%  
        add_segments(x = min(toplo$dt), xend = max(toplo$dt), y = ln, yend = ln, line = list(color = 'grey', dash = 3), showlegend = F)
      
    }
    
    
    nm <- paste0('p', lev)
    
    assign(nm, p)
    
  }

  out <- subplot(p1, p2, p3, p4, p5, p6, shareX = F, nrows = length(levs), shareY = F, titleY = T) %>%
    layout(
      xaxis = list(title = NA, gridcolor = '#FFFFFF')
    ) %>% 
    config(
      toImageButtonOptions = list(
        format = "svg",
        filename = "myplot"
      )
    )
  
  return(out)
  
}

#' reactable table summaries
rct_tab <- function(datin, dtvar = c('year', 'date'), typ = c('tn', 'tots'), val = 'tn_load'){
  
  dtvar <- match.arg(dtvar)
  typ <- match.arg(typ)
  
  sticky_style <- list(position = "sticky", left = 0, background = "#fff", zIndex = 1,
                       borderRight = "1px solid #eee", fontWeight = 'bold')
  
  if(dtvar == 'date')
    datin <- datin %>% 
      mutate(dy = 1) %>% 
      unite('date', year, month, dy, sep = '-', remove = T) %>% 
      mutate(
        date = ymd(date)
      )
  
  if(typ == 'tn'){
    
    totab <- datin %>% 
      rename(dt = !!dtvar) %>% 
      select(bay_segment, dt, source, !!val) %>% 
      pivot_wider(names_from = source, values_from = !!val) %>% 
      mutate(
        Total = rowSums(select(., -dt, -bay_segment), na.rm = T), 
        dt = gsub('\\-[0-9]*$', '', dt)
      )

    out <- reactable(totab,
                     groupBy = 'bay_segment',
                     columns = list(
                       dt = colDef(name = toTitleCase(tolower(dtvar)), 
                                     format = colFormat(digits = 0, separators = FALSE), 
                                     style = sticky_style, 
                                     headerStyle = sticky_style, 
                                     footerStyle = sticky_style
                       ), 
                       bay_segment = colDef(name = ''), 
                       Total = colDef(
                         class = "sticky right-col-1", 
                         headerClass = "sticky right-col-1",
                         footerClass = "sticky right-col-1"
                       )
                     ),
                     defaultColDef = colDef(
                       footerStyle = list(fontWeight = "bold"),
                       format = colFormat(digits = 2, separators = TRUE),
                       resizable = TRUE
                     )
    )
  
  }
  
  if(typ == 'tots'){
    
    totab <- datin %>% 
      rename(dt = !!dtvar)
    
    out <- reactable(totab,
                     groupBy = 'bay_segment',
                     columns = list(
                       dt = colDef(name = toTitleCase(tolower(dtvar)), 
                                     format = colFormat(digits = 0, separators = FALSE), 
                                     style = sticky_style, 
                                     headerStyle = sticky_style, 
                                     footerStyle = sticky_style
                       ), 
                       bay_segment = colDef(name = ''), 
                       tn_load = colDef(name = "TN load (tons / yr)"), 
                       tp_load = colDef(name = "TP load (tons / yr)"),
                       hy_load = colDef(name = "Hydrologic load (mill m3 / yr)"), 
                       tnhy = colDef(name = 'TN vs Hydrology ratio'), 
                       tphy = colDef(name = 'TP vs Hydrology ratio')
                     ),
                     defaultColDef = colDef(
                       footerStyle = list(fontWeight = "bold"),
                       format = colFormat(digits = 2, separators = TRUE),
                       resizable = TRUE
                     )
    )

  }
    
  return(out)
  
}

#' hydrological reactable table summaries
hy_tab <- function(datin){
  
  sticky_style <- list(position = "sticky", left = 0, background = "#fff", zIndex = 1,
                       borderRight = "1px solid #eee", fontWeight = 'bold')
  
  datin <- datin %>% 
    mutate(dy = 1) %>% 
    unite('date', year, month, dy, sep = '-', remove = T) %>% 
    mutate(
      date = ymd(date)
    )
    
  totab <- datin %>% 
    rename(dt = date) %>% 
    select(bay_segment, dt, `Total (10e6 m3/mo)` = hy_load_106_m3_mo) %>%
    mutate(
      dt = gsub('\\-[0-9]*$', '', dt)
    )

  out <- reactable(totab,
                   groupBy = 'bay_segment',
                   columns = list(
                     dt = colDef(name = 'Date', 
                                 format = colFormat(digits = 0, separators = FALSE), 
                                 style = sticky_style, 
                                 headerStyle = sticky_style, 
                                 footerStyle = sticky_style
                     ), 
                     bay_segment = colDef(name = ''), 
                     Total = colDef(
                       class = "sticky right-col-1", 
                       headerClass = "sticky right-col-1",
                       footerClass = "sticky right-col-1"
                     )
                   ),
                   defaultColDef = colDef(
                     footerStyle = list(fontWeight = "bold"),
                     format = colFormat(digits = 2, separators = TRUE),
                     resizable = TRUE
                   )
  )
  
  return(out)
  
}

# calculate dps reuse and end of pipe from city of tampa raw data (used in R/dat_proc.R)
#
# R-002 and R-003 are not counted (see T:\03_BOARDS_COMMITTEES\05_TBNMC\2022_RA_Update\01_FUNDING_OUT\DELIVERABLES\TO-9\datastick_deliverables\LoadingCodes&Datasets\2021\PointSource2021\Domestic2021\1_DPS_2021a_20221025.sas)
# flow in million gallons per day
# multiply flow by day in month to get million gallons per month
# multiply flow by 3785.412 to get cubic meters per month
# multiply N by flow and divide by 1000 to get kg N per month 
#   multiply m3 by 1000 to get L, then divide by 1e6 to convert mg to kg)
#   same as dividing by 1000
# TN dps reuse is multiplied by 0.3 for land application attenuation factor (70%)
# TP, TSS, BOD  dps reuse is multiplied by 0.05 for land application attenuation factor (95%)
# see line 473, 475, 477, 479 2_DPS_2021b_20221025.sas
# 
# BOD is reported as CBOD starting nov 2022, see JH email 10/4/23
#
# hydro load (m3 / mo) is also attenuated for the reuse, multiplied by 0.6 (40% attenutation)
# see line 471 2_DPS_2021b_20221025.sas
#
# path is location to raw csv
# 
# output is load for tp, tn, tss, bod as tons per month
# hydro load is million (10^6 or 1e6) cubic meters per month
dps_est <- function(path){

  out <- read_excel(path, sheet = 'Data') %>% 
    select(Year, Month, matches('D-001|R-001'), `Total N`, `Total P`, TSS, BOD) %>% 
    rename(
      `DPS - end of pipe` = matches('D-001'), 
      `DPS - reuse` = matches('R-001')
    ) %>% 
    na.omit() %>% 
    filter(Year != 'Year') %>% 
    pivot_longer(names_to = 'source', values_to = 'flow_mgd', c(`DPS - end of pipe`, `DPS - reuse`)) %>% 
    pivot_longer(names_to = 'var', values_to = 'conc_mgl', c(`Total N`:BOD)) %>% 
    mutate_at(vars(Year, Month, flow_mgd, conc_mgl), as.numeric) %>% 
    mutate(
      dys = days_in_month(ymd(paste(Year, Month, '01', sep = '-'))), 
      flow_mgm = flow_mgd * dys, # million gallons per month
      flow_m3m = flow_mgm * 3785.412, # cubic meters per month
      load_kg = conc_mgl * flow_m3m / 1000, # kg var per month, 
      load_tons = load_kg / 907.1847, # kg to tons,
      load_tons = case_when(
        grepl('reuse', source) & var == 'Total N' ~ load_tons * 0.3, 
        grepl('reuse', source) & var %in% c('Total P', 'TSS', 'BOD') ~ load_tons * 0.05, 
        T ~ load_tons
      ),
      flow_m3m = case_when(
        grepl('reuse', source) ~ flow_m3m * 0.6, 
        T ~ flow_m3m
      ),
      entity = 'Tampa', 
      bayseg = 2, # HB
      var = factor(var, levels = c('Total N', 'Total P', 'TSS', 'BOD'), 
                   labels = c('tn_load', 'tp_load', 'tss_load', 'bod_load')
      ), 
      hy_load = flow_m3m / 1e6 # flow as mill m3 /month
    ) %>% 
    select(-flow_mgm, -flow_mgd, -conc_mgl, -dys, -load_kg, -flow_m3m) %>%
    pivot_wider(names_from = 'var', values_from = 'load_tons') %>% 
    select(Year, Month, entity, source, bayseg, tn_load, tp_load, tss_load, bod_load, hy_load)

  return(out)
  
}

# calculate difference between updated and original dps data for hfc/city of tampa
# tn, tp, tss, bod, hy
# tn, tp, tss, bod as tons
# hy as 10^6 m^3
#
# results as per month or per year based on agg fun
# total logical indicating if diffs are separated as reuse/end of pipe or total of both
dpsdiff_fun <- function(dpsupdate, annual = F, total = F){
  
  ##
  # original data
  # domestic point source prior to 2017-2021 RA 
  dpsmosdat1 <- read_sas(here('data/raw/dps0420monthentbas.sas7bdat')) %>% 
    filter(Year < 2017)
  dpsmosdat2 <- read_sas(here('data/raw/dps1721monthentbas.sas7bdat')) 
  
  olddat <- bind_rows(dpsmosdat1, dpsmosdat2) %>% 
    filter(entity == 'Tampa') %>% 
    filter(Year > 2011) %>%  # earliest year in updated data is 2012
    mutate(
      source = case_when(
        grepl('REUSE$', source2) ~ 'DPS - reuse', 
        grepl('SW$', source2) ~ 'DPS - end of pipe'
      )
    ) %>% 
    select(
      Year,
      Month,
      entity, 
      source, 
      tn_load = tnloadtons, 
      tp_load = tploadtons, 
      tss_load = tssloadtons, 
      bod_load = bodloadtons,
      hy_load = h2oload10e6m3
    )
  
  ##
  # hfc/city of tampa updated data
  newdat <- dpsupdate %>% 
    filter(Year < 2022)
  
  ##
  # combine
  
  # prep for plot
  cmbdat <- full_join(olddat, newdat, by = c('Year', 'Month', 'entity', 'source'), suffix = c('.old', '.new')) %>% 
    pivot_longer(names_to = 'var', values_to = 'val', -c(Year, Month, entity, source, bayseg)) %>%
    separate(var, into = c('var', 'type'), sep = '\\.') %>% 
    pivot_wider(names_from = type, values_from = val)
  
  # get total dps if true
  if(total)
    cmbdat <- cmbdat %>% 
      mutate(
        source = 'DPS'
      ) %>% 
      summarise(
        old = sum(old), 
        new = sum(new), 
        .by = c(Year, Month, entity, source, bayseg, var)
      )
    
  # get annual loads if true
  if(annual)
    cmbdat <- cmbdat %>% 
      summarize(
        old = sum(old), 
        new = sum(new),
        .by = c(Year, entity, source, bayseg, var)
      )
  
  # get differece between new and original data
  out <- cmbdat %>% 
    mutate(
      diffv = new - old
    )
  
  return(out)
  
}      