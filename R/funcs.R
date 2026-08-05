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
      bay_segment = 'Hillsborough Bay', 
      var = factor(var, levels = c('Total N', 'Total P', 'TSS', 'BOD'), 
                   labels = c('tn_load', 'tp_load', 'tss_load', 'bod_load')
      ), 
      hy_load = flow_m3m / 1e6 # flow as mill m3 /month
    ) %>% 
    select(-flow_mgm, -flow_mgd, -conc_mgl, -dys, -load_kg, -flow_m3m) %>%
    pivot_wider(names_from = 'var', values_from = 'load_tons') %>% 
    select(Year, Month, entity, source, bay_segment, tn_load, tp_load, tss_load, bod_load, hy_load)

  return(out)
  
}

# calculate difference between updated and original dps data for hfc/city of tampa
# tn, tp, tss, bod, hy
# tn, tp, tss, bod as tons
# hy as 10^6 m^3
#
# results as per month or per year based on agg fun
# total logical indicating if diffs are separated as reuse/end of pipe or total of both
# optional varsel as character string for one to many loading variables to return only as diff in wide format
dpsdiff_fun <- function(dpsupdate, annual = F, total = F, varsel = NULL){

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
    pivot_longer(names_to = 'var', values_to = 'val', -c(Year, Month, entity, source, bay_segment)) %>%
    separate(var, into = c('var', 'type'), sep = '\\.') %>% 
    pivot_wider(names_from = type, values_from = val) %>% 
    rename(year = Year, month = Month)
  
  # get total dps if true
  if(total)
    cmbdat <- cmbdat %>% 
      mutate(
        source = 'DPS'
      ) %>% 
      summarise(
        old = sum(old, na.rm = T), 
        new = sum(new, na.rm = T), 
        .by = c(year, month, entity, source, bay_segment, var)
      )
    
  # get annual loads if true
  if(annual)
    cmbdat <- cmbdat %>% 
      summarize(
        old = sum(old, na.rm = T), 
        new = sum(new, na.rm = T),
        .by = c(year, entity, source, bay_segment, var)
      )
  
  # get differece between new and original data
  out <- cmbdat %>% 
    mutate(
      diffv = new - old
    ) 
  
  # subset variables, return only diff in wide format
  if(!is.null(varsel)){

    if(any(!varsel %in% c('tn_load', 'tp_load', 'tss_load', 'bod_load', 'hy_load')))
      stop('varsel must be one to many of tn_load, tp_load, tss_load, bod_load, hy_load')

    out <- out %>%
      filter(var %in% varsel) %>%
      select(-new, -old) %>%
      mutate(var = paste0(var, '_diffv')) %>%
      pivot_wider(names_from = 'var', values_from = 'diffv')

  }

  return(out)

}

# recode a tbeploads `source` value to the canonical 5-level load-estimates scheme
# (AD/DPS/GWS/IPS/NPS) expected by tnsrc_plo/ldtot_plo/hy_plo -- any other source
# label is silently dropped by those plotting functions, so every tbeploads-derived
# object must be recoded through this before being combined into tnanndat/mosdat/
# totanndat/mohydat
recode_src5 <- function(source){
  dplyr::case_when(
    source == 'AD' ~ 'AD',
    grepl('^DPS', source) ~ 'DPS',
    source %in% c('GW', 'SPR') ~ 'GWS',
    source %in% c('IPS', 'ML') ~ 'IPS',
    source == 'NPS' ~ 'NPS'
  )
}

# disaggregate monthly basin-level NPS TN (tbeploads::anlz_nps(summ = 'basin', summtime =
# 'month')) to individual MS4/NPS entities for use in mosentdat.
#
# tbeploads has no function that produces monthly, entity-resolved NPS loads directly:
# anlz_nps has no entity grouping option, and anlz_aa (the only NPS -> entity allocator)
# is annual-only. This is a modeled approximation, not a re-derivation of a real
# monthly-entity process (none exists to reproduce): each entity's FIXED (time-invariant,
# since it depends only on static land-use/soils data), land-use-weighted fractional
# share of a basin's NPS TN load (from util_aa_npsfactors) is applied to that basin's
# monthly TN totals, then the result is rescaled so each entity/bay_seg/year sums exactly
# to the trusted annual total from anlz_aa(annavg = FALSE).
#
# npsfactors: list output of tbeploads::util_aa_npsfactors(tbbase, rcclucsid, emc)
# nps_mo_basin: tbeploads::anlz_nps(..., summ = 'basin', summtime = 'month')
# aa_yr: tbeploads::anlz_aa(..., annavg = FALSE)
#
# returns: year, month, bayseg (numeric segidmos code), entity, source = 'NPS', tn_load
nps_entmo_fun <- function(npsfactors, nps_mo_basin, aa_yr){

  # entity's fixed fractional share of each bay_seg x basin's NPS TN load, replicating
  # anlz_aa's own entity relabeling exactly so results are consistent with its entity
  # categories (Agriculture -> "All"; generic MS4 permits and Port Manatee in MTB/LTB ->
  # "Non-MS4/Ag NPS"); bay_seg 6/7 remapped to 55 to match anlz_aa's post-disaggregation
  # segment merge
  entity_basin_share <- npsfactors$tn %>%
    dplyr::inner_join(npsfactors$rc, by = c('bay_seg', 'basin', 'clucsid')) %>%
    dplyr::mutate(
      entity = dplyr::case_when(
        !is.na(category) & category == 'Agriculture' ~ 'All',
        entity %in% c('MSGP COT', 'MSGP PINELLAS') ~ 'Non-MS4/Ag NPS',
        bay_seg %in% c(3L, 4L) & entity == 'PORT MANATEE' ~ 'Non-MS4/Ag NPS',
        TRUE ~ entity
      ),
      factor_prod = factor_tn * factor_rc,
      bay_seg = dplyr::if_else(bay_seg %in% c(6L, 7L), 55L, bay_seg)
    ) %>%
    dplyr::group_by(bay_seg, basin, entity) %>%
    dplyr::summarise(factor_prod = sum(factor_prod, na.rm = TRUE), .groups = 'drop')

  # segment name -> bay_seg, matching anlz_aa's internal mapping
  seg_bay <- c(
    'Old Tampa Bay' = 1L, 'Hillsborough Bay' = 2L, 'Middle Tampa Bay' = 3L,
    'Lower Tampa Bay' = 4L, 'Terra Ceia Bay' = 6L, 'Manatee River' = 7L,
    'Boca Ciega Bay South' = 55L
  )

  entity_month_raw <- nps_mo_basin %>%
    dplyr::filter(source == 'NPS') %>%
    dplyr::mutate(
      bay_seg = seg_bay[segment],
      bay_seg = dplyr::if_else(bay_seg %in% c(6L, 7L), 55L, bay_seg)
    ) %>%
    dplyr::filter(!is.na(bay_seg)) %>%
    dplyr::inner_join(entity_basin_share, by = c('bay_seg', 'basin'), relationship = 'many-to-many') %>%
    dplyr::mutate(tn_load_wt = tn_load * factor_prod) %>%
    dplyr::group_by(bay_seg, entity, Year, Month) %>%
    dplyr::summarise(tn_load = sum(tn_load_wt, na.rm = TRUE), .groups = 'drop')

  entity_year_raw <- entity_month_raw %>%
    dplyr::group_by(bay_seg, entity, year = Year) %>%
    dplyr::summarise(tn_load_raw = sum(tn_load, na.rm = TRUE), .groups = 'drop')

  # identify NPS-path rows by exclusion rather than by source label: anlz_aa's NPS path
  # sets source to the entity's matched allocation type ("MS4" for tracked jurisdictions,
  # "Nonpoint Source/MS4" for individually-tracked MS4 subdivision permits) but leaves it
  # NA for synthetic aggregate categories with no direct allocations-table match ("All"
  # = Agriculture, "Non-MS4/Ag NPS" = generic MSGP permits/Port Manatee) -- all of these
  # are real NPS entities that belong here, whereas IPS/DPS/ML rows always carry one of
  # exactly those four labels, so excluding them is a robust way to select the full NPS set
  entity_year_true <- aa_yr %>%
    dplyr::filter(!source %in% c('DPS - end of pipe', 'DPS - reuse', 'IPS', 'ML')) %>%
    dplyr::select(bay_seg, entity, year, load_tons)

  # scale factor per entity/bay_seg/year, with explicit divide-by-zero handling: if the
  # land-use-weighted disaggregation produces exactly zero for an entity/year that
  # nonetheless has a real annual total (e.g. a newly-added entity with no matching
  # CLUCSID-weighted footprint that year), spread the true annual total evenly across
  # the 12 months rather than dropping the entity or leaving it NA -- flagged via
  # `evenspread` so these rows are identifiable downstream
  scale_df <- entity_year_true %>%
    dplyr::full_join(entity_year_raw, by = c('bay_seg', 'entity', 'year')) %>%
    dplyr::mutate(
      load_tons = dplyr::coalesce(load_tons, 0),
      tn_load_raw = dplyr::coalesce(tn_load_raw, 0),
      evenspread = load_tons > 0 & tn_load_raw == 0,
      scale = dplyr::case_when(
        tn_load_raw > 0 ~ load_tons / tn_load_raw,
        TRUE ~ 1
      )
    )

  scaled <- entity_month_raw %>%
    dplyr::inner_join(scale_df %>% dplyr::filter(!evenspread), by = c('bay_seg', 'entity', 'Year' = 'year')) %>%
    dplyr::mutate(tn_load = tn_load * scale) %>%
    dplyr::select(bay_seg, entity, Year, Month, tn_load)

  evenspread_rows <- scale_df %>%
    dplyr::filter(evenspread) %>%
    dplyr::select(bay_seg, entity, year, load_tons) %>%
    tidyr::crossing(Month = 1:12) %>%
    dplyr::mutate(tn_load = load_tons / 12, Year = year) %>%
    dplyr::select(bay_seg, entity, Year, Month, tn_load)

  # bayseg is returned as the numeric segidmos code (not the label) so this plugs
  # directly into dat_proc.R's existing bayseg -> bay_segment factor() conversion
  out <- dplyr::bind_rows(scaled, evenspread_rows) %>%
    dplyr::mutate(source = 'NPS') %>%
    dplyr::rename(year = Year, month = Month, bayseg = bay_seg) %>%
    dplyr::select(year, month, bayseg, entity, source, tn_load)

  attr(out, 'evenspread_df') <- scale_df %>% dplyr::filter(evenspread)
  attr(out, 'scale_df') <- scale_df

  return(out)

}