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
    )
  
  return(out)
  
}

#' plot total load as tn, hyd, or ratio, annual or monthly
ldtot_plo <- function(datin, yval = c('tn_load', 'hy_load', 'tnhy'), addlns = F, addtnlns = F){
  
  levs <- c('All Segments (- N. BCB)', 'Old Tampa Bay', 'Hillsborough Bay', 'Middle Tampa Bay', 'Lower Tampa Bay', 'Remainder Lower Tampa Bay')
  
  # ref lines
  lndf <- data.frame(
    bay_segment = levs[-1], 
    ln = c(1.08, 1.62, 1.24, 0.97, 1.59)
  )
  
  lntndf <- data.frame(
    bay_segment = levs[-1], 
    ln = c(486, 1451, 799, 349, 629)
  )
  
  ylbs <- tibble(
    yval = c('tn_load', 'hy_load', 'tnhy'), 
    ttl = c('Total Nitrogen (tons / yr)', 'Total Hydro Load (mill m3 / yr)', 'TN vs Hydrology ratio')
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
    if(lev != 1 & addlns){
      
      ln <- lndf[lndf$bay_segment %in% levs[lev], 'ln']
      
      p <- p %>%  
        add_segments(x = min(toplo$dt), xend = max(toplo$dt), y = ln, yend = ln, line = list(color = 'grey', dash = 3), showlegend = F)
      
    }
    
    # horizontal ref tn line
    if(lev != 1 & addtnlns){
      
      ln <- lntndf[lntndf$bay_segment %in% levs[lev], 'ln']
      
      p <- p %>%  
        add_segments(x = min(toplo$dt), xend = max(toplo$dt), y = ln, yend = ln, line = list(color = 'grey', dash = 3), showlegend = F)
      
    }
    
    nm <- paste0('p', lev)
    
    assign(nm, p)
    
  }

  out <- subplot(p1, p2, p3, p4, p5, p6, shareX = F, nrows = length(levs), shareY = F, titleY = T) %>%
    layout(
      xaxis = list(title = NA, gridcolor = '#FFFFFF')
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
                       hy_load = colDef(name = "Hydrologic load (mill m3 / yr)"), 
                       tnhy = colDef(name = 'TN vs Hydrology ratio')
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