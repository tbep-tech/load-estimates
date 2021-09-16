#' plot tn load by source, annual or monthly
tnsrc_plo <- function(datin, xval = c('YEAR', 'date')){
  
  typs <- c('AD', 'DPS', 'GWS', 'IPS', 'NPS')
  levs <- c('All Segments (- N. BCB)', 'Old Tampa Bay', 'Hillsborough Bay', 'Middle Tampa Bay', 'Lower Tampa Bay', 'Remainder Lower Tampa Bay')
  
  cols <- qualitative_hcl(length(unique(tndat$SOURCE)), palette = "Dynamic")

  xval <- match.arg(xval)
  
  for(lev in seq_along(levs)){
    
    toplo <- datin %>% 
      filter(bay_segment %in% !!levs[lev]) %>% 
      rename(dt = !!xval) %>% 
      mutate(SOURCE = factor(SOURCE, levels = typs)) %>% 
      spread(SOURCE, tn_load, fill = 0, drop = F)
    
    showleg <- F
    if(lev == 1)
      showleg <- T
    
    p <- plot_ly(toplo)  %>% 
      add_markers(x = ~dt, y = ~NPS, color = I(cols[5]), stackgroup = 'one', mode = 'none', marker = list(opacity = 0, size = 0), 
                  showlegend = showleg, legendgroup = 'grp5', name = 'NPS') %>%   
      add_markers(x = ~dt, y = ~IPS, color = I(cols[4]), stackgroup = 'one', mode = 'none', marker = list(opacity = 0, size = 0), 
                  showlegend = showleg, legendgroup = 'grp4', name = 'IPS') %>% 
      add_markers(x = ~dt, y = ~GWS, color = I(cols[3]), stackgroup = 'one', mode = 'none', marker = list(opacity = 0, size = 0), 
                  showlegend = showleg, legendgroup = 'grp3', name = 'GWS') %>% 
      add_markers(x = ~dt, y = ~DPS, color = I(cols[2]), stackgroup = 'one', mode = 'none', marker = list(opacity = 0, size = 0), 
                  showlegend = showleg, legendgroup = 'grp2', name = 'DPS') %>% 
      add_markers(x = ~dt, y = ~AD, color = I(cols[1]), stackgroup = 'one', mode = 'none', marker = list(opacity = 0, size = 0), 
                  showlegend = showleg, legendgroup = 'grp1', name = 'AD') %>% 
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
        yaxis = list(title = 'Total Nitrogen (tons / yr)')
      )
    
    if(lev != 2)
      p <- p %>% 
      layout(
        yaxis = list(title = NA)
      )
    
    nm <- paste0('p', lev)
    
    assign(nm, p)
    
  }
  
  out <- subplot(p1, p2, p3, p4, p5, p6, shareX = T, nrows = length(levs), shareY = F, titleY = T) %>%
    layout(
      xaxis = list(title = NA, gridcolor = '#FFFFFF'),
      barmode = 'stack',
      legend = list(title = list(text = 'Source'))
    )
  
  return(out)
  
}

#' plot total load as tn, hyd, or ratio, annual or monthly
ldtot_plo <- function(datin, yval = c('tn_load', 'hy_load', 'tnhy'), xval = c('YEAR', 'date'), addlns = F){
  
  levs <- c('All Segments (- N. BCB)', 'Old Tampa Bay', 'Hillsborough Bay', 'Middle Tampa Bay', 'Lower Tampa Bay', 'Remainder Lower Tampa Bay')
  
  # ref lines
  lndf <- data.frame(
    bay_segment = levs[-1], 
    ln = c(1.08, 1.62, 1.24, 0.97, 1.59)
  )
  
  ylbs <- tibble(
    yval = c('tn_load', 'hy_load', 'tnhy'), 
    ttl = c('Total Nitrogen (tons / yr)', 'Total Hydro Load (mill m3 / yr)', 'TN vs Hydrology ratio')
  ) 
    
  xval <- match.arg(xval)
  yval <- match.arg(yval)
  
  ttl <- ylbs %>% 
    filter(yval == !!yval) %>% 
    pull(ttl)
  
  for(lev in seq_along(levs)){
    
    toplo <- totdat %>% 
      filter(bay_segment %in% !!levs[lev]) %>% 
      rename(
        dt = !!xval, 
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
    
    nm <- paste0('p', lev)
    
    assign(nm, p)
    
  }
  
  out <- subplot(p1, p2, p3, p4, p5, p6, shareX = T, nrows = length(levs), shareY = F, titleY = T) %>%
    layout(
      xaxis = list(title = NA, gridcolor = '#FFFFFF')
    )
  
  return(out)
  
}

#' reactable table summaries
rct_tab <- function(datin, dtvar = c('YEAR', 'date'), typ = c('tn', 'tots')){
  
  dtvar <- match.arg(dtvar)
  typ <- match.arg(typ)
  
  sticky_style <- list(position = "sticky", left = 0, background = "#fff", zIndex = 1,
                       borderRight = "1px solid #eee", fontWeight = 'bold')
  
  if(typ == 'tn'){
    
    totab <- tndat %>% 
      rename(dt = !!dtvar) %>% 
      select(bay_segment, dt, SOURCE, tn_load) %>% 
      pivot_wider(names_from = SOURCE, values_from = tn_load) %>% 
      rowwise() %>% 
      mutate(Total = sum(AD, DPS , IPS, NPS ,GWS, na.rm = T)) %>% 
      ungroup()
      
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