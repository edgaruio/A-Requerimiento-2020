library(shiny)

shinyServer(function(input, output, session) {
  addClass(selector = "body", class = "sidebar-collapse")
  
  ### Tab 1 ====
  
  data_f1 <- reactive({
    data_f1 <- poblacion_cajas %>%
      filter(TIPO %in% c(input$xcategoria1)) %>% 
      filter(FECHA >= input$xfecha[1] & FECHA <= input$xfecha[2])
    return(data_f1)
  })
  
  output$plot_cajas <- renderPlotly({
    data_plot <- data_f1() %>% 
      select(-TIPO)
    
    m <- list(l = 50,r = 50,b = 0,t = 50, pad = 0)
    colors <- c('rgb(333,133,133)', 'rgb(111,103,167)', 'rgb(222,104,87)', 'rgb(114,147,203)')
    f1 <- list(family = "Arial, sans-serif",size = 18,color = "lightgrey")
    f2 <- list(family = "Old Standard TT, serif",size = 14,color = "lightgrey")
    
    plot_ly(data_plot, x = ~FECHA, y = ~COLSUBSIDIO, name = "COLSUBSIDIO", type = 'bar', marker = list(color = c("#F0E442"))) %>%
      add_trace(y = ~COMPENSAR, name = "COMPENSAR", marker = list(color = c("orange"))) %>%
      add_trace(y = ~CAFAM, name = "CAFAM", marker = list(color = c("#0072B2"))) %>%
      layout(margin = m,
             title = 'Historico Afiliados CCF',
             font = list(color = 'lightgrey'),
             xaxis = list(showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE, titlefont = f1, tickfont = f2, 
                          tickcolor = 'rgb(127,127,127)'),
             yaxis = list(title = "Afiliados", showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE, titlefont = f1, 
                          tickfont = f2, tickcolor = 'rgb(127,127,127)'),
             # legend = list(x = 0.8, y = 0.5),
             paper_bgcolor='transparent',
             plot_bgcolor='transparent') %>%
      config(displayModeBar = F)
  })
  
  output$dt_cajas <- renderDataTable({
    data_plot <- data_f1()
    datatable(data_plot,
              rownames = F,
              options = list(paging = T, searching = F, scrollX = "50px")
              ) %>%
      formatCurrency(columns = "CAFAM", interval = 3, mark = ",", currency = "", digits = 0) %>%
      formatCurrency(columns = "COLSUBSIDIO", interval = 3, mark = ",", currency = "", digits = 0) %>% 
      formatCurrency(columns = "COMPENSAR", interval = 3, mark = ",", currency = "", digits = 0)
  })
  
  data_participacion_ccf <- reactive({
    
    test <- data_f1() %>% 
      mutate(llave = paste(TIPO, FECHA, sep = "_")) %>% 
      select(-c(FECHA,TIPO)) %>% 
      gather("CCF","AFILIADOS",CAFAM:COMPENSAR) %>% 
      data.frame() %>% 
      mutate(FECHA = substr(llave, start = nchar(llave)-9, stop = nchar(llave)),
             TIPO = substr(llave, start = 1, stop = nchar(llave)-11)) %>% 
      select(-llave)
    
    aux1 <- test %>% 
      filter(TIPO != "Total") %>% 
      select(FECHA, TIPO, CCF, AFILIADOS) %>% 
      group_by(FECHA, TIPO) %>% 
      mutate(PROP = AFILIADOS/sum(AFILIADOS)) %>% 
      ungroup()
    
    aux2 <- test %>% 
      filter(TIPO == "Total") %>% 
      select(FECHA, TIPO, CCF, AFILIADOS) %>% 
      group_by(FECHA) %>% 
      mutate(PROP = AFILIADOS/sum(AFILIADOS)) %>% 
      ungroup()
    
    union_test <- bind_rows(aux1,aux2) %>% 
      data.frame() %>% 
      filter(TIPO %in% c(input$xcategoria1)) %>% 
      filter(FECHA >= input$xfecha[1] & FECHA <= input$xfecha[2]) %>% 
      select(FECHA, CCF, PROP)
    
    return(union_test)
  })
  
  output$plot_cajas_por <- renderPlotly({
    
    m <- list(l = 50,r = 50,b = 0,t = 50, pad = 0)
    f1 <- list(family = "Arial, sans-serif",size = 18,color = "lightgrey")
    f2 <- list(family = "Old Standard TT, serif",size = 14,color = "lightgrey")
    
    data_participacion_ccf() %>%
      mutate(CCF = ordered(CCF,
                           levels = c("COLSUBSIDIO","COMPENSAR","CAFAM"),
                           labels = c("COLSUBSIDIO","COMPENSAR","CAFAM"))
             ) %>%
      plot_ly(x = ~FECHA, y = ~PROP, color = ~CCF, colors = c("#F0E442","orange","#0072B2"),
              hoverinfo = "text", 
              text = ~paste("Porcentaje: ", round(100*PROP, 1), "%")) %>%
      add_bars() %>%
      layout(barmode = "stack",
             margin = m,
             title = 'Participacion Afiliados CCF',
             font = list(color = 'lightgrey'),
             xaxis = list(showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE, titlefont = f1, tickfont = f2, 
                          tickcolor = 'rgb(127,127,127)'),
             yaxis = list(title = "Participación", showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE, titlefont = f1, 
                          tickfont = f2, tickcolor = 'rgb(127,127,127)'),
             paper_bgcolor='transparent',
             plot_bgcolor='transparent') %>%
      config(displayModeBar = F)
  })
  
  output$dt_cajas_por <- renderDataTable({
    datatable(data_participacion_ccf(),
              rownames = F,
              filter = "top",
              options = list(paging = T, searching = F, scrollX = "50px"), 
              colnames = c("FECHA","CCF","PORCENTAJE")
              ) %>%
      formatPercentage("PROP", digits = 1, interval = 3, dec.mark = getOption("OutDec"))
  })
  
  ### Tab 2 ====
  
  data_f2 <- reactive({
    data_f2 <- poblacion_potencial %>%
      filter(Piramide2 %in% c(input$xpiramide)) %>% 
      filter(Categoria %in% c(input$xcategoria2)) %>% 
      data.frame()
    return(data_f2)
  })
  
  output$dt_afiliados <- renderDataTable({
    data_plot <- data_f2() %>% 
      select(nuevo_ciiu, ENE2020, FEB2020, MAR2020, ABR2020) %>% 
      group_by(nuevo_ciiu) %>% 
      summarise(ENE2020 = sum(ENE2020, na.rm = T), 
                FEB2020 = sum(FEB2020, na.rm = T), 
                MAR2020 = sum(MAR2020, na.rm = T), 
                ABR2020 = sum(ABR2020, na.rm = T)) %>% 
      mutate(PSI = round((ABR2020-MAR2020)*log(ABR2020/MAR2020), 2),
             PSI_Agru = case_when(
               PSI <= 0.1 ~ "Se mantiene",
               PSI > 0.1 & PSI <= 0.25 ~ "Cambio moderado",
               PSI > 0.25 ~ "Cambio significativo"),
             AUMENTA = ifelse(ABR2020>MAR2020, "SI", "NO")) %>% 
      data.frame() %>%
      adorn_totals(dat = ., where =  "row")
    datatable(data_plot,
              rownames = F,
              options = list(paging = F, searching = F, scrollX = "50px"),
              colnames = c("ACTIVIDAD", "ENE2020", "FEB2020", "MAR2020", "ABR2020", "PSI", "PSI Agrupado", "AUMENTA")
              ) %>% 
      formatCurrency(columns = "ENE2020", interval = 3, mark = ",", currency = "", digits = 0) %>%
      formatCurrency(columns = "FEB2020", interval = 3, mark = ",", currency = "", digits = 0) %>% 
      formatCurrency(columns = "MAR2020", interval = 3, mark = ",", currency = "", digits = 0) %>% 
      formatCurrency(columns = "ABR2020", interval = 3, mark = ",", currency = "", digits = 0) %>% 
      formatCurrency(columns = "PSI", interval = 3, mark = ",", currency = "", digits = 1) %>% 
      # formatStyle('PSI_Agru',
      #             backgroundColor = styleEqual(c("Se mantiene","Cambio moderado","Cambio significativo"),
      #                                          c('green','yellow','orangered'))) %>%
      # formatStyle('AUMENTA',
      #             backgroundColor = styleEqual(c("SI","NO"),
      #                                          c('green','orangered')))
      formatStyle(
        columns = c("PSI"),
        valueColumns = c("AUMENTA"),
        backgroundColor = styleEqual(c("SI","NO"),
                                     c("green", "red"))
      )
  })
  
  data_f3 <- reactive({
    data_f3 <- poblacion_potencial %>%
      filter(nuevo_ciiu %in% c(input$xnuevociiu)) %>% 
      filter(Categoria %in% c(input$xcategoria3)) %>% 
      data.frame()
    return(data_f3)
  })
  
  output$dt_afiliados_ciuu <- renderDataTable({
    data_plot <- data_f3() %>% 
      select(Piramide2, ENE2020, FEB2020, MAR2020, ABR2020) %>% 
      group_by(Piramide2) %>% 
      summarise(ENE2020 = sum(ENE2020, na.rm = T), 
                FEB2020 = sum(FEB2020, na.rm = T), 
                MAR2020 = sum(MAR2020, na.rm = T), 
                ABR2020 = sum(ABR2020, na.rm = T)) %>% 
      mutate(PSI = round((ABR2020-MAR2020)*log(ABR2020/MAR2020), 2),
             PSI_Agru = case_when(
               PSI <= 0.1 ~ "Se mantiene",
               PSI > 0.1 & PSI <= 0.25 ~ "Cambio moderado",
               PSI > 0.25 ~ "Cambio significativo"
               ),
             AUMENTA = ifelse(ABR2020>MAR2020, "SI", "NO")) %>% 
      data.frame() %>%
      adorn_totals(dat = ., where =  "row")
    datatable(data_plot,
              rownames = F,
              options = list(paging = F, searching = F, scrollX = "50px"),
              colnames = c("PIRAMIDE", "ENE2020", "FEB2020", "MAR2020", "ABR2020", "PSI", "PSI Agrupado", "AUMENTA")
    ) %>% 
      formatCurrency(columns = "ENE2020", interval = 3, mark = ",", currency = "", digits = 0) %>%
      formatCurrency(columns = "FEB2020", interval = 3, mark = ",", currency = "", digits = 0) %>% 
      formatCurrency(columns = "MAR2020", interval = 3, mark = ",", currency = "", digits = 0) %>% 
      formatCurrency(columns = "ABR2020", interval = 3, mark = ",", currency = "", digits = 0) %>%
      formatCurrency(columns = "PSI", interval = 3, mark = ",", currency = "", digits = 1) %>%
      # formatStyle('PSI_Agru', 
      #             backgroundColor = styleEqual(c("Se mantiene","Cambio moderado","Cambio significativo"), 
      #                                          c('green','yellow','orangered'))) %>% 
      # formatStyle('AUMENTA', 
      #             backgroundColor = styleEqual(c("SI","NO"), 
      #                                          c('green','orangered')))
      formatStyle(
        columns = c("PSI"),
        valueColumns = c("AUMENTA"),
        backgroundColor = styleEqual(c("SI","NO"),
                                     c("green", "red"))
      )
  })
  
  # dt_psi <- reactive({
  #   data_plot <- data_f2() %>% 
  #     select(nuevo_ciiu, ENE2020, FEB2020, MAR2020, ABR2020) %>% 
  #     group_by(nuevo_ciiu) %>% 
  #     summarise(ENE2020 = sum(ENE2020, na.rm = T), 
  #               FEB2020 = sum(FEB2020, na.rm = T), 
  #               MAR2020 = sum(MAR2020, na.rm = T), 
  #               ABR2020 = sum(ABR2020, na.rm = T))
  #   
  #   psi <- sum((data_plot[, ncol(data_plot)] - data_plot[, ncol(data_plot)-1]) * log((data_plot[, ncol(data_plot)] )/(data_plot[, ncol(data_plot)-1])))
  #   return(psi)
  # })
  
  # output$indicador_psi <- renderValueBox({
  #   aux <- dt_psi()
  #   valueBox(
  #     value = formatC(aux, digits = 0, format = "d", big.mark=","),
  #     subtitle = "Indicador PSI",
  #     icon = icon("check"),
  #     color = "teal"
  #   )
  # })
  
  # output$ts_afiliados <- renderPlotly({
  #   
  #   ts_plot <- data_f2() %>% 
  #     select(nuevo_ciiu, ENE2020, FEB2020, MAR2020, ABR2020) %>% 
  #     group_by(nuevo_ciiu) %>% 
  #     summarise(ENE2020 = sum(ENE2020, na.rm = T), 
  #               FEB2020 = sum(FEB2020, na.rm = T), 
  #               MAR2020 = sum(MAR2020, na.rm = T), 
  #               ABR2020 = sum(ABR2020, na.rm = T)) %>% 
  #     gather("Fecha","Afiliados", ENE2020:ABR2020) %>% 
  #     mutate(Fecha = ordered(Fecha,
  #                            levels = c("ENE2020", "FEB2020", "MAR2020", "ABR2020"),
  #                            labels = c("ENE2020", "FEB2020", "MAR2020", "ABR2020")))
  #   
  #   f1 <- list(family = "Arial, sans-serif", size = 14, color = "lightgrey")
  #   f2 <- list(family = "Old Standard TT, serif", size = 12, color = "lightgrey")
  #   
  #   p5 <- plot_ly(ts_plot) %>%
  #     add_trace(x = ~Fecha, y = ~Afiliados, color = ~nuevo_ciiu, type = 'scatter', 
  #               mode = 'lines+markers',line = list(width = 2), hoverinfo = "text", 
  #               text = ~paste(paste("Numero afiliados", Afiliados), nuevo_ciiu, sep = "<br />"), 
  #               colors = c("red","blue","green","orange")) %>%
  #     layout(
  #       xaxis = list(zeroline = TRUE, title = "", titlefont = f1),
  #       yaxis = list(range = c(min(ts_plot$Afiliados), max(ts_plot$Afiliados * 1.5)), 
  #                    side = 'left', rangemode = "tozero", overlaying = "y", 
  #                    title = 'Afiliados', showgrid = FALSE, 
  #                    zeroline = TRUE, showticklabels = TRUE, titlefont = f1),
  #       legend = list(x = 0.06, y = 0.98, titlefont = f2),
  #       paper_bgcolor='transparent',
  #       plot_bgcolor='transparent') %>%
  #     config(displayModeBar = F)
  #   p5  
  #   
  #   })
  
})
