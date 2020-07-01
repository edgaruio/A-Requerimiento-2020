
shinyServer(function(input, output, session) {
  
 
  # ====== primera fila ----------------------

  output$info_empresa <- renderValueBox({
    bd_empresas<-bd_empresas
    valueBox(
      value = formatC(bd_empresas$razonsocial[1],format="s"),
      subtitle = "Empresa",
      icon = icon("users"),
      color = "blue"
    )
  })

  output$info_cluster <- renderValueBox({
    bd_empresas<-bd_empresas
    valueBox(
      value = formatC(bd_empresas$cluster[1],format="s"),
      subtitle = "Cluster",
      icon = icon("users"),
      color = "blue"
    )
  })

  output$info_pir1 <- renderValueBox({
    bd_empresas<-bd_empresas
    valueBox(
      value = formatC(bd_empresas$piramide1[1],format="s"),
      subtitle = "Piramide 1",
      icon = icon("home"),
      color = "blue"
    )
  })

  output$info_pir2 <- renderValueBox({
    bd_empresas<-bd_empresas
    valueBox(
      value = formatC(bd_empresas$piramide2[1],format="s"),
      subtitle = "Piramide 2",
      icon = icon("home"),
      color = "blue"
    )
  })
  
  output$pro_salario <- renderValueBox({
    valueBox(
      value = paste("$ ",formatC(bd_empresas$pro_salario,digits = 0, format = "d", big.mark=","), sep = " "),
      subtitle = "Promedio Salario",
      icon = icon("credit-card"),
      color = "blue"
    )
  })
  
  output$pro_edad <- renderValueBox({
    valueBox(
      value = formatC(bd_empresas$pro_edad,digits = 0, format = "d", big.mark=","),
      subtitle = "Promedio Edad",
      icon = icon("user"),
      color = "blue"
    )
  })
  
  output$conteo_empleados <- renderValueBox({
    bd_empresas<-bd_empresas %>%
      select(id_empresa,afiliados) %>%
      group_by(id_empresa) %>%
      summarise(afiliados = sum(afiliados))
    valueBox(
      value = formatC(bd_empresas$afiliados,digits = 0, format = "d", big.mark=","),
      subtitle = "Total Empleados",
      icon = icon("child"),
      color = "blue"
    )
  })

  output$plot_pira_ind <- renderPlotly({
    aux1 <- consulta_piramide %>%
      filter(!is.na(Edad)) %>%
      mutate(edad_agru =  cut(Edad, breaks = c(0,10,20,30,40,50,60,80,90,100,110,120,130))) %>%
      group_by(edad_agru, Genero) %>%
      summarise(clientes=n_distinct(id_persona)) %>%
      mutate(con_clientes = ifelse(test = Genero == "M",yes = -clientes, no = clientes)) %>%
      filter(!is.na(edad_agru))

    m <- list(l = 0,r = 0,b = 100,t = 100, pad = 0)
    f1 <- list(family = "Arial, sans-serif",size = 18,color = "lightgrey")
    f2 <- list(family = "Old Standard TT, serif",size = 14,color = "lightgrey")

    aux1 %>%
      plot_ly(x= ~con_clientes, y=~edad_agru,color=~Genero) %>%
      add_bars(orientation = 'h', hoverinfo = 'text', text = ~clientes, textposition = "outside", textfont = list(color = 'darkgrey', size = 10)) %>%
      layout(margin = m,
             bargap = 0.1,
             barmode = 'overlay',
             title = 'Piramide Poblacional',
             xaxis = list(title='Afiliados', titlefont = f1, tickfont = f2,  zeroline = FALSE, showline = FALSE, showgrid = FALSE, showticklabels = FALSE,
                          range = c(-round(max(abs(aux1$con_clientes))*1.3, digits = 0), round(max(abs(aux1$con_clientes))*1.3, digits = 0))),
             yaxis = list(title='Edad', titlefont = f1, tickfont = f2),
             font = list(color = 'lightgrey'),
             paper_bgcolor='transparent',
             plot_bgcolor='transparent') %>%
      config(displayModeBar = F)

  })

  output$plot1 <- renderPlotly({
    data_plot <- bd_empresas %>%
      dplyr::select(id_empresa,seg_alto:seg_medio) %>%
      group_by(id_empresa) %>%
      summarise(Basico = sum(seg_basico),
                Medio = sum(seg_medio),
                Joven = sum(seg_joven),
                Alto = sum(seg_alto)) %>%
      gather(key = "Segmento", value = "Conteo", 2:5)

    m <- list(l = 0,r = 0,b = 100,t = 100, pad = 0)
    colors <- c('rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)')
    f1 <- list(family = "Arial, sans-serif", size = 18, color = "lightgrey")
    f2 <- list(family = "Old Standard TT, serif", size = 14, color = "lightgrey")

    p1 <- plot_ly(data_plot, labels = ~Segmento, values = ~Conteo, type = 'pie',hole = 0,
                  textinfo = 'label+percent',
                  insidetextfont = list(color = '#FFFFFF'),
                  hoverinfo = 'text',text = ~paste('Total empleados:', Conteo),showlegend = T,
                  marker = list(colors = colors,
                                line = list(color = '#FFFFFF', width = 2))) %>%
      layout(margin = m,
             title = 'Participación por Segmento',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, titlefont = f1, tickfont = f2),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, titlefont = f1, tickfont = f2),
             legend = list(x = 0.8, y = 0.5),
             font = list(color = 'lightgrey'),
             paper_bgcolor='transparent',
             plot_bgcolor='transparent') %>%
      config(displayModeBar = F)
    p1
  })

  output$plot2 <- renderPlotly({
    data_plot <- bd_empresas
    data_plot <- data_plot %>%
      dplyr::select(id_empresa,cat_a:cat_c) %>%
      group_by(id_empresa) %>%
      summarise(A = sum(cat_a),
                B = sum(cat_b),
                C = sum(cat_c)) %>%
      gather(key = "Categoria", value = "Conteo", 2:4)

    m <- list(l = 0,r = 0,b = 100,t = 100, pad = 0)
    colors <- c('rgb(333,133,133)', 'rgb(111,103,167)', 'rgb(222,104,87)', 'rgb(114,147,203)')
    f1 <- list(family = "Arial, sans-serif", size = 18, color = "lightgrey")
    f2 <- list(family = "Old Standard TT, serif", size = 14, color = "lightgrey")

    p1 <- plot_ly(data_plot, labels =~ Categoria, values = ~Conteo, type = 'pie',hole = 0,
                  textinfo = 'label+percent',
                  insidetextfont = list(color = '#FFFFFF'),
                  hoverinfo = 'text',text = ~paste('Total empleados:', Conteo),showlegend = T,
                  marker = list(colors = colors,
                                line = list(color = '#FFFFFF', width = 2))) %>%
      layout(margin = m,
             title = 'Participación por Categoria',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, titlefont = f1, tickfont = f2),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, titlefont = f1, tickfont = f2),
             legend = list(x = 0.8, y = 0.5),
             font = list(color = 'lightgrey'),
             paper_bgcolor='transparent',
             plot_bgcolor='transparent') %>%
      config(displayModeBar = F)
    p1
  })

  # ====== Segunda fila ----------------------

  output$conteo_famisanar <- renderValueBox({
    valueBox(
      value = paste(formatC(100*bd_empresas$num_famisanar/bd_empresas$afiliados,
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(bd_empresas$num_famisanar),")",sep = ""),
      subtitle = "Afiliados con Famisanar",
      icon = icon("briefcase"),
      color = "purple",
      width = 4
    )
  })
  
  output$conteo_pac_famisanar <- renderValueBox({
    valueBox(
      value = paste(formatC(100*bd_empresas$num_pac_famisanar/bd_empresas$num_famisanar,
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(bd_empresas$num_pac_famisanar),")",sep = ""),
      subtitle = "Famisanar - PAC",
      icon = icon("briefcase"),
      color = "purple",
      width = 4
    )
  })
  
  output$conteo_sura_eps <- renderValueBox({
    valueBox(
      value = paste(formatC(100*bd_empresas$num_suramericana/bd_empresas$afiliados,
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(bd_empresas$num_suramericana),")",sep = ""),
      subtitle = "Afiliados con Suramericana",
      icon = icon("briefcase"),
      color = "purple"
    )
  })

  output$conteo_pac_sura <- renderValueBox({
    valueBox(
      value = paste(formatC(100*bd_empresas$num_pac_suramericana/bd_empresas$num_suramericana,
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(bd_empresas$num_pac_suramericana),")",sep = ""),
      subtitle = "Suramericana - PAC",
      icon = icon("briefcase"),
      color = "purple"
    )
  })
  
  output$conteo_ips <- renderValueBox({
    valueBox(
      value = paste(formatC(100*bd_empresas$num_ips_colsubsidio/bd_empresas$num_famisanar,
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(bd_empresas$num_ips_colsubsidio),")",sep = ""),
      subtitle = "IPS Colsubsidio",
      icon = icon("briefcase"),
      color = "purple",
      width = 4
    )
  })
  
  output$conteo_data <- renderValueBox({
    valueBox(
      value = paste(formatC(100*bd_empresas$habeas_data/bd_empresas$afiliados,
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(bd_empresas$habeas_data),")",sep = ""),
      subtitle = "Afiliados con Habeas Data",
      icon = icon("database"),
      color = "purple"
    )
  })
  
  output$conteo_consumo_credito <- renderValueBox({
    valueBox(
      value = paste(formatC(100*bd_empresas$consumo_credito/bd_empresas$afiliados,
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(bd_empresas$consumo_credito),")",sep = ""),
      subtitle = "Afiliados con Crédito Consumo",
      icon = icon("credit-card"),
      color = "teal"
    )
  })

  output$conteo_cupo_credito <- renderValueBox({
    valueBox(
      value = paste(formatC(100*bd_empresas$cupo_credito/bd_empresas$afiliados,
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(bd_empresas$cupo_credito),")",sep = ""),
      subtitle = "Afiliados con Cupo Crédito",
      icon = icon("credit-card"),
      color = "purple"
    )
  })

  output$conteo_uso_mes <- renderValueBox({
    valueBox(
      value = paste(formatC(100*bd_empresas$uso_mes/bd_empresas$afiliados,
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(bd_empresas$uso_mes),")",sep = ""),
      subtitle = "Afiliados con Uso TMS",
      icon = icon("ok", lib = "glyphicon"),
      color = "teal"
    )
  })

  # Salud
  output$conteo_salud <- renderValueBox({
    valueBox(
      value = paste(formatC(100*bd_empresas$salud/bd_empresas$afiliados,
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(bd_empresas$salud),")",sep = ""),
      subtitle = "Afiliados con consumo en Salud",
      icon = icon("briefcase"),
      color = "teal"
    )
  })
  # Supermecados
  output$conteo_supermercado <- renderValueBox({
    valueBox(
      value = paste(formatC(100*bd_empresas$supermercados/bd_empresas$afiliados,
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(bd_empresas$supermercados),")",sep = ""),
      subtitle = "Afiliados con consumo en Supermercado",
      icon = icon("cart-plus"),
      color = "teal"
    )
  })

  # Medicamentos
  output$conteo_drogueria <- renderValueBox({
    valueBox(
      value = paste(formatC(100*bd_empresas$medicamentos/bd_empresas$afiliados,
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(bd_empresas$medicamentos),")",sep = ""),
      subtitle = "Afiliados con Consumo en Droguerias",
      icon = icon("plus"),
      color = "teal"
    )
  })
  
  output$conteo_vivienda <- renderValueBox({
    valueBox(
      value = formatC(bd_empresas$compra_vivienda, digits = 0, format = "f", big.mark=","),
      subtitle = "Afiliados con compra de Vivienda",
      icon = icon("home"),
      color = "teal"
    )
  })
  
  output$conteo_educacion <- renderValueBox({
    valueBox(
      value = formatC(bd_empresas$educacion,digits = 0, format = "f", big.mark=","),
      subtitle = "Educación (Niños en colegios)",
      icon = icon("graduation-cap"),
      color = "teal"
    )
  })

  # RyT
  output$conteo_ryt_club <- renderValueBox({
    valueBox(
      value = paste(formatC(100*bd_empresas$club/bd_empresas$afiliados,
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(bd_empresas$club),")",sep = ""),
      subtitle = "Afiliados con consumo en Clubes",
      icon = icon("rocket"),
      color = "teal"
    )
  })

  output$conteo_ryt_hoteles <- renderValueBox({
    valueBox(
      value = paste(formatC(100*bd_empresas$hotel/bd_empresas$afiliados,
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(bd_empresas$hotel),")",sep = ""),
      subtitle = "Afiliados con consumo en Hoteles",
      icon = icon("rocket"),
      color = "teal"
    )
  })

  output$conteo_ryt_piscilago <- renderValueBox({
    valueBox(
      value = paste(formatC(100*bd_empresas$piscilago/bd_empresas$afiliados,
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(bd_empresas$piscilago),")",sep = ""),
      subtitle = "Afiliados con consumo en Piscilago",
      icon = icon("rocket"),
      color = "teal"
    )
  })

  output$conteo_ryt <- renderValueBox({
    valueBox(
      value = paste(formatC(100*bd_empresas$ryt/bd_empresas$afiliados,
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(bd_empresas$ryt),")",sep = ""),
      subtitle = "Afiliados que han usado Recreacion y Turismo",
      icon = icon("rocket"),
      color = "teal"
    )
  })

  # ====== tercera fila ----------------------

  output$conteo_kit <- renderValueBox({
    valueBox(
      value = paste(formatC(100*bd_empresas$kit_redimido/bd_empresas$kit_derecho,
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(bd_empresas$kit_redimido),"/",comma(bd_empresas$kit_derecho),")",sep = ""),
      subtitle = "Porcentaje de Redención Kit Escolar",
      icon = icon("book"),
      color = "green"
    )
  })

  output$conteo_monetarias <- renderValueBox({
    valueBox(
      value = paste(formatC(100*bd_empresas$cuota_redimida/bd_empresas$cuota_derecho,
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(bd_empresas$cuota_redimida),"/",comma(bd_empresas$cuota_derecho),")",sep = ""),
      subtitle = "Porcentaje de Redención Cuota Monetaria",
      icon = icon("play"),
      color = "green"
    )
  })
  
  output$conteo_lonchera <- renderValueBox({
    valueBox(
      value = paste(formatC(100*bd_empresas$bono_redimido/bd_empresas$bono_derecho,
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(bd_empresas$bono_redimido),"/",comma(bd_empresas$bono_derecho),")",sep = ""),
      subtitle = "Porcentaje de Redención Bono Lonchera",
      icon = icon("star"),
      color = "green"
    )
  })
  
  # output$conteo_subsidio <- renderValueBox({
  #   bd_empresas<-bd_empresas %>%
  #     select(id_empresa,Auxilios_pago,Afiliados) %>%
  #     group_by(id_empresa) %>%
  #     summarise(Auxilios_pago = sum(Auxilios_pago),
  #               Afiliados = sum(Afiliados))
  #   valueBox(
  #     value = paste(formatC(100*bd_empresas$Auxilios_pago/bd_empresas$Afiliados,
  #                           digits = 1, format = "f", big.mark=","),"%", " (",bd_empresas$Auxilios_pago,")",sep = ""),
  #     subtitle = "Otros auxilios entregados",
  #     icon = icon("telegram"),
  #     color = "purple"
  #   )
  # })

  output$conteo_subsidio_vivienda <- renderValueBox({
    valueBox(
      value = paste(formatC(100*bd_empresas$subsidio_asignado/bd_empresas$afiliados,
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(bd_empresas$subsidio_asignado),")",sep = ""),
      subtitle = "Afiliados con Subsidio Vivienda",
      icon = icon("hotel"),
      color = "teal"
    )
  })

  # ====== cuarta fila ----------------------

  output$conteo_hoteles <- renderValueBox({
    valueBox(
      value = paste(formatC(100*bd_empresas$pros_hotel/bd_empresas$afiliados,
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(bd_empresas$pros_hotel),")",sep = ""),
      subtitle = "Afiliados Prospectos Hoteles",
      icon = icon("bicycle"),
      color = "purple"
    )
  })

  output$conteo_piscilago <- renderValueBox({
    valueBox(
      value = paste(formatC(100*bd_empresas$pros_pisi/bd_empresas$afiliados,
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(bd_empresas$pros_pisi),")",sep = ""),
      subtitle = "Afiliados Prospectos Piscilago",
      icon = icon("bicycle"),
      color = "purple"
    )
  })

  output$conteo_club <- renderValueBox({
    valueBox(
      value = paste(formatC(100*bd_empresas$pros_club/bd_empresas$afiliados,
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(bd_empresas$pros_club),")",sep = ""),
      subtitle = "Afiliados Prospectos Clubes",
      icon = icon("bicycle"),
      color = "purple"
    )
  })

  output$pre_aprobado_hipotecario <- renderValueBox({
    valueBox(
      value = paste(formatC(100*bd_empresas$pre_aprobado_hipo/bd_empresas$afiliados,
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(bd_empresas$pre_aprobado_hipo),")",sep = ""),
      subtitle = "Afiliados Pre Aprobado Hipotecario",
      icon = icon("eraser"),
      color = "olive"
    )
  })

  output$pre_aprobado_cupo <- renderValueBox({
    valueBox(
      value = paste(formatC(100*bd_empresas$pre_aprobado_cupo/bd_empresas$afiliados,
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(bd_empresas$pre_aprobado_cupo),")",sep = ""),
      subtitle = "Pre Aprobado Cupo",
      icon = icon("eraser"),
      color = "olive"
    )
  })

  output$conteo_cuad_a <- renderValueBox({
    valueBox(
      value = paste(formatC(100*bd_empresas$a3/bd_empresas$afiliados,
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(bd_empresas$a3),")",sep = ""),
      subtitle = "Afiliados Cuadrante A",
      icon = icon("home"),
      color = "blue"
    )
  })

  output$conteo_cuad_a1 <- renderValueBox({
    valueBox(
      value = paste(formatC(100*bd_empresas$a1/bd_empresas$afiliados,
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(bd_empresas$a1),")",sep = ""),
      subtitle = "Afiliados Cuadrante A1",
      icon = icon("home"),
      color = "blue"
    )
  })

  output$conteo_cuad_a2 <- renderValueBox({
    valueBox(
      value = paste(formatC(100*bd_empresas$a2/bd_empresas$afiliados,
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(bd_empresas$a2),")",sep = ""),
      subtitle = "Afiliados Cuadrante A2",
      icon = icon("home"),
      color = "blue"
    )
  })

  output$conteo_cuad_b <- renderValueBox({
    valueBox(
      value = paste(formatC(100*bd_empresas$b1/bd_empresas$afiliados,
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(bd_empresas$b1),")",sep = ""),
      subtitle = "Afiliados Cuadrante B",
      icon = icon("home"),
      color = "blue"
    )
  })
  
  #  Credito Social       Educacion Mercadeo social             RyT           Salud 
  
  output$conteo_ues1 <- renderValueBox({

    if (nrow(consumo_emp) == 0) {
      data_f1 <- data.frame(consumo = 0)
    } else {
      data_f1 <- consumo_emp %>%
        dplyr::filter(ues == "Credito social") %>%
        group_by(ues) %>% 
        summarise(consumo = round(sum(consumo, na.rm = T),1))
    }

    valueBox(
      value = paste("$ ",formatC(sum(data_f1$consumo, na.rm = T), digits = 0, format = "d", big.mark=","), sep = " "),
      subtitle = "Crédito social",
      icon = icon("dollar"),
      color = "teal"
    )
  })
  
  output$conteo_ues2 <- renderValueBox({
    
    if (nrow(consumo_emp) == 0) {
      data_f1 <- data.frame(consumo = 0)
    } else {
      data_f1 <- consumo_emp %>%
        dplyr::filter(ues == "RyT") %>%
        group_by(ues) %>% 
        summarise(consumo = round(sum(consumo, na.rm = T),1))
    }
    
    valueBox(
      value = paste("$ ",formatC(sum(data_f1$consumo, na.rm = T), digits = 0, format = "d", big.mark=","), sep = " "),
      subtitle = "Recreación y Turísmo",
      icon = icon("dollar"),
      color = "teal"
    )
  })
  
  output$conteo_ues3 <- renderValueBox({
    
    if (nrow(consumo_emp) == 0) {
      data_f1 <- data.frame(consumo = 0)
    } else {
      data_f1 <- consumo_emp %>%
        dplyr::filter(ues == "Mercadeo social") %>%
        group_by(ues) %>% 
        summarise(consumo = round(sum(consumo, na.rm = T),1))
    }
    
    valueBox(
      value = paste("$ ",formatC(sum(data_f1$consumo, na.rm = T), digits = 0, format = "d", big.mark=","), sep = " "),
      subtitle = "Mercadeo Social",
      icon = icon("dollar"),
      color = "teal"
    )
  })
  
  output$conteo_ues4 <- renderValueBox({
    
    if (nrow(consumo_emp) == 0) {
      data_f1 <- data.frame(consumo = 0)
    } else {
      data_f1 <- consumo_emp %>%
        dplyr::filter(ues == "Educacion") %>%
        group_by(ues) %>% 
        summarise(consumo = round(sum(consumo, na.rm = T),1))
    }
    
    valueBox(
      value = paste("$ ",formatC(sum(data_f1$consumo, na.rm = T), digits = 0, format = "d", big.mark=","), sep = " "),
      subtitle = "Educación",
      icon = icon("dollar"),
      color = "teal"
    )
  })
  
  output$conteo_ues5 <- renderValueBox({
    
    if (nrow(consumo_emp) == 0) {
      data_f1 <- data.frame(consumo = 0)
    } else {
      data_f1 <- consumo_emp %>%
        dplyr::filter(ues == "Salud") %>%
        group_by(ues) %>% 
        summarise(consumo = round(sum(consumo, na.rm = T),1))
    }
    
    valueBox(
      value = paste("$ ",formatC(sum(data_f1$consumo, na.rm = T), digits = 0, format = "d", big.mark=","), sep = " "),
      subtitle = "Salud",
      icon = icon("dollar"),
      color = "teal"
    )
  })
  
  # output$consumo_valor_ues <- renderPlotly({
  #   data_plot <- bd_empresas %>%
  #     left_join(consumo_emp, by = "id_empresa") %>%
  #     group_by(ues) %>%
  #     summarise(consumo = round(sum(consumo, na.rm = T)/1000000,1)) %>%
  #     data.frame()
  # 
  #   m <- list(l = 50,r = 50,b = 100,t = 100, pad = 0)
  #   colors <- c('rgb(333,133,133)', 'rgb(111,103,167)', 'rgb(222,104,87)', 'rgb(114,147,203)')
  #   f1 <- list(family = "Arial, sans-serif",size = 18,color = "lightgrey")
  #   f2 <- list(family = "Old Standard TT, serif",size = 14,color = "lightgrey")
  # 
  #   p1 <- plot_ly(data_plot, x = ~ues, y = ~consumo, type = 'bar', text = ~comma(consumo), textposition = 'auto') %>%
  #     layout(margin = m,
  #            title = 'Consumo por UES (Millones)',
  #            font = list(color = 'lightgrey'),
  #            xaxis = list(showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE, titlefont = f1, tickfont = f2, tickcolor = 'rgb(127,127,127)'),
  #            yaxis = list(showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE, titlefont = f1, tickfont = f2, tickcolor = 'rgb(127,127,127)'),
  #            legend = list(x = 0.8, y = 0.5),
  #            paper_bgcolor='transparent',
  #            plot_bgcolor='transparent') %>%
  #     config(displayModeBar = F)
  #   p1
  # })
  
  # output$consumo_transa_servicio <- renderPlotly({
  #   data_plot <- bd_empresas %>%
  #     left_join(consumo_emp, by = "id_empresa") %>%
  #     group_by(servicio) %>%
  #     summarise(consumo = round(sum(consumo, na.rm = T)/1000000,1)) %>%
  #     data.frame()
  # 
  #   m <- list(l = 50,r = 50,b = 100,t = 100, pad = 0)
  #   colors <- c('rgb(333,133,133)', 'rgb(111,103,167)', 'rgb(222,104,87)', 'rgb(114,147,203)')
  #   f1 <- list(family = "Arial, sans-serif",size = 18,color = "lightgrey")
  #   f2 <- list(family = "Old Standard TT, serif",size = 14,color = "lightgrey")
  # 
  #   p1 <- plot_ly(data_plot, x = ~servicio, y = ~consumo, type = 'bar', text = ~comma(consumo), textposition = 'auto') %>%
  #     layout(margin = m,
  #            title = 'Consumo por Servicio (Millones)',
  #            font = list(color = 'lightgrey'),
  #            xaxis = list(showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE, titlefont = f1, tickfont = f2, tickcolor = 'rgb(127,127,127)'),
  #            yaxis = list(showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE, titlefont = f1, tickfont = f2, tickcolor = 'rgb(127,127,127)'),
  #            legend = list(x = 0.8, y = 0.5),
  #            paper_bgcolor='transparent',
  #            plot_bgcolor='transparent') %>%
  #     config(displayModeBar = F)
  #   p1
  # })

})



