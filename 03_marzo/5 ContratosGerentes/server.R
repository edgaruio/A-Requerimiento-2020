
shinyServer(function(input, output, session) {
  
  contratos_fil <- eventReactive(input$go,{
    contratos_fil <- contratos %>% 
      filter(if (!tolower(input$usuario) %in% c("ana.garzon@colsubsidio.com", "diego.bastidas@colsubsidio.com")) usuarios == tolower(input$usuario) else TRUE,
             if (!tolower(input$clave) %in% c("contratos1")) clave == tolower(input$clave) else TRUE)
    return(contratos_fil)
  })
  
  output$contratos_filtrado <- renderDataTable({
    datatable(contratos_fil(), rownames = FALSE, filter = "top",
              options = list(autoWidth = TRUE, dom="t", scrollX = TRUE, pageLength = nrow(contratos_fil()), scrollY = "400px"
              ))
  })
  
  observe({
    updateSelectizeInput(session, "xdependencia",
                         choices = toupper(c(unique(contratos_fil()$dependencia), "TODAS")),
                         selected = "TODAS")
    updateSelectizeInput(session, "xtipo_contrato",
                         choices = toupper(c(unique(contratos_fil()$tipo_contrato), "TODAS")),
                         selected = "TODAS")
    updateSelectizeInput(session, "xtipo_requerimiento",
                         choices = toupper(c(unique(contratos_fil()$tipo_requerimiento), "TODAS")),
                         selected = "TODAS")
  })
  
  credenciales_fil <- eventReactive(input$go,{
    credenciales_fil <- credenciales %>% 
      filter(if (!input$usuario %in% c("ana.garzon@colsubsidio.com", "diego.bastidas@colsubsidio.com")) usuarios == input$usuario else TRUE,
             if (!input$clave %in% c("contratos1")) clave == input$clave else TRUE)
    return(credenciales_fil)
  })
  
      
  data_clean <- reactive({
    df_clean <- contratos_fil() %>%
      filter(!is.na(abogado_1)) %>%
      mutate(valor_inicial_del_contrato_o_convenio = gsub(".","",valor_inicial_del_contrato_o_convenio,fixed = T),
             valor_estimado = gsub(".","",valor_estimado,fixed = T),
             valor_mixto_ingreso = gsub(".","",valor_mixto_ingreso,fixed = T),
             valor_mixto_gasto = gsub(".","",valor_mixto_gasto,fixed = T),
             fecha_solicitud = as.numeric(fecha_solicitud),
             fecha_final_respuesta = as.numeric(fecha_final_respuesta)) %>%
      mutate(fecha_solicitud_1 = as.numeric(fecha_solicitud_1),
             fecha_solicitud_2 = as.numeric(fecha_solicitud_2),
             fecha_solicitud_3 = as.numeric(fecha_solicitud_3),
             fecha_solicitud_4 = as.numeric(fecha_solicitud_4),
             fecha_solicitud_5 = as.numeric(fecha_solicitud_5),
             fecha_solicitud_6 = as.numeric(fecha_solicitud_6),
             fecha_solicitud_7 = as.numeric(fecha_solicitud_7)) %>%
      left_join(tb_tipo_contrato, by = c("tipo_de_contrato_o_convenio"="Llave")) %>%
      mutate(tipo_contrato = ifelse(is.na(tipo_contrato),"Sin informacion",tipo_contrato),
             valor_inicial_del_contrato_o_convenio = as.numeric(valor_inicial_del_contrato_o_convenio),
             valor_estimado = as.numeric(valor_estimado),
             valor_mixto_ingreso = as.numeric(valor_mixto_ingreso),
             valor_mixto_gasto = as.numeric(valor_mixto_gasto),
             fecha_solicitud = as.Date(fecha_solicitud, origin = "1900-01-01"),
             fecha_final_respuesta = as.Date(fecha_final_respuesta, origin = "1900-01-01"),
             fecha_solicitud_1 = as.Date(fecha_solicitud_1, origin = "1900-01-01"),
             fecha_solicitud_2 = as.Date(fecha_solicitud_2, origin = "1900-01-01"),
             fecha_solicitud_3 = as.Date(fecha_solicitud_3, origin = "1900-01-01"),
             fecha_solicitud_4 = as.Date(fecha_solicitud_4, origin = "1900-01-01"),
             fecha_solicitud_5 = as.Date(fecha_solicitud_5, origin = "1900-01-01"),
             fecha_solicitud_6 = as.Date(fecha_solicitud_6, origin = "1900-01-01"),
             fecha_solicitud_7 = as.Date(fecha_solicitud_7, origin = "1900-01-01"),
             numero_de_contrato_o_convenio = substr(numero_de_contrato_o_convenio, 1, 14)) %>%
      data.frame()
    return(df_clean)
  })

  data_fit <- reactive({
    aux_ultimo_estado_juridica <- data_clean() %>%
      filter(is.na(fecha_final_respuesta)) %>%
      mutate(lista_estado_1 = paste(estado_de_la_solicitud_1,lista_de_estados_de_la_solicitud_1, sep = "|"),
             lista_estado_2 = paste(estado_de_la_solicitud_1,lista_de_estados_de_la_solicitud_1, sep = "|"),
             lista_estado_3 = paste(estado_de_la_solicitud_1,lista_de_estados_de_la_solicitud_1, sep = "|"),
             lista_estado_4 = paste(estado_de_la_solicitud_1,lista_de_estados_de_la_solicitud_1, sep = "|"),
             lista_estado_5 = paste(estado_de_la_solicitud_1,lista_de_estados_de_la_solicitud_1, sep = "|"),
             lista_estado_6 = paste(estado_de_la_solicitud_1,lista_de_estados_de_la_solicitud_1, sep = "|"),
             lista_estado_7 = paste(estado_de_la_solicitud_1,lista_de_estados_de_la_solicitud_1, sep = "|")) %>%
      dplyr::select(numero_de_contrato_o_convenio,
                    lista_estado_1,lista_estado_2,lista_estado_3,lista_estado_4,lista_estado_5,lista_estado_6,lista_estado_7) %>%
      gather("estado", "lista", 2:8) %>%
      na.omit() %>%
      group_by(numero_de_contrato_o_convenio) %>%
      arrange(desc(estado)) %>%
      filter(row_number()==1) %>%
      ungroup() %>%
      filter_all(any_vars(str_detect(lista, pattern = "JURIDICA"))) %>%
      select(numero_de_contrato_o_convenio,lista) %>%
      rename(ultimo_lista_estado_jur=lista)

    aux_ultimo_estado_ues <- data_clean() %>%
      filter(is.na(fecha_final_respuesta)) %>%
      mutate(lista_estado_1 = paste(estado_de_la_solicitud_1,lista_de_estados_de_la_solicitud_1, sep = "|"),
             lista_estado_2 = paste(estado_de_la_solicitud_1,lista_de_estados_de_la_solicitud_1, sep = "|"),
             lista_estado_3 = paste(estado_de_la_solicitud_1,lista_de_estados_de_la_solicitud_1, sep = "|"),
             lista_estado_4 = paste(estado_de_la_solicitud_1,lista_de_estados_de_la_solicitud_1, sep = "|"),
             lista_estado_5 = paste(estado_de_la_solicitud_1,lista_de_estados_de_la_solicitud_1, sep = "|"),
             lista_estado_6 = paste(estado_de_la_solicitud_1,lista_de_estados_de_la_solicitud_1, sep = "|"),
             lista_estado_7 = paste(estado_de_la_solicitud_1,lista_de_estados_de_la_solicitud_1, sep = "|")) %>%
      dplyr::select(numero_de_contrato_o_convenio,
                    lista_estado_1,lista_estado_2,lista_estado_3,lista_estado_4,lista_estado_5,lista_estado_6,lista_estado_7) %>%
      gather("estado", "lista", 2:8) %>%
      na.omit() %>%
      group_by(numero_de_contrato_o_convenio) %>%
      arrange(desc(estado)) %>%
      filter(row_number()==1) %>%
      ungroup() %>%
      filter_all(any_vars(str_detect(lista, pattern = "UES"))) %>%
      select(numero_de_contrato_o_convenio,lista) %>%
      rename(ultimo_lista_estado_ues=lista)

    aux_dias_acumulados <- data_clean() %>%
      select(numero_de_contrato_o_convenio,fecha_solicitud,fecha_final_respuesta,estado_de_la_solicitud_1:fecha_solicitud_7) %>%
      mutate(dias_acu_juridica_10 = ifelse(estado_de_la_solicitud_1 == "EN TRAMITE JURIDICA",
                                           difftime(fecha_solicitud_1,fecha_solicitud,units = "days"), NA),
             dias_acu_juridica_21 = ifelse(estado_de_la_solicitud_2 == "EN TRAMITE JURIDICA",
                                           difftime(fecha_solicitud_2,fecha_solicitud_1,units = "days"), NA),
             dias_acu_juridica_32 = ifelse(estado_de_la_solicitud_3 == "EN TRAMITE JURIDICA",
                                           difftime(fecha_solicitud_3,fecha_solicitud_2,units = "days"), NA),
             dias_acu_juridica_43 = ifelse(estado_de_la_solicitud_4 == "EN TRAMITE JURIDICA",
                                           difftime(fecha_solicitud_4,fecha_solicitud_3,units = "days"), NA),
             dias_acu_juridica_54 = ifelse(estado_de_la_solicitud_5 == "EN TRAMITE JURIDICA",
                                           difftime(fecha_solicitud_5,fecha_solicitud_4,units = "days"), NA),
             dias_acu_juridica_65 = ifelse(estado_de_la_solicitud_6 == "EN TRAMITE JURIDICA",
                                           difftime(fecha_solicitud_6,fecha_solicitud_5,units = "days"), NA),
             dias_acu_juridica_76 = ifelse(estado_de_la_solicitud_7 == "EN TRAMITE JURIDICA",
                                           difftime(fecha_solicitud_7,fecha_solicitud_6,units = "days"), NA)) %>%
      mutate(acumulado_juridica = rowSums(.[,c('dias_acu_juridica_10','dias_acu_juridica_21','dias_acu_juridica_32',
                                               'dias_acu_juridica_43','dias_acu_juridica_54','dias_acu_juridica_65',
                                               'dias_acu_juridica_76')],
                                          na.rm = T)) %>%
      mutate(dias_acu_ues_10 = ifelse(estado_de_la_solicitud_1 == "EN TRAMITE UES",
                                      difftime(fecha_solicitud_1,fecha_solicitud,units = "days"), NA),
             dias_acu_ues_21 = ifelse(estado_de_la_solicitud_2 == "EN TRAMITE UES",
                                      difftime(fecha_solicitud_2,fecha_solicitud_1,units = "days"), NA),
             dias_acu_ues_32 = ifelse(estado_de_la_solicitud_3 == "EN TRAMITE UES",
                                      difftime(fecha_solicitud_3,fecha_solicitud_2,units = "days"), NA),
             dias_acu_ues_43 = ifelse(estado_de_la_solicitud_4 == "EN TRAMITE UES",
                                      difftime(fecha_solicitud_4,fecha_solicitud_3,units = "days"), NA),
             dias_acu_ues_54 = ifelse(estado_de_la_solicitud_5 == "EN TRAMITE UES",
                                      difftime(fecha_solicitud_5,fecha_solicitud_4,units = "days"), NA),
             dias_acu_ues_65 = ifelse(estado_de_la_solicitud_6 == "EN TRAMITE UES",
                                      difftime(fecha_solicitud_6,fecha_solicitud_5,units = "days"), NA),
             dias_acu_ues_76 = ifelse(estado_de_la_solicitud_7 == "EN TRAMITE UES",
                                      difftime(fecha_solicitud_7,fecha_solicitud_6,units = "days"), NA)) %>%
      mutate(acumulado_ues = rowSums(.[,c('dias_acu_ues_10','dias_acu_ues_21','dias_acu_ues_32',
                                          'dias_acu_ues_43','dias_acu_ues_54','dias_acu_ues_65',
                                          'dias_acu_ues_76')],
                                     na.rm = T)) %>%
      select(numero_de_contrato_o_convenio,acumulado_juridica,acumulado_ues) %>%
      left_join(aux_ultimo_estado_juridica, by = "numero_de_contrato_o_convenio") %>%
      left_join(aux_ultimo_estado_ues, by = "numero_de_contrato_o_convenio") %>%
      data.frame()
  })
  
  
  ### Data filtrada
  data_f <- reactive({
    df <- data_clean() %>%
      dplyr::filter(if (!(input$xdependencia %in% "TODAS")) dependencia %in% input$xdependencia else TRUE,
                    if (!(input$xtipo_contrato %in% "TODAS")) tipo_de_contrato_o_convenio %in% input$xtipo_contrato else TRUE,
                    if (!(input$xtipo_requerimiento %in% "TODAS")) tipo_requerimiento %in% input$xtipo_requerimiento else TRUE)
    return(df)
  })
  
  output$info_gerencia <- renderValueBox({
    if (nrow(data_f()) == 0) {
      aux <- "No hay solicitudes"
    } else {
      aux <- credenciales_fil()$gerencia
    }
    valueBox(
      value = formatC(aux,format="s"),
      subtitle = "DEPENDENCIA",
      icon = icon("home"),
      color = "teal")
  })
  
  output$num_informes <- renderValueBox({
    if (nrow(data_f()) == 0) {
      aux <- "No hay solicitudes"
    } else {
      aux <- nrow(data_f())
    }
    valueBox(
      value = formatC(aux, digits = 0, format = "d", big.mark=","),
      subtitle = "Contratos 2020",
      color = "teal",
      icon = icon("child"))
  })
  
  output$valor_total_estimado <- renderValueBox({
    if (nrow(data_f()) == 0) {
      aux <- "No hay solicitudes"
    } else {
      aux <- paste("$ ", comma(sum(data_f()$valor_estimado, na.rm = TRUE)), sep = "")
    }
    valueBox(
      value = aux,
      subtitle = "Valor Estimado",
      icon = icon("dollar"),
      color = "teal")
  })
  
  output$valor_ingreso <- renderValueBox({
    if (nrow(data_f()) == 0) {
      aux <- "No hay solicitudes"
    } else { 
      aux <- data_f() %>% 
        dplyr::select(contabilizacion,valor_estimado,valor_mixto_ingreso,valor_mixto_gasto) %>% 
        mutate(valor_total_gasto = ifelse(contabilizacion == "GASTO", valor_estimado, 
                                          ifelse(contabilizacion == "MIXTO", valor_mixto_gasto, NA)),
               valor_total_ingreso = ifelse(contabilizacion == "INGRESO", valor_estimado, 
                                            ifelse(contabilizacion == "MIXTO", valor_mixto_ingreso, NA)))
      aux <- comma(sum(aux$valor_total_ingreso, na.rm = T))
    }
    valueBox(
      value = aux,
      subtitle = "Ingreso",
      icon = icon("dollar"),
      color = "teal"
    )
  })
  
  output$valor_gasto <- renderValueBox({
    if (nrow(data_f()) == 0) {
      aux <- "No hay solicitudes"
    } else {
      aux <- data_f() %>% 
        dplyr::select(contabilizacion,valor_estimado,valor_mixto_ingreso,valor_mixto_gasto) %>% 
        mutate(valor_total_gasto = ifelse(contabilizacion == "GASTO", valor_estimado, 
                                          ifelse(contabilizacion == "MIXTO", valor_mixto_gasto, NA)),
               valor_total_ingreso = ifelse(contabilizacion == "INGRESO", valor_estimado, 
                                            ifelse(contabilizacion == "MIXTO", valor_mixto_ingreso, NA)))
      aux <- comma(sum(aux$valor_total_gasto, na.rm = T))
    }
    valueBox(
      value = aux,
      subtitle = "Gastos",
      icon = icon("dollar"),
      color = "teal"
    )
  })
  
  output$estado_finalizado <- renderValueBox({
    
    if (nrow(data_f()) == 0) {
      aux <- "No hay solicitudes"
    } else {
      aux <- data_f() %>%
        filter(!is.na(fecha_final_respuesta))
      aux <- comma(dim(aux)[1])
    }
    valueBox(
      value = aux,
      subtitle = "CONTRATOS FINALIZADOS",
      icon = icon("check"),
      color = "teal"
    )
  })
  
  output$estado_juridica <- renderValueBox({
    if (nrow(data_f()) == 0) {
      aux <- "No hay solicitudes"
    } else {
      aux <- data_f() %>%
        filter(is.na(fecha_final_respuesta)) %>% 
        dplyr::select(numero_de_contrato_o_convenio,
                      estado_de_la_solicitud_1,estado_de_la_solicitud_2,estado_de_la_solicitud_3,estado_de_la_solicitud_4,
                      estado_de_la_solicitud_5,estado_de_la_solicitud_6,estado_de_la_solicitud_7) %>%
        gather("estado", "lista", 2:8) %>% 
        na.omit() %>% 
        group_by(numero_de_contrato_o_convenio) %>% 
        arrange(desc(estado)) %>% 
        filter(row_number()==1) %>% 
        ungroup() %>% 
        filter(lista == "EN TRAMITE JURIDICA")
      aux <- comma(dim(aux)[1])
    }
    valueBox(
      value = aux,
      subtitle = "CONTRATOS JURÍDICA",
      icon = icon("check"),
      color = "teal"
    )
  })
  
  output$estado_ues <- renderValueBox({
    
    if (nrow(data_f()) == 0) {
      aux <- "No hay solicitudes"
    } else {
      aux <- data_f() %>%
        filter(is.na(fecha_final_respuesta)) %>% 
        dplyr::select(numero_de_contrato_o_convenio,
                      estado_de_la_solicitud_1,estado_de_la_solicitud_2,estado_de_la_solicitud_3,estado_de_la_solicitud_4,
                      estado_de_la_solicitud_5,estado_de_la_solicitud_6,estado_de_la_solicitud_7) %>%
        gather("estado", "lista", 2:8) %>% 
        na.omit() %>% 
        group_by(numero_de_contrato_o_convenio) %>% 
        arrange(desc(estado)) %>% 
        filter(row_number()==1) %>% 
        ungroup() %>% 
        filter(lista == "EN TRAMITE UES")
      aux <- comma(dim(aux)[1])
    }
    valueBox(
      value = aux,
      subtitle = "Contratos UES",
      icon = icon("check"),
      color = "teal"
    )
  })
  
  # Por dependencias
  output$plot1 <- renderPlotly({
    aux <- data_f() %>%
      group_by(dependencia) %>%
      summarise(conteo = n(),
                plata = sum(valor_estimado, na.rm = T)) %>%
      arrange(desc(conteo))
    aux %>%
      plot_ly(y = ~reorder(dependencia,conteo), x = ~conteo, type = 'bar', orientation = 'h',
              hoverinfo = 'text', text = ~paste(conteo, "($ ", comma(plata), ")"),textposition = 'inside', color = ~dependencia, colors = "Spectral") %>%
      layout(title = "Solicitudes por Dependencia",
             xaxis = list(title=""),
             yaxis = list(title = "", zeroline = T, showline = T, showticklabels = F, showgrid = T),
             legend = list(x = 100, y = 0.5))
  })
  
  # Por tipo requerimiento
  output$plot2 <- renderPlotly({
    aux <- data_f() %>%
      group_by(tipo_requerimiento) %>%
      summarise(conteo = n(),
                plata = sum(valor_estimado, na.rm = T)) %>%
      arrange(desc(conteo))
    plot_ly(aux, labels = ~tipo_requerimiento, values = ~conteo, type = 'pie', hole = 0, alpha = 0.9, textinfo = 'percent',
            text = ~paste(conteo, "($ ", comma(plata), ")"), textposition = 'inside') %>%
      layout(title = "Solicitudes por Tipo de Requerimiento",
             xaxis = list(title=""),
             yaxis = list(title = "", zeroline = T, showline = T, showticklabels = F, showgrid = T),
             legend = list(x = 100, y = 0.5))
  })
})



