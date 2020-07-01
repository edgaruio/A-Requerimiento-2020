shinyServer(function(input, output, session) {
  options(scipen = 999)
  
  ### Manipulacion datos ====
  
  data_read <- reactive({
    req(input$file1)
    df <- read_excel(input$file1$datapath, na = c("", "NA", "#/NA", "na"), col_names = TRUE, sheet = 1) %>% 
      data.frame()
    names(df) <- tolower(gsub(".","_",names(df),fixed = T))
    names(df) <- chartr("áéíóú","aeiou",names(df))
    return(df)
  })
  
  data_clean <- reactive({
    df_clean <- data_read() %>%
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
  
  observe({
    updateSelectizeInput(session, "xdependencia",
                         choices = toupper(c(unique(data_clean()$dependencia), "TODAS")),
                         selected = "TODAS")
    updateSelectizeInput(session, "xtipo_contrato",
                         choices = toupper(c(unique(data_clean()$tipo_contrato), "TODAS")),
                         selected = "TODAS")
    updateSelectizeInput(session, "xtipo_requerimiento",
                         choices = toupper(c(unique(data_clean()$tipo_requerimiento), "TODAS")),
                         selected = "TODAS")
    updateSliderInput(session, "slider2", 
                      value = c(min(data_clean()$valor_estimado, na.rm = T),
                                max(data_clean()$valor_estimado, na.rm = T)),
                      min = min(data_clean()$valor_estimado, na.rm = T),
                      max = max(data_clean()$valor_estimado, na.rm = T))
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
  
  ### Salidas tablas ====
  output$estructura_cs <- renderDataTable({
    datatable(data_fit(), rownames = FALSE, filter = "top",
              options = list(autoWidth = TRUE, dom="t", scrollX = TRUE, pageLength = nrow(data_clean()), scrollY = "400px"
              ))
  })
  
  output$estructura_glob <- renderDataTable({
    datatable(data_clean(), rownames = FALSE, filter = "top",
              options = list(autoWidth = TRUE, dom="t", scrollX = TRUE, pageLength = nrow(data_clean()), scrollY = "400px"
              ))
  })

  output$anomalias <- renderDataTable({
    datatable(data_clean() %>% filter(duplicated(numero_de_contrato_o_convenio)), rownames = FALSE, filter = "top",
              options = list(autoWidth = TRUE, dom="t", scrollX = TRUE, pageLength = nrow(data_clean()), scrollY = "400px"
              ))
  })
  
  ### Indicadores ====
  data_f <- eventReactive(input$go,{
    df <- data_clean() %>%
      dplyr::filter(if (!(input$xdependencia %in% "TODAS")) dependencia %in% input$xdependencia else TRUE,
                    if (!(input$xtipo_contrato %in% "TODAS")) tipo_de_contrato_o_convenio %in% input$xtipo_contrato else TRUE,
                    if (!(input$xtipo_requerimiento %in% "TODAS")) tipo_requerimiento %in% input$xtipo_requerimiento else TRUE,
                    valor_estimado <= input$slider2[2] & valor_estimado >= input$slider2[1])
    return(df)
  })

  output$Num_Informes <- renderInfoBox({

    if (nrow(data_f()) == 0) {
      aux <- "No hay solicitudes"
    } else {
      aux <- comma(dim(data_f())[1])
    }

    infoBox(
      title = "Numero de Solicitudes",
      subtitle = "Contratos 2020",
      value = aux,
      color = "navy",
      icon = icon("receipt"))
  })

  output$Valor_total_estimado <- renderInfoBox({

    if (nrow(data_f()) == 0) {
      aux <- "No hay solicitudes"
    } else {
      aux <- comma(sum(data_f()$valor_estimado, na.rm = TRUE))
    }

    infoBox(
      title = "Valor Estimado",
      subtitle = "Total",
      value = aux,
      icon = icon("dollar"),
      color = "navy")
  })

  output$Valor_ingreso <- renderInfoBox({

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
    
    infoBox(
      title = "Total Ingresos",
      subtitle = "Ingreso",
      value = aux,
      icon = icon("dollar"),
      color = "navy"
    )
  })

  output$Valor_gasto <- renderInfoBox({

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
    infoBox(
      title = "Total Gastos",
      subtitle = "Gastos",
      value = aux,
      icon = icon("dollar"),
      color = "navy"
    )
  })

  output$estado_finalizado <- renderInfoBox({

    if (nrow(data_f()) == 0) {
      aux <- "No hay solicitudes"
    } else {
      aux <- data_f() %>%
        filter(!is.na(fecha_final_respuesta))
      aux <- comma(dim(aux)[1])
    }
    infoBox(
      title = "Finalizados",
      subtitle = "",
      value = aux,
      icon = icon("receipt"),
      color = "navy"
    )
  })

  output$estado_juridica <- renderInfoBox({

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
        # filter_all(any_vars(str_detect(lista, pattern = "EN TRAMITE JURIDICA")))
      aux <- comma(dim(aux)[1])
    }
    infoBox(
      title = "En tramite juridica",
      subtitle = "",
      value = aux,
      icon = icon("receipt"),
      color = "navy"
    )
  })

  output$estado_ues <- renderInfoBox({

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
        # filter_all(any_vars(str_detect(lista, pattern = "EN TRAMITE UES")))
      aux <- comma(dim(aux)[1])
    }
    infoBox(
      title = "En tramite UES",
      subtitle = "",
      value = aux,
      icon = icon("receipt"),
      color = "navy"
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
  
  # Promedio global de Finalizacion
  output$plot3 <- renderPlot({
    aux <- data_f() %>%
      select(dependencia,fecha_solicitud,fecha_final_respuesta) %>%
      filter(!is.na(fecha_final_respuesta)) %>%
      mutate(dif_dias = difftime(fecha_final_respuesta,fecha_solicitud,units = "days")) %>%
      dplyr::filter(!is.na(dif_dias)) %>%
      group_by(dependencia) %>%
      summarise(pro_finalizacion = round(mean(dif_dias),1)) %>%
      ungroup() %>%
      mutate(dia_min = 0) %>%
      arrange(desc(pro_finalizacion))
      
    ggplot(aux, aes(x=dia_min, xend=pro_finalizacion, y=dependencia)) +
      geom_segment(aes(x=dia_min,
                       xend=pro_finalizacion,
                       y=dependencia,
                       yend=dependencia),
                   color="#b2b2b2", size=1.5)+
      ggalt::geom_dumbbell(color="light blue",
                    size_x=4.5,
                    size_xend = 4.5,
                    colour_x="#edae52",
                    colour_xend = "#9fb059")+
      labs(x=NULL, y=NULL,
           title="Contratos Finalizados 2020",
           subtitle="Total promedio atención (Días)")+
      geom_text(color="black", size=5, hjust=-0.5,
                aes(x=dia_min, label=dia_min))+
      geom_text(aes(x=pro_finalizacion, label=pro_finalizacion),
                color="black", size=5, hjust=-0.5)
  })
  
  # Tabla contratos Finalizados
  output$estructura_plot3 <- renderDataTable({
    aux <- data_f() %>%
      filter(!is.na(fecha_final_respuesta)) %>%
      select(numero_de_contrato_o_convenio,fecha_solicitud,fecha_final_respuesta) %>%
      mutate(dif_dias = difftime(fecha_final_respuesta,fecha_solicitud,units = "days")) %>%
      dplyr::filter(!is.na(dif_dias)) %>%
      arrange(desc(dif_dias))
    datatable(aux,
              rownames = FALSE, filter = "top",
              options = list(autoWidth = TRUE, dom="t", scrollX = TRUE, pageLength = nrow(aux), scrollY = "200px"))
  })
  
  # Promedio en juridica 
  output$plot4 <- renderPlot({
    aux <- data_f() %>%
      left_join(data_fit() %>% select(numero_de_contrato_o_convenio,acumulado_juridica), by = "numero_de_contrato_o_convenio") %>% 
      group_by(dependencia) %>%
      summarise(pro_juridica = round(mean(acumulado_juridica),1)) %>%
      ungroup() %>%
      mutate(dia_min = 0) %>%
      arrange(desc(pro_juridica))

    ggplot(aux, aes(x=dia_min, xend=pro_juridica, y=dependencia)) +
      geom_segment(aes(x=dia_min,
                       xend=pro_juridica,
                       y=dependencia,
                       yend=dependencia),
                   color="#b2b2b2", size=1.5)+
      ggalt::geom_dumbbell(color="light blue",
                           size_x=4.5,
                           size_xend = 4.5,
                           colour_x="#edae52",
                           colour_xend = "#9fb059")+
      labs(x=NULL, y=NULL,
           title="Contratos en Trámite Juridica 2020",
           subtitle=" Promedio atención (Días)")+
      geom_text(color="black", size=5, hjust=-0.5,
                aes(x=dia_min, label=dia_min))+
      geom_text(aes(x=pro_juridica, label=pro_juridica),
                color="black", size=5, hjust=-0.5)
  })
  
  # Tabla contratos en juridica
  output$estructura_plot4 <- renderDataTable({
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
      data.frame() %>% 
      filter(lista == "EN TRAMITE JURIDICA") %>%
      # filter_all(any_vars(str_detect(lista, pattern = "JURIDICA"))) %>%
      select(numero_de_contrato_o_convenio) %>% 
      data.frame() %>% 
      left_join(data_fit(), by = "numero_de_contrato_o_convenio")
    # aux <- data_f() %>% 
    #   filter(is.na(fecha_final_respuesta)) %>% 
    #   filter(numero_de_contrato_o_convenio %in% aux1$numero_de_contrato_o_convenio) %>% 
    #   mutate(max_fecha = apply(.[,c('fecha_solicitud_1','fecha_solicitud_2','fecha_solicitud_3','fecha_solicitud_4',
    #                                 'fecha_solicitud_5','fecha_solicitud_6','fecha_solicitud_7')], 1, max, na.rm=TRUE)) %>%
    #   mutate(max_fecha = as.Date.character(max_fecha, tryFormats = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d","%Y/%m/%d"))) %>%
    #   mutate(dif_dias = difftime(max_fecha,fecha_solicitud,units = "days")) %>%
    #   select(numero_de_contrato_o_convenio,fecha_solicitud,fecha_final_respuesta,Ultima_modificacion = max_fecha,dif_dias) %>%
    #   arrange(desc(dif_dias))
    datatable(aux,
              rownames = FALSE, filter = "top",
              options = list(autoWidth = TRUE, dom="t", scrollX = TRUE, pageLength = nrow(aux), scrollY = "200px"))
  })
  
  # Promedio contratos en UES
  output$plot5 <- renderPlot({
    aux <- data_f() %>%
      left_join(data_fit() %>% select(numero_de_contrato_o_convenio,acumulado_ues), by = "numero_de_contrato_o_convenio") %>% 
      group_by(dependencia) %>%
      summarise(pro_ues = round(mean(acumulado_ues),1)) %>%
      ungroup() %>%
      mutate(dia_min = 0) %>%
      arrange(desc(pro_ues))

    ggplot(aux, aes(x=dia_min, xend=pro_ues, y=dependencia)) +
      geom_segment(aes(x=dia_min,
                       xend=pro_ues,
                       y=dependencia,
                       yend=dependencia),
                   color="#b2b2b2", size=1.5)+
      ggalt::geom_dumbbell(color="light blue",
                    size_x=4.5,
                    size_xend = 4.5,
                    colour_x="#edae52",
                    colour_xend = "#9fb059")+
      labs(x=NULL, y=NULL,
           title="Contratos en Trámite UES 2020",
           subtitle=" Promedio atención (Días)")+
      geom_text(color="black", size=5, hjust=-0.5,
                aes(x=dia_min, label=dia_min))+
      geom_text(aes(x=pro_ues, label=pro_ues),
                color="black", size=5, hjust=-0.5)
  })
  
  # Tabla contratos en juridica
  output$estructura_plot5 <- renderDataTable({
    aux <- data_f() %>%
      filter(is.na(fecha_final_respuesta)) %>% 
      select(numero_de_contrato_o_convenio,
             estado_de_la_solicitud_1,estado_de_la_solicitud_2,estado_de_la_solicitud_3,estado_de_la_solicitud_4,
             estado_de_la_solicitud_5,estado_de_la_solicitud_6,estado_de_la_solicitud_7) %>%
      gather("estado", "lista", 2:8) %>% 
      na.omit() %>% 
      group_by(numero_de_contrato_o_convenio) %>% 
      arrange(desc(estado)) %>% 
      dplyr::filter(row_number()==1) %>% 
      ungroup() %>% 
      data.frame() %>% 
      dplyr::filter(lista == "EN TRAMITE UES") %>%
      # filter_all(any_vars(str_detect(lista, pattern = "UES"))) %>%
      dplyr::select(numero_de_contrato_o_convenio) %>% 
      data.frame() %>% 
      left_join(data_fit(), by = "numero_de_contrato_o_convenio")
    # aux <- data_f() %>%
    #   filter(is.na(fecha_final_respuesta)) %>% 
    #   dplyr::filter(numero_de_contrato_o_convenio %in% aux1$numero_de_contrato_o_convenio) %>%
    #   mutate(max_fecha = apply(.[,c('fecha_solicitud_1','fecha_solicitud_2','fecha_solicitud_3','fecha_solicitud_4',
    #                                 'fecha_solicitud_5','fecha_solicitud_6','fecha_solicitud_7')], 1, max, na.rm=TRUE)) %>%
    #   mutate(max_fecha = as.Date.character(max_fecha, tryFormats = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d","%Y/%m/%d"))) %>%
    #   mutate(dif_dias = difftime(max_fecha,fecha_solicitud,units = "days")) %>%
    #   dplyr::select(numero_de_contrato_o_convenio,fecha_solicitud,fecha_final_respuesta,Ultima_modificacion = max_fecha, dif_dias) %>%
    #   arrange(desc(dif_dias))
    datatable(aux,
              rownames = FALSE, filter = "top",
              options = list(autoWidth = TRUE, dom="t", scrollX = TRUE, pageLength = nrow(aux), scrollY = "200px"))
  })

  ### Consolidado 2019 ====
  
  # output$Num_Informes_2019 <- renderInfoBox({
  #   
  #   aux <- dim(contratos2019)[1]
  #   infoBox(
  #     title = "Numero de Solicitudes",
  #     subtitle = "Contratos 2019",
  #     value = aux,
  #     color = "navy",
  #     icon = icon("receipt"))
  # })
  # 
  # 
  # # Por dependencias
  # output$plot1_2019 <- renderPlotly({
  #   aux <- contratos2019 %>%
  #     group_by(dependencia) %>%
  #     summarise(conteo = n(),
  #               plata = sum(valor.estimado.clm, na.rm = T)) %>%
  #     arrange(desc(conteo))
  #   aux %>%
  #     plot_ly(y = ~reorder(dependencia,conteo), x = ~conteo, type = 'bar', orientation = 'h',
  #             hoverinfo = 'text', text = ~paste(conteo, "($ ", comma(plata), ")"),textposition = 'inside', color = ~dependencia, colors = "Spectral") %>%
  #     layout(title = "Solicitudes por Dependencia",
  #            xaxis = list(title=""),
  #            yaxis = list(title = "", zeroline = T, showline = T, showticklabels = F, showgrid = T),
  #            legend = list(x = 100, y = 0.5))
  # })
  
  
})    


