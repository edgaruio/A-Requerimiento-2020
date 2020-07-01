shinyServer(function(input, output, session) {
  
  ### Cargue de datos ====
  
  data_clean <- reactive({
    req(input$file1)
    df <- fread(input$file1$datapath, na.strings = c("", "NA", "#/NA", "na")) %>% 
      data.frame() %>% 
      filter(!is.na(ABOGADO.1))
    names(df) <- tolower(gsub(".","_",names(df),fixed = T))
    names(df) <- chartr("áéíóú","aeiou",names(df))
    df_clean <- df %>%
      mutate(valor_inicial_del_contrato_o_convenio = gsub(".","",valor_inicial_del_contrato_o_convenio,fixed = T),
             valor_estimado = gsub(".","",valor_estimado,fixed = T),
             valor_mixto_ingreso = gsub(".","",valor_mixto_ingreso,fixed = T),
             valor_mixto_gasto = gsub(".","",valor_mixto_gasto,fixed = T)) %>%
      mutate(valor_inicial_del_contrato_o_convenio = as.numeric(valor_inicial_del_contrato_o_convenio)/1000000,
             valor_estimado = as.numeric(valor_estimado)/1000000,
             valor_mixto_ingreso = as.numeric(valor_mixto_ingreso)/1000000,
             valor_mixto_gasto = as.numeric(valor_mixto_gasto)/1000000,
             fecha_solicitud = as.Date.character(fecha_solicitud, tryFormats = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d","%Y/%m/%d")),
             fecha_final_respuesta = as.Date.character(fecha_final_respuesta, tryFormats = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d","%Y/%m/%d")),
             fecha_solicitud_1 = as.Date(fecha_solicitud_1, tryFormats = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d","%Y/%m/%d")),
             fecha_solicitud_2 = as.Date(fecha_solicitud_2, tryFormats = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d","%Y/%m/%d")),
             fecha_solicitud_3 = as.Date(fecha_solicitud_3, tryFormats = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d","%Y/%m/%d")),
             fecha_solicitud_4 = as.Date(fecha_solicitud_4, tryFormats = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d","%Y/%m/%d")),
             fecha_solicitud_5 = as.Date(fecha_solicitud_5, tryFormats = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d","%Y/%m/%d")),
             fecha_solicitud_6 = as.Date(fecha_solicitud_6, tryFormats = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d","%Y/%m/%d")),
             fecha_solicitud_7 = as.Date(fecha_solicitud_7, tryFormats = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d","%Y/%m/%d"))
             ) %>%
      data.frame()
    return(df_clean)
  })
  
  output$estructura_cs <- renderPrint({str(data_clean())})
  output$estructura_glob <- renderDataTable({
    datatable(data_clean(), rownames = FALSE, filter = "top",
              options = list(pageLength = 3, autoWidth = TRUE, dom="t", scrollX = TRUE, scrollY = TRUE
              ))
  })

  ### Indicadores ====

  data_f <- eventReactive(input$go,{
    df <- data_clean() %>%
      dplyr::filter(if (!(input$xdependencia %in% "TODAS")) dependencia %in% input$xdependencia else TRUE,
                    if (!(input$xtipo_contrato %in% "TODAS")) tipo_de_contrato_o_convenio %in% input$xtipo_contrato else TRUE,
                    if (!(input$xtipo_requerimiento %in% "TODAS")) tipo_requerimiento %in% input$xtipo_requerimiento else TRUE,
                    valor_inicial_del_contrato_o_convenio <= input$slider2[2] & valor_inicial_del_contrato_o_convenio >= input$slider2[1])
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
      title = "Total Valor Estimado",
      subtitle = "Millones",
      value = aux,
      icon = icon("dollar"),
      color = "navy")
  })

  output$Valor_total_inicial <- renderInfoBox({

    if (nrow(data_f()) == 0) {
      aux <- "No hay solicitudes"
    } else {
      aux <- comma(sum(data_f()$valor_inicial_del_contrato_o_convenio, na.rm = TRUE))
    }

    infoBox(
      title = "Total Valor Inicial",
      subtitle = "Millones",
      value = aux,
      icon = icon("dollar"),
      color = "navy")
  })

  output$Valor_ingreso <- renderInfoBox({

    if (nrow(data_f()) == 0) {
      aux <- "No hay solicitudes"
    } else {
      aux <- comma(sum(data_f()$valor_mixto_ingreso, na.rm = T))
    }
    infoBox(
      title = "Total Ingresos",
      subtitle = "Ingreso (Millones)",
      value = aux,
      icon = icon("dollar"),
      color = "navy"
    )
  })

  output$Valor_egreso <- renderInfoBox({

    if (nrow(data_f()) == 0) {
      aux <- "No hay solicitudes"
    } else {
      aux <- comma(sum(data_f()$valor_mixto_gasto, na.rm = T))
    }
    infoBox(
      title = "Total Gastos",
      subtitle = "Gastos (Millones)",
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
        dplyr::select(estado_de_la_solicitud_1,estado_de_la_solicitud_2,estado_de_la_solicitud_3,estado_de_la_solicitud_4,
                      estado_de_la_solicitud_5,estado_de_la_solicitud_6,estado_de_la_solicitud_7) %>%
        filter_all(any_vars(str_detect(., pattern = "FINALIZADO")))
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
        dplyr::select(estado_de_la_solicitud_1,estado_de_la_solicitud_2,estado_de_la_solicitud_3,estado_de_la_solicitud_4,
                      estado_de_la_solicitud_5,estado_de_la_solicitud_6,estado_de_la_solicitud_7) %>%
        filter_all(any_vars(str_detect(., pattern = "EN TRAMITE JURIDICA")))
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
        dplyr::select(estado_de_la_solicitud_1,estado_de_la_solicitud_2,estado_de_la_solicitud_3,estado_de_la_solicitud_4,
                      estado_de_la_solicitud_5,estado_de_la_solicitud_6,estado_de_la_solicitud_7) %>%
        filter_all(any_vars(str_detect(., pattern = "EN TRAMITE UES")))
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

  output$plot1 <- renderPlotly({
    aux <- data_f() %>%
      # dplyr::filter(TIPO.REQUERIMIENTO %in% c("CREACIÓN DE CONTRATO/CONVENIO/ACUERDO","CREACIÓN DE OTROSÍ",
      #                                         "REVISIÓN DE CONTRATO/CONVENIO/ACUERDO","REVISIÓN DE OTROSÍ/ANEXO")) %>%
      group_by(dependencia) %>%
      summarise(conteo = n(),
                plata = sum(valor_estimado, na.rm = T)) %>%
      arrange(desc(conteo))
    # aux <- aux[1:5,]
    aux %>%
      plot_ly(y = ~reorder(dependencia,conteo), x = ~conteo, type = 'bar', orientation = 'h',
              hoverinfo = 'text', text = ~paste(conteo, "($ ", comma(plata), ")"),textposition = 'inside', color = ~dependencia, colors = "Spectral") %>%
      layout(title = "Solicitudes por Dependencia",
             xaxis = list(title=""),
             yaxis = list(title = "", zeroline = T, showline = T, showticklabels = F, showgrid = T),
             legend = list(x = 100, y = 0.5))
  })

  output$plot2 <- renderPlotly({
    aux <- data_f() %>%
      # dplyr::filter(TIPO.REQUERIMIENTO %in% c("CREACIÓN DE CONTRATO/CONVENIO/ACUERDO","CREACIÓN DE OTROSÍ",
      #                                         "REVISIÓN DE CONTRATO/CONVENIO/ACUERDO","REVISIÓN DE OTROSÍ/ANEXO")) %>%
      group_by(tipo_requerimiento) %>%
      summarise(conteo = n(),
                plata = sum(valor_estimado, na.rm = T)) %>%
      arrange(desc(conteo))
    # aux <- aux[1:5,]
    plot_ly(aux, labels = ~tipo_requerimiento, values = ~conteo, type = 'pie', hole = 0, alpha = 0.9, textinfo = 'percent',
            text = ~paste(conteo, "($ ", comma(plata), ")"), textposition = 'inside') %>%
      layout(title = "Solicitudes por Tipo de Requerimiento",
             xaxis = list(title=""),
             yaxis = list(title = "", zeroline = T, showline = T, showticklabels = F, showgrid = T),
             legend = list(x = 100, y = 0.5))
  })


  output$plot3 <- renderPlot({
    aux <- data_f() %>%
      filter_all(any_vars(str_detect(.,pattern = "FINALIZADO"))) %>%
      select(dependencia,fecha_solicitud,fecha_final_respuesta) %>%
      dplyr::filter(!is.na(fecha_final_respuesta)) %>%
      # mutate(inicio_anio = "01/01/2020") %>%
      mutate(
        # inicio_anio = as.Date.character(inicio_anio, format = "%d/%m/%Y"),
        dif_dias = difftime(fecha_final_respuesta,fecha_solicitud,units = "days")
        # ,dia_min = difftime(fecha_solicitud,inicio_anio,units = "days") + 1
        ) %>%
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
           subtitle=" Promedio atención (Días)")+
      geom_text(color="black", size=5, hjust=-0.5,
                aes(x=dia_min, label=dia_min))+
      geom_text(aes(x=pro_finalizacion, label=pro_finalizacion),
                color="black", size=5, hjust=-0.5)
  })

  output$estructura_plot3 <- renderDataTable({
    aux <- data_f() %>%
      filter_all(any_vars(str_detect(.,pattern = "FINALIZADO"))) %>%
      select(numero_de_contrato_o_convenio,fecha_solicitud,fecha_final_respuesta) %>%
      mutate(dif_dias = difftime(fecha_final_respuesta,fecha_solicitud,units = "days")) %>%
      dplyr::filter(!is.na(dif_dias)) %>%
      arrange(desc(dif_dias))
    datatable(aux,
              rownames = FALSE, filter = "top",
              options = list(autoWidth = TRUE, dom="t", scrollX = TRUE, pageLength = nrow(aux), scrollY = "200px"))
  })

  output$plot4 <- renderPlot({
    aux <- data_f() %>%
      filter_all(any_vars(str_detect(.,pattern = "EN TRAMITE JURIDICA"))) %>%
      mutate(max_fecha = apply(.[,c('fecha_solicitud_1','fecha_solicitud_2','fecha_solicitud_3','fecha_solicitud_4',
                                    'fecha_solicitud_5','fecha_solicitud_6','fecha_solicitud_7')], 1, max, na.rm=TRUE)) %>%
      select(dependencia,fecha_solicitud,max_fecha) %>%
      mutate(max_fecha = as.Date.character(max_fecha, tryFormats = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d","%Y/%m/%d"))) %>%
      mutate(dif_dias = difftime(max_fecha,fecha_solicitud,units = "days")) %>%
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
           title="Contratos en Trámite Juridica 2020",
           subtitle=" Promedio atención (Días)")+
      geom_text(color="black", size=5, hjust=-0.5,
                aes(x=dia_min, label=dia_min))+
      geom_text(aes(x=pro_finalizacion, label=pro_finalizacion),
                color="black", size=5, hjust=-0.5)
  })

  output$estructura_plot4 <- renderDataTable({
    aux <- data_f() %>%
      filter_all(any_vars(str_detect(.,pattern = "EN TRAMITE JURIDICA"))) %>%
      mutate(max_fecha = apply(.[,c('fecha_solicitud_1','fecha_solicitud_2','fecha_solicitud_3','fecha_solicitud_4',
                                    'fecha_solicitud_5','fecha_solicitud_6','fecha_solicitud_7')], 1, max, na.rm=TRUE)) %>%
      mutate(max_fecha = as.Date.character(max_fecha, tryFormats = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d","%Y/%m/%d"))) %>%
      mutate(dif_dias = difftime(max_fecha,fecha_solicitud,units = "days")) %>%
      select(numero_de_contrato_o_convenio,fecha_solicitud,fecha_final_respuesta,Ultima_modificacion = max_fecha,dif_dias) %>%
      arrange(desc(dif_dias))
    datatable(aux,
              rownames = FALSE, filter = "top",
              options = list(autoWidth = TRUE, dom="t", scrollX = TRUE, pageLength = nrow(aux), scrollY = "200px"))
  })

  output$plot5 <- renderPlot({
    aux <- data_f() %>%
      filter_all(any_vars(str_detect(.,pattern = "EN TRAMITE UES"))) %>%
      mutate(max_fecha = apply(.[,c('fecha_solicitud_1','fecha_solicitud_2','fecha_solicitud_3','fecha_solicitud_4',
                                    'fecha_solicitud_5','fecha_solicitud_6','fecha_solicitud_7')], 1, max, na.rm=TRUE)) %>%
      select(dependencia,fecha_solicitud,max_fecha) %>%
      mutate(max_fecha = as.Date.character(max_fecha, tryFormats = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d","%Y/%m/%d"))) %>%
      mutate(dif_dias = difftime(max_fecha,fecha_solicitud,units = "days")) %>%
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
           title="Contratos en Trámite UES 2020",
           subtitle=" Promedio atención (Días)")+
      geom_text(color="black", size=5, hjust=-0.5,
                aes(x=dia_min, label=dia_min))+
      geom_text(aes(x=pro_finalizacion, label=pro_finalizacion),
                color="black", size=5, hjust=-0.5)
  })

  output$estructura_plot5 <- renderDataTable({
    aux <- data_f() %>%
      filter_all(any_vars(str_detect(.,pattern = "EN TRAMITE UES"))) %>%
      mutate(max_fecha = apply(.[,c('fecha_solicitud_1','fecha_solicitud_2','fecha_solicitud_3','fecha_solicitud_4',
                                    'fecha_solicitud_5','fecha_solicitud_6','fecha_solicitud_7')], 1, max, na.rm=TRUE)) %>%
      mutate(max_fecha = as.Date.character(max_fecha, tryFormats = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d","%Y/%m/%d"))) %>%
      mutate(dif_dias = difftime(max_fecha,fecha_solicitud,units = "days")) %>%
      select(numero_de_contrato_o_convenio,fecha_solicitud,fecha_final_respuesta,Ultima_modificacion = max_fecha, dif_dias) %>%
      arrange(desc(dif_dias))
    datatable(aux,
              rownames = FALSE, filter = "top",
              options = list(autoWidth = TRUE, dom="t", scrollX = TRUE, pageLength = nrow(aux), scrollY = "200px"))
  })

  ### Individual ====

  df_individual_f <- reactive({
    df_contrato <- data_clean() %>%
      dplyr::filter(numero_de_contrato_o_convenio %in% input$TipoId)
  })

  output$contrato_fil <- renderDataTable({
    aux <- df_individual_f()
    datatable(aux,
              rownames = FALSE, filter = "top",
              options = list(autoWidth = TRUE, dom="t", scrollX = TRUE, pageLength = nrow(aux), scrollY = "200px"))
  })
  
})    


# ?addProviderTiles

