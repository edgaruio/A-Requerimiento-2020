library(shiny)

shinyServer(function(input, output, session) {
  
  ### Global ====
  
  output$pronos_grupo <- renderDataTable({
    datatable(pronostico_grupo %>% select(material, texto, pronostico, metodo, rmse, Calificacion, Puntaje),
              rownames = FALSE, filter = "top",
              options = list(autoWidth = TRUE, dom="rt", scrollX = TRUE, pageLength = nrow(pronostico_grupo), scrollY = "400px"),
              colnames = c("Material","Texto material","Pronostico","Metodo","RMSE","Calificación RFM","Puntaje RFM"))
  })
  
  # data_f <- eventReactive(input$actualizar,{
  #   data_f <- pronostico_grupo %>% 
  #     filter(material == input$num_material)
  #   return(data_f)
  # })
  
  output$num_materiales <- renderValueBox({
    aux <- nrow(pronostico_grupo)
    valueBox(
      value = formatC(aux,digits = 0, format = "d", big.mark=","),
      subtitle = "Total Materiales",
      color = "light-blue",
      icon = icon("check"))
  })
  
  output$num_rfm_alto <- renderValueBox({
    aux <- pronostico_grupo %>% filter(Calificacion == "1. Alto")
    valueBox(
      value = formatC(nrow(aux),digits = 0, format = "d", big.mark=","),
      subtitle = "Materiales RFM Alto",
      color = "light-blue",
      icon = icon("check"))
  })
  
  output$num_rfm_medio <- renderValueBox({
    aux <- pronostico_grupo %>% filter(Calificacion == "2. Medio")
    valueBox(
      value = formatC(nrow(aux),digits = 0, format = "d", big.mark=","),
      subtitle = "Materiales RFM Medio",
      color = "light-blue",
      icon = icon("check"))
  })
  
  output$num_rfm_bajo <- renderValueBox({
    aux <- pronostico_grupo %>% filter(Calificacion == "3. Bajo")
    valueBox(
      value = formatC(nrow(aux),digits = 0, format = "d", big.mark=","),
      subtitle = "Materiales RFM Bajo",
      color = "light-blue",
      icon = icon("check"))
  })
  
  output$downloadData <- downloadHandler(
    filename = function(){
      paste("Prono_glob", Sys.time(), ".xlsx", sep = "_")
    },
    content = function(file){
      writexl::write_xlsx(pronostico_grupo %>% 
                            dplyr::select(material, texto, pronostico, metodo, rmse, Calificacion, Puntaje),file)
    }
  )
  
  # output$forecast_plots <- renderPlotly({
  #   if (input$forecast == "fit_arima") {
  #     autoplot(fit_arima,
  #              holdout = sp500_test,
  #              forc_name = 'ARIMA',
  #              ts_object_name = 'S&P 500')
  #   } else if (input$forecast == "fit_BC") {
  #     autoplot(fit_BC,
  #              holdout = sp500_test,
  #              forc_name = 'Box-Cox Transformation',
  #              ts_object_name = 'S&P 500')
  #   } else if (input$forecast == "fit_ets") {
  #     autoplot(fit_ets,
  #              holdout=sp500_test,
  #              forc_name = 'Exponential Smoothing',
  #              ts_object_name = 'S&P 500')
  #   } else if (input$forecast == "fit_meanf") {
  #     autoplot(fit_meanf,
  #              holdout = sp500_test,
  #              forc_name = 'Mean',
  #              ts_object_name = 'S&P 500')
  #   } else if (input$forecast == "fit_naive") {
  #     autoplot(fit_naive,
  #              holdout = sp500_test,
  #              forc_name = 'Naive',
  #              ts_object_name = 'S&P 500')
  #   } else if (input$forecast == "fit_snaive") {
  #     autoplot(fit_snaive,
  #              holdout = sp500_test,
  #              forc_name = 'Seasonal Naive',
  #              ts_object_name = 'S&P 500')
  #   } else if (input$forecast == "fit_net") {
  #     autoplot(fit_net,
  #              holdout = sp500_test,
  #              forc_name = 'Neural Networks',
  #              ts_object_name = 'S&P 500')
  #   }
  # })
  
  
  
  
})
