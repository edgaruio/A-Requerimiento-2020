library(shiny)
library(rdrop2)
library(DT)



shinyServer(function(input, output, session) {
  
  output$the_data <- renderDataTable({
    mydata <- load_db()
    datatable(mydata)
    
  })
  
})