library(shiny)
library(googlesheets)

shinyUI(
  fluidPage(
    titlePanel("Read a public Dropbox"),
    sidebarLayout(
      sidebarPanel(
        h6("Prueba carga de datos")
      ),
      mainPanel(
        DT::dataTableOutput("the_data")
      )
    )
  ))