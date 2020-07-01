library(shiny); library(shinydashboard); library(shiny)

js <- '.nav-tabs-custom .nav-tabs li.active {
    border-top-color: #00CED1;
}"'

dbHeader <- dashboardHeader(title = "Aportes CCF")
dbHeader$children[[3]]$children <-  tags$img(src = "logo.png", height=40, width=200, align="right")

dashboardPage(skin = "blue",
              dbHeader,
              dashboardSidebar(
                disable = TRUE,
                collapsed = TRUE,
                sidebarMenu()
              ),
              dashboardBody(
                useShinyjs(),
                tags$style(js),
                shinyDashboardThemes(theme = "grey_dark"),
                # Resumen global
                br(),
                h4("Seleccione la poblacion a analizar:"),
                br(),
                tabBox(id = "General", width = 12,
                       # tab1
                       tabPanel(h3("Aportes CCF"),
                                column(2,
                                       br(),br(),
                                       pickerInput(inputId = "xcategoria1", label = h3("Seleccione Categoria"), multiple = FALSE,
                                                   choices = c("Categoria A" = "A",
                                                               "Categoria B" = "B",
                                                               "Categoria C" = "C",
                                                               "Convenios" = "Convenios",
                                                               "Total" = "Total"),
                                                   selected = c("Total"),
                                                   options = pickerOptions(actionsBox = TRUE, title = "Seleccione:", header = "")),
                                       dateRangeInput("xfecha", label = h3("Seleccione fecha"), language = "es", separator = "a",
                                                      start = fecha_min, end = fecha_max, min = fecha_min, max = fecha_max),
                                       br()
                                ),
                                column(10,
                                       br(),br(),
                                       tabBox(id = "tabcajas1", width = 12,
                                              tabPanel("Historico Poblacion Cajas", 
                                                       withLoader(plotlyOutput("plot_cajas", height = 400), 
                                                                  type = "html", loader = "loader1")),
                                              tabPanel("Detalle Poblacion",
                                                       withLoader(dataTableOutput("dt_cajas", height = 400), 
                                                                  type = "html", loader = "loader1")
                                                       )
                                              ),
                                       br(),br(),
                                       tabBox(id = "tabcajas2", width = 12,
                                              tabPanel("Participacion Población",
                                                       withLoader(plotlyOutput("plot_cajas_por"),
                                                                  type = "html", loader = "loader1")
                                                       ),
                                              tabPanel("Detalle Participacion",
                                                       withLoader(dataTableOutput("dt_cajas_por", height = 400),
                                                                  type = "html", loader = "loader1")
                                                       )
                                              )
                                )
                                ),
                       # tab2
                       tabPanel(h3("Aportes 4%"),
                                br(),br(),
                                # box(solidHeader = FALSE, width = 12, status = "primary",
                                    fluidRow(
                                      # h3("Resumen"),
                                      br(),br(),
                                      box(title = "Resumen por piramide", solidHeader = FALSE, width = 12, status = "primary",
                                          column(2,
                                                 pickerInput(inputId = "xpiramide", label = "Piramide:", choices = name_piramide,
                                                             multiple = TRUE,
                                                             selected = name_piramide[1],
                                                             options = pickerOptions(actionsBox = TRUE, 
                                                                                     title = "Seleccione Piramide", header = "")),
                                                 pickerInput(inputId = "xcategoria2", label = "Categoria Afiliado:", 
                                                             choices = name_categoria,
                                                             multiple = TRUE,
                                                             selected = name_categoria[1],
                                                             options = pickerOptions(actionsBox = TRUE, 
                                                                                     title = "Seleccione Categoria", header = ""))
                                                 ),
                                          column(1),
                                          column(10,
                                                 withLoader(dataTableOutput("dt_afiliados"),
                                                            type = "html", loader = "loader1")
                                                 )
                                          )
                                      ),
                                br(),br(),
                                    fluidRow(
                                      box(title = "Resumen por actividad", solidHeader = FALSE, width = 12, status = "primary",
                                          column(2,
                                                 pickerInput(inputId = "xnuevociiu", label = "Actividad:", choices = name_actividad,
                                                             multiple = TRUE,
                                                             selected = name_actividad[1],
                                                             options = pickerOptions(actionsBox = TRUE, 
                                                                                     title = "Seleccione Actividad", header = "")),
                                                 pickerInput(inputId = "xcategoria3", label = "Categoria Afiliado:", 
                                                             choices = name_categoria,
                                                             multiple = TRUE,
                                                             selected = name_categoria[1],
                                                             options = pickerOptions(actionsBox = TRUE, 
                                                                                     title = "Seleccione Categoria", header = ""))
                                                 ),
                                          column(1),
                                          column(10,
                                                 withLoader(dataTableOutput("dt_afiliados_ciuu"),
                                                            type = "html", loader = "loader1")
                                                 )
                                          )
                                      ),
                                    fluidRow(
                                      # h3("Indicador PSI"),
                                      # br(),br(),
                                      # box(title = "Indicador PSI", solidHeader = FALSE, width = 12, status = "primary",
                                      #     # fluidRow(valueBoxOutput("indicador_psi",width = 12)),
                                      #     fluidRow(withLoader(plotlyOutput("ts_afiliados", height = 500), 
                                      #                         type = "html", loader = "loader1"))
                                      #     )
                                      )
                                    # )
                                )
                       # ,
                       # # tab3
                       # tabPanel(h3("Poblacion Subsidios"),
                       #          br(),br(),
                       #          box(solidHeader = FALSE, width = 12, status = "primary",
                       #            tabBox(id="tabchart2", width = 12,
                       #                   tabPanel("Distribución Subsidios", 
                       #                            withLoader(plotlyOutput("plot_subsidios", height = 400), 
                       #                                       type = "html", loader = "loader1")),
                       #                   tabPanel("Detalle", 
                       #                            withLoader(dataTableOutput("dt_subsidios"), type = "html", loader = "loader1"))))
                       #          ),
                       # # tab4
                       # tabPanel(h3("Poblacion Cesantes"),
                       #          br(),br(),
                       #          box(solidHeader = FALSE, width = 12, status = "primary",
                       #            tabBox(id="tabchart3", width = 12,
                       #                   tabPanel("Distribucion Cesantes", 
                       #                            withLoader(plotlyOutput("plot_cesantes", height = 400), 
                       #                                       type = "html", loader = "loader1")),
                       #                   tabPanel("Detalle", 
                       #                            withLoader(dataTableOutput("dt_cesantes"), 
                       #                                       type = "html", loader = "loader1")))
                       #            )
                       #          )
                       )
                )
              )


