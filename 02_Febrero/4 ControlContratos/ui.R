### Header ----
header <- dashboardHeader(title = "Tablero de Contratos", 
                          titleWidth = 265)


### Sidebar ----
sidebar <- dashboardSidebar(width="300px",
                            sidebarMenu(disable = TRUE, br(),
                                        tags$img(src = "logo.png", height=40, width=250, align="center"),
                                        tags$hr(style="border-color: yellow;"),
                                        shinyjs::hidden(menuSubItem("INSTRUCCIONES", tabName = "dummy")),
                                        tags$hr(style="border-color: yellow;"),
                                        menuItem("CARGAR DATOS", tabName = "datos", icon = icon("dashboard")),
                                        tags$hr(style="border-color: yellow;"),
                                        menuItem("INDICADORES", tabName = "descrip", icon = icon("id-card")) ,
                                        selectInput("xdependencia", "Dependecia", choices = "TODAS", selected="TODAS", multiple = T),
                                        selectInput("xtipo_contrato", "Tipo de Contrato", choices = "TODAS", selected="TODAS", multiple = T),
                                        selectInput("xtipo_requerimiento", "Tipo Requerimiento", choices = "TODAS", selected="TODAS", multiple = T),
                                        sliderInput("slider2", label = h3("Valor Estimado"),
                                                    min = min_valor_estimado,
                                                    max = max_valor_estimado,
                                                    value = c(min_valor_estimado,
                                                              max_valor_estimado)),
                                        actionButton("go", "Aplicar Filtros", icon = icon("refresh")),
                                        tags$hr(style="border-color: yellow;"),
                                        menuItem("CONSOLIDADO 2019", tabName = "consolidado2019", icon = icon("eye"))
                                        )
                            )

body <- dashboardBody(
  tags$style(
    type="text/css",
    ".shiny-output-error { visibility: hidden; }",
    ".shiny-output-error:before { visibility: hidden; }"
  ),
  tabItems(
    ### Dummy ====
    tabItem("dummy",
            fluidRow(
              column(1),
              column(10,
                     h1("Análisis de Contratos"),
                     br(),br(),
                     h3("La presente aplicacion tiene por objetivo ser una herramienta de consulta descriptiva de los Contratos de 
                        la Caja de Compensación Colsubsidio. Dispone de 3 ventanas. En la primera pestaña puede cargar archivo .xlsx
                        y previsualizar el consolidado de contratos. En la segunda ventana puede encontrar indicadores de los estados de contratos.
                        Finalmente, se puede encontrar una ventana donde puede consultar por contrato individual.")
              ),
              column(1)
            )
    ),
    ### Cargue datos ====
    tabItem(tabName = "datos",
            h2("Cargue Base de Datos"),
            h5("Por favor, seleccione la base de datos con el formato indicado en las instrucciones de la aplicacion."),
            fileInput("file1", label= "", multiple = FALSE, placeholder="Explore el archivo", buttonLabel="Buscar",
                      accept = c(".xlsx")),
            fluidRow(
              # box(withLoader(dataTableOutput("estructura_cs"),type="html",loader="loader1"),
              #     collapsible = F, width="100%", collapsed = T, title = "Estructura: Base Contratos"),
              box(withLoader(dataTableOutput("estructura_glob"),type="html",loader="loader1"), 
                  collapsible = T, width="100%", collapsed = F, title = "Previzualizacion de datos")
            ),
            h3("Para revisar"),
            fluidRow(
              box(withLoader(dataTableOutput("anomalias"),type="html",loader="loader1"),
                  collapsible = T, width="100%", collapsed = F, title = "Contratos Duplicados")
            )
    ),
    ### Indicdores ====
    tabItem("descrip",
            h3("Tablero Indicadores"),
            fluidRow(
              br(),br(),
              fluidRow(
                infoBoxOutput("Num_Informes", width = 3),
                infoBoxOutput("Valor_total_estimado", width = 3),
                infoBoxOutput("Valor_ingreso", width = 3),
                infoBoxOutput("Valor_gasto", width = 3)
                ),
              br(),br(),
              fluidRow(
                column(2),
                valueBoxOutput("estado_finalizado", width = 2),
                column(1),
                valueBoxOutput("estado_ues", width = 2),
                column(1),
                valueBoxOutput("estado_juridica", width = 2)
                ),
              br(),br(),br(),
              fluidRow(
                column(6,withLoader(plotlyOutput("plot1", height = 500), type = "html", loader = "loader1")),
                column(6,withLoader(plotlyOutput("plot2", height = 500), type = "html", loader = "loader1"))
                ),
              br(),br(),br(),
              h3("Contratos Finalizados"),
              fluidRow(
                column(6,withLoader(plotOutput("plot3", height = 700), type = "html", loader = "loader5")),
                column(6,
                       box(width = 12,title = "Contratos Finalizados",solidHeader = FALSE,collapsible = TRUE,status = "primary",
                           dataTableOutput("estructura_plot3")))
                ),
              br(),br(),br(),
              h3("Contratos Trámite Juridica"),
              fluidRow(
                column(6,
                       withLoader(plotOutput("plot4", height = 700), type = "html", loader = "loader5")),
                column(6,
                       box(width = 12,title = "Contratos Tramite Juridica",solidHeader = FALSE,collapsible = TRUE,status = "primary",
                           dataTableOutput("estructura_plot4")))
                ),
              br(),br(),br(),
              h3("Contratos Trámite Ues"),
              fluidRow(
                column(6,withLoader(plotOutput("plot5", height = 700), type = "html", loader = "loader5")),
                column(6,
                       box(width = 12,title = "Contratos Tramite UES",solidHeader = FALSE,collapsible = TRUE,status = "primary",
                           dataTableOutput("estructura_plot5")))
              ),
              br(),br(),br()
              ),
    ),
    ### Consolidado 2019 ====
    tabItem(
      tabName = "consolidado2019",
      h3("Tablero Indicadores 2019"),
      fluidRow(
        br(),br(),
        fluidRow(
          infoBoxOutput("Num_Informes_2019", width = 3),
          infoBoxOutput("Valor_total_estimado_2019", width = 3),
          infoBoxOutput("Valor_ingreso_2019", width = 3),
          infoBoxOutput("Valor_gasto_2019", width = 3)
        ),
        br(),br(),
        fluidRow(
          column(2),
          valueBoxOutput("estado_finalizado_2019", width = 2),
          column(1),
          valueBoxOutput("estado_ues_2019", width = 2),
          column(1),
          valueBoxOutput("estado_juridica_2019", width = 2)
        ),
        br(),br(),br(),
        fluidRow(
          column(6,withLoader(plotlyOutput("plot1_2019", height = 500), type = "html", loader = "loader1")),
          column(6,withLoader(plotlyOutput("plot2_2019", height = 500), type = "html", loader = "loader1"))
        ),
        br(),br(),br(),
        h3("Contratos Finalizados"),
        fluidRow(
          column(6,withLoader(plotOutput("plot3_2019", height = 700), type = "html", loader = "loader5")),
          column(6,
                 box(width = 12,title = "Contratos Finalizados",solidHeader = FALSE,collapsible = TRUE,status = "primary",
                     dataTableOutput("estructura_plot3_2019")))
        ),
        br(),br(),br(),
        h3("Contratos Trámite Juridica"),
        fluidRow(
          column(6,
                 withLoader(plotOutput("plot4_2019", height = 700), type = "html", loader = "loader5")),
          column(6,
                 box(width = 12,title = "Contratos Tramite Juridica",solidHeader = FALSE,collapsible = TRUE,status = "primary",
                     dataTableOutput("estructura_plot4_2019")))
        ),
        br(),br(),br(),
        h3("Contratos Trámite Ues"),
        fluidRow(
          column(6,withLoader(plotOutput("plot5_2019", height = 700), type = "html", loader = "loader5")),
          column(6,
                 box(width = 12,title = "Contratos Tramite UES",solidHeader = FALSE,collapsible = TRUE,status = "primary",
                     dataTableOutput("estructura_plot5_2019")))
          )
        )
    )
  )
)

dashboardPage(header,
              sidebar,
              body,
              tags$head(tags$style(HTML('
                            .skin-blue .main-header .logo {
                            background-color: #3c8dbc;
                            }
                            .skin-blue .main-header .logo:hover {
                            background-color: #3c8dbc;
                            }
                            '))
                        
                        
                        
              )
)

