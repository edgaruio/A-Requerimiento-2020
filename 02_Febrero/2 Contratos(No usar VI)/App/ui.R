### Header ----
header <- dashboardHeader(title = "Análisis de Contratos", 
                          titleWidth = 265)


### Sidebar ----
sidebar <- dashboardSidebar(width="300px",
                            sidebarMenu(#shinyjs::useShinyjs(),
                                        disable = TRUE, br(),
                                        tags$img(src = "logo.png", height=40, width=250, align="center"),
                                        br(),
                                        tags$hr(),
                                        shinyjs::hidden(menuSubItem("INSTRUCCIONES", tabName = "dummy")),
                                        tags$hr(),
                                        menuItem("Cargar datos", tabName = "datos", icon = icon("dashboard")),
                                        tags$hr(),
                                        menuItem("Indicadores", tabName = "descrip", icon = icon("id-card")) ,
                                        selectInput("xdependencia", "Dependecia", choices = name_depedencia, selected="TODAS", multiple = F),
                                        selectInput("xtipo_contrato", "Tipo de Contrato", choices = name_contrato_convenio, selected="TODAS", multiple = F),
                                        selectInput("xtipo_requerimiento", "Tipo Requerimiento", choices = name_tipo_reque, selected="TODAS", multiple = F),
                                        sliderInput("slider2", label = h3("Rango Valor Inicial (Millones)"),
                                                    min = min(base_contratos$valor_inicial_del_contrato_o_convenio_m, na.rm = T), 
                                                    max = max(base_contratos$valor_inicial_del_contrato_o_convenio_m, na.rm = T), 
                                                    value = c(min(base_contratos$valor_inicial_del_contrato_o_convenio_m, na.rm = T), 
                                                              max(base_contratos$valor_inicial_del_contrato_o_convenio_m, na.rm = T))),
                                        br(),
                                        actionButton("go", "Aplicar Filtros", icon = icon("refresh")),
                                        br(),br(),tags$hr(),
                                        menuItem("Consulta Individual", tabName = "individual", icon = icon("filter"))
                                        )
                            )

body <- dashboardBody(
  tags$style(
    type="text/css",
    ".shiny-output-error { visibility: hidden; }",
    ".shiny-output-error:before { visibility: hidden; }"
  ),
  tabItems(
    ### Dummy ----
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
    tabItem(tabName = "datos",
            h2("Cargue Base de Datos"),
            h5("Por favor, seleccione la base de datos con el formato indicado en las instrucciones de la aplicacion."),
            fileInput("file1", label= "", multiple = FALSE, placeholder="Explore el archivo", buttonLabel="Buscar",
                      accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
            fluidRow(
              valueBoxOutput("n_casos", width = 3),
              valueBoxOutput("n_afiliados", width = 3),
              valueBoxOutput("n_empresas", width = 3),
              valueBoxOutput("n_sin_informacion", width = 3)
            ),
            fluidRow(
              box(withLoader(verbatimTextOutput("estructura_cs"),type="html",loader="loader1"), 
                  collapsible = F, width="100%", collapsed = T, title = "Estructura: Base Contratos"),
              box(withLoader(dataTableOutput("estructura_glob"),type="html",loader="loader1"), 
                  collapsible = T, width="100%", collapsed = F, title = "Previzualizacion de datos")
            )
    ),
    tabItem("descrip",
            h3("Tablero Indicadores"),
            fluidRow(
              br(),br(),
              fluidRow(
                    column(1),
                    valueBoxOutput("Num_Informes", width = 2),
                    valueBoxOutput("Valor_total_estimado", width = 2),
                    valueBoxOutput("Valor_total_inicial", width = 2),
                    valueBoxOutput("Valor_ingreso", width = 2),
                    valueBoxOutput("Valor_egreso", width = 2)
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
              fluidRow(
                column(6,withLoader(plotOutput("plot3", height = 800), type = "html", loader = "loader5")),
                column(6,
                       box(width = 12,title = "Contratos Finalizados",solidHeader = TRUE,collapsible = TRUE,status = "primary",
                           dataTableOutput("estructura_plot3")))
                ),
              br(),br(),br(),
              fluidRow(
                column(6,withLoader(plotOutput("plot4", height = 800), type = "html", loader = "loader5")),
                column(6,
                       box(width = 12,title = "Contratos Tramite Juridica",solidHeader = TRUE,collapsible = TRUE,status = "primary",
                           dataTableOutput("estructura_plot4")))
                ),
              br(),br(),br(),
              fluidRow(
                column(6,withLoader(plotOutput("plot5", height = 800), type = "html", loader = "loader5")),
                column(6,
                       box(width = 12,title = "Contratos Tramite UES",solidHeader = TRUE,collapsible = TRUE,status = "primary",
                           dataTableOutput("estructura_plot5")))
              )
              ),
    ),
    tabItem(
      tabName = "individual",
      fluidRow(
        column(3, textInput("TipoId", label = h3("Numero Contrato o Convenio"),value = "CD-2019-002715"))
      ),
      br(),
      box(title = NULL,
          status = "primary",
          solidHeader = FALSE,
          width = 12,
          fluidRow(
            dataTableOutput("contrato_fil")
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

