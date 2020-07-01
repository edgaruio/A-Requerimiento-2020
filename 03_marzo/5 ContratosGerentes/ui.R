library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyBS)
library(shinycssloaders)
library(shinyalert)

dashboardPage(dashboardHeader(title = "Contratos"),
              dashboardSidebar(
                sidebarMenu(disable = TRUE,br(),
                            tags$img(src = "logo.png", height=40, width=200, align="center"),
                            tags$hr(),
                            shinyjs::hidden(menuItem("INSTRUCCIONES", tabName = "dummy")),
                            tags$hr(),
                            menuItem("INDICADORES", tabName = "individual", icon = icon("area-chart"))
                            )),
              dashboardBody(
                shinyDashboardThemes(theme = "blue_gradient"),
                tags$style(type="text/css",
                           ".shiny-output-error { visibility: hidden; }",
                           ".shiny-output-error:before { visibility: hidden; }"),
                tabItems(
                  tabItem("dummy",
                          fluidRow(
                            column(1),
                            column(10,
                                   h1("Descripción la aplicación"),
                                   br(),
                                   h3(
                                   "La presente aplicación es una herramienta de consulta descriptiva que permite el seguimiento del 
                                   estado de las solicitudes en el proceso contractual."
                                   ),
                                   br(),
                                   h3(
                                   "En la siguiente pestaña 'Indicadores' cada usuario debe registrar su usuario (correo de colsubsidio) y
                                   una clave asignada por el administrador de la aplicación."
                                   ),
                                   br(),
                                   h3( 
                                     "Fecha de actualización: 12 de marzo de 2020."
                                   )
                            ),
                            column(1)
                          )
                  ),
                  ### Individual ====
                  tabItem(tabName = "individual",
                          fluidPage(
                            title = "DB",
                            br(),br(),
                            fluidRow(
                              column(5,
                                     textInput("usuario", label = "Usuario (Minúsculas)", value = "")),
                              column(4,
                                     textInput("clave", label = "Clave (Minúsculas)", value = "")),
                              column(3,
                                     br(),
                                     actionButton("go", label = "Consultar"))
                            ),
                            br(),br(),br(),
                            fluidRow(
                            box(title = "Información Contratos",width=12,status="primary",solidHeader=FALSE,collapsible=FALSE,collapsed = TRUE,
                              br(),
                              fluidRow(
                                column(4,
                                       selectInput("xdependencia", "Dependecia", choices = "", selected= "", multiple = F)),
                                column(4,
                                       selectInput("xtipo_contrato", "Tipo de Contrato", choices = "", selected="", multiple = F)),
                                column(4,
                                       selectInput("xtipo_requerimiento", "Tipo Requerimiento", choices = "", selected="", multiple = F))
                              ),
                              br(),
                              fluidRow(
                                valueBoxOutput("info_gerencia",width = 12)
                              ),
                              br(),
                              column(9,
                                     br(),
                                     withLoader(plotlyOutput("plot2", height = 500), type = "html", loader = "loader1"),
                                     br(),br(),
                                     withLoader(plotlyOutput("plot1", height = 500), type = "html", loader = "loader1")
                                     ),
                              column(3,
                                     fluidRow(
                                       valueBoxOutput("num_informes",width = 12),
                                       valueBoxOutput("valor_total_estimado",width = 12),
                                       valueBoxOutput("valor_ingreso",width = 12),
                                       valueBoxOutput("valor_gasto",width = 12),
                                       valueBoxOutput("estado_finalizado", width = 12),
                                       valueBoxOutput("estado_juridica", width = 12),
                                       valueBoxOutput("estado_ues", width = 12)
                                       )
                                     ),
                              fluidRow(br(),
                                       column(width = 4,
                                              withLoader(plotlyOutput("plot_pira_ind", height = 500), type = "html", loader = "loader1")),
                                       column(width = 4,
                                              withLoader(plotlyOutput("plot1_pro", height = 500), type = "html", loader = "loader1")),
                                       column(width = 4,
                                              withLoader(plotlyOutput("plot2_pro", height = 500), type = "html", loader = "loader1")),
                                       br()
                              )
                            )
                            )
                            )
                  )
                )
              )
)

