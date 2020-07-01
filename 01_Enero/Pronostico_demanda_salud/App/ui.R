library(shiny); library(shinydashboard); library(shiny)

dashboardPage(skin = "blue",
              dashboardHeader(title = "Pronostico Demanda Salud", titleWidth = 350),
              dashboardSidebar(
                sidebarMenu(disable = TRUE,br(),
                            tags$img(src = "logo.png", heigth = 40, width = 200, align = "center"),
                            br(),br(),
                            menuItem("PRONÓSTICO GLOBAL", tabName = "global", icon = icon("area-chart"))
                )
              ),
              dashboardBody(
                shinyDashboardThemes(
                  theme = "grey_dark"
                ),
                tags$head(tags$style(HTML('
                /* logo */
                .skin-blue .main-header .logo {
                background-color: #006b6f;
                font-family: Courier;
                }
                /* navbar (rest of the header) */
                .skin-blue .main-header .navbar {
                background-color: #00868B;
                                          }'))),
                tabItems(
                  tabItem("global",
                          fluidRow(
                            column(9,
                                   box(solidHeader = FALSE, width = 12, height = 550, status = "primary",
                                       DT::dataTableOutput("pronos_grupo")
                                   )
                            ),
                            column(3,
                                   box(solidHeader = FALSE, width = 12, height = 550, status = "primary",
                                       valueBoxOutput("num_materiales",width = 12),
                                       valueBoxOutput("num_rfm_alto",width = 12),
                                       valueBoxOutput("num_rfm_medio",width = 12),
                                       valueBoxOutput("num_rfm_bajo",width = 12)
                                   )
                            )
                          ),
                          downloadButton("downloadData", "Descargar Base Pronosticos"))
                )
              )
)


