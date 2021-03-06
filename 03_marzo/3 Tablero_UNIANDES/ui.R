library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyBS)
library(shinycssloaders)
library(shinyalert)

dashboardPage(dashboardHeader(title = "Universidad de los Andes", titleWidth = 300),
              dashboardSidebar(
                sidebarMenu(disable = TRUE,br(),
                            tags$img(src = "logo.png", height=40, width=200, align="center"),
                            tags$hr(),
                            shinyjs::hidden(menuItem("INSTRUCCIONES", tabName = "dummy")),
                            tags$hr(),
                            menuItem("CONSOLIDADO", tabName = "individual", icon = icon("area-chart"))
                            )),
              dashboardBody(
                shinyDashboardThemes(theme = "purple_gradient"),
                tags$style(type="text/css",
                           ".shiny-output-error { visibility: hidden; }",
                           ".shiny-output-error:before { visibility: hidden; }"),
                tabItems(
                  tabItem("dummy",
                          fluidRow(
                            tags$img(src = "Andes.png", align="center"),
                            column(1),
                            column(10,
                                   h1("Descripción de los campos"),
                                   h3("Esta aplicación muestra participación de afiliados de la Universidad de los Andes en Cobertura, Derechos, Prospectos
                                      y Consumo. Además muestra el consumo por UES de la Universidad de los Andes. Un ejemplo del cálculo de un indicador 
                                      se muestra a contiuación:"),
                                   tags$img(src = "indicador.png", align="center"),
                                   br(),br(),
                                   h2("Nota: Para ampliar la descripción de cada campo y cómo se calcula, puede dar click en el indicador de interés."),
                                   h3("Fecha actualizacion: 25/06/2020")
                            ),
                            column(1)
                          )
                  ),
                  ### Individual ====
                  tabItem(tabName = "individual",
                          fluidPage(
                            title = "DB",
                            fluidRow(
                            box(title = "Información por empresa (Corte Mayo)",width=12,status="primary",solidHeader=TRUE,collapsible=FALSE,collapsed = TRUE,
                              fluidRow(
                                valueBoxOutput("info_empresa",width = 6),
                                bsTooltip("info_empresa", "Razón Social", 
                                          placement = "bottom", trigger = "click",options = NULL),
                                valueBoxOutput("info_pir1",width = 3),
                                bsTooltip("info_pir1", "Pirámide 1", 
                                          placement = "bottom", trigger = "click",options = NULL),
                                valueBoxOutput("info_pir2",width = 3),
                                bsTooltip("info_pir2", "Pirámide 2", 
                                          placement = "bottom", trigger = "click",options = NULL)
                                ),
                              fluidRow(
                                valueBoxOutput("conteo_empleados",width = 3),
                                bsTooltip("conteo_empleados", "Número de empleados", 
                                          placement = "bottom", trigger = "click",options = NULL),
                                valueBoxOutput("pro_salario",width = 3),
                                bsTooltip("pro_salario", "Promedio Salario", 
                                          placement = "bottom", trigger = "click",options = NULL),
                                valueBoxOutput("pro_edad",width = 3),
                                bsTooltip("pro_edad", "Promedio Edad", 
                                          placement = "bottom", trigger = "click",options = NULL),
                                valueBoxOutput("info_cluster",width = 3),
                                bsTooltip("info_cluster", "Cluster", 
                                          placement = "bottom", trigger = "click",options = NULL)
                              ),
                              fluidRow(br(),
                                       column(width = 4,
                                              withLoader(plotlyOutput("plot_pira_ind", height = 500), type = "html", loader = "loader1")),
                                       column(width = 4,
                                              withLoader(plotlyOutput("plot1", height = 500), type = "html", loader = "loader1")),
                                       column(width = 4,
                                              withLoader(plotlyOutput("plot2", height = 500), type = "html", loader = "loader1")),
                                       br()
                              )
                            )
                            ),
                            fluidRow(
                            box(title = "Gestion de Afiliados Cobertura",width=12,status="primary",solidHeader=FALSE,collapsible=TRUE,collapsed = TRUE,
                                br(),
                                h3("Corte Mayo"),
                                br(),
                                fluidRow(
                                  valueBoxOutput("conteo_famisanar",width = 3),
                                  bsTooltip("conteo_famisanar", "Afiliados con cobertura en Famisanar. Tiene como base el total de afiliados de UNIANDES", 
                                            placement = "bottom", trigger = "click",options = NULL),
                                  valueBoxOutput("conteo_pac_famisanar",width = 3),
                                  bsTooltip("conteo_pac_famisanar", "Afilidos con cobertura Plan Atención Complementario Famisanar. Tiene como base el total de afiliados con cobertura en Famisanar", 
                                            placement = "bottom", trigger = "click",options = NULL),
                                  valueBoxOutput("conteo_sura_eps",width = 3),
                                  bsTooltip("conteo_sura_eps", "Afiliados con cobertura Suramericana. Tiene como base el total de afiliados de UNIANDES", 
                                            placement = "bottom", trigger = "click",options = NULL),
                                  valueBoxOutput("conteo_pac_sura",width = 3),
                                  bsTooltip("conteo_pac_sura", "Afilidos con cobertura Plan Atención Complementario Suramericana. Tiene como base el total de afiliados con cobertura en Suramericana", 
                                            placement = "bottom", trigger = "click",options = NULL),
                                  valueBoxOutput("conteo_ips",width = 3),
                                  bsTooltip("conteo_ips", "Afiliados con cobertura IPS Colsubsidio. Tiene como base el total de afiliados con cobertura Famisanar", 
                                            placement = "bottom", trigger = "click",options = NULL),
                                  valueBoxOutput("conteo_cupo_credito",width = 3),
                                  bsTooltip("conteo_cupo_credito", "Afiliados con Crédito Cupo Activo", 
                                            placement = "bottom", trigger = "click",options = NULL),
                                  valueBoxOutput("conteo_data",width = 3),
                                  bsTooltip("conteo_data", "Corresponde al porcentaje de Afiliados con autorización de contactabilidad", 
                                            placement = "bottom", trigger = "click",options = NULL)
                                ),
                                br(),
                                h3("Consumos (Últimos 12 meses)"),
                                br(),
                                fluidRow(
                                  valueBoxOutput("conteo_consumo_credito",width = 3),
                                  bsTooltip("conteo_consumo_credito", "Afiliados con Uso de Créditos de Consumo", 
                                            placement = "bottom", trigger = "click",options = NULL),
                                  valueBoxOutput("conteo_uso_mes",width = 3),
                                  bsTooltip("conteo_uso_mes", "Afilidos con Uso de TMS en Convenios", 
                                            placement = "bottom", trigger = "click",options = NULL),
                                  valueBoxOutput("conteo_salud",width = 3),
                                  bsTooltip("conteo_salud", "Afiliados con consumo de productos de Salud", 
                                            placement = "bottom", trigger = "click",options = NULL),
                                  valueBoxOutput("conteo_supermercado",width = 3),
                                  bsTooltip("conteo_supermercado", "Afiliados con compras en Supermecado", 
                                            placement = "bottom", trigger = "click",options = NULL),
                                  valueBoxOutput("conteo_drogueria",width = 3),
                                  bsTooltip("conteo_drogueria", "Afiliados con compras en Droguerías", 
                                            placement = "bottom", trigger = "click",options = NULL),
                                  valueBoxOutput("conteo_vivienda",width = 3),
                                  bsTooltip("conteo_vivienda", "Afiliados con compra de vivienda y fecha entrega programada",
                                            placement = "bottom", trigger = "click",options = NULL),
                                  valueBoxOutput("conteo_educacion",width = 3),
                                  bsTooltip("conteo_educacion", "Número de hijos en Colegios Colsubsidio",
                                            placement = "bottom", trigger = "click",options = NULL),
                                  valueBoxOutput("conteo_ryt_hoteles",width = 3),
                                  bsTooltip("conteo_ryt_hoteles", "Afiliados con uso de Hoteles Colsubsidio", 
                                            placement = "bottom", trigger = "click",options = NULL),
                                  valueBoxOutput("conteo_ryt_club",width = 3),
                                  bsTooltip("conteo_ryt_club", "Afiliados con uso de Clubes Colsubsidio", 
                                            placement = "bottom", trigger = "click",options = NULL),
                                  valueBoxOutput("conteo_ryt_piscilago",width = 3),
                                  bsTooltip("conteo_ryt_piscilago", "Afiliados con uso de Piscilago", 
                                            placement = "bottom", trigger = "click",options = NULL),
                                  valueBoxOutput("conteo_ryt",width = 3),
                                  bsTooltip("conteo_ryt", "Afiliados con uso de al menos una vez en Hotel, Club, Piscilago", 
                                            placement = "bottom", trigger = "click",options = NULL),
                                 valueBoxOutput("conteo_subsidio_vivienda",width = 3),
                                 bsTooltip("conteo_subsidio_vivienda", "Corresponde al porcentaje de afiliados con Subsidio de Vivienda", 
                                           placement = "bottom", trigger = "click",options = NULL)
                                 )
                                )
                            ),
                            fluidRow(
                            box(title = "Gestion de Derechos",width=12,status="primary",solidHeader=FALSE,collapsible=TRUE,collapsed = TRUE,
                                fluidRow(
                                  valueBoxOutput("conteo_kit",width = 4),
                                  bsTooltip("conteo_kit", "Redención kit escolar. Se calcula mediante el cociente del número de afiliados que redimen y el número de afiliados con derecho a kit escolar (Consolidado 2020)", 
                                            placement = "bottom", trigger = "click",options = NULL),
                                  valueBoxOutput("conteo_monetarias",width = 4),
                                  bsTooltip("conteo_monetarias", "Redención cuota monetaria. Corresponde al cociente del número de afiliados que redimen y el número de afiliados con derecho a cuota monetaria (Consolidado 2020)", 
                                            placement = "bottom", trigger = "click",options = NULL),
                                  valueBoxOutput("conteo_lonchera",width = 4),
                                  bsTooltip("conteo_lonchera", "Redención cuota monetaria. Corresponde al cociente del número de afiliados que redimen y el número de afiliados con derecho a bono lonchera (Consolidado 2020)", 
                                            placement = "bottom", trigger = "click",options = NULL)
                                )
                            )
                            ),
                            fluidRow(
                              box(title = "Prospectos",width=12,status="primary",solidHeader=FALSE,collapsible=TRUE,collapsed = TRUE,
                                  fluidRow(
                                    valueBoxOutput("conteo_hoteles",width = 4),
                                    bsTooltip("conteo_hoteles", "Corresponde al porcentaje de afiliados Prospecto Hoteles", 
                                              placement = "bottom", trigger = "click",options = NULL),
                                    valueBoxOutput("conteo_piscilago",width = 4),
                                    bsTooltip("conteo_piscilago", "Corresponde al porcentaje de afiliados Prospecto Piscilago", 
                                              placement = "bottom", trigger = "click",options = NULL),
                                    valueBoxOutput("conteo_club",width = 4),
                                    bsTooltip("conteo_club", "Corresponde al porcentaje de afiliados Prospecto Clubes", 
                                              placement = "bottom", trigger = "click",options = NULL)
                                  ),
                                  fluidRow(
                                    valueBoxOutput("pre_aprobado_hipotecario",width = 6),
                                    bsTooltip("pre_aprobado_hipotecario", "Corresponde al porcentaje de afiliados Preaprobados Hipotecario", 
                                              placement = "bottom", trigger = "click",options = NULL),
                                    valueBoxOutput("pre_aprobado_cupo",width = 6),
                                    bsTooltip("pre_aprobado_cupo", "Corresponde al porcentaje de afiliados Preaprobados cupo", 
                                              placement = "bottom", trigger = "click",options = NULL)
                                  ),
                                  fluidRow(
                                    valueBoxOutput("conteo_cuad_a",width = 3),
                                    bsTooltip("conteo_cuad_a", "Afiliados en caso de tener Crédito hipotecario sin desembolsar, sin tener otorgado necesariamente subsidio de vivienda", 
                                              placement = "bottom", trigger = "click",options = NULL),
                                    valueBoxOutput("conteo_cuad_a1",width = 3),
                                    bsTooltip("conteo_cuad_a1", "Afiliados con subsidio asignado con vigencia menor a 6 meses", 
                                              placement = "bottom", trigger = "click",options = NULL),
                                    valueBoxOutput("conteo_cuad_a2",width = 3),
                                    bsTooltip("conteo_cuad_a2", "Afiliados con subsidio asignado con vigencia mayor de 6 meses", 
                                              placement = "bottom", trigger = "click",options = NULL),
                                    valueBoxOutput("conteo_cuad_b",width = 3),
                                    bsTooltip("conteo_cuad_b", "Afiliados con interés de compra (Basado en encuestas y diagnóstico de necesidades)", 
                                              placement = "bottom", trigger = "click",options = NULL)
                                  )
                              )
                            ),
                            fluidRow(
                              box(title = "Consumo por Empresa (Últimos 12 meses)", width = 12, status = "primary", solidHeader = FALSE, collapsible = TRUE, collapsed = TRUE,
                                  fluidRow(
                                    br(),
                                    column(1),
                                    column(10,
                                           h3("Consumo UES"),
                                           valueBoxOutput("conteo_ues1",width = 6),
                                           valueBoxOutput("conteo_ues2",width = 6),
                                           valueBoxOutput("conteo_ues3",width = 6),
                                           valueBoxOutput("conteo_ues4",width = 6),
                                           valueBoxOutput("conteo_ues5",width = 6)
                                           # plotlyOutput("consumo_valor_ues", height = 500)
                                           ),
                                    column(1),
                                    #        # plotlyOutput("consumo_transa_servicio", height = 500)
                                    #        ),
                                    br()
                                  ))
                            ))
                  )
                )
              )
)

