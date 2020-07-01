# Consulta aportes
library(dplyr); library(data.table); library(RODBC); library(DT); library(janitor); library(tidyr)
library(RODBC); library(lubridate); library(tools)

### Graficos ====

# Aporte
fecha_filtro <- "2020-05-01"
channel <- odbcDriverConnect(
  "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=//Bogak08beimrodc/bi/Proteccion_Social/Aporte.accdb")
tb_aporte <- sqlQuery(channel, paste ("select * from aporte")) %>% 
  data.frame() 
str(tb_aporte)
odbcCloseAll()

df_aporte <- tb_aporte %>% 
  filter(fecha >= fecha_filtro) %>% 
  mutate(id_empresa =as.character(id_empresa)) %>% 
  select(-c(año,mes,fecha)) 
str(df_aporte)
table(duplicated(df_aporte$id_empresa))


# Consolidada
consolidada <- readRDS("//Bogak08beimrodc/bi/Base_Mes/ConsolidadosMensuales/ConsolidacionABR2020.rds")
names(consolidada)

empresas <- consolidada %>% 
  select(id_empresa:Num_cesantias) %>% 
  distinct() %>% 
  data.frame() %>% 
  filter(Piramide2 %in% c("1.1 Platinum","1.2 Premium",
                          "2.1 Gold","2.2 Silver",
                          "3.1 VIP","3.2 VIP Estándar",
                          "4.1 Estándar","4.2 Trans. Mas de 100 Trab.","4.3 Trans.Juridica Ent. 11 a 99 Trab.",
                          "4.4 Trans.Natural Ent. 11 a 99 Trab.","4.5 Transaccional",
                          "5.1 Colsubsidio")) %>% 
  mutate(Piramide2 = ifelse(Piramide2 %in% c("4.1 Estándar","4.2 Trans. Mas de 100 Trab.","4.3 Trans.Juridica Ent. 11 a 99 Trab.",
                                             "4.4 Trans.Natural Ent. 11 a 99 Trab.","4.5 Transaccional"),
                            "4. Micro", Piramide2),
         Piramide2 = ifelse(Piramide2 %in% c("5.1 Colsubsidio"), "5. Micro", Piramide2),
         Con_aporte = ifelse(id_empresa %in% df_aporte$id_empresa, "Si", "No"))
str(empresas)
  
test1 <- empresas %>% 
  group_by(Piramide2) %>% 
  summarise(Num_Empresas = n_distinct(id_empresa),
            Aporte = sum(promedio_aportes, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(Total_E = sum(Num_Empresas),
         Participacion_E = Num_Empresas/Total_E,
         Total_A = sum(Aporte),
         Participacion_A = Aporte/Total_A) %>% 
  select(-c(Total_E,Total_A,Aporte)) %>% 
  adorn_totals(dat = ., where =  "row") %>% 
  data.frame()
str(test1)
names(table(empresas$Piramide2))

test2 <- empresas %>% 
  group_by(Piramide2,Con_aporte) %>% 
  summarise(Num_Empresas = n_distinct(id_empresa)) %>% 
  ungroup() %>% 
  spread(key = "Con_aporte", value = "Num_Empresas", fill = 0) %>% 
  mutate(Part_Si = Si/sum(Si),
         Part_No = No/sum(No)) %>% 
  select(Piramide2, Si, Part_Si, No, Part_No) %>% 
  adorn_totals(dat = ., where =  "row") %>% 
  data.frame()
str(test2)
names(table(empresas$Piramide2))

test <- test1 %>% 
  left_join(test2, by = "Piramide2")
str(test)

dt1 <- datatable(test,
          options = list(searching = F, paging = F, scrollCollapse = F,
                         # columnDefs = list(list(className = "dt-left", targets = 3))
                         columnDefs = list(list(className = "dt-left"))
                         ),
          colnames = c("Piramide","Empresas","Part. Empresas", "Part. Aporte", 
                       "Aporte (Si)", "Part. Aporte (Si)", "Aporte (No)", "Part. Aporte (No)"),
          rownames = F) %>%
  formatCurrency("Num_Empresas", currency = "", interval = 3, digits = 0, mark = ",") %>% 
  formatCurrency("Si", currency = "", interval = 3, digits = 0, mark = ",") %>% 
  formatCurrency("No", currency = "", interval = 3, digits = 0, mark = ",") %>% 
  formatPercentage("Participacion_E", 1) %>%
  formatPercentage("Participacion_A", 1) %>%
  formatPercentage("Part_Si", 1) %>%
  formatPercentage("Part_No", 1) %>%
  formatStyle("Participacion_E",
              background = styleColorBar(test$Participacion_E, "steelblue", -90), 
              backgroundPosition = "left",
              backgroundSize = '95% 50%',
              backgroundRepeat = 'no-repeat') %>% 
  formatStyle("Participacion_A",
              background = styleColorBar(test$Participacion_A, "yellow", -90), 
              backgroundPosition = "left",
              backgroundSize = '95% 50%',
              backgroundRepeat = 'no-repeat') %>% 
  formatStyle("Part_Si",
              background = styleColorBar(test$Participacion_E, "green", -90), 
              backgroundPosition = "left",
              backgroundSize = '95% 50%',
              backgroundRepeat = 'no-repeat') %>% 
  formatStyle("Part_No",
              background = styleColorBar(test$Participacion_A, "red", -90), 
              backgroundPosition = "left",
              backgroundSize = '95% 50%',
              backgroundRepeat = 'no-repeat')
class(dt1)

# Reactable
# Para hacer en shiny https://rdrr.io/github/glin/reactable/man/reactable-shiny.html
library(reactable); library(sparkline)
df_aporte_gra <- tb_aporte %>% 
  filter(fecha >= "2019-06-01") %>% 
  mutate(id_empresa =as.character(id_empresa)) %>% 
  select(-c(año,mes)) %>% 
  left_join(empresas %>% select(id_empresa, Piramide2), by = "id_empresa") %>% 
  mutate(Piramide2= ifelse(is.na(Piramide2), "6. Otra", Piramide2)) %>% 
  group_by(Piramide2,fecha) %>% 
  summarise(Aporte = round(sum(aporte_4, na.rm = T)/1000000)) %>% 
  data.frame() %>% 
  select(-fecha)
str(df_aporte_gra)

graph_reactable <- df_aporte_gra %>% 
  group_by(Piramide2) %>%
  summarise(Aporte = list(Aporte)) %>%
  mutate(Boxplot = NA, Tendencia = NA)
str(graph_reactable)

dt2 <- reactable(graph_reactable, 
          columns = list(
            Aporte = colDef(cell = function(values) {
              sparkline(values, type = "bar", chartRangeMin = 0, 
                        chartRangeMax = max(df_aporte_gra$Aporte, na.rm = T),
                        width = 65, height = 30)
              }),
            Boxplot = colDef(cell = function(value, index) {
              sparkline(graph_reactable$Aporte[[index]], type = "box",
                        width = 65, height = 30)
              }),
            Tendencia = colDef(cell = function(value, index) {
              sparkline(graph_reactable$Aporte[[index]],
                        width = 65, height = 30)
              })
            )
          )
class(dt2)

### Ajuste Pressentacion 2020 Recaudo ====

# Recaudo
channel <- odbcDriverConnect(
  "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=//bogak08beimrodc/bi/Proteccion_Social/Recaudo.accdb"
)
sqlTables(channel)

recaudo <- sqlQuery( 
  channel , 
  paste ("select * from Recaudo"),
  as.is=T
) %>% 
  data.frame()  

odbcCloseAll()

str(recaudo)
table(recaudo$fuente)

funcion_fecha <- function(fecha){
  fecha <- as.character(fecha)
  fecha <- as.Date.character(paste(substr(fecha,1,4), substr(fecha, 5, nchar(fecha)), "01", sep = "-"), format = "%Y-%m-%d")
  return(fecha)
}

df_recaudo <- recaudo %>% 
  filter(año >= 2018) %>% 
  mutate(fecha_periodo_pago = funcion_fecha(periodo_pago),
         fecha_transaccion = as.Date.character(paste(año, mes, "01", sep = "-"), formar = "%Y-%m-%d") - 15) %>% 
  mutate(fecha_periodo_pago = paste0(year(fecha_periodo_pago), month(fecha_periodo_pago)),
         fecha_transaccion = paste0(year(fecha_transaccion), toupper(month(fecha_transaccion)))) %>% 
  mutate(TipoPago=case_when(fecha_periodo_pago < fecha_transaccion ~ "Pago anterior",
                            fecha_periodo_pago == fecha_transaccion ~ "Aporte",
                            fecha_periodo_pago > fecha_transaccion ~ "Anticipo")) %>%
  select(fecha_transaccion, fecha_periodo_pago, TipoPago, valor_planilla) %>%
  group_by(fecha_transaccion, TipoPago) %>%
  summarise(valor = sum(valor_planilla, na.rm = T)) %>%
  mutate(Participacion = 100*round(valor/sum(valor), 4)) %>%
  ungroup() %>%
  data.frame() %>% 
  mutate(TipoPago = ordered(factor(TipoPago,
                                   levels = c("Aporte", "Anticipo", "Pago anterior"),
                                   labels = c("Aporte", "Anticipo", "Pago anterior"))),
         fecha_transaccion = funcion_fecha(fecha_transaccion))
str(df_recaudo)

# ggplot(df_recaudo) +
#   aes(x = fecha_transaccion, fill = TipoPago, weight = Participacion) +
#   geom_bar() +
#   scale_fill_brewer(palette = "Paired") +
#   labs(x = "Fecha", y = "Porcentaje", title = "Participación según tipo de Aporte", 
#        subtitle = "", 
#        fill = "Tipo Aporte") +
#   coord_flip() +
#   scale_x_date(date_labels = "%Y/%b", date_breaks = "1 months") +
#   theme_ft_rc() +
#   geom_text(data=df_recaudo, aes(x = fecha_transaccion, y = Participacion, label = paste0(Participacion,"%")), size=3,
#             color = "white", position = position_stack(vjust = .5))

library(esquisse); library(ggplot2)
# esquisser()

df_recaudo_ajus <- df_recaudo %>% 
  mutate(Participacion = ifelse(Participacion < 7, NA, Participacion))
str(df_recaudo_ajus)

ggplot(df_recaudo) +
  aes(x = fecha_transaccion, fill = TipoPago, weight = Participacion) +
  geom_bar() +
  scale_x_date(date_labels = "%Y %b", date_breaks = "4 months") +
  scale_fill_brewer(palette = "Set2") +
  labs(y = "Porcentaje", x = "Fecha", title = "Participación según tipo de Aporte", subtitle = "", 
       fill = "Tipo de Pago:") +
  theme_ft_rc() +
  theme(legend.position = "bottom") +
  geom_text(data=df_recaudo_ajus, aes(x = fecha_transaccion, y = Participacion, 
                                      label = paste0(Participacion,"%")), size = 2,
            color = "white", position = position_stack(vjust = .5)) + 
  coord_flip()

df_tasa_cre <- df_recaudo %>% 
  filter(TipoPago == "Aporte") %>% 
  select(fecha_transaccion, valor) %>% 
  mutate(Tasa = round(100*(valor-lag(valor))/lag(valor), 1), 
         Baja = ifelse(Tasa >=0, "Si", "No"))
str(df_tasa_cre)
esquisse::esquisser()

df_tasa_cre <- df_tasa_cre %>%
 filter(!is.na(Baja))

df_tasa_cre_mas <- df_tasa_cre %>% 
  filter(Tasa >= 0)
df_tasa_cre_menos <- df_tasa_cre %>% 
  filter(Tasa < 0)

library(ggplot2)

ggplot(df_tasa_cre) +
  aes(x = fecha_transaccion, fill = Baja, weight = Tasa) +
  geom_bar() +
  scale_fill_hue() +
  labs(x = "Fecha", y = "Tasa", title = "Tasa de Creciemiento de Aporte", fill = "Aumento") +
  geom_text(data=df_tasa_cre_mas, aes(y=Tasa, label=Tasa), size=3, fontface="italic", hjust = -0.25) +
  geom_text(data=df_tasa_cre_menos, aes(y=Tasa, label=Tasa), size=3, fontface="italic", hjust = 1.0) +
  scale_x_date(date_labels = "%Y %b", date_breaks = "4 months") + 
  coord_flip() +
  theme_ft_rc()
  
