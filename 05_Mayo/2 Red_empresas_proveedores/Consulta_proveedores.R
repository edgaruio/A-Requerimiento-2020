# Descriptivo proveedores
library(readxl); library(dplyr)

proveedores <- read_excel("Total proveedores 02-12-2019.xlsx", sheet = "BD") %>% 
  data.frame() %>% 
  select(NIT,NIT.CON.DIG,Estado.Afiliación,Tipo.Empresa.CRM,No..Trabajadores,Origen.Proveedor) %>% 
  mutate_all(as.character) %>% 
  filter(!is.na(NIT))
str(proveedores)
table(duplicated(proveedores$NIT.CON.DIG))

consolidado_emp <- readRDS("//Bogak08beimrodc/bi/Base_Mes/ConsolidadosMensuales/ConsolidacionMAR2020.rds") %>% 
  select(id_empresa:Num_cesantias) %>% 
  distinct() %>% 
  select(id_empresa,RazonSocial,NumIdEmpresa,NumIdEmpresa_SD,Piramide1,Piramide2,CIIU:DescripcionCIIU,tipo_empresa,
         cx_empresa,cy_empresa,LocalidadEmpresa,MunicipioEmpresa,
         NumEmpleados,Gen_F,Gen_M,Cat_A,Cat_B,Cat_C) %>% 
  mutate_at(.vars = c("NumIdEmpresa","NumIdEmpresa_SD","CIIU"), .funs = as.character)
str(consolidado_emp)

rentabilidad <- readRDS("consolidada_rentabilidad.rds") %>% 
  mutate_at(.vars = c("NumIdEmpresa","NumIdEmpresa_SD"), .funs = as.character) %>% 
  select(NumIdEmpresa,NumIdEmpresa_SD,aporte1,remanente_neto1,consumo_emp1,consumo_ind1)
str(rentabilidad)

tbciiu <- consolidado_emp %>% 
  group_by(CIIU,SectorCIIU) %>% 
  summarise(conteo = n()) %>% 
  data.frame() %>% 
  select(CIIU, SectorCIIU_noafil = SectorCIIU,-conteo)
str(tbciiu)  

empresas_noafil <- readRDS("EmpresasSCRAPPED.rds") %>% 
  select(nit,ciiu,depto,mpio) %>% 
  mutate(num_idempresa = substr(nit, 1, nchar(nit) - 2),
         CIIU_noafil = substr(gsub("\\D","",ciiu), 1, 4)) %>% 
  select(num_idempresa,CIIU_noafil) %>% 
  left_join(tbciiu, by = c("CIIU_noafil"="CIIU"))
str(empresas_noafil)

str(rentabilidad)
df_proveedores <- proveedores %>% 
  left_join(consolidado_emp, by = c("NIT"="NumIdEmpresa_SD")) %>% 
  left_join(rentabilidad, by = c("NIT"="NumIdEmpresa_SD")) %>% 
  left_join(empresas_noafil, by = c("NIT"="num_idempresa")) %>% 
  mutate(proveedor = ifelse(!is.na(id_empresa), "Afiliado", "No afiliado"))
sum(is.na(df_proveedores$CIIU_noafil)) 
str(df_proveedores)

df_proveedores_col <- df_proveedores %>% 
  filter(proveedor == "Afiliado")
table(duplicated(df_proveedores_col$id_empresa))
saveRDS(df_proveedores_col, "proveedores_col.rds")

sort(unique(consolidado_emp$SectorCIIU))
names_obj1 <- c("Comercio al por Mayor y al por Menor",
                "Industria")
names_obj2 <- c("Comercio al por mayor y al por menor",
               "Hidrocarburos",
               "Elaboración de productos alimenticios",
               "Fabricación de productos textiles",
               "Industria")

df_empresas_obj <- consolidado_emp %>% 
  filter(SectorCIIU %in% names_obj1) %>% 
  filter(ActividadCIIU %in% names_obj2) %>% 
  filter(tipo_empresa == "Aporte 4%")
unique(consolidado_emp$tipo_empresa)
saveRDS(df_empresas_obj, "df_empresas_obj.rds")

library(esquisse); library(ggplot2); library(stringr)
esquisse::esquisser()


# Analisis descriptivo
sum(df_proveedores$aporte1, na.rm = T)/1000000
sum(df_proveedores$remanente_neto1, na.rm = T)/1000000
sum(df_proveedores$NumEmpleados, na.rm = T)
sum(df_proveedores$Gen_F, na.rm = T)
sum(df_proveedores$Gen_M, na.rm = T)
sum(df_proveedores$Cat_A, na.rm = T)
sum(df_proveedores$Cat_B, na.rm = T)
sum(df_proveedores$Cat_C, na.rm = T)
sum(df_proveedores$consumo_emp1, na.rm = T)/1000000
sum(df_proveedores$consumo_ind1, na.rm = T)/1000000

df_proveedores %>% 
  group_by(Origen.Proveedor) %>% 
  summarise(conteo = n_distinct(NIT.CON.DIG)) %>% 
  ungroup() %>% 
  mutate(participacion = conteo/sum(conteo)) %>% 
  arrange(desc(participacion))
df_proveedores %>% 
  group_by(tipo_empresa) %>% 
  summarise(conteo = n_distinct(NIT.CON.DIG)) %>% 
  ungroup() %>% 
  mutate(participacion = conteo/sum(conteo)) %>% 
  arrange(desc(participacion))


tb0 <- df_proveedores %>% 
  mutate(UES = case_when(
    Origen.Proveedor == "Clinicas" ~ "Salud",
    Origen.Proveedor == "Contratistas" ~ "Otras",
    Origen.Proveedor == "Medicamentos" ~ "Medicamentos",
    Origen.Proveedor == "Supermercado" ~ "Supermercado",
    Origen.Proveedor == "Tecnologia" ~ "Otras"
    )) %>% 
  group_by(SectorCIIU_noafil,UES) %>% 
  summarise(conteo = n_distinct(NIT.CON.DIG)) %>% 
  ungroup() %>% 
  mutate(participacion = round(100*conteo/sum(conteo), 4)) %>% 
  mutate(SectorCIIU_noafil = ifelse(is.na(SectorCIIU_noafil), "Otros", SectorCIIU_noafil))
tb0
sum(tb0$conteo)

ggplot(tb0) +
  aes(x = UES, y = SectorCIIU_noafil, fill = conteo) +
  geom_tile(size = 1L) +
  scale_fill_gradient() +
  scale_y_discrete(
    limits = rev(levels(as.factor(tb0$SectorCIIU_noafil))),
    labels = function(x) str_wrap(x, width = 25)) +
  # scale_x_discrete(limits = c("Medicamentos", "Supermercados", "Salud", "Otras")) +
  labs(x = "UES", y = "Sector CIIU", title = "Distribución Proveedores (Total)", fill = "Conteo") +
  theme_minimal() +
  theme(text = element_text(size=13)) +
  # geom_text(aes(label = paste0(round(participacion, 1), " %")), size = 6, col = "gray")
  geom_text(aes(label = formatC(conteo, format = "d", digits = 0, big.mark = ",", )), size = 5, col = "gray")

tb0 %>% 
  group_by(UES) %>% 
  summarise(conteo = sum(conteo)) %>% 
  ungroup() %>% 
  mutate(participacion = conteo/sum(conteo))


tb1 <- df_proveedores %>% 
  filter(proveedor == "Afiliado") %>% 
  group_by(SectorCIIU,Origen.Proveedor) %>% 
  summarise(conteo = n_distinct(NIT.CON.DIG)) %>% 
  ungroup() %>% 
  mutate(participacion = round(100*conteo/sum(conteo), 4))
tb1

ggplot(tb1) +
 aes(x = Origen.Proveedor, y = SectorCIIU, fill = conteo) +
 geom_tile(size = 1L) +
 scale_fill_gradient() +
  scale_y_discrete(
    limits = rev(levels(as.factor(tb1$SectorCIIU))),
    labels = function(x) str_wrap(x, width = 25)) +
 labs(x = "Origen Proveedor", y = "Sector CIIU", title = "Distribución Proveedores Afiliados", fill = "Conteo") +
 theme_minimal() +
  theme(text = element_text(size=13)) +
  # geom_text(aes(label = paste0(round(participacion, 1), " %")), size = 6, col = "gray")
  geom_text(aes(label = formatC(conteo, format = "d", digits = 0, big.mark = ",", )), size = 5, col = "gray")

tb2 <- df_proveedores %>% 
  filter(proveedor == "Afiliado") %>% 
  group_by(Piramide2,Origen.Proveedor) %>% 
  summarise(conteo = n_distinct(NIT.CON.DIG)) %>% 
  ungroup() %>% 
  mutate(participacion = round(100*conteo/sum(conteo), 4))
tb2

ggplot(tb2) +
  aes(x = Origen.Proveedor, y = Piramide2, fill = conteo) +
  geom_tile(size = 1L) +
  scale_fill_gradient() +
  scale_y_discrete(
    limits = rev(levels(as.factor(tb2$Piramide2))),
    labels = function(y) str_wrap(y, width = 25)) +
  labs(x = "Origen Proveedor", y = "Piramide", title = "Distribución Proveedores Afiliados", fill = "Conteo") +
  theme_minimal() +
  theme(text = element_text(size=13)) +
  # geom_text(aes(label = paste0(round(participacion, 1), " %")), size = 5, col = "gray")
  geom_text(aes(label = formatC(conteo, format = "d", digits = 0, big.mark = ",", )), size = 5, col = "gray")


# c("Platinum","Premium","Gold","Silver","VIP","VIP Estándar","Estándar","Transaccional")

tb3 <- df_proveedores %>% 
  filter(proveedor == "Afiliado") %>% 
  group_by(Piramide2,Origen.Proveedor) %>% 
  summarise(conteo = sum(NumEmpleados)) %>% 
  ungroup() %>% 
  mutate(participacion = round(100*conteo/sum(conteo), 4))
tb3

ggplot(tb3) +
  aes(x = Origen.Proveedor, y = Piramide2, fill = conteo) +
  geom_tile(size = 1L) +
  scale_fill_gradient() +
  scale_y_discrete(
    limits = rev(levels(as.factor(tb2$Piramide2))),
    # limits = rev(c("Platinum","Premium","Gold","Silver","VIP","VIP Estándar","Transaccional")),
    labels = function(y) str_wrap(y, width = 25)) +
  labs(x = "Origen Proveedor", y = "Piramide", title = "Distribución empleados de empresas proveedores", fill = "Empleados") +
  theme_minimal() +
  theme(text = element_text(size=13)) +
  # geom_text(aes(label = paste0(round(participacion, 1), " %")), size = 5, col = "gray")
  geom_text(aes(label = formatC(conteo, format = "d", digits = 0, big.mark = ",", )), size = 5, col = "gray")

# Mapa
library(leaflet)
df_proveedores_mapa <- filter(df_proveedores, !is.na(cx_empresa))
pal <- colorFactor(palette = 'Dark2',domain = df_proveedores_mapa$Piramide1)
map <- leaflet(options = leafletOptions(zoomControl = FALSE, attributionControl=FALSE, minZoom = 8, maxZoom = 16))
map %>%
  addProviderTiles(provider = "CartoDB.Positron",
                   options = providerTileOptions(opacity = 1)) %>%
  addCircleMarkers(data = df_proveedores, lng = ~cx_empresa, lat = ~cy_empresa, fillOpacity = 0.6, 
                   color = ~pal(df_proveedores_mapa$Piramide1), stroke = FALSE, 
                   radius = ~ifelse(df_proveedores_mapa$Piramide1 == "1 Emp Grandes", 12,
                                    ifelse(df_proveedores_mapa$Piramide1 == "2 Emp Medio", 9,
                                           ifelse(df_proveedores_mapa$Piramide1 == "3 Empresas Pymes", 6, 
                                                  ifelse(df_proveedores_mapa$Piramide1 == "4 Micro", 2 , 2))))) %>% 
  addLegend(pal=pal, values= df_proveedores_mapa$Piramide1, opacity=0.7, title = "Piramide 1", position = "bottomright") 
# %>%
#   addPolygons(data=localidad, fill = F, stroke = T, color = "navy", weight = 1) %>%
#   addPolygons(data=cundi, fill = F, stroke = T, color = "red", weight = 1) %>%


df_proveedores %>% 
  group_by(MunicipioEmpresa) %>% 
  summarise(conteo = n_distinct(NIT.CON.DIG)) %>% 
  arrange(desc(conteo))

library(tidyr); library(writexl)
matriz_sector <- consolidado_emp %>% 
  group_by(SectorCIIU) %>% 
  summarise(conteo = n_distinct(id_empresa)) %>% 
  ungroup() %>% 
  mutate(SectorCIIU2 = SectorCIIU,
         participacion = conteo/sum(conteo)) %>% 
  select(SectorCIIU, SectorCIIU2, participacion) %>% 
  na.omit() %>% 
  spread(SectorCIIU2,participacion)
matriz_sector
write_xlsx(matriz_sector, "matriz_sector.xlsx")
unique(matriz_sector$SectorCIIU)

library(DescTools)

names1 <- c("Agricultura, Caza, Silvicultura y pesca"                                                   
            ,"Comercio al por Mayor y al por Menor"                                                      
            ,"Construcción, demoliciones, terrenos, Vías."                                               
            ,"Electricidad, Gas, Agua y explotación de minas"                                            
            ,"Industria"                                                                                 
            ,"Otros"                                                                                     
            ,"Prestación de Servicios"
            ,"Público"                                                                                   
            ,"Temporales"                                                                                
            ,"Transporte y Almacenamiento")

tab <- matrix(sample(c(0:100),size = 100), ncol = 10, byrow = T)
class(tab)
dimnames(tab) <- list(names1, names1)

PlotCirc(
  tab
  ,
  acol = c("dodgerblue","seagreen2","limegreen","olivedrab2","goldenrod2","tomato2"),
  rcol = SetAlpha(c("red","orange","olivedrab1"), 0.5)
)

### Pruebas

# Libraries
# library(tidyverse)
library(hrbrthemes)
library(circlize)
library(kableExtra)
library(viridis)
library(igraph)
library(ggraph)
library(colormap)

# Load dataset from github
data <- read.table("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/13_AdjacencyDirectedWeighted.csv", header=TRUE)
class(data)
class(tab)


library(tibble)
# I need a long format
df_tab <- tab %>% 
  data.frame()
class(df_tab)

colnames(df_tab) <- names1
rownames(df_tab) <- colnames(df_tab)

matriz_sector <- read_excel("matriz_sector.xlsx") %>% 
  select(-1)
str(matriz_sector)

# short names
colnames(matriz_sector) <- names1
rownames(matriz_sector) <- colnames(matriz_sector)

data_long <- matriz_sector %>%
  rownames_to_column %>%
  gather(key = 'key', value = 'value', -rowname)

# parameters
circos.clear()
circos.par(start.degree = 90, gap.degree = 4, track.margin = c(-0.1, 0.1), points.overflow.warning = FALSE)
par(mar = rep(0, 4))

# color palette
mycolor <- viridis(10, alpha = 1, begin = 0, end = 1, option = "D")
# mycolor <- mycolor[sample(1:10)]

# Base plot
chordDiagram(
  x = data_long, 
  grid.col = mycolor,
  transparency = 0.25,
  directional = 1,
  direction.type = c("arrows", "diffHeight"), 
  diffHeight  = -0.04,
  annotationTrack = "grid", 
  annotationTrackHeight = c(0.05, 0.1),
  link.arr.type = "big.arrow", 
  link.sort = TRUE, 
  link.largest.ontop = TRUE)

# Add text and axis
circos.trackPlotRegion(
  track.index = 1, 
  bg.border = NA, 
  panel.fun = function(x, y) {
    
    xlim = get.cell.meta.data("xlim")
    sector.index = get.cell.meta.data("sector.index")
    
    # Add names to the sector.
    circos.text(
      x = mean(xlim),
      y = 10,
      labels = sector.index,
      facing = "downward",
      cex = 0.7
    )
    
    # # Add graduation on axis
    # circos.axis(
    #   h = "top",
    #   major.at = seq(from = 0, to = xlim[2], by = ifelse(test = xlim[2]>10, yes = 2, no = 1)),
    #   minor.ticks = 1,
    #   major.tick.percentage = 0.5,
    #   labels.niceFacing = FALSE)
  }
)

