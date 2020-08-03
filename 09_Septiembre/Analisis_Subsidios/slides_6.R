# Nuevos datos

# Cargamos librerias
rm(list = ls())
library(RODBC); library(dplyr); library(data.table); library(esquisse)

# cargamos información de afiliados
consulta_sub <- fread("Analisis_Subsidios/consulta_subsidio_24102019.txt") %>% 
  mutate(Afiliado_Fecha_nacimiento = as.Date.character(gsub(" 00:00:00","",Persona_Fecha_nacimiento,fixed = T), format = "%d/%m/%Y"), 
         Beneficiario_Fecha_nacimiento = as.Date.character(gsub(" 00:00:00","",Persona_1_Fecha_nacimiento,fixed = T), format = "%d/%m/%Y")) %>% 
  mutate(Edad_afiliado = as.numeric(difftime(Sys.Date(),Afiliado_Fecha_nacimiento,units = "days"))/365,
         Edad_Beneficiario = as.numeric(difftime(Sys.Date(),Beneficiario_Fecha_nacimiento,units = "days"))/365,
         Edad_Beneficiario_mes = paste(year(Beneficiario_Fecha_nacimiento),month(Beneficiario_Fecha_nacimiento),sep = "_"))
str(consulta_sub)
saveRDS(consulta_sub, "Analisis_Subsidios/Salidas/consulta_sub24102019.rds")


# Cruzamos conbono lonchera
hijos_afil <- fread("Analisis_Subsidios/HijosAfiliados.csv") %>% 
  data.frame()
str(hijos_afil)

# Cruce con Bono Lonchera
con2 <- odbcDriverConnect( "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=//bogak08beimrodc/BI/Proteccion_Social/BonoLonchera.accdb")
# sqlTables(con2)
# sqlColumns(con2, "BonoLochera")
bono <- sqlFetch(con2, "BonoLochera")
str(bono)
odbcCloseAll()
table(paste(bono$AÑO_REDENCION,bono$MES_REDENCION,sep="-"))

# Bono lonchera
df_bono <- bono %>%
  mutate(anio_mes = paste(AÑO, MES, sep = "_")) %>% 
  filter(anio_mes == "2019_8") %>% 
  mutate(Fecha = as.Date.character(paste(AÑO, MES, 01, sep= "/"), format = "%Y/%m/%d"),
         id_persona = as.character(id_persona)) %>% 
  select(id_persona, Fecha, REDIMIO) %>% 
  mutate(redimio = ifelse(is.na(REDIMIO), "No", "Si"))
str(df_bono)

# Cuota monetaria
con3 <- odbcDriverConnect( "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=//bogak08beimrodc/BI/Proteccion_Social/GiroCuotaMonetaria2018.accdb")
# sqlTables(con3)
# sqlColumns(con3, "giro_cuotamonetaria")
giro_cm <- sqlFetch(con3, "giro_cuotamonetaria")
str(giro_cm)
odbcCloseAll()

df_girocm <- giro_cm %>% 
  mutate(anio_mes= paste(año,mes,sep="_")) %>% 
  filter(anio_mes == "2019_8") %>% 
  mutate(Fecha = as.Date.character(paste(año,mes,01,sep="/"), format = "%Y/%m/%d"),
         id_persona = as.character(id_persona)) %>% 
  select(id_persona, Fecha)
str(df_girocm)

# Kit Escolar
con4.3 <- odbcDriverConnect( "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=//bogak08beimrodc/BI/Proteccion_Social/Kit_Escolar_2019.accdb")
kit2019 <- sqlFetch(con4.3, "base_kit_escolar_2019")
str(kit2019)
odbcCloseAll()

df_kit2019 <- kit2019 %>% 
  select(id_persona, estado_kit_escolar, estado_bono_escolar)
str(df_kit2019)

bono_agru <- df_bono %>% 
  group_by(id_persona) %>% 
  summarise(n_bono_girado = n())
str(bono_agru)

kit_agru <- kit2019 %>% 
  group_by(id_persona) %>% 
  summarise(n_kit_girado = n())
str(kit_agru)

giro_cm_agru <- df_girocm %>% 
  group_by(id_persona) %>% 
  summarise(n_cm_girado = n())
str(giro_cm_agru)

# Cruce población
poblacion_sub_info <- consulta_sub %>% 
  mutate(bono_girado = ifelse(id_persona %in% df_bono$id_persona, "Si", "No"),
         kit_girada = ifelse(id_persona %in% df_kit2019$id_persona, "Si", "No"),
         cm_girada = ifelse(id_persona %in% df_girocm$id_persona, "Si", "No")) %>% 
  left_join(bono_agru, by = 'id_persona') %>% 
  left_join(kit_agru, by = 'id_persona') %>% 
  left_join(giro_cm_agru, by = 'id_persona')
str(poblacion_sub_info)

# Poblacion famisanar
afil_segmento_ind <- readRDS("Analisis_Subsidios/bd_afiliados_04092019.rds") %>% 
  select(id_persona,filial_famisanar)
str(afil_segmento_ind)

# Consulta Famisanar
consulta_famisanar <- fread("Analisis_Subsidios/Consulta_Famisanar.txt")
str(consulta_famisanar)

# Poblacion
poblacion_sub_info <- left_join(poblacion_sub_info,afil_segmento_ind, by = 'id_persona') %>% 
  mutate(famisanar_titular = ifelse(id_persona %in% consulta_famisanar$id_persona, "Si", "No"),
         famisanar_beneficiario = ifelse(id_persona_familiar %in% consulta_famisanar$id_persona, "Si", "No"))
str(poblacion_sub_info)
saveRDS(poblacion_sub_info, file = "Analisis_Subsidios/Salidas/poblacion_sub_info.rds")

# Cercania

# Conectamos a access - Pronosticos para: 
# Cuota monetaria girada, kit escolar, Bono lonchera & otros subsidios
rm(list = ls())
options(scipen = 999)
library(RODBC); library(data.table); library(dplyr); library(tidyr); library(forecast); library(ggplot2); library(readxl)

# Consolidada
consolidada <- readRDS("Analisis_Subsidios/ConsolidacionSEP2019.rds") %>% 
  select(id_persona,cx_persona,cy_persona,cx_empresa,cy_empresa,Salario) %>% 
  filter(!is.na(cx_persona) | !is.na(cx_empresa)) %>% 
  group_by(id_persona) %>% 
  arrange(desc(Salario)) %>% 
  filter(row_number()==1) %>% 
  ungroup() %>% 
  select(-Salario)
table(duplicated(consolidada$id_persona))
str(consolidada)

poblacion_sub_info_geo <- poblacion_sub_info %>% 
  select(id_persona,categoria) %>% 
  filter(categoria %in% c("A","B")) %>% 
  distinct() %>% 
  left_join(consolidada, by = c("id_persona"="id_persona"))
str(poblacion_sub_info_geo)

infraestructura <- readxl::read_excel("Analisis_Subsidios/INFRAESTRUCTURA_PROPIA_COLS.xlsx")
str(infraestructura)
table(infraestructura$TIPO)

supermercados <- infraestructura %>% 
  filter(TIPO == "SUPERMERCADOS")
saveRDS(supermercados, "Analisis_Subsidios/Salidas/supermercados_geo.rds")

droguerias <- infraestructura %>% 
  filter(TIPO == "DROGUERIA")
saveRDS(droguerias, "Analisis_Subsidios/Salidas/droguerias_geo.rds")


### Calculamos el supermercado mas cercano
library(geosphere)
bd_matriz_v <- data.frame(distm(poblacion_sub_info_geo[,c('cx_persona','cy_persona')], supermercados[,c('CX','CY')], fun=distVincentyEllipsoid)/1000)
str(bd_matriz_v)

# Matriz trabajan
bd_matriz_t <- data.frame(distm(poblacion_sub_info_geo[,c('cx_empresa','cy_empresa')], supermercados[,c('CX','CY')], fun=distVincentyEllipsoid)/1000)
str(bd_matriz_t)

# Calculo distancias
mat_v <- bd_matriz_v
poblacion_sub_info_geo$dis_v_super <- round(apply(mat_v[1:dim(mat_v)[2]],1,min),2)
# poblacion_sub_info_geo$point_v <- supermercados$NOMBRE[max.col(-mat_v)]

mat_t <- bd_matriz_t
poblacion_sub_info_geo$dis_t_super <- round(apply(mat_t[1:dim(mat_t)[2]],1,min),2)
# poblacion_sub_info_geo$point_t <- supermercados$NOMBRE[max.col(-mat_t)]

### Para la drogueria mas cercana
bd_matriz_v2 <- data.frame(distm(poblacion_sub_info_geo[,c('cx_persona','cy_persona')], droguerias[,c('CX','CY')], fun=distVincentyEllipsoid)/1000)
str(bd_matriz_v2)

# Matriz trabajan
bd_matriz_t2 <- data.frame(distm(poblacion_sub_info_geo[,c('cx_empresa','cy_empresa')], droguerias[,c('CX','CY')], fun=distVincentyEllipsoid)/1000)
str(bd_matriz_t2)

# Calculo distancias
mat_v2 <- bd_matriz_v2
poblacion_sub_info_geo$dis_v <- round(apply(mat_v2[1:dim(mat_v2)[2]],1,min),2)
# poblacion_sub_info_geopoblacion_sub_info_geo$point_v <- geo_supermercados$NOMBRE[max.col(-mat_v2)]

mat_t2 <- bd_matriz_t2
poblacion_sub_info_geo$dis_t <- round(apply(mat_t2[1:dim(mat_t2)[2]],1,min),2)
# df_kit_geo$point_t <- geo_supermercados$NOMBRE[max.col(-mat_t2)]

# Escribir base de datos
fwrite(df_bono_geo, "df_bono_geo.csv", row.names = F)
fwrite(df_kit_geo, "df_kit_geo.csv", row.names = F)

# Diagrama VENN

# venn_bono2019 <- poblacion_sub_info %>%
#   filter(n_bono_girado >= 1) %>% 
#   select(id_persona_familiar) %>% 
#   as.matrix() %>% 
#   as.vector()
venn_bono2019 <- df_bono %>% select(id_persona) %>% na.omit() %>% as.matrix() %>% as.vector()
length(unique(venn_bono2019))

# venn_kit2019 <- poblacion_sub_info %>%
#   filter(n_kit_girado >= 1) %>% 
#   select(id_persona) %>% 
#   as.matrix() %>% 
#   as.vector()
venn_kit2019 <- df_kit2019 %>% select(id_persona) %>% na.omit() %>% as.matrix() %>% as.vector()
length(unique(venn_kit2019))

# venn_girocm2019 <- poblacion_sub_info %>%
#   filter(n_cm_girado >= 1) %>% 
#   select(id_persona) %>% 
#   as.matrix() %>% 
#   as.vector()
venn_girocm2019 <- df_girocm %>% select(id_persona) %>% na.omit() %>% as.matrix() %>% as.vector()
length(unique(venn_girocm2019))

# Load library
library(VennDiagram)
library(RColorBrewer)
myCol <- brewer.pal(3, "Pastel2")

# Chart
venn.diagram(
  x = list(venn_bono2019, venn_girocm2019, venn_kit2019),
  main = "Participación Subsidio (Último Corte)",
  main.cex = 0.5,
  main.col = "gray",
  sub = "Afiliados",
  sub.cex = 0.2,
  sub.col = "gray",
  category.names = c("Bono Lonchera" , "Cuota Monetaria" , "Kit Escolar"),
  filename = 'Analisis_Subsidios/Venn_ultimo_corte.png',
  output=TRUE,
  
  # Output features
  imagetype="png" ,
  height = 600 , 
  width = 600 , 
  resolution = 400,
  compression = "lzw",
  
  # Circles
  lwd = 1,
  lty = 'blank',
  fill = myCol,
  
  # Numbers
  cex = .3,
  fontface = "bold",
  fontfamily = "sans",
  
  # Set names
  cat.cex = 0.3,
  cat.fontface = "bold",
  cat.default.pos = "outer",
  cat.pos = c(-27, 27, 135),
  cat.dist = c(0.055, 0.055, 0.085),
  cat.fontfamily = "sans",
  rotation = 1,
  print.mode=c("raw", "percent"),
  cat.col = "gray"
)

## Version 2
saveRDS(mat_t, "Analisis_Subsidios/mat_t.rds")
saveRDS(mat_t2, "Analisis_Subsidios/mat_t2.rds")
saveRDS(mat_v, "Analisis_Subsidios/mat_v.rds")
saveRDS(mat_v2, "Analisis_Subsidios/mat_v2.rds")
rm(mat_t, mat_t2, mat_v,mat_v2)
## Sample data
# if (!requireNamespace("BiocManager", quietly=TRUE))
#   install.packages("BiocManager")
# BiocManager::install("systemPipeR")
library(systemPipeR)
setlist <- list(A=venn_bono2019, B=venn_girocm2019, C=venn_kit2019)
class(setlist)
setlist[[1]]

## 3-way Venn diagram
vennset <- overLapper(setlist[1:3], type="vennsets", keepdups = TRUE)
vennPlot(vennset)


# Entrega Beneficiarios nacios 2019 por mes
library(writexl)
bd_afiliados <- readRDS("Analisis_Subsidios/bd_afiliados_24102019.rds") %>% 
  select(id_persona,Salario) %>% 
  group_by(id_persona) %>% 
  arrange(desc(Salario)) %>% 
  filter(row_number()==1) %>% 
  data.frame()
table(duplicated(bd_afiliados$id_persona))
str(bd_afiliados)


bd_entrega_sub <- consulta_sub %>% 
  left_join(bd_afiliados, by = "id_persona") %>% 
  mutate(gruposalario = case_when(Salario <= 828116 ~ "0 y 1 SMMLV",
                                  Salario > 828116 ~ "Mas de 1 SMMLV")) %>% 
  filter(categoria %in% c("A","B"),
         parentesco == "HIJO",
         marca_afiliado_unico == "X",
         year(Beneficiario_Fecha_nacimiento) %in% c(2018,2019)) %>% 
  select(gruposalario,id_persona_familiar,categoria,Edad_Beneficiario_mes) %>% 
  group_by(gruposalario,Edad_Beneficiario_mes,categoria) %>% 
  summarise(Conteo = n_distinct(id_persona_familiar)) %>% 
  data.frame()
str(bd_entrega_sub)
writexl::write_xlsx(bd_entrega_sub, path = "Analisis_Subsidios/bd_SMMLV_AyB.xlsx")

