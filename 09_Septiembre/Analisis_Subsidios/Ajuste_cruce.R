# Cargamos librerias
rm(list = ls())
library(RODBC); library(dplyr); library(data.table); library(esquisse)


# tic("Base de Datos de Hijos")

# con <- odbcDriverConnect( "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=//bogak08beimrodc/BI/Persona/Persona.accdb")
# # sqlTables(con)
# # sqlColumns(con, "Persona")
# edades <- sqlQuery(con , paste0("select Id_persona, Fecha_nacimiento from Persona"), as.is=T)
# odbcCloseAll()

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

# df_bono <- bono %>%
#   filter(AÑO_REDENCION == 2019) %>% 
#   mutate(Fecha = as.Date.character(paste(AÑO, MES, 01, sep= "/"), format = "%Y/%m/%d"),
#          id_persona = as.character(id_persona)) %>% 
#   select(id_persona, Fecha, REDIMIO) %>% 
#   left_join(hijos_afil, by = c("id_persona"="Id.Persona")) %>% 
#   mutate(n_hijos = rowSums(.[4:20], na.rm = T)) %>% 
#   group_by(Fecha) %>% 
#   summarise(n_beneficiarios = sum(n_hijos))
# str(df_bono)
# fwrite(df_bono, file = "Analisis_Subsidios/beneficarios_bono.csv", row.names = F)
# esquisser()

# df_bono <- bono %>%
#   filter(AÑO_REDENCION == 2019) %>% 
#   mutate(Fecha = as.Date.character(paste(AÑO, MES, 01, sep= "/"), format = "%Y/%m/%d"),
#          id_persona = as.character(id_persona)) %>% 
#   select(id_persona, Fecha, REDIMIO) %>% 
#   left_join(hijos_afil, by = c("id_persona"="Id.Persona")) %>% 
#   mutate(redimio = ifelse(is.na(REDIMIO), "No", "Si")) %>% 
#   select(id_persona,redimio,Edad_Hijos0:Edad_Hijos6) %>% 
#   mutate(Menor1 = Edad_Hijos0,
#          Entre1y2 = Edad_Hijos1+Edad_Hijos2, 
#          Entre3y4 = Edad_Hijos3+Edad_Hijos4,
#          Entre5y6 = Edad_Hijos5+Edad_Hijos6) %>% 
#   select(id_persona,redimio,Menor1,Entre1y2,Entre3y4,Entre5y6) %>% 
#   gather(key = "edad", value = "n_bene", 3:6) %>% 
#   group_by(edad,redimio) %>% 
#   summarise(n_bene = sum(n_bene, na.rm = T)) %>% 
#   spread(key = redimio, value = n_bene, fill = 0) %>% 
#   mutate(girado = Si + No,
#          redimido = Si) %>% 
#   select(edad,girado,redimido) %>% 
#   data.frame()
# fwrite(df_bono, "Analisis_Subsidios/bono_gir_red_edad.csv", row.names = F, sep = ";", dec = ",")

df_bono <- bono %>%
  mutate(anio_mes = paste(AÑO, MES, sep = "_")) %>% 
  filter(anio_mes == "2019_8") %>% 
  mutate(Fecha = as.Date.character(paste(AÑO, MES, 01, sep= "/"), format = "%Y/%m/%d"),
         id_persona = as.character(id_persona)) %>% 
  select(id_persona, Fecha, REDIMIO) %>% 
  left_join(hijos_afil, by = c("id_persona"="Id.Persona")) %>% 
  mutate(redimio = ifelse(is.na(REDIMIO), "No", "Si")) %>% 
  select(id_persona,redimio,Edad_Hijos0:Edad_Hijos6)
str(df_bono)

# GiroCuotaMonetaria2018    
con3 <- odbcDriverConnect( "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=//bogak08beimrodc/BI/Proteccion_Social/GiroCuotaMonetaria2018.accdb")
# sqlTables(con3)
# sqlColumns(con3, "giro_cuotamonetaria")
giro_cm <- sqlFetch(con3, "giro_cuotamonetaria")
str(giro_cm)
odbcCloseAll()

tx_giro_cm <- fread("Analisis_Subsidios/Redencion_CM.txt")
str(tx_giro_cm)

tx_giro_cm2018 <- fread("Analisis_Subsidios/Redencion_CM_2018.txt")
str(tx_giro_cm2018)

derecho_cm <- giro_cm %>% 
  mutate(id_persona = as.character(id_persona),
         anio_mes = as.character(paste0(año,mes))) %>% 
  select(anio_mes,id_persona) %>% 
  group_by(anio_mes) %>% 
  summarise(n_derecho_cm = n()) %>% 
  data.frame()
str(derecho_cm)

redimida_cm <- tx_giro_cm %>% 
  mutate(id_persona = as.character(id_persona),
         FechaTx = as.Date.character(FechaTx, "%d/%m/%Y"),
         anio_mes = as.character(paste(year(FechaTx), MES, sep = ""))) %>% 
  # mutate(valor = gsub(",", ".", VrTx, fixed = T)) %>% 
  # mutate(valor = as.numeric(gsub("$ ", "", valor, fixed = T))) %>% 
  # mutate(prox_cm = round(valor/31400, 0)) 
  select(anio_mes,id_persona) %>% 
  dplyr::group_by(anio_mes) %>% 
  summarise(n_redimida_cm = n())
str(redimida_cm)

redimida_cm2018 <- tx_giro_cm2018 %>% 
  mutate(id_persona = as.character(id_persona),
         FechaTx = as.Date.character(FechaTx, "%d/%m/%Y"),
         anio_mes = as.character(paste(year(FechaTx), MES, sep = ""))) %>% 
  # mutate(valor = gsub(",", ".", VrTx, fixed = T)) %>% 
  # mutate(valor = as.numeric(gsub("$ ", "", valor, fixed = T))) %>% 
  # mutate(prox_cm = round(valor/31400, 0)) 
  select(anio_mes,id_persona) %>% 
  dplyr::group_by(anio_mes) %>% 
  summarise(n_redimida_cm = n())
str(redimida_cm2018)

df_cm <- derecho_cm %>% 
  left_join(redimida_cm, by = "anio_mes") %>% 
  left_join(redimida_cm2018, by = "anio_mes") %>% 
  mutate(n_redimida_cm = ifelse(is.na(n_redimida_cm.x), n_redimida_cm.y , n_redimida_cm.x)) %>% 
  select(-c(n_redimida_cm.x,n_redimida_cm.y)) %>% 
  na.omit() %>% 
  mutate(porc = round(n_redimida_cm/n_derecho_cm, 2))
str(df_cm)

# esquisser()

library(ggplot2)
ggplot(df_cm) +
 aes(x = anio_mes, weight = porc) +
 geom_bar(fill = "#FF6666") +
 labs(x = "Fecha", y = "Proporción", title = "Redencíon Cuota Monetaria", subtitle = " ") +
 theme_minimal()



df_girocm <- giro_cm %>% 
  filter(año == 2019) %>% 
  mutate(Fecha = as.Date.character(paste(año,mes,01,sep="/"), format = "%Y/%m/%d"),
         id_persona = as.character(id_persona)) %>% 
  select(id_persona, Fecha) %>% 
  left_join(hijos_afil, by = c("id_persona"="Id.Persona")) %>% 
  mutate(n_hijos = rowSums(.[3:19], na.rm = T)) %>% 
  group_by(Fecha) %>% 
  summarise(n_beneficiarios = sum(n_hijos))
str(df_girocm)
fwrite(df_girocm, file = "Analisis_Subsidios/beneficarios_giro.csv", row.names = F)
esquisser()

# Kit Escolar    
con4.1 <- odbcDriverConnect( "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=//bogak08beimrodc/BI/Proteccion_Social/Kit_Escolar_2017.accdb")
kit2017 <- sqlFetch(con4.1, "kitescolar")
str(kit2017)
odbcCloseAll()

df_kit2017 <- kit2017 %>% 
  group_by(año) %>% 
  summarise(n_beneficiarios = n())
str(df_kit2017)

con4.2 <- odbcDriverConnect( "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=//bogak08beimrodc/BI/Proteccion_Social/Kit_Escolar_2018.accdb")
kit2018 <- sqlFetch(con4.2, "kit_escolar_consolidado")
str(kit2018)
odbcCloseAll()

df_kit2018 <- kit2018 %>% 
  mutate(año = 2018) %>% 
  group_by(año) %>% 
  
  summarise(n_beneficiarios = n())
  

con4.3 <- odbcDriverConnect( "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=//bogak08beimrodc/BI/Proteccion_Social/Kit_Escolar_2019.accdb")
kit2019 <- sqlFetch(con4.3, "base_kit_escolar_2019")
str(kit2019)
odbcCloseAll()

df_kit <- kit2019 %>% 
  mutate(Fecha = as.Date.character(paste(year(fecha_cargue), month(fecha_cargue), 01, sep = "/")), format = "%Y/%m/%d") %>% 
  select(id_persona,fecha_cargue) %>% 
  mutate(id_persona = as.character(id_persona)) %>% 
  inner_join(hijos_afil, by = c("id_persona"="Id.Persona")) %>% 
  data.frame()
str(df_kit)

prono_kit <- df_kit %>% 
  mutate(n_personas = rowSums(.[,6:13], na.rm = T)) %>%
  mutate(anio = year(fecha_cargue)) %>% 
  group_by(anio) %>% 
  summarise(n_beneficiarios = sum(n_personas, na.rm = T)) 
str(prono_kit)

df_kit2018 <- kit2018 %>% 
  mutate(anio = 2018,
         kit_girado = Total_kit_escolar_aprobado,
         kit_redimido = total_kit_escolar_redimido,
         Bono_kit_girado = Total_kit_escolar_aprobado,
         Bono_kit_redimido = total_bono_kit_escolar_redimido) %>% 
  group_by(anio) %>% 
  summarise(porc_redencion_kit = sum(kit_redimido)/sum(kit_girado),
            porc_redencion_bonokit = sum(Bono_kit_redimido)/sum(Bono_kit_girado))
str(df_kit2018)

df_kit2019 <- kit2019 %>% 
  select(id_persona, estado_kit_escolar, estado_bono_escolar) %>% 
  mutate(anio = 2019,
         kit_girado = 1,
         kit_redimido = ifelse(estado_kit_escolar == "ENTREGADO", 1, 0),
         Bono_kit_girado = 1,
         Bono_kit_redimido = ifelse(estado_bono_escolar == "ENTREGADO", 1, 0)) %>% 
  group_by(anio) %>% 
  summarise(porc_redencion_kit = sum(kit_redimido)/sum(kit_girado),
            porc_redencion_bonokit = sum(Bono_kit_redimido)/sum(Bono_kit_girado))
str(df_kit2019)

df_kit_red <- rbind(df_kit2018, df_kit2019) %>% 
  mutate(anio = as.factor(anio))
str(df_kit_red)
esquisser()

ggplot(df_kit_red) +
 aes(x = anio, weight = porc_redencion_kit) +
 geom_bar(fill = "#FF6666") +
 labs(x = "Año", y = "Proporción", title = "Redención Kit Escolar", subtitle = " ") +
 theme_minimal()

ggplot(df_kit_red) +
  aes(x = anio, weight = porc_redencion_bonokit) +
  geom_bar(fill = "#FF6666") +
  labs(x = "Año", y = "Proporción", title = "Redención Bono Kit Escolar", subtitle = " ") +
  theme_minimal()


# Consulta afiliados
afil <- readRDS("Analisis_Subsidios/Consolidacion.rds")
str(afil)


# df_kit %>% 
#   filter(!is.na(Edad_Hijos5)|
#            !is.na(Edad_Hijos6)|
#            !is.na(Edad_Hijos7)|
#            !is.na(Edad_Hijos8)|
#            !is.na(Edad_Hijos9)|
#            !is.na(Edad_Hijos10)|
#            !is.na(Edad_Hijos11)|
#            !is.na(Edad_Hijos12)) %>% dim()
# 
# df_kit %>% 
#   filter(!is.na(Edad_Hijos5)|
#            !is.na(Edad_Hijos6)|
#            !is.na(Edad_Hijos7)|
#            !is.na(Edad_Hijos8)|
#            !is.na(Edad_Hijos9)|
#            !is.na(Edad_Hijos10)|
#            !is.na(Edad_Hijos11)) %>% dim()
# df_kit %>% 
#   filter(!is.na(Edad_Hijos5)|
#            !is.na(Edad_Hijos6)|
#            !is.na(Edad_Hijos7)|
#            !is.na(Edad_Hijos8)|
#            !is.na(Edad_Hijos9)|
#            !is.na(Edad_Hijos10)) %>% dim()


# Afiliados con hijos por categoria
str(afil)
afil_edit <- afil %>% 
  select(id_persona,Categoria) %>% 
  mutate(Categoria = as.character(Categoria)) %>% 
  left_join(hijos_afil, by = c("id_persona"="Id.Persona")) %>% 
  data.frame() %>% 
  mutate(n_hijos = rowSums(.[,3:21], na.rm = T))
str(afil_edit)

library(tidyr)
test_bono <- afil_edit %>% 
  mutate(n_hijos = rowSums(.[,3:9], na.rm = T)) %>% 
  filter(Categoria %in% c("A","B")) %>% 
  group_by(Categoria, n_hijos) %>% 
  summarise(Beneficiarios = sum(n_hijos, na.rm = T)) %>% 
  data.frame()
str(test_bono)
fwrite(test_bono, "Analisis_Subsidios/consulta_edades_bono.csv", row.names = F, sep = ";", dec = ",")

test_kit_5_12 <- afil_edit %>% 
  mutate(n_hijos = rowSums(.[,8:15], na.rm = T)) %>% 
  filter(Categoria %in% c("A","B")) %>% 
  group_by(Categoria,n_hijos) %>% 
  summarise(beneficros_5_12 = sum(n_hijos, na.rm = T))
str(test_kit_5_12)

test_kit_6_12 <- afil_edit %>% 
  mutate(n_hijos = rowSums(.[,9:15], na.rm = T)) %>% 
  filter(Categoria %in% c("A","B")) %>% 
  group_by(Categoria) %>% 
  summarise(n_hijos_6_12 = sum(n_hijos, na.rm = T))
str(test_kit_6_12)

test_kit_7_12 <- afil_edit %>% 
  mutate(n_hijos = rowSums(.[,10:15], na.rm = T)) %>% 
  filter(Categoria %in% c("A","B")) %>% 
  group_by(Categoria) %>% 
  summarise(n_hijos_7_12 = sum(n_hijos, na.rm = T))
str(test_kit_7_12)

test_kit_8_12 <- afil_edit %>% 
  mutate(n_hijos = rowSums(.[,11:15], na.rm = T)) %>% 
  filter(Categoria %in% c("A","B")) %>% 
  group_by(Categoria) %>% 
  summarise(n_hijos_8_12 = sum(n_hijos, na.rm = T))
str(test_kit_8_12)

test_kit_9_12 <- afil_edit %>% 
  mutate(n_hijos = rowSums(.[,12:15], na.rm = T)) %>% 
  filter(Categoria %in% c("A","B")) %>% 
  group_by(Categoria) %>% 
  summarise(n_hijos_9_12 = sum(n_hijos, na.rm = T))
str(test_kit_9_12)

test_kit_10_12 <- afil_edit %>% 
  mutate(n_hijos = rowSums(.[,13:15], na.rm = T)) %>% 
  filter(Categoria %in% c("A","B")) %>% 
  group_by(Categoria) %>% 
  summarise(n_hijos_10_12 = sum(n_hijos, na.rm = T))
str(test_kit_10_12)

test_kit_11_12 <- afil_edit %>% 
  mutate(n_hijos = rowSums(.[,14:15], na.rm = T)) %>% 
  filter(Categoria %in% c("A","B")) %>% 
  group_by(Categoria) %>% 
  summarise(n_hijos_11_12 = sum(n_hijos, na.rm = T))
str(test_kit_11_12)

test_kit <- test_kit_5_12 %>% 
  left_join(test_kit_6_12, by = "Categoria") %>% 
  left_join(test_kit_7_12, by = "Categoria") %>% 
  left_join(test_kit_8_12, by = "Categoria") %>% 
  left_join(test_kit_9_12, by = "Categoria") %>% 
  left_join(test_kit_10_12, by = "Categoria") %>% 
  left_join(test_kit_11_12, by = "Categoria")
fwrite(test_kit, "Analisis_Subsidios/edades_kit.csv", row.names = F, sep = ";", dec = ",")

afil_cat <- afil %>%
  select(id_persona,Categoria,Salario) %>% 
  filter(Categoria %in% c("A","B")) %>% 
  mutate(salario_smmlv = Salario/828116) %>% 
  mutate(salario_smmlv_r = cut(salario_smmlv, breaks = c(0,1,1.5,2,2.5,3,max(salario_smmlv)), labels = c("0y1","1y1.5","1.5,2","2y2.5","2.5y3","3ymas"), 
                               right = F)) %>% 
  na.omit() %>% 
  group_by(Categoria,salario_smmlv_r) %>% 
  summarise(n_per = n_distinct(id_persona)) %>% 
  data.frame()
str(afil_cat)
fwrite(afil_cat, "Analisis_Subsidios/afil_cat_sal.csv", row.names = F, sep = ";", dec = ",")

# cargamos información de afiliados
consulta_sub <- fread("Analisis_Subsidios/consulta_subsidio.txt")
str(consulta_sub)

test_sub <- consulta_sub %>%
  filter(categoria %in% c("A","B"),
         marca_afiliado_unico == "X") %>%
  group_by(id_persona) %>% 
  mutate(n_hijo = sum(parentesco == "HIJO", na.rm = T),
         n_hijo_cat = as.character(ifelse(n_hijo > 3, "Mas de 3", n_hijo))) %>% 
  ungroup() %>% 
  select(id_persona_familiar, n_hijo_cat, categoria, Persona_1_Edad) %>% 
  mutate(r_edad_beneficiaro = cut(Persona_1_Edad, breaks = c(0,1,3,5,6,12,18,25,55,60,65,max(Persona_1_Edad)), 
                                  labels = c("0y1","1y3","3y5","5y6","6y12","12y18","18y25","25y55","55y60","60y65","65ymas"), 
                                  right = T)) %>% 
  na.omit() %>% 
  group_by(categoria,n_hijo_cat,r_edad_beneficiaro) %>% 
  summarise(n_benefios = n_distinct(id_persona_familiar)) %>% 
  data.frame()
table(consulta_sub$marca_afiliado_unico)

bono_agru <- bono %>% 
  filter(AÑO == 2019) %>% 
  group_by(id_persona) %>% 
  summarise(n_bono_girado = n())
str(bono_agru)

kit_agru <- kit2019 %>% 
  group_by(id_persona) %>% 
  summarise(n_kit_girado = n())
str(kit_agru)
  
giro_cm_agru <- giro_cm %>% 
  filter(año == 2019) %>% 
  group_by(id_persona) %>% 
  summarise(n_cm_girado = n(),
            valor_cm_girada = sum(valor))
str(giro_cm_agru)

# Sunkey graph
test_sub_info <- consulta_sub %>% 
  # mutate(derecho_bono = ifelse(categoria %in% c("A","B") & parentesco == "HIJO" & Persona_1_Edad %in% c(0:6), "Si", "No"),
  #        derecho_kit = ifelse(categoria %in% c("A","B") & parentesco == "HIJO" & Persona_1_Edad %in% c(5:12), "Si", "No"),
  #        derecho_cm = ifelse(categoria %in% c("A","B") & parentesco == "HIJO" & Persona_1_Edad %in% c(0:6), "Si", "No")) %>% 
  mutate(bono_girado = ifelse(id_persona %in% bono$id_persona, "Si", "No"),
         kit_girada = ifelse(id_persona %in% kit2019$id_persona, "Si", "No"),
         cm_girada = ifelse(id_persona %in% giro_cm$id_persona, "Si", "No")) %>% 
  mutate(n_bono_girado = ifelse(id_persona %in% bono_agru$id_persona, bono_agru$n_bono_girado, NA),
         n_kit_girada = ifelse(id_persona %in% kit_agru$id_persona, kit_agru$n_kit_girado, NA),
         n_cm_girada = ifelse(id_persona %in% giro_cm_agru$id_persona, giro_cm_agru$n_cm_girado, NA))

test_sub_info %>% 
  group_by(piramide_1,piramide_2) %>% 
  summarise(n_trabajadores = n_distinct(id_persona),
            n_benefiarios = n_distinct(id_persona_familiar)) %>% 
  fwrite(., "Analisis_Subsidios/pira_afil_bene.csv", row.names = F, sep = ";", dec = ",")


str(test_sub_info)
fwrite(test_sub_info, "Analisis_Subsidios/test_sub_info.csv", row.names = F, sep = ";", dec = ",")
saveRDS(test_sub_info, "Analisis_Subsidios/test_sub_info.rds")


# Parentesco
parentesco <- data.frame(table(consulta_sub$parentesco))
fwrite(parentesco, "Analisis_Subsidios/parentesco.csv", row.names = F, sep = ";", dec = ",")

# Tipo subsidios - grafico de venn
afil_consumo <- readRDS("Analisis_Subsidios/bd_afiliados_04092019.rds")
str(afil_consumo)


poblacion_sub <- afil_consumo %>% 
  select(id_persona, id_empresa, categoria, Bono_derecho:cuota_redimida) %>% 
  mutate(n_bono_girado = ifelse(id_persona %in% bono_agru$id_persona, bono_agru$n_bono_girado, NA),
         n_kit_girada = ifelse(id_persona %in% kit_agru$id_persona, kit_agru$n_kit_girado, NA),
         n_cm_girada = ifelse(id_persona %in% giro_cm_agru$id_persona, giro_cm_agru$n_cm_girado, NA)) %>% 
  filter(Bono_derecho == 1 | Bono_redimido == 1| kit_redimido == 1 | kit_derecho == 1 | cuota_derecho == 1| cuota_redimida == 1)
str(poblacion_sub)

poblacion_sub %>% 
  filter(categoria == "B") %>% 
  summarise(n_bono_girado = sum(n_bono_girado, na.rm = T),
            n_kit_girada = sum(n_kit_girada, na.rm = T), 
            n_cm_girada = sum(n_cm_girada, na.rm = T))


venn_bono2019 <- poblacion_sub %>%
  filter(n_bono_girado >= 1) %>% 
  select(id_persona) %>% 
  as.matrix() %>% 
  as.vector()
venn_bono2019

venn_kit2019 <- poblacion_sub %>%
  filter(n_kit_girada >= 1) %>% 
  select(id_persona) %>% 
  as.matrix() %>% 
  as.vector()
venn_kit2019

venn_girocm2019 <- poblacion_sub %>%
  filter(n_cm_girada >= 1) %>% 
  select(id_persona) %>% 
  as.matrix() %>% 
  as.vector()
venn_girocm2019

# Load library
library(VennDiagram)
library(RColorBrewer)
myCol <- brewer.pal(3, "Pastel2")

# Chart
venn.diagram(
  x = list(venn_bono2019, venn_girocm2019, venn_kit2019),
  main = "Participación Subsidio 2019",
  main.cex = 0.5,
  sub = "Afiliados",
  sub.cex = 0.2,
  category.names = c("Bono Lonchera" , "Cuota Monetaria" , "Kit"),
  filename = 'Analisis_Subsidios/venn_diagramm_catB.png',
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
  print.mode=c("raw", "percent")
)


## Grafico de redes
library(networkD3); library(htmlwidgets)
gra_redes <- test_sub %>% 
  select(categoria, r_edad_beneficiaro, n_benefios) %>% 
  data.frame()
str(gra_redes)

# nodos
nodes <- data.frame(
  name=c(as.character(gra_redes$categoria), 
         as.character(gra_redes$r_edad_beneficiaro)) %>% unique()
)

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
gra_redes$IDsource <- match(gra_redes$categoria, nodes$name)-1 
gra_redes$IDtarget <- match(gra_redes$r_edad_beneficiaro, nodes$name)-1

# Make the Network
p <- sankeyNetwork(Links = gra_redes, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "n_benefios", NodeID = "name", 
                   sinksRight=FALSE, fontSize = 12)
p


# tasas de redencion

t_bono <- bono %>% 
  mutate(fecha = as.Date.character(paste(AÑO,MES,01,sep="/"), "%Y/%m/%d")) %>% 
  group_by(fecha) %>% 
  summarise(n_girado = n(),
            n_redimido = sum(!is.na(REDIMIO))) %>% 
  mutate(porcentaje = round(n_redimido/n_girado,2))
str(t_bono)
esquisser()


ggplot(t_bono) +
 aes(x = fecha, weight = porcentaje) +
 geom_bar(fill = "#FF6666") +
 labs(x = "Fecha", y = "Proporción", title = "Tasa Redención Bono", subtitle = " ") +
 theme_minimal()


t_kit <- rbind(
  kit2018 %>% group_by()
  
)

# cargamos información de afiliados
consulta_cc <- fread("Analisis_Subsidios/Consulta_cc.txt") %>% 
  data.frame()
str(consulta_cc)

n<-1000
muestr_cc<- sample(1:nrow(consulta_cc),size=n,replace=FALSE)
muestr_cc

library(writexl)
test_cc_muestra <- consulta_cc[muestr_cc, ]

writexl::write_xlsx(
  list(
    test_cc_muestra = test_cc_muestra),
  # direccion donde se va a guardar
  paste0("Analisis_Subsidios/test_cc_muestra",as.character(Sys.Date()),'.xlsx')
  )

#### Consumos ====================
rm(list = ls())

conn_bono <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=D:/A COLSUBSIDIO/APP_Segmento_Ind/Data/Originales/BonoLonchera.accdb")
subset(sqlTables(conn_bono), TABLE_TYPE == "TABLE") 
bono <- sqlFetch(conn_bono, "BonoLochera") 
str(bono)
odbcClose(conn_bono)

df_bono <- bono %>% 
  filter(AÑO == 2019) %>% 
  select(id_persona,REDIMIO) %>% 
  mutate(id_persona = as.character(id_persona),
         Bono_derecho = 1,
         Bono_redimido = ifelse(is.na(REDIMIO),0,1)) %>% 
  select(-REDIMIO) %>% 
  group_by(id_persona) %>% 
  summarise(Bono_derecho = ifelse(sum(Bono_derecho)>=1,1,0),
            Bono_redimido = ifelse(sum(Bono_redimido)>=1,1,0)) %>% 
  left_join(hijos_afil, by = c("id_persona"="Id.Persona"))
str(df_bono)
table(duplicated(df_bono$id_persona))
rm(bono)

conn_kit_2019 <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=D:/A COLSUBSIDIO/APP_Segmento_Ind/Data/Originales/Kit_Escolar_2019.accdb")
subset(sqlTables(conn_kit_2019), TABLE_TYPE == "TABLE") 
kit_2019 <- sqlFetch(conn_kit_2019, "base_kit_escolar_2019") 
str(kit_2019)
odbcClose(conn_kit_2019)

df_kit <- kit_2019 %>% 
  select(id_persona,estado_kit_escolar) %>% 
  na.omit() %>% 
  group_by(id_persona,estado_kit_escolar) %>% 
  summarise(c_kit = n()) %>% 
  spread(estado_kit_escolar,c_kit, fill = 0) %>% 
  data.frame() %>% 
  mutate(kit_derecho = DISPONIBLE + ENTREGADO) %>% 
  dplyr::rename("kit_redimido"="ENTREGADO") %>% 
  mutate(id_persona = as.character(id_persona),
         kit_redimido = ifelse(kit_redimido == 0, 0, 1),
         kit_derecho = ifelse(kit_derecho == 0, 0, 1)) %>% 
  select(-DISPONIBLE) %>% 
  left_join(hijos_afil, by = c("id_persona"="Id.Persona"))
str(df_kit)
rm(kit_2019)


# cuota redimida y cuota derecho
conn_derecho_cm <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=D:/A COLSUBSIDIO/APP_Segmento_Ind/Data/Originales/Consulta_BD_SegInd.accdb")
subset(sqlTables(conn_derecho_cm), tableType = "SYSTEM TABLE")
df_derecho_cm <- sqlFetch(conn_derecho_cm, "giro_cuotamonetaria")
str(df_derecho_cm)
odbcClose(conn_derecho_cm)

df_cm <- df_derecho_cm %>% 
  mutate(id_persona = as.character(id_persona)) %>% 
  group_by(id_persona) %>%
  summarise(valor_girado = sum(valor, na.rm = T)) %>% 
  left_join(hijos_afil, by = c("id_persona"="Id.Persona")) 
str(df_cm)

# # Cruce con Bono Lonchera
# con_ventas <- odbcDriverConnect( "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=//bogak08beimrodc/bi/Consumo/Convenios/consumo_convenios.accdb")
# ventas <- sqlFetch(con_ventas, "Ventas")
# str(ventas)
# odbcCloseAll()

# saveRDS(ventas, file = "Analisis_Subsidios/ventas.rds")
library(readxl)
ventas <- readRDS("Analisis_Subsidios/ventas.rds") %>% 
  data.frame() %>% 
  mutate(CODIGO.DE.ESTABLECIMIENTO = as.character(CODIGO.DE.ESTABLECIMIENTO),
         TARJETA = as.character(TARJETA),
         NIT.ESTABLECIMIENTO = as.character(NIT.ESTABLECIMIENTO),
         anio = year(FECHA.PROCESO)) %>% 
  select(CODIGO.DE.ESTABLECIMIENTO,TARJETA,NOMBRE.ESTABLECIMIENTO,VALOR,UNIDAD.NEGOCIO,NIT.ESTABLECIMIENTO,CANAL,anio) %>% 
  filter(anio == 2019) %>% 
  group_by(TARJETA,NIT.ESTABLECIMIENTO) %>% 
  summarise(valor = sum(VALOR, na.rm = T)) %>% 
  data.frame()
str(ventas)

cobranzas <- fread("Analisis_Subsidios/cobranzas30_09_2019.csv", encoding = "UTF-8") %>% 
  data.frame() %>% 
  select(Tipo.Identificación,Nro.Identificación,Tarjeta,Id.Amparador) %>% 
  mutate(tipo_id = ifelse(Tipo.Identificación == 2, "CC",
                          ifelse(Tipo.Identificación == 3, "CE", "TI"))) %>%
  mutate(id_persona = paste(tipo_id,Nro.Identificación,sep = ""),
         Tarjeta = as.character(Tarjeta)) %>% 
  select(id_persona,Tarjeta,Id.Amparador)
str(cobranzas)

convenios <- read_excel("Analisis_Subsidios/Convenios_TMS_febrero2019.xlsx") %>% 
  data.frame() %>% 
  select(NIT,CATEGORIA,SUBCATEGORIA)
str(convenios)

ventas_cc <- ventas %>% 
  inner_join(cobranzas, c("TARJETA" = "Tarjeta")) %>% 
  inner_join(convenios, c("NIT.ESTABLECIMIENTO" = "NIT"))
str(ventas_cc)
sum(ventas$NIT.ESTABLECIMIENTO %in% convenios$NIT)

# Para Bono
consumos_bono <- df_bono %>% 
  inner_join(ventas_cc, by = c("id_persona"="id_persona")) %>% 
  mutate(n_hijos = as.character(rowSums(.[,4:22], na.rm = T)),
         valor_m = valor/1000000) %>% 
  data.frame()
str(consumos_bono)

tb_bono_consumo <- consumos_bono %>% 
  select(n_hijos,SUBCATEGORIA,valor_m) %>% 
  na.omit() %>% 
  group_by(n_hijos,SUBCATEGORIA) %>% 
  summarise(valor_m = round(sum(valor_m, na.rm = T), 1)) %>% 
  spread(key = n_hijos, value = valor_m) %>% 
  data.frame()
tb_bono_consumo
rownames(tb_bono_consumo) <- tb_bono_consumo$SUBCATEGORIA
tb_bono_consumo <- tb_bono_consumo[,c(2:8)]
names(tb_bono_consumo) <- gsub("X", "", names(tb_bono_consumo))
p1 <- ggballoonplot(tb_bono_consumo, fill = "value")+
  scale_fill_viridis_c(option = "C")+
  # scale_fill_gradientn(colors = my_cols)+
  ggtitle("Valor Consumo - Bono Lonchera por Categoria Convenio y N. Hijos")
p1
jpeg(filename="Analisis_Subsidios/consumo_bono.jpeg", height = 22, width = 36, res= 200, units = "cm")
p1               # Gráfico
dev.off()

#Para Kit
consumos_kit <- df_kit %>% 
  inner_join(ventas_cc, by = c("id_persona"="id_persona")) %>% 
  mutate(n_hijos = as.character(rowSums(.[,4:22], na.rm = T)),
         valor_m = valor/1000000) %>% 
  data.frame()
str(consumos_kit)

tb_kit_consumo <- consumos_kit %>% 
  select(n_hijos,SUBCATEGORIA,valor_m) %>% 
  na.omit() %>% 
  group_by(n_hijos,SUBCATEGORIA) %>% 
  summarise(valor_m = round(sum(valor_m, na.rm = T), 1)) %>% 
  spread(key = n_hijos, value = valor_m) %>% 
  data.frame()
tb_kit_consumo
rownames(tb_kit_consumo) <- tb_kit_consumo$SUBCATEGORIA
tb_kit_consumo <- tb_kit_consumo[,c(2:9)]
names(tb_kit_consumo) <- gsub("X", "", names(tb_kit_consumo))
p2 <- ggballoonplot(tb_kit_consumo, fill = "value")+
  scale_fill_viridis_c(option = "C")+
  # scale_fill_gradientn(colors = my_cols)+
  ggtitle("Valor Consumo - Kit Escolar por Categoria Convenio y N. Hijos")
p2
jpeg(filename="Analisis_Subsidios/consumo_kit.jpeg", height = 22, width = 36, res= 200, units = "cm")
p2               # Gráfico
dev.off()


#Para cm
consumos_cm <- df_cm %>% 
  inner_join(ventas_cc, by = c("id_persona"="id_persona")) %>% 
  mutate(n_hijos = as.character(rowSums(.[,3:21], na.rm = T)),
         valor_m = valor/1000000,
         valor_girado = valor_girado/1000000) %>% 
  data.frame()
str(consumos_cm)

tb_cm_consumo <- consumos_cm %>% 
  select(n_hijos,SUBCATEGORIA,valor_m) %>% 
  na.omit() %>% 
  group_by(n_hijos,SUBCATEGORIA) %>% 
  summarise(valor_m = round(sum(valor_m, na.rm = T), 1)) %>% 
  spread(key = n_hijos, value = valor_m) %>% 
  data.frame()
tb_cm_consumo
rownames(tb_cm_consumo) <- tb_cm_consumo$SUBCATEGORIA
tb_cm_consumo <- tb_cm_consumo[,c(2:9)]
names(tb_cm_consumo) <- gsub("X", "", names(tb_cm_consumo))
p3 <- ggballoonplot(tb_cm_consumo, fill = "value")+
  scale_fill_viridis_c(option = "C")+
  # scale_fill_gradientn(colors = my_cols)+
  ggtitle("Valor Consumo - Cuota Monetaria por Categoria Convenio y N. Hijos")
p3
jpeg(filename="Analisis_Subsidios/consumo_cm.jpeg", height = 22, width = 36, res= 200, units = "cm")
p3               # Gráfico
dev.off()

### test cc

test_cc <- read_excel("Analisis_Subsidios/test_cc_muestra2019-10-11.xlsx") %>% 
  data.frame() %>% 
  mutate(FECHA.TITULAR = as.Date.character(FECHA.TITULAR, "%d/%m/%Y"),
         FECHA.AFILIADO = as.Date.character(FECHA.AFILIADO, "%d/%m/%Y")) %>% 
  mutate(test_cc_tit = ifelse(Persona_Fecha_nacimiento == FECHA.TITULAR, 1, 0),
         test_cc_fami = ifelse(Persona_1_Fecha_nacimiento == FECHA.AFILIADO, 1, 0))
str(test_cc)
sum(test_cc$test_cc_tit, na.rm = T)
sum(test_cc$test_cc_fami, na.rm = T)
sum(is.na(test_cc$FECHA.TITULAR))
sum(is.na(test_cc$FECHA.AFILIADO))
table(duplicated(test_cc$id_persona))

bd_test <- test_cc %>% 
  group_by(test_cc_tit,test_cc_fami) %>% 
  summarise(conteo_tit = n_distinct(id_persona),
            conteo_afil = n_distinct(id_persona_familiar))
