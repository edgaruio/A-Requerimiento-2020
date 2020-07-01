# Cargamos librerias
rm(list = ls())
library(RODBC); library(dplyr); library(data.table)

consolidada <- readRDS('//BOG08HERNYATT/Analitica/BaseConsolidada/ConsolidacionABR2020.rds') %>% 
  filter(marca_afiliado_unico) %>% 
  select(id_persona, numero_cuota_monetaria)
str(consolidada)
sort(names(consolidada))

# cargamos información de afiliados
consulta_sub <- fread("consulta_subsidio.txt") %>% 
  filter(marca_afiliado_unico  == "X") %>% 
  mutate(Afiliado_Fecha_nacimiento = as.Date.character(gsub(" 00:00:00","",Persona_Fecha_nacimiento,fixed = T), format = "%d/%m/%Y"), 
         Beneficiario_Fecha_nacimiento = as.Date.character(gsub(" 00:00:00","",Persona_1_Fecha_nacimiento,fixed = T), format = "%d/%m/%Y")) %>% 
  mutate(Edad_afiliado = as.numeric(difftime(Sys.Date(),Afiliado_Fecha_nacimiento,units = "days"))/365,
         Edad_Beneficiario = as.numeric(difftime(Sys.Date(),Beneficiario_Fecha_nacimiento,units = "days"))/365,
         Edad_Beneficiario_mes = paste(year(Beneficiario_Fecha_nacimiento),month(Beneficiario_Fecha_nacimiento),sep = "_")) %>% 
  left_join(consolidada, by = "id_persona") %>% 
  mutate(cuota_monetaria = ifelse(numero_cuota_monetaria >= 1, "Si", "No"))
str(consulta_sub)
table(duplicated(consulta_sub$id_persona))

# Cruce con Bono Lonchera
con2 <- odbcDriverConnect( "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=//bogak08beimrodc/BI/Proteccion_Social/BonoLonchera.accdb")
bono <- sqlFetch(con2, "BonoLochera")
str(bono)
odbcCloseAll()
table(paste(bono$AÑO_REDENCION,bono$MES_REDENCION,sep="-"))

# Bono lonchera
df_bono <- bono %>%
  mutate(anio_mes = paste(AÑO, MES, sep = "_")) %>% 
  filter(AÑO == 2020) %>% 
  mutate(Fecha = as.Date.character(paste(AÑO, MES, 01, sep= "/"), format = "%Y/%m/%d"),
         id_persona = as.character(id_persona)) %>% 
  select(id_persona, Fecha, REDIMIO) %>% 
  mutate(redimio = ifelse(is.na(REDIMIO), "No", "Si"))
str(df_bono)

# Cuota monetaria
con3 <- odbcDriverConnect( "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=//bogak08beimrodc/BI/Proteccion_Social/GiroCuotaMonetaria2018.accdb")
giro_cm <- sqlFetch(con3, "giro_cuotamonetaria")
str(giro_cm)
odbcCloseAll()

df_girocm <- giro_cm %>% 
  mutate(anio_mes= paste(año,mes,sep="_")) %>% 
  filter(año == 2020) %>% 
  mutate(Fecha = as.Date.character(paste(año,mes,01,sep="/"), format = "%Y/%m/%d"),
         id_persona = as.character(id_persona)) %>% 
  select(id_persona, Fecha)
str(df_girocm)

# Kit Escolar
con4.3 <- odbcDriverConnect( "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=//bogak08beimrodc/BI/Proteccion_Social/Kit_Escolar_2020.accdb")
sqlTables(con4.3) 
kit2020 <- sqlFetch(con4.3, "base_kit_escolar_2020")
str(kit2020)
odbcCloseAll()

table(kit2020$nuevo_estado_kit)
df_kit2020 <- kit2020 %>% 
  filter( nuevo_estado_kit != "ANULADO") %>% 
  select(id_persona, id_persona_grupofamiliar, nuevo_estado_kit)
str(df_kit2020)

# Subsidio mentario individual
# Cuota monetaria
con20 <- odbcDriverConnect( "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=//bogak08beimrodc/BI/Proteccion_Social/Subsidio_monetario_individual.accdb")
sqlTables(con20)
sub_cm <- sqlFetch(con20, "Subsidio_monetario_individual") 
str(sub_cm)
odbcCloseAll()

df_sub_cm <- sub_cm %>% 
  data.frame() %>%
  select(tx_documento_persona,tx_documento_persona_a_cargo, mes) %>%
  filter(mes == 4) %>% 
  mutate_all(as.character)
str(df_sub_cm)

poblacion_sub_info <- consulta_sub %>% 
  mutate(nid_persona_familiar = gsub("\\D","", id_persona_familiar)) %>% 
  mutate(bono_girado = ifelse(id_persona %in% df_bono$id_persona & parentesco == "HIJO" & Edad_Beneficiario <= 6, 
                              "Si", "No"),
         kit_girada = ifelse(id_persona %in% df_kit2020$id_persona & id_persona_familiar %in% df_kit2020$id_persona_grupofamiliar, 
                             "Si", "No"), 
         cuota_monetaria_per = ifelse(nid_persona_familiar %in% df_sub_cm$tx_documento_persona_a_cargo, "Si", "No"))
str(poblacion_sub_info)

# Salida para app de poblacion afiliados

saveRDS(poblacion_sub_info, "poblacion_subsidio_info.rds")


# Salida 1

beneficiaros_edades <- poblacion_sub_info %>% 
  filter(Edad_Beneficiario <= 18) %>%
  mutate(Edad_Beneficiario = round(Edad_Beneficiario)) %>% 
  group_by(Edad_Beneficiario, categoria) %>% 
  summarise(Beneficiarios = n_distinct(id_persona_familiar)) %>% 
  ungroup() %>% 
  spread(categoria, Beneficiarios)
str(beneficiaros_edades)

library(writexl)
write_xlsx(beneficiaros_edades, "beneficiaros_edades_categoria.xlsx")

# Salida 2
beneficiaros_bono <- poblacion_sub_info %>% 
  filter(bono_girado == "Si") %>% 
  mutate(Edad_Beneficiario = round(Edad_Beneficiario)) %>% 
  group_by(Edad_Beneficiario) %>% 
  summarise(Beneficiarios_bono = n_distinct(id_persona_familiar)) %>% 
  ungroup() 
str(beneficiaros_bono)

beneficiaros_kit <- poblacion_sub_info %>% 
  filter(kit_girada == "Si") %>% 
  mutate(Edad_Beneficiario = round(Edad_Beneficiario)) %>% 
  group_by(Edad_Beneficiario) %>% 
  summarise(Beneficiarios_kit = n_distinct(id_persona_familiar)) %>% 
  ungroup() %>% 
  filter(Edad_Beneficiario <= 18)
str(beneficiaros_kit)

beneficiaros_cm <- poblacion_sub_info %>% 
  filter(cuota_monetaria == "Si") %>% 
  filter(cuota_monetaria_per == "Si") %>% 
  filter(Edad_Beneficiario <= 18) %>% 
  select(id_persona, nid_persona_familiar, numero_cuota_monetaria, Edad_Beneficiario) %>% 
  distinct() %>% 
  mutate(Edad_Beneficiario = round(Edad_Beneficiario)) %>% 
  group_by(Edad_Beneficiario) %>% 
  summarise(Beneficiarios_cm = n_distinct(nid_persona_familiar)) %>% 
  ungroup()
str(beneficiaros_cm)
sum(beneficiaros_cm$Beneficiarios_cm)
sum(consolidada$numero_cuota_monetaria)
library(writexl)
write_xlsx(beneficiaros_bono, "beneficiaros_bono.xlsx")
write_xlsx(beneficiaros_kit, "beneficiaros_kit.xlsx")
write_xlsx(beneficiaros_cm, "beneficiaros_cm.xlsx")

