# Consulta 0

# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# Consideraciones iniciales ----
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

# reboot
rm(list = ls())

# notacion cientifica
options(scipen = 999)

# librerias
library(tidyr)
library(dplyr)
library(RODBC)


# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# Data_consolidada ----
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

Data_consolidada <- readRDS('//BOG08HERNYATT/Analitica/BaseConsolidada/ConsolidacionDIC2019.rds')
sort(names(Data_consolidada))

# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# Habeas y contacto ----
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

# autorizacion
channel <- odbcDriverConnect(
  "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=//Bogak08beimrodc/bi/Contacto/Fuentes/Autorizacion.accdb"
)
tb_autorizaciones <- sqlQuery( 
  channel , 
  paste ("select * from tb_autorizaciones")
) %>% 
  data.frame() %>% 
  mutate(
    id_persona = as.character(id_persona)
  ) %>% 
  mutate(
    Contacto_autorizacion = toupper(as.character(autorizacion)) 
  ) %>% 
  select(
    id_persona,Contacto_autorizacion
  )
odbcCloseAll()

# Celular
channel <- odbcDriverConnect(
  "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=//Bogak08beimrodc/bi/Contacto/Fuentes/Celular.accdb"
  # "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=d:/02 - Bases/0 - Bases Beimar/Contacto/Fuentes/Celular.accdb"
)
tb_celular <- sqlQuery( 
  channel , 
  paste ("select * from tb_celular")
) %>% 
  data.frame()  %>% 
  mutate(
    id_persona = as.character(id_persona),
    Contacto_Celular = (telefono_celular)
  ) %>% 
  select(
    id_persona,Contacto_Celular
  )
odbcCloseAll()

# Mail
channel <- odbcDriverConnect(
  "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=//Bogak08beimrodc/bi/Contacto/Fuentes/Mail.accdb"
  # "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=d:/02 - Bases/0 - Bases Beimar/Contacto/Fuentes/Mail.accdb"
)
tb_mail <- sqlQuery( 
  channel , 
  paste ("select * from tb_mail")
) %>% 
  data.frame() %>% 
  mutate(
    id_persona = as.character(id_persona),
    Contacto_Mail = toupper(correo_electronico)
  ) %>% 
  select(
    id_persona,Contacto_Mail
  )
odbcCloseAll()

# tabla de contacto con HABEAS, celular y Mail
DATA_Habeas <- tb_autorizaciones %>% 
  left_join(tb_celular) %>% 
  left_join(tb_mail) %>% 
  filter(!duplicated(id_persona)) %>%
  filter(Contacto_autorizacion == "SI")


# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# Empresas en Riesgo ----
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
library(readxl)

listado_empresas <- read_excel("Base_unicos_dian.xlsx") %>% 
  data.frame()
names(listado_empresas) <- tolower(gsub(".","_",names(listado_empresas),fixed = T))
names(listado_empresas) <- chartr("áéíóú","aeiou",names(listado_empresas))
str(listado_empresas)

df_listado <- listado_empresas %>% 
  select(estado_de_afiliacion,identificador_unico_empresas,nombre_de_la_cuenta,ultima_carta_de_riesgo_recibida,
         fecha_radicacion_ultima_carta,fecha_radicacion_carta,
         calificacion_del_riesgo,calificacion_del_riesgo2,año,mes) %>% 
  mutate(fecha_radicacion_ultima_carta = as.Date(fecha_radicacion_ultima_carta, origin = "1900-01-01"),
         fecha_radicacion_carta = as.Date(fecha_radicacion_carta, origin = "1900-01-01")) %>% 
  filter(ultima_carta_de_riesgo_recibida %in% c("4 - Estudio","5 - Retiro"))
str(df_listado)
table(df_listado$ultima_carta_de_riesgo_recibida)

df_listado_retiro <- df_listado %>% 
  filter(ultima_carta_de_riesgo_recibida == "5 - Retiro")

df_listado_estudio <- df_listado %>% 
  filter(ultima_carta_de_riesgo_recibida == "4 - Estudio")

# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# Empresas en sin productos de credito ----
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

df_seg_individual <- readRDS("bd_afiliados_28012020.rds") %>% 
  select(id_persona,id_empresa,razon_social,RyT,Consumo_credito:Salud,uso_mes) %>% 
  mutate(algun_consumo = rowSums(.[,c("Consumo_credito","uso_mes")], na.rm = TRUE),
         algun_consumo = ifelse(algun_consumo>0,"Si","No")) %>% 
  group_by(id_empresa,razon_social,algun_consumo) %>% 
  summarise(porc_consumo = n()) %>% 
  data.frame() %>% 
  spread(algun_consumo,porc_consumo, fill = 0) %>% 
  mutate(num_afil = rowSums(.[,c("Si","No")]),
         porc_consumo = No/num_afil) %>% 
  # arrange(desc(num_afil),porc_consumo)
  filter(porc_consumo > 0.50)
str(df_seg_individual)
table(duplicated(df_seg_individual$id_empresa))
sum(df_seg_individual$num_afil)

test <- df_seg_individual %>% filter(id_empresa %in% df_listado$identificador_unico_empresas)

### Hoja 1 - Afiliados con Habeas ====  
library(data.table)
afilidos_cubo <- fread("Afiliados_2020-02-06 22_25_44.csv") %>% 
  select(id_persona)

consulta_cupo_saldo <- readRDS("cupo_credito.rds") %>% 
  select(id_persona,ESTADOCUPO,Saldo_cupo=Saldo)
str(consulta_cupo_saldo)

str(Data_consolidada)
df_entrega <- Data_consolidada %>% 
  filter(id_empresa %in% df_listado$identificador_unico_empresas) %>% 
  select(id_persona,id_empresa,RazonSocial,Nombre:Segmento_poblacional,estado_civil,id_empresa,Piramide1,Piramide2,SectorCIIU,ActividadCIIU,MunicipioPersona) %>% 
  left_join(DATA_Habeas, by = "id_persona") %>% 
  mutate(Contacto_autorizacion = ifelse(is.na(Contacto_autorizacion), "Zona gris", Contacto_autorizacion)) %>% 
  filter(Contacto_autorizacion %in% c("SI","Zona gris")) %>% 
  mutate(empresa_en_riesgo = ifelse(id_empresa %in% df_listado$identificador_unico_empresas, "Si entrega carta", "No carta"),
         carta_retiro = ifelse(id_empresa %in% df_listado_retiro$identificador_unico_empresas, "Si", "No"),
         carta_estudio = ifelse(id_empresa %in% df_listado_estudio$identificador_unico_empresas, "Si", "No"),
         productos_empresas_menor_a_50_porciento = ifelse(id_empresa %in% df_seg_individual$id_empresa, "Si", "No"),
         vive_5km_cubo = ifelse(id_persona %in% afilidos_cubo$id_persona, "Si", "No")) %>%
  left_join(consulta_cupo_saldo, by = "id_persona") %>% 
  mutate(Contacto_Celular = ifelse(is.na(Contacto_Celular),"No","Si"),
         Contacto_Mail = ifelse(is.na(Contacto_Mail),"No","Si"))
str(df_entrega)

# Conteo zona gris
df_entrega_gris <- Data_consolidada %>% 
  filter(id_empresa %in% df_listado$identificador_unico_empresas) %>% 
  select(id_persona,id_empresa,RazonSocial,Nombre:Segmento_poblacional,estado_civil,id_empresa,Piramide1,Piramide2,SectorCIIU,ActividadCIIU,MunicipioPersona) %>% 
  select(-Salario) %>% 
  left_join(tb_autorizaciones, by = "id_persona")
sum(is.na(df_entrega_gris$Contacto_autorizacion))

########################################################################
# Exportar en excel entregable ----
########################################################################
writexl::write_xlsx(df_entrega, "afiliados_carta.xlsx")



########################################################################
# Exportar en excel entregable ----
########################################################################
library(writexl); library(dplyr); library(readxl); library(tidyr)

tabla <- read_excel("afiliados_carta.xlsx") %>% 
  data.frame()
str(tabla)
writexl::write_xlsx(tabla, "copia_afiliados_carta.xlsx")

df_entrega <- tabla %>% filter(MunicipioPersona == "BOGOTA D.C.")
writexl::write_xlsx(df_entrega, "afiliados_carta_07022020.xlsx")


test <- df_entrega %>% 
  mutate(carta_retiro = ifelse(carta_retiro == "Si", "Carta retiro", carta_retiro),
         carta_estudio = ifelse(carta_estudio == "Si", "Carta estudio", carta_estudio)) %>% 
  mutate(tipo_riesgo = paste(carta_retiro,carta_estudio)) %>% 
  mutate(tipo_riesgo = gsub("No","",tipo_riesgo,fixed = T)) %>% 
  group_by(tipo_riesgo,Contacto_autorizacion) %>% 
  summarise(conteo = n()) %>% 
  data.frame() %>% 
  spread(tipo_riesgo,conteo)
str(test)
write_xlsx(test, "agregado.xlsx")
