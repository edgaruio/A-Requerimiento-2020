# *******************************************************************
# Seleccion de muestra y construccion tamaño de muestra
# *******************************************************************

# Cargamos datos
rm(list = ls())
library(sae); library(samplesize4surveys); library(TeachingSampling)
library(sampling); library(readxl); library(survey); library(stratification)
library(corrplot); library(lattice); library(dplyr); library(data.table)
library(RODBC)

# Autorizacion contabilidad
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
str(DATA_Habeas)
table(duplicated(DATA_Habeas$id_persona))

##### Muestra

info_inicial <- readRDS('//BOG08HERNYATT/Analitica/BaseConsolidada/ConsolidacionABR2020.rds') %>% 
  filter(marca_afiliado_unico) %>% 
  filter(Categoria %in% c("B","C")) %>% 
  select(id_persona,id_empresa,Nombre,Edad,Categoria,Segmento_poblacional,Edad,Salario,MunicipioPersona) %>% 
  data.frame()
names(info_inicial)
table(duplicated(info_inicial$id_persona))
table(info_inicial$Categoria)

n1 <- 3*round(20000*(213791/(213791+161425)))
n2 <- 3*round(20000*(161425/(213791+161425)))

# Seleccion de la muestra
set.seed(26052020)
library(stratification)

# LH <- strata.LH(info_inicial$Salario, CV= 0.0005, Ls = 6, takeall = T)
# LH
# LH_tabla <- LH
# LH_tabla$iter.detail

# info_inicial$estrato_Salario <- cut(x = info_inicial$Salario,
#                                     breaks = c(min(info_inicial$Salario),LH$bh,max(info_inicial$Salario)),
#                                     include.lowest = T,
#                                     right = F)
# str(info_inicial)
# n_h <- round((LH$nh),0)

indica <- sampling::strata(data = info_inicial,
                           stratanames = "Categoria",
                           size = c(n1,n2), 
                           description = T, 
                           method = "srswor")


muestra <- info_inicial[indica$ID_unit,]

df_muestra <- muestra %>% 
  left_join(DATA_Habeas %>% select(id_persona, Contacto_autorizacion, Contacto_Mail, Contacto_Celular) ,by="id_persona") %>% 
  filter(Contacto_autorizacion == "SI") %>% 
  filter(!is.na(Contacto_Mail) & !is.na(Contacto_Celular) & !is.na(MunicipioPersona)) %>% 
  select(Nombre,id_persona, Edad, Contacto_Celular, Contacto_Mail, Contacto_autorizacion, MunicipioPersona) %>% 
  mutate_all(as.character) %>% 
  sample_n(20000)
str(df_muestra)
table(df_muestra$Categoria)

# Salida
library(writexl)

write_xlsx(df_muestra, "muestra_byc.xlsx")
