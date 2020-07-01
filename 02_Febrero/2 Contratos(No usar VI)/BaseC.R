
# Librerias ----
install.packages("stringi")
library(readxl)
library(dplyr)
library(stringi)
library(xlsx)
library(stringr)
library(rtrim)

# carga base de datos ----
# base_2018 <- read_excel("BD/Contratos Diarios 2018 I.xlsx", sheet = "4-087A ", range = "A1:AP2380", col_types = "text") %>% 
#   data.frame()

base_2019 <- read_excel("C:/Users/jaimvape/OneDrive - ESCUELA COLOMBIANA DE INGENIERIA JULIO GARAVITO/Colsubsidio/Trabajo/Contratos/BD/Contratos diarios 2019.xlsx", sheet = "Hoja1", range = "A1:BT2140", col_types = "text") %>% 
  data.frame()

#names(base_2019)

# Estandarizar campos ----

names(base_2019) <- chartr("áéíóú","aeiou",names(base_2019))

# base_consolidada <- bind_rows(base_2018, base_2019)

base_consolidada <- base_2019

#str(base_consolidada)

#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#CAMPOS
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

# Campo: Centro_de_costos ------------------------------------------------------------------------------

# c <- base_consolidada %>% group_by(Centro.de.costos) %>%  count() %>% ungroup() %>% mutate(Centro.de.costos = as.numeric(Centro.de.costos))%>% arrange(Centro.de.costos)
# View(c)
# 
# 
# base_consolidada <- base_consolidada %>% mutate(Centro.de.costos = gsub("01","1", Centro.de.costos, fixed = T),
#                                   Centro.de.costos = gsub("02","2", Centro.de.costos, fixed = T),
#                                   Centro.de.costos = gsub("03","3", Centro.de.costos, fixed = T),
#                                   Centro.de.costos = gsub("04","4", Centro.de.costos, fixed = T),
#                                   Centro.de.costos = gsub("05","5", Centro.de.costos, fixed = T),
#                                   Centro.de.costos = gsub("06","6", Centro.de.costos, fixed = T),
#                                   Centro.de.costos = gsub("07","7", Centro.de.costos, fixed = T),
#                                   Centro.de.costos = gsub("08","8", Centro.de.costos, fixed = T),
#                                   Centro.de.costos = gsub("09","9", Centro.de.costos, fixed = T))
# 
# str(base_consolidada$Centro.de.costos)
# 
# c <- base_consolidada %>% group_by(Centro.de.costos) %>%  count() %>% ungroup() %>% mutate(Centro.de.costos = as.numeric(Centro.de.costos))%>% arrange(Centro.de.costos)
# View(c)


# Tabla_homologacion_centro_costo ----

# centro_costo <- data.frame(
#   Llave = seq(1:17),
#   desc_cco = c("Administración",
#                "Mercadeo",
#                "Salud EPS-S",
#                "Salud IPS",
#                "Salud EPS-C",
#                "Medicina Prepagada",
#                "Salud y Nutricion ley 21 de 1982",
#                "Educacion Formal",
#                "Educacion para el Trabajo y Desarrollo Humano",
#                "Biblioteca",
#                "Cultura",
#                "Vivienda",
#                "Recreacion, Deporte y Turismo",
#                "Credito Social",
#                "Fomento al Emprendimiento y la Empresarialidad",
#                "Convenios y/o Programas Especiales",
#                "Fondos de Ley"))
# 
# centro_costo$Llave <- as.character(centro_costo$Llave)

# Homologación_centro_costo ----

# base_consolidada_cco <- base_consolidada %>% left_join(centro_costo, by = c("Centro.de.costos"="Llave"))
# 
# base_consolidada_cco <- base_consolidada_cco %>% select(1:11,43,12:42)
# 
# 
# base_consolidada_cco %>% group_by(Centro.de.costos, desc_cco) %>%  count()
# 
# View(base_consolidada_cco)
# 
# base_consolidada <- base_consolidada_cco


# Campo: modalidad_contratacion -------------------------------------------------------------------------------

# c <- base_consolidada %>% group_by(Modalidad.de.contratacion) %>%  count() %>% ungroup() %>% mutate(Modalidad.de.contratacion = as.numeric(Modalidad.de.contratacion))%>% arrange(Modalidad.de.contratacion)
# View(c)


# base_consolidada <- base_consolidada %>% mutate(Modalidad.de.contratacion = gsub("01","1", Modalidad.de.contratacion, fixed = T),
#                                                 Modalidad.de.contratacion = gsub("02","2", Modalidad.de.contratacion, fixed = T),
#                                                 Modalidad.de.contratacion = gsub("03","3", Modalidad.de.contratacion, fixed = T),
#                                                 Modalidad.de.contratacion = gsub("04","4", Modalidad.de.contratacion, fixed = T),
#                                                 Modalidad.de.contratacion = gsub("05","5", Modalidad.de.contratacion, fixed = T),
#                                                 Modalidad.de.contratacion = gsub("06","6", Modalidad.de.contratacion, fixed = T),
#                                                 Modalidad.de.contratacion = gsub("07","7", Modalidad.de.contratacion, fixed = T),
#                                                 Modalidad.de.contratacion = gsub("08","8", Modalidad.de.contratacion, fixed = T),
#                                                 Modalidad.de.contratacion = gsub("09","9", Modalidad.de.contratacion, fixed = T),
#                                                 Modalidad.de.contratacion = as.factor(Modalidad.de.contratacion))

#summary(base_consolidada$Modalidad.de.contratacion) Mirar resumen del campo como Factor




# Tabla_homologacion_modalidad_contratación ----

# modalidad_contratacion <- data.frame(
#   Llave = seq(1:3),
#   desc_mco = c("Contrato",
#                "Convenio",
#                "Ordenes de Servicio y/o de Compra"
#                ))
# 
# modalidad_contratacion$Llave <- as.factor(modalidad_contratacion$Llave)
# modalidad_contratacion$desc_mco <- as.character(modalidad_contratacion$desc_mco)

# Homologación_modalidad_contratacion ----

# base_consolidada_mco <- base_consolidada %>% left_join(modalidad_contratacion, by = c("Modalidad.de.contratacion"="Llave"))
# 
# base_consolidada_mco$Modalidad.de.contratacion <- as.factor(base_consolidada_mco$Modalidad.de.contratacio)

# base_consolidada_mco <- base_consolidada_mco %>% select(1:19,44,20:43)
# View(base_consolidada)

# base_consolidada_mco %>% group_by(Modalidad.de.contratacion, desc_mco) %>%  count()
# 
# View(base_consolidada_mco)

# base_consolidada <- base_consolidada_mco


# Campo: tipo_contrato_convenio -------------------------------------------------------------------------------------

# c <- base_consolidada %>% group_by(Tipo.de.contrato.o.convenio) %>%  count() %>% ungroup() %>% mutate(Tipo.de.contrato.o.convenio = as.numeric(Tipo.de.contrato.o.convenio))%>% arrange(Tipo.de.contrato.o.convenio)
# View(c)
# 
# 
# base_consolidada <- base_consolidada %>% mutate(Tipo.de.contrato.o.convenio = gsub("01","1", Tipo.de.contrato.o.convenio, fixed = T),
#                                                 Tipo.de.contrato.o.convenio = gsub("02","2", Tipo.de.contrato.o.convenio, fixed = T),
#                                                 Tipo.de.contrato.o.convenio = gsub("03","3", Tipo.de.contrato.o.convenio, fixed = T),
#                                                 Tipo.de.contrato.o.convenio = gsub("04","4", Tipo.de.contrato.o.convenio, fixed = T),
#                                                 Tipo.de.contrato.o.convenio = gsub("05","5", Tipo.de.contrato.o.convenio, fixed = T),
#                                                 Tipo.de.contrato.o.convenio = gsub("06","6", Tipo.de.contrato.o.convenio, fixed = T),
#                                                 Tipo.de.contrato.o.convenio = gsub("07","7", Tipo.de.contrato.o.convenio, fixed = T),
#                                                 Tipo.de.contrato.o.convenio = gsub("08","8", Tipo.de.contrato.o.convenio, fixed = T),
#                                                 Tipo.de.contrato.o.convenio = gsub("09","9", Tipo.de.contrato.o.convenio, fixed = T))
# 
# 


# Tabla_homologacion_tipo_contrato_convenio ----

# tipo_contrato_convenio <- data.frame(
#   Llave = seq(1:11),
#   desc_tcc = c("Prestación de Servicios",
#   "Consultoría",
#   "Mantenimiento y/o Reparación",
#   "Contrato de Obra",
#   "Compra Venta",
#   "Suministro",
#   "Concesión",
#   "Comodato",
#   "Arrendamiento",
#   "Seguros",
#   "Otros"
#   ))
# 
# tipo_contrato_convenio$Llave <- as.character(tipo_contrato_convenio$Llave)

# Homologación_tipo_contrato_convenio ----

# base_consolidada_tcc <- base_consolidada %>% left_join(tipo_contrato_convenio, by = c("Tipo.de.contrato.o.convenio"="Llave"))
# 
# base_consolidada_tcc <- base_consolidada_tcc %>% select(1:21,45,22:44)
# 
# 
# base_consolidada_tcc %>% group_by(Tipo.de.contrato.o.convenio, desc_tcc) %>%  count()
# 
# View(base_consolidada_tcc)
# 
# base_consolidada <- base_consolidada_tcc


# Campo: tipo_aporte_cooperante ----

# names(base_consolidada)
# c <- base_consolidada %>% group_by(Tipo.de.aporte.cooperante) %>%  count() %>% ungroup() %>% mutate(Tipo.de.aporte.cooperante = as.numeric(Tipo.de.aporte.cooperante))%>% arrange(Tipo.de.aporte.cooperante)
# View(c)
# 
# 
# base_consolidada <- base_consolidada %>% mutate(Tipo.de.aporte.cooperante = gsub("01","1", Tipo.de.aporte.cooperante, fixed = T),
#                                                 Tipo.de.aporte.cooperante = gsub("02","2", Tipo.de.aporte.cooperante, fixed = T),
#                                                 Tipo.de.aporte.cooperante = gsub("03","3", Tipo.de.aporte.cooperante, fixed = T),
#                                                 Tipo.de.aporte.cooperante = gsub("04","4", Tipo.de.aporte.cooperante, fixed = T),
#                                                 Tipo.de.aporte.cooperante = gsub("05","5", Tipo.de.aporte.cooperante, fixed = T),
#                                                 Tipo.de.aporte.cooperante = gsub("06","6", Tipo.de.aporte.cooperante, fixed = T),
#                                                 Tipo.de.aporte.cooperante = gsub("07","7", Tipo.de.aporte.cooperante, fixed = T),
#                                                 Tipo.de.aporte.cooperante = gsub("08","8", Tipo.de.aporte.cooperante, fixed = T),
#                                                 Tipo.de.aporte.cooperante = gsub("09","9", Tipo.de.aporte.cooperante, fixed = T))
# 
# str(base_consolidada$Tipo.de.aporte.cooperante)
# 
# c <- base_consolidada %>% group_by(Tipo.de.aporte.cooperante) %>%  count() %>% ungroup() %>% mutate(Tipo.de.aporte.cooperante = as.numeric(Tipo.de.aporte.cooperante))%>% arrange(Tipo.de.aporte.cooperante)
# View(c)

#a <- base_2018 %>% group_by(Centro.de.costos) %>%  count() %>% ungroup() %>% mutate(Centro.de.costos = as.numeric(Centro.de.costos))%>% arrange(Centro.de.costos)
#View(a)
# d <- base_2018 %>% group_by(Tipo.de.aporte.cooperante) %>%  count()%>% arrange(Tipo.de.aporte.cooperante)
# View(d)
# 
# d <- base_2019 %>% group_by(Tipo.de.aporte.cooperante) %>%  count()%>% arrange(Tipo.de.aporte.cooperante)
# View(d)


# Tabla_homologacion_tipo_aporte_coopertante ----

# tipo_aporte_cooperante <- data.frame(
#   Llave = seq(1:4),
#   desc_tac = c("Especie",
#                "Dinero",
#                "Dinero y Especie",
#                "No Aplica"
#   ))
# 
# tipo_aporte_cooperante$Llave <- as.character(tipo_aporte_cooperante$Llave)

# Homologación_tipo_aporte_cooperante ----

# base_consolidada_tac <- base_consolidada %>% left_join(tipo_aporte_cooperante, by = c("Tipo.de.aporte.cooperante"="Llave"))
# 
# View(base_consolidada_tac)
# 
# base_consolidada_tac <- base_consolidada_tac %>% select(1:30,46,31:45)
# 
# 
# base_consolidada_tac %>% group_by(Tipo.de.aporte.cooperante, desc_tac) %>%  count()
# 
# View(base_consolidada_tac)
# 
# base_consolidada <- base_consolidada_tac






# Campo: tipo_aporte_ccf ----

# names(base_consolidada)
# c <- base_consolidada %>% group_by(Tipo.de.aporte.de.la.CCF) %>%  count() %>% ungroup() %>% mutate(Tipo.de.aporte.de.la.CCF = as.numeric(Tipo.de.aporte.de.la.CCF))%>% arrange(Tipo.de.aporte.de.la.CCF)
# View(c)
# 
# 
# base_consolidada <- base_consolidada %>% mutate(Tipo.de.aporte.de.la.CCF = gsub("01","1", Tipo.de.aporte.de.la.CCF, fixed = T),
#                                                 Tipo.de.aporte.de.la.CCF = gsub("02","2", Tipo.de.aporte.de.la.CCF, fixed = T),
#                                                 Tipo.de.aporte.de.la.CCF = gsub("03","3", Tipo.de.aporte.de.la.CCF, fixed = T),
#                                                 Tipo.de.aporte.de.la.CCF = gsub("04","4", Tipo.de.aporte.de.la.CCF, fixed = T),
#                                                 Tipo.de.aporte.de.la.CCF = gsub("05","5", Tipo.de.aporte.de.la.CCF, fixed = T),
#                                                 Tipo.de.aporte.de.la.CCF = gsub("06","6", Tipo.de.aporte.de.la.CCF, fixed = T),
#                                                 Tipo.de.aporte.de.la.CCF = gsub("07","7", Tipo.de.aporte.de.la.CCF, fixed = T),
#                                                 Tipo.de.aporte.de.la.CCF = gsub("08","8", Tipo.de.aporte.de.la.CCF, fixed = T),
#                                                 Tipo.de.aporte.de.la.CCF = gsub("09","9", Tipo.de.aporte.de.la.CCF, fixed = T))
# 
# str(base_consolidada$Tipo.de.aporte.de.la.CCF)
# 
# c <- base_consolidada %>% group_by(Tipo.de.aporte.de.la.CCF) %>%  count() %>% ungroup() %>% mutate(Tipo.de.aporte.de.la.CCF = as.numeric(Tipo.de.aporte.de.la.CCF))%>% arrange(Tipo.de.aporte.de.la.CCF)
# View(c)

#a <- base_2018 %>% group_by(Centro.de.costos) %>%  count() %>% ungroup() %>% mutate(Centro.de.costos = as.numeric(Centro.de.costos))%>% arrange(Centro.de.costos)
#View(a)
# d <- base_2018 %>% group_by(Tipo.de.aporte.de.la.CCF) %>%  count()%>% arrange(Tipo.de.aporte.de.la.CCF)
# View(d)
# 
# d <- base_2019 %>% group_by(Tipo.de.aporte.de.la.CCF) %>%  count()%>% arrange(Tipo.de.aporte.de.la.CCF)
# View(d)


# Tabla_homologacion_tipo_aporte_ccf ----

# tipo_aporte_ccf <- data.frame(
#   Llave = seq(1:4),
#   desc_ccf = c("Especie",
#                "Dinero",
#                "Dinero y Especie",
#                "No Aplica"
#   ))
# 
# tipo_aporte_ccf$Llave <- as.character(tipo_aporte_ccf$Llave)

# Homologación_tipo_aporte_ccf ----

# base_consolidada_ccf <- base_consolidada %>% left_join(tipo_aporte_ccf, by = c("Tipo.de.aporte.de.la.CCF"="Llave"))
# 
# View(base_consolidada_ccf)
# 
# base_consolidada_ccf <- base_consolidada_ccf %>% select(1:32,47,33:46)
# 
# 
# base_consolidada_ccf %>% group_by(Tipo.de.aporte.de.la.CCF, desc_ccf) %>%  count()
# 
# View(base_consolidada_ccf)
# 
# base_consolidada <- base_consolidada_ccf
# 



# Campo: anticipo_legalizado ----

# names(base_consolidada)
# c <- base_consolidada %>% group_by(Anticipo.legalizado) %>%  count() %>% ungroup() %>% mutate(Anticipo.legalizado = as.numeric(Anticipo.legalizado))%>% arrange(Anticipo.legalizado)
# View(c)
# 
# 
# base_consolidada <- base_consolidada %>% mutate(Anticipo.legalizado = gsub("01","1", Anticipo.legalizado, fixed = T),
#                                                 Anticipo.legalizado = gsub("02","2", Anticipo.legalizado, fixed = T),
#                                                 Anticipo.legalizado = gsub("03","3", Anticipo.legalizado, fixed = T),
#                                                 Anticipo.legalizado = gsub("04","4", Anticipo.legalizado, fixed = T),
#                                                 Anticipo.legalizado = gsub("05","5", Anticipo.legalizado, fixed = T),
#                                                 Anticipo.legalizado = gsub("06","6", Anticipo.legalizado, fixed = T),
#                                                 Anticipo.legalizado = gsub("07","7", Anticipo.legalizado, fixed = T),
#                                                 Anticipo.legalizado = gsub("08","8", Anticipo.legalizado, fixed = T),
#                                                 Anticipo.legalizado = gsub("09","9", Anticipo.legalizado, fixed = T))
# 
# str(base_consolidada$Anticipo.legalizado)
# 
# c <- base_consolidada %>% group_by(Anticipo.legalizado) %>%  count() %>% ungroup() %>% mutate(Anticipo.legalizado = as.numeric(Anticipo.legalizado))%>% arrange(Anticipo.legalizado)
# View(c)

#a <- base_2018 %>% group_by(Centro.de.costos) %>%  count() %>% ungroup() %>% mutate(Centro.de.costos = as.numeric(Centro.de.costos))%>% arrange(Centro.de.costos)
#View(a)
# d <- base_2018 %>% group_by(Anticipo.legalizado) %>%  count()%>% arrange(Anticipo.legalizado)
# View(d)
# 
# d <- base_2019 %>% group_by(Anticipo.legalizado) %>%  count()%>% arrange(Anticipo.legalizado)
# View(d)


# Tabla_homologacion_anticipo_legalizado ----

# anticipo_legalizado <- data.frame(
#   Llave = seq(1:2),
#   desc_al = c("Si",
#                "No"
#   ))
# 
# anticipo_legalizado$Llave <- as.character(anticipo_legalizado$Llave)

# Homologación_anticipo_legalizado ----

# base_consolidada_al <- base_consolidada %>% left_join(anticipo_legalizado, by = c("Anticipo.legalizado"="Llave"))
# 
# View(base_consolidada_al)
# 
# base_consolidada_al <- base_consolidada_al %>% select(1:39,48,40:47)
# 
# 
# base_consolidada_al %>% group_by(Anticipo.legalizado, desc_al) %>%  count()
# 
# View(base_consolidada_al)
# 
# base_consolidada <- base_consolidada_al



# Campo: estado_contrato_convenio ----

# names(base_consolidada)
# c <- base_consolidada %>% group_by(Estado.del.contrato.o.convenio) %>%  count() %>% ungroup() %>% mutate(Estado.del.contrato.o.convenio = as.numeric(Estado.del.contrato.o.convenio))%>% arrange(Estado.del.contrato.o.convenio)
# View(c)
# 
# 
# base_consolidada <- base_consolidada %>% mutate(Estado.del.contrato.o.convenio = gsub("01","1", Estado.del.contrato.o.convenio, fixed = T),
#                                                 Estado.del.contrato.o.convenio = gsub("02","2", Estado.del.contrato.o.convenio, fixed = T),
#                                                 Estado.del.contrato.o.convenio = gsub("03","3", Estado.del.contrato.o.convenio, fixed = T),
#                                                 Estado.del.contrato.o.convenio = gsub("04","4", Estado.del.contrato.o.convenio, fixed = T),
#                                                 Estado.del.contrato.o.convenio = gsub("05","5", Estado.del.contrato.o.convenio, fixed = T),
#                                                 Estado.del.contrato.o.convenio = gsub("06","6", Estado.del.contrato.o.convenio, fixed = T),
#                                                 Estado.del.contrato.o.convenio = gsub("07","7", Estado.del.contrato.o.convenio, fixed = T),
#                                                 Estado.del.contrato.o.convenio = gsub("08","8", Estado.del.contrato.o.convenio, fixed = T),
#                                                 Estado.del.contrato.o.convenio = gsub("09","9", Estado.del.contrato.o.convenio, fixed = T))
# 
# str(base_consolidada$Estado.del.contrato.o.convenio)
# 
# c <- base_consolidada %>% group_by(Estado.del.contrato.o.convenio) %>%  count() %>% ungroup() %>% mutate(Estado.del.contrato.o.convenio = as.numeric(Estado.del.contrato.o.convenio))%>% arrange(Estado.del.contrato.o.convenio)
# View(c)

#a <- base_2018 %>% group_by(Centro.de.costos) %>%  count() %>% ungroup() %>% mutate(Centro.de.costos = as.numeric(Centro.de.costos))%>% arrange(Centro.de.costos)
#View(a)
# d <- base_2018 %>% group_by(Estado.del.contrato.o.convenio) %>%  count()%>% arrange(Estado.del.contrato.o.convenio)
# View(d)
# 
# d <- base_2019 %>% group_by(Estado.del.contrato.o.convenio) %>%  count()%>% arrange(Estado.del.contrato.o.convenio)
# View(d)
# 

# Tabla_homologacion_estado_contrato_convenio ----

# estado_contrato_convenio <- data.frame(
#   Llave = seq(1:5),
#   desc_ecc = c("Vigente",
#                "Modificación-Adición",
#                "Modificación-Reducción",
#                "Liquidado",
#                "Suspendido"
#   ))
# 
# estado_contrato_convenio$Llave <- as.character(estado_contrato_convenio$Llave)

# Homologación_estado_contrato_convenio ----

# base_consolidada_ecc <- base_consolidada %>% left_join(estado_contrato_convenio, by = c("Estado.del.contrato.o.convenio"="Llave"))
# 
# View(base_consolidada_ecc)
# 
# base_consolidada_ecc <- base_consolidada_ecc %>% select(1:41,49,42:48)
# 
# 
# base_consolidada_ecc %>% group_by(Estado.del.contrato.o.convenio, desc_ecc) %>%  count()
# 
# View(base_consolidada_ecc)
# 
# base_consolidada <- base_consolidada_ecc
# 
# names(base_consolidada)





# Campo: abogado -------------------------------------------------------------------------------------------------------

# c <- base_consolidada %>% group_by(Abogado) %>%  count() %>% arrange(Abogado)
# View(c)

# glimpse(base_consolidada)


base_consolidada <- base_consolidada %>% mutate_at(c("Valor.Estimado.CLM", "ABOGADO_1", "No."), as.character)

z <- base_consolidada %>% 
  mutate_at(c('ABOGADO_1'), list(~trimws(toupper(stri_trans_general(., id="Latin-ASCII"))))) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("ALVARO CHAPARRO -DIEGO FERNANDO BASTIDAS"), "ALVARO CHAPARRO - DIEGO FERNANDO BASTIDAS", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("AN PATRICIA GARZON"), "ANA  PATRICIA GARZON", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("ANA MILENA PORVEDA / DIEGO RODRIGUEZ", "ANA MILENA POVEDA -DIEGO RODRIGUEZ"), "ANA MILENA POVEDA - DIEGO RODRIGUEZ", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("ANA MILENA POVEDA -DIEGO FELIPE", "ANA MILENA POVEDA  - DIEGO FELIPE"), "ANA MILENA POVEDA - DIEGO FELIPE", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("ANA PATRIC IA GARZON", "ANA PATRICIA", "ANA PATRICA GARZON", "ANA PATRICIA GARZZON", "ANA PATRICIA GAZON", "ANA PATRICIA GZARZON", "ANA PATROCIA GARZON", "ANA  PATRICIA GARZON"), "ANA PATRICIA GARZON", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("ANA PATRICIA GARZON                             DIEGO GONZALEZ", "ANA PATRICIA GARZON                            DIEGO GONZALEZ	", "ANA PATRICIA GARZON                        DIEGO GONZALEZ", "ANA PATRICIA GARZON          DIEGO GONZALEZ", "ANA PATRICIA GARZON                            DIEGO GONZALEZ"), "ANA PATRICIA GARZON  DIEGO GONZALEZ", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("ANA PATRICIA GARZON  DIEGO FERNANDO GONZALEZ"), "ANA PATRICIA GARZON DIEGO FERNANDO GONZALEZ", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("ANA PATRICIA GARZON  DIEGO GONZALEZ"), "ANA PATRICIA GARZON DIEGO GONZALEZ", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("ANA PATRICIA GARZON NATHALY.RINCON", "ANA PATRICIA GARZON - NATHALY RINCON"), "ANA PATRICIA GARZON NATHALY RINCON", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("ANDRES FELIPE MUERIEL SANCHEZ", "ANDRES FELIPE MURIEL"), "ANDRES FELIPE MURIEL SANCHEZ", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("ANDRES FELIPE MURIEL DIEGO RODRIGUEZ"), "ANDRES FELIPE MURIEL- DIEGO FELIPE RODRIGUEZ", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("ANDRES FELIPE MURIEL -DIEGO FELIPE", "ANDRES MURIEL SANCHEZ - DIEGO FELIPE"), "ANDRES FELIPE MURIEL - DIEGO FELIPE RODRIGUEZ", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("ANDRES FELIPE MURIEL -DIEGO GONZALEZ"), "ANDRES FELIPE MURIEL - DIEGO GONZALEZ", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("DAVID", "DAVID   VALLEJO.", "DAVID  ESTEBAN VALLEJO", "DAVID  VALLEJO", "DAVID ESTEB AN VALLEJO", "DAVID ESTEBA VALLEJO", "DAVID ESTEBAN  VALLEJO", "DAVID VALLEJO", "DIEGO ESTEBAN VALLEJO"), "DAVID ESTEBAN VALLEJO", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("DIANA  PATRICIA MILLAN", "DIANA MILLAN"), "DIANA PATRICIA MILLAN", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("DIEGO  BASTIDAS", "DIEGO BASTIDIAS", "DIEGO BASTISDAS"), "DIEGO BASTIDAS", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("DIEGO  GONZALEZ"), "DIEGO FERNANDO GONZALEZ", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("DIEGO  RODRIGUEZ", "DIEGO RODRIGUEZ"), "DIEGO FELIPE RODRIGUEZ", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("DIEGO BASTIDAS                   DIEGO GONZALEZ"), "DIEGO BASTIDAS  DIEGO GONZALEZ", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("DIEGO FELIPE  RODRIGUEZ"), "DIEGO FELIPE RODRIGUEZ", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("ALVARO CHAPARRO-DIEGO FELIPE"), "ALVARO CHAPARRO - DIEGO FELIPE RODRIGUEZ", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("ALVARO CHAPARRO - ANA PATRICIA"), "ALVARO CHAPARRO - ANA PATRICIA GARZON", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("ANA GARZON DIEGO GONZALEZ KAREN ACOSTA"), "ANA PATRICIA GARZON - DIEGO FERNANDO GONZALEZ - KAREN ACOSTA", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("ANA MILENA POVEDA - DIEGO FELIPE"), "ANA MILENA POVEDA - DIEGO FELIPE RODRIGUEZ", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("ANA MILENA POVEDA - DIEGO GONZALEZ"), "ANA MILENA POVEDA - DIEGO FERNANDO GONZALEZ", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("ANA MILENA POVEDA - DIEGO RODRIGUEZ"), "ANA MILENA POVEDA - DIEGO FELIPE RODRIGUEZ", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("ANA PATRICIA GARZON  DAVID  VALLEJO"), "ANA PATRICIA GARZON - DAVID ESTEBAN VALLEJO", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("ANA PATRICIA GARZON  DIEGO FERNANDO BASTIDAS"), "ANA PATRICIA GARZON -  DIEGO FERNANDO BASTIDAS", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("ANA PATRICIA GARZON DIEGO FERNANDO GONZALEZ"), "ANA PATRICIA GARZON - DIEGO FERNANDO GONZALEZ", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("ANA PATRICIA GARZON DIEGO GONZALEZ"), "ANA PATRICIA GARZON - DIEGO FERNANDO GONZALEZ", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("ANA PATRICIA GARZON NATHALY RINCON"), "ANA PATRICIA GARZON - NATHALY RINCON", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("ANA PATRICIA GARZON/DIEGO FELIPE"), "ANA PATRICIA GARZON - DIEGO FELIPE RODRIGUEZ", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("ANDRES FELIPE MURIEL- DIEGO RODRIGUEZ", "ANDRES FELIPE MURIEL - DIEGO FELIPE", "ANDRES FELIPE MURIEL - DIEGO FELIPE RODRIGUEZ", "ANDRES MURIEL SANCHEZ-DIEGO FELIPE RODRIGUEZ", "ANDRES FELIPE MURIEL- DIEGO FELIPE RODRIGUEZ", "ANDRES MURIEL SANCHEZ- DIEGO FELIPE"), "ANDRES FELIPE MURIEL - DIEGO FELIPE RODRIGUEZ", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("ANDRES FELIPE MURIEL - DIEGO GONZALEZ"), "ANDRES FELIPE MURIEL - DIEGO FERNANDO GONZALEZ", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("ANDRES FELIPE MURIEL - NATHALY"), "ANDRES FELIPE MURIEL - NATHALY RINCON", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("ANDRES FELIPE MURIEL SANCHEZ"), "ANDRES FELIPE MURIEL", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("ANGELICA REYES  DIEGO GONZALEZ"), "MARIA ANGELICA REYES - DIEGO FERNANDO GONZALEZ", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("ANGELICA REYES ANA PATRICIA GARZON"), "MARIA ANGELICA REYES - ANA PATRICIA GARZON", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("DAVID ESTEBAN  VALLEJO  ANA PATRICIA GARZON"), "DAVID ESTEBAN VALLEJO - ANA PATRICIA GARZON", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("DAVID ESTEBAN VALLEJO/ NATHALY"), "DAVID ESTEBAN VALLEJO - NATHALY RINCON", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("DAVID VALLEJO DIEGO GONZALEZ"), "DAVID ESTEBAN VALLEJO - DIEGO FERNANDO GONZALEZ", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("DIANA MILLAN DIEGO BASTIDAS", "DINA PATRICIA MILLAN"), "DIANA PATRICIA MILLAN - DIEGO FERNANDO BASTIDAS", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("DIANA PORRAS -DIEGO BASTIDAS"), "DIANA PORRAS - DIEGO FERNANDO BASTIDAS", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("DIEGO BASTIDAS", "DIEGO FERNADO BASTIDAS", "DIEGO FERNANDO  BASTIDAS", "DIEGO FERNANDO BASTIDAS", "DIEGO FERNANDOBASTIDAS"), "DIEGO FERNANDO BASTIDAS", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("DIEGO BASTIDAS - NATHALY RINCON", "DIEGO FERNANDO BASTIDAS                     NATHALY"), "DIEGO FERNANDO BASTIDAS - NATHALY RINCON", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("DIEGO BASTIDAS  DIEGO GONZALEZ", "DIEGO FERNANDO BASTIDAS / DIEGO GONZALEZ", "DIEGO FERNANDO BASTIDAS DIEGO GONZALEZ", "DIEGO FERNANDO BASTIDAS/DIEGO GONZALEZ", "DIEGO FERNANDO BASTIDAS / DIEGO GONZALEZ", "DIEGO FERNANDO BASTIDAS/DIEGO GONZALEZ"), "DIEGO FERNANDO BASTIDAS - DIEGO FERNANDO GONZALEZ", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("DIEGO BASTIDAS DIANA MILLAN"), "DIEGO FERNANDO BASTIDAS - DIANA PATRICIA MILLAN", ABOGADO_1))%>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("DIEGO BASTIDAS DIXON CARDENAS"), "DIEGO FERNANDO BASTIDAS - DIXON CARDENAS", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("DIEGO BASTIDAS KAREN ACOSTA"), "DIEGO FERNANDO BASTIDAS - KAREN ACOSTA", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("DIEGO FELIPE", "DIEGO FELIPE RODRIGUEZ"), "DIEGO FELIPE RODRIGUEZ", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("DIEGO FENANDO GONZALEZ", "DIEGO FERNADO GONZALEZ", "DIEGO FERNANDO FONZALEZ", "DIEGO FERNANDO GONZALES", "DIEGO FERNANDO GONZALEZ","DIEGO GONZALEZ"), "DIEGO FERNANDO GONZALEZ", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("DIEGO FERNANDO BASTIDAS                                 DAVID VALLEJO"), "DIEGO FERNANDO BASTIDAS - DAVID ESTEBAN VALLEJO", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("DIEGO GONZALEZ  NATHALY RINCON", "DIEGO GONZALEZ NATHALY RINCON"), "DIEGO FERNANDO GONZALEZ - NATHALY RINCON", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("DIEGO GONZALEZ ANA PATRICIA GARZON"), "DIEGO FERNANDO GONZALEZ - ANA PATRICIA GARZON", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("DIEGO GONZALEZ KAREN ACOSTA"), "DIEGO FERNANDO GONZALEZ - KAREN ACOSTA", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("DIEGO GONZALEZ MARIA ANGELICA REYES"), "DIEGO FERNANDO GONZALEZ - MARIA ANGELICA REYES", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("ANGELICA REYES"), "MARIA ANGELICA REYES", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("DIEGO FERNANDO BASTIDAS / DIEGO GONZALEZ 08/03/19", "DIEGO FERNANDO BASTIDAS/DIEGO GONZALEZ 12/3/19"), "DIEGO FERNANDO BASTIDAS - DIEGO FERNANDO GONZALEZ", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("EDWIN FANDINO - DIEGO BASTIDAS"), "EDWIN FANDINO - DIEGO FERNANDO BASTIDAS", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("EDWIN J. FANDINO"), "EDWIN FANDINO", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("FABIO HERNANDES BAQUERO - DIEGO FELIPE", "FABIO HERNANDEZ - DIEGO FELIPE", "FABIO HERNANDEZ BAQUERO - DIEGO FELIPE", "FABIO HERNANDEZ BAQUERO  DIEGO FELIPE", "FABIO HERNANDEZ DIEGO FELIPE RODRIGUEZ"), "FABIO HERNANDEZ BAQUERO - DIEGO FELIPE RODRIGUEZ", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("FABIO HERNANDES BAQUERO - DIEGO GONZALEZ", "FABIO HERNANDEZ BAQUERO - DIEGO GONZALEZ", "FABIO HERNANDEZ - DIEGO GONZALEZ"), "FABIO HERNANDEZ BAQUERO - DIEGO FERNANDO GONZALEZ", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("FABIO HERNANDEZ - ANA PATRICIA GARZON", "FABIO HERNANDEZ BAQUERO - ANA PATRICIA", "FABIO HERNANDEZ  -   ANA PATRICIA"), "FABIO HERNANDEZ BAQUERO - ANA PATRICIA GARZON", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("FRANCY QUITIAN H - ANA PATRICIA", "FRANCY QUITIAN H. - ANA PATRICIA", "FRANCY QUITIAN H. -ANA PATRICIA", "FRANCY QUITIAN H.  - ANA PATRICIA"), "FRANCY JULIETH QUITIAN - ANA PATRICIA GARZON", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("FRANCY QUITIAN H. - DIEGO FERNANDO BASTIDAS"), "FRANCY JULIETH QUITIAN - DIEGO FERNANDO BASTIDAS", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("GUILERMO CARDOZO                ANA PATRICIA", "GUILLERMO CARDOZO - ANA PATRICIA", "GUILLERMO JOSE CARDOZO - ANA PATRICIA", "GUILLERMO JOSE CARDOZO -ANA PATRICIA"), "GUILLERMO JOSE CARDOZO - ANA PATRICIA GARZON", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("GUILERMO JOSE CARDOZO TAFUR", "GUILLERMO JOSE CARDOZO", "GUILLERMO JOSE CARDOZO TAFUR"), "GUILLERMO JOSE CARDOZO", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("GUILLERMO CARDOZO-DIEGO GONZALEZ", "GUILLERMO CARDOZO -DIEGO GONZALEZ", "GUILLERMO CARDOZO TAFUR- DIEGO GONZALEZ", "GUILLERMO CARDOZO TAFUR-DIEGO GONZALEZ", "GUILLERMO CARDOZO TAFUR - DIEGO GONZALEZ", "GUILLERMO JOSE CARDOZO - DIEGO GONZALEZ"), "GUILLERMO JOSE CARDOZO - DIEGO FERNANDO GONZALEZ", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("GUILLERMO CARDOZO TAFUR- DIEGO FELIPE", "GUILLERMO CARDOZO TAFUR-DIEGO FELIPE", "GUILLERMO JOSE CARDOZO - DIEGO FELIPE", "GUILLERMO JOSE CARDOZO - DIEGOFELIPE", "GUILLERMO JOSE CARDOZO -DIEGO FELIPE"), "GUILLERMO JOSE CARDOZO - DIEGO FELIPE RODRIGUEZ", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("GUILLERMO CARDOZO TAFUR- NATHALY RINCON", "GUILLERMO CARDOZO TAFUR-NATHALY RINCON"), "GUILLERMO JOSE CARDOZO - NATHALY RINCON", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("KAREN ACOSTA TORRES"), "KAREN ACOSTA", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("KAREN ACOSTA- DIEGO GONZALEZ", "KAREN ACOSTA - DIEGO GONZALEZ", "KAREN ACOSTA   DIEGO GONZALEZ", "KAREN ACOSTA DIEGO GONZALEZ"), "KAREN ACOSTA - DIEGO FERNANDO GONZALEZ", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("KAREN ACOSTA ANA GARZON"), "KAREN ACOSTA - ANA PATRICIA GARZON", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("LILIANA PAREDES / DIEGO BASTIDAS"), "LILIANA PAREDES - DIEGO FERNANDO BASTIDAS", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("MARIA ANGELICA REYES -  DIEGO GONZALEZ", "MARIA ANGELICA REYES - DIEGO FERNANDO GONZALEZ", "MARIA ANGELICA REYES - GONZALEZ", "MARIA ANGELICA REYES - DIEGO GONZALEZ", "MARIA ANGELICA REYES                               DIEGO GONZALEZ", "MARIA ANGELICA REYES                             DIEGO GONZALEZ", "MARIA ANGELICA REYES  DIEGO GONZALEZ", "MARIA ANGELICA REYES DIEGO GONZALEZ"), "MARIA ANGELICA REYES - DIEGO FERNANDO GONZALEZ", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("MARIA ANGELICA REYES - ANA PATRICIA GARZON", "MARIA ANGELICA REYES  ANA PATRICIA GARZON", "MARIA ANGELICA REYES ANA PATRICIA GARZON"), "MARIA ANGELICA REYES - ANA PATRICIA GARZON", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("MARIA ANGELICA REYES  DIEGO FELIPE"), "MARIA ANGELICA REYES - DIEGO FELIPE RODRIGUEZ", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("MARIA ANGELICA REYES DAVID ESTEBAN VALLEJO"), "MARIA ANGELICA REYES - DAVID ESTEBAN VALLEJO", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("MONICA DEL PILAR CORREA - ANA PATRICIA"), "MONICA DEL PILAR CORREA - ANA PATRICIA GARZON", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("NASTHALY RINCON", "NATALY RINCON", "NATHALIA RINCON", "NATHALY", "NATHALY  RINCON", "NATHALY RINCO", "NATHALY RINCON", "NATHALY RINCON-", "NATHALY RINON", "NATHALY.RINCON", "NATHLY RINCON", "NATHY RINCON"), "NATHALY RINCON", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("NASTHALY RINCON   DIEGO GONZALEZ", "NATHALY  RINCON DIEGO GONZALEZ", "NATHALY RINCON - DIEGO GONZALEZ", "NATHALY RINCON  DIEGO GONZALEZ", "NATHALY RINCON . DIEGO GONZALEZ", "NATHALY RINCON DIEGO GONZALEZ", "NATHALY RINCON/DIEGO FERNANDO GONZALEZ"), "NATHALY RINCON - DIEGO FERNANDO GONZALEZ", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("NATHALY  RINCON/DIEGO FERNANDO BASTIDAS", "NATHALY RINCON/ DIEGO FERNANDO BASTIDAS"), "NATHALY RINCON - DIEGO FERNANDO BASTIDAS", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("NATHALY RINCO DAVID VALLEJO", "NATHALY RINCON  DAVID VALLEJO", "NATHALY RINCON DAVID VALLEJO"), "NATHALY RINCON - DAVID ESTEBAN VALLEJO", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("NATHALY RINCON- PATRICIA GARZON", "NATHALY RINCON  ANA PATRICIA", "NATHALY RINCON  ANA PATRICIA GARZON", "NATHALY RINCON ANA PATRICIA"), "NATHALY RINCON - ANA PATRICIA GARZON", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("NATHALY RINCON  SERGIO BERNAL", "NATHALY RINCON SERGIO BERNAL"), "NATHALY RINCON - SERGIO BERNAL", ABOGADO_1))%>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("PATRICIA GARZON"), "ANA PATRICIA GARZON", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("SEBASTIAN MARTINEZ", "SEBASTIAN MARTINEZ ESPINOSA"), "SEBASTIAN MARTINEZ ESPINOZA", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("SEBASTIAN MARTINEZ-DIEGO FELIPE", "SEBASTIAN MARTINEZ - DIEGO FELIPE", "SEBASTIAN MARTINEZ -DIEGO FELIPE", "SEBASTIAN MARTINEZ E.  - DIEGO FELIPE", "SEBASTIAN MARTINEZ ESPINOSA DIEGO FELIPE RODRIGUEZ"), "SEBASTIAN MARTINEZ ESPINOSA - DIEGO FELIPE RODRIGUEZ", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("SEBASTIAN MARTINEZ-NATHALY RINCON", "SEBASTIAN MARTINEZ - NATHALY"), "SEBASTIAN MARTINEZ ESPINOSA - NATHALY RINCON", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("SEBASTIAN MARTINEZ - ANA PATRICIA", "SEBASTIAN MARTINEZ E. - ANA PATRICIA"), "SEBASTIAN MARTINEZ ESPINOSA - ANA PATRICIA GARZON", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("SEBASTIAN MARTINEZ - DIEGO GONZALEZ"), "SEBASTIAN MARTINEZ ESPINOSA - DIEGO FERNANDO GONZALEZ", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("VICTORIA CAROLINA RAMIREZ", "VICTORIA RAMIREZ", "VICTORIA CAROLINA RAMIREZ -"), "VICTORIA CAROLINA RAMIREZ", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("VICTORIA CAROLINA RAMIREZ - DIEGO FERNANDO BASTIDAS", "VICTORIA RAMIREZ -DIEGO FERNANDO BASTIDAS"), "VICTORIA CAROLINA RAMIREZ - DIEGO FERNANDO BASTIDAS", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("ANA PATRICIA GARZON - DAVID ESTEBAN VALLEJO"), "DAVID ESTEBAN VALLEJO - ANA PATRICIA GARZON", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("ANA PATRICIA GARZON - DIEGO FERNANDO GONZALEZ"), "DIEGO FERNANDO GONZALEZ - ANA PATRICIA GARZON", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("ANA PATRICIA GARZON - NATHALY RINCON"), "NATHALY RINCON - ANA PATRICIA GARZON", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("DIANA PATRICIA MILLAN - DIEGO FERNANDO BASTIDAS"), "DIEGO FERNANDO BASTIDAS - DIANA PATRICIA MILLAN", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("DIEGO FERNANDO BASTIDAS - NATHALY RINCON"), "NATHALY RINCON - DIEGO FERNANDO BASTIDAS", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("DAVID ESTEBAN VALLEJO - NATHALY RINCON"), "NATHALY RINCON - DAVID ESTEBAN VALLEJO", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("DIEGO FERNANDO GONZALEZ - NATHALY RINCON"), "NATHALY RINCON - DIEGO FERNANDO GONZALEZ", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("DIEGO FERNANDO GONZALEZ - KAREN ACOSTA"), "KAREN ACOSTA - DIEGO FERNANDO GONZALEZ", ABOGADO_1)) %>% 
  mutate(ABOGADO_1=ifelse(ABOGADO_1 %in% c("DIEGO FERNANDO GONZALEZ - MARIA ANGELICA REYES"), "MARIA ANGELICA REYES - DIEGO FERNANDO GONZALEZ", ABOGADO_1))


abog_hom <- read_excel("BD/Abogados.xlsx", sheet = "Hoja2", range = "B1:f69", col_types = "text") %>% 
  data.frame()

str(test1)
str(test2)

test1 <- abog_hom %>% slice(1:5) %>% 
  mutate(llave = trim(Abogadoinicial))

test2 <- z %>% filter(grepl("ALVARO CHAPARRO - ANA PATRICIA GARZON", ABOGADO_1)) %>% 
  select(1,2) %>% 
  mutate(llave = trim( ABOGADO_1))

test3 <- test2 %>%left_join(test1, by = c("ABOGADO_1"="Abogadoinicial"))

rm(test4)

test1$Abogadoinicial
str(test1)

prueba  <- gsub(test1$Abogadoinicial[4],pattern = "-",replacement = "")
gsub(prueba,pattern = " ",replacement = "")


test1 <- test1 %>% mutate(Abogadoinicial_unspace = gsub("\\s", "", Abogadoinicial)) %>% 
  mutate(Abogadoinicial_unspace = gsub("-", "", Abogadoinicial_unspace))

prueba <- gsub("\\s", "", test1$Abogadoinicial)


z <- z %>% left_join(abog_hom, by = c("ABOGADO_1"="Abogadoinicial"))

z %>% group_by(Abogado1, Abogado2, Abogado3) %>% count()

#  c <- z %>% group_by(Abogado) %>%  count() %>% arrange(Abogado)
# View(c)


#d <- base_2018 %>% group_by(Abogado) %>%  count()%>% arrange(Abogado)
#View(d)

#d <- base_2019 %>% group_by(Abogado) %>%  count()%>% arrange(Abogado)
#View(d)



# Campo: Tipo.Requerimiento ----

str(z)

zz <- z %>% 
  mutate_at(c('Tipo.Requerimiento'), list(~trimws(toupper(stri_trans_general(., id="Latin-ASCII")))))

c <- zz %>% group_by(Tipo.Requerimiento) %>%  count() %>% arrange(Tipo.Requerimiento)
View(c)


# Campo: Dependencia ----

str(z)

zz <- zz %>% 
  mutate_at(c('Dependencia'), list(~trimws(toupper(stri_trans_general(., id="Latin-ASCII")))))

c <- zz %>% group_by(Dependencia) %>%  count() %>% arrange(Dependencia)
View(c)


View(zz)


# Campo: Modalidad ----

str(z)

zz <- zz %>% 
  mutate_at(c('Modalidad'), list(~trimws(toupper(stri_trans_general(., id="Latin-ASCII")))))

c <- zz %>% group_by(Modalidad) %>%  count() %>% arrange(Modalidad)
View(c)

# library(formattable)
# 
# zz$Valor.Estimado.CLM <- currency(zz$Valor.Estimado.CLM, digits = 0L)
# 
# str(zz$Valor.Estimado.CLM)
# 
# str(zz$Valor.inicial.del.contrato.o.convenio)
# 
# zz$Valor.inicial.del.contrato.o.convenio <- currency(zz$Valor.inicial.del.contrato.o.convenio, digits = 0L)
# 
# 
# View(zz)



# Tabla_homologacion_Dependencia ----
# tabla22 <- zz %>% group_by(Dependencia, Dependencia2) %>% summarise( n=n())
# write.csv2(tabla22, "dep_prueba.csv")

abcd <- base %>% group_by(Dependencia) %>% summarise(n=n())

dep_hom <- read_excel("BD/DEPENDENCIAS.xlsx", sheet = "Sheet1", range = "B1:C550", col_types = "text") %>% 
  data.frame()
View(dep_hom)
str(dep_hom)

dependencia_homol <- dep_hom

dependencia_homol$Dependencia1 <- as.character(dependencia_homol$Dependencia1)


# Homologación_Dependencia ----


base_consolidada_dep <- zz %>% left_join(dependencia_homol, by = c("Dependencia"="Dependencia1"))

www <- base_consolidada_dep %>% group_by(Dependencia, Dependencia2) %>% summarise(n=n())

# write.csv2(www, file = "prueba.csv")
# View(base_consolidada_dep)

base_consolidada_dep <- base_consolidada_dep %>% select(1:6,50,7:49)

View(base_consolidada_dep)

base_consolidada <- base_consolidada_dep




# Tabla_homologacion_tipo_requerimiento ----

req_hom <- read_excel("BD/Requerimiento.xlsx", sheet = "Sheet1", range = "B1:C1275", col_types = "text") %>% 
  data.frame()
View(req_hom)
str(req_hom)

requerimiento_homol <- req_hom

requerimiento_homol$Tipo.Requerimiento <- as.character(requerimiento_homol$Tipo.Requerimiento)


# Homologación_Requerimiento ----


base_consolidada_req <- base_consolidada %>% left_join(requerimiento_homol, by = c("Tipo.Requerimiento"="Tipo.Requerimiento"))

www <- base_consolidada_req %>% group_by(Tipo.Requerimiento, TIPO.REQUERIMIENTO) %>% summarise(n=n())

qw <- base_consolidada_req %>% group_by(TIPO.REQUERIMIENTO) %>% summarise(n=n())
# write.csv2(www, file = "prueba.csv")
# View(base_consolidada_dep)

base_consolidada_req <- base_consolidada_req %>% select(1:5,51,6:50)

View(base_consolidada_req)

base_consolidada <- base_consolidada_req



saveRDS(object = base_consolidada, file = "base2019.rds")


base <- readRDS(file = "base2019.rds")
View(base)

str(base)

library(formattable)

base$Valor.Estimado.CLM <- currency(base$Valor.Estimado.CLM, digits = 0L)

str(base$Valor.Estimado.CLM)

str(base$Valor.inicial.del.contrato.o.convenio)

base$Valor.inicial.del.contrato.o.convenio <- currency(base$Valor.inicial.del.contrato.o.convenio, digits = 0L)


saveRDS(object = base, file = "base2019.rds")


base <- readRDS(file = "base2019.rds")


valortotal <- base %>% summarise(sum(as.numeric(Valor.inicial.del.contrato.o.convenio)))
