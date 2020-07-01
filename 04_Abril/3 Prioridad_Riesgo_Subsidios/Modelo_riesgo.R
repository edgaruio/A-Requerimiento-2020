# Construcción modelos
library(dplyr); library(data.table); library(lubridate); library(readxl); library(writexl)

# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# Data_consolidada ----
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

consolidada <- readRDS('//BOG08HERNYATT/Analitica/BaseConsolidada/ConsolidacionFEB2020.rds') %>% 
    mutate(NumIdPersona = as.character(NumIdPersona),
           NumIdEmpresa = as.character(NumIdEmpresa))
str(consolidada)

empresas <- consolidada %>% select(NumIdEmpresa,Piramide2) %>% distinct()

# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# Empresas en riesgo ----
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
empresas_riesgo <- read_excel("Informacion/Base_cruce_w07_cobra.xlsx", sheet = "Hoja1") %>% 
  data.frame() %>% 
  select(id_persona) %>% 
  distinct() %>% 
  mutate(empresa_vulnerable = 1)
str(empresas_riesgo)
table(duplicated(empresas_riesgo$id_persona))

# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# Base solicitudes ----
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

solicitudes <- fread("Informacion/Reporte Postulaciones FOSFEC 4 Marzo.csv", encoding = 'UTF-8') %>% 
  data.frame()
names(solicitudes) <- chartr("áéíóú","aeiou",tolower(names(solicitudes)))
solicitudes_feb <- solicitudes %>% 
  mutate(fecha.de.postulacion = substr(fecha.de.postulacion, start = 1, stop = 9)) %>% 
  mutate(fecha.de.postulacion = as.Date.character(fecha.de.postulacion, format = "%m/%d/%Y"),
         anio = year(fecha.de.postulacion),
         mes = month(fecha.de.postulacion)) %>% 
  filter(anio == 2020 & mes == 2) %>% 
  mutate(estado = ifelse(estado.del.proceso == "Aceptadas", 1, 0)) %>% 
  select(numero.de.identificacion,estado)
str(solicitudes_feb)

# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# Afiliados con credito de vividenda ----
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
vivienda <- read_excel("Informacion/W070504.xlsx", sheet = "Hoja1") %>% 
  data.frame() %>% 
  distinct() %>% 
  dplyr::rename(num_id_persona=NUMERO.DE.IDENTIFICACION.DE) %>% 
  mutate(num_id_persona = as.character(num_id_persona),
         prestamo_vivienda = 1)
str(vivienda)
table(duplicated(vivienda$num_id_persona))

# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# Base modelo ----
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
sort(names(consolidada))
base_modelo <- consolidada %>% 
  filter(marca_afiliado_unico) %>% 
  select(id_persona:Segmento_poblacional,total_numero_grupo_familiar,estado_civil:segmento_grupo_familiar,id_empresa,Piramide1,Piramide2) %>% 
  left_join(solicitudes_feb, by = c("NumIdPersona"="numero.de.identificacion")) %>% 
  left_join(empresas_riesgo, by = "id_persona") %>% 
  left_join(vivienda, by = c("NumIdPersona"="num_id_persona")) %>% 
  filter(!is.na(estado)) %>% 
  mutate(empresa_vulnerable = ifelse(is.na(empresa_vulnerable), 0, 1),
         prestamo_vivienda = ifelse(is.na(prestamo_vivienda), 0, 1),
         Categoria = as.character(Categoria),
         estado = as.factor(estado)) %>% 
  select(c(i_persona,estado,prestamo_vivienda,empresa_vulnerable,estado_civil,Genero,Categoria,Piramide2)) %>% 
  na.omit()
str(base_modelo)
levels(base_modelo$estado)

write_xlsx(base_modelo, "base_modelo.xlsx")


# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# Construccion modelo ----
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

m1_t <- glm(estado~.,data=base_modelo,family=binomial(logit))
summary(m1_t)
m1_t <- MASS::stepAIC(m1_t, direction = "both")
summary(m1_t)

modelo_final <- glm(estado~Categoria + Piramide2, data=base_modelo,family=binomial(logit))
summary(modelo_final)

# Predicciones ajuste base solicitudes
str(solicitudes)
solicitudes_predict <- solicitudes %>% 
  mutate(Genero = sexo,
         Categoria = as.character(toupper(categoria.de.cotizacion))) %>% 
  left_join(empresas, by = c("nit"="NumIdEmpresa")) %>% 
  select(Categoria,Piramide2) %>% 
  mutate(Categoria = ifelse(is.na(Categoria) | Categoria == "", "A", Categoria))
str(solicitudes_predict)
table(solicitudes_predict$Categoria)


predicciones <- predict(object = modelo_final, type = "response", newdata=solicitudes_predict)

solicitudes$Score <- predicciones
write_xlsx(solicitudes, "salida_solicitudes.xlsx")
