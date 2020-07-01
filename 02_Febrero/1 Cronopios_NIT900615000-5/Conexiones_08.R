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
# Tb Tabla homologacion
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=//bogak08beimrodc/bi/Tabla_conversion/Tabla_conversion.accdb")
sqlTables(channel)
tb_segmento_poblacional <- sqlQuery(channel, paste ("select * from Tb_segmento_poblacional"), as.is=T) %>% 
  data.frame()
tb_piramide1 <- sqlQuery(channel, paste ("select * from Tb_Piramide1"), as.is=T) %>% 
  data.frame()
tb_piramide2 <- sqlQuery(channel, paste ("select * from Tb_Piramide2"), as.is=T) %>% 
  data.frame()
tb_grupo_familiar <- sqlQuery(channel, paste ("select * from Tb_segmento_grupo_familia"), as.is=T) %>% 
  data.frame()
odbcCloseAll() 
odbcCloseAll() 

channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=//bogak08beimrodc/bi/Persona/PERSONA.accdb")
sqlTables(channel)
Tb_persona<- sqlQuery(channel, paste ("select id_persona, Primer_nombre, Primer_apellido, Genero, Edad from Persona"), as.is=T) %>% 
  data.frame()
str(Tb_persona)
odbcCloseAll()
table(duplicated(Tb_persona$id_persona))

# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# Tb Afiliados
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=//bogak08beimrodc/bi/Persona/Afiliados 2018.accdb")
sqlTables(channel)
Tb_2018<- sqlQuery(channel, paste ("select * from Afiliados"), as.is=T) %>% 
  data.frame() %>% 
  filter(id_empresa == "NIT9006150005")
odbcCloseAll() 

channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=//bogak08beimrodc/bi/Persona/Afiliados 2019.accdb")
sqlTables(channel)
Tb_2019<- sqlQuery(channel, paste ("select * from Afiliados"), as.is=T) %>% 
  data.frame() %>% 
  filter(id_empresa == "NIT9006150005")
odbcCloseAll() 
str(Tb_2019)

# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# Construir data final ----
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
str(tb_grupo_familiar)
bd_entrega <- bind_rows(Tb_2018 %>% mutate(anio = 2018), Tb_2019 %>% mutate(anio = 2019)) %>% 
  left_join(tb_segmento_poblacional, by = "codigo_segmento_poblacional") %>% 
  left_join(tb_grupo_familiar, by = c("grupo_familiar"="codigo_segmento_grupo_familiar")) %>%
  select(-c(id_empresa,no_documento_persona,codigo_segmento_poblacional,grupo_familiar,codigo_piramide_2,
            codigo_tipo_aportante)) %>% 
  left_join(Tb_persona, by = "id_persona") %>% 
  mutate(anio_mes = factor(paste0(anio,mes), levels = c("20181","20182","20183","20184","20185","20186","20187","20188","20189","201810","201811","201812",
                                                        "20191","20192","20193","20194","20195","20196","20197","20198","20199","201910","201911","201912")))
str(bd_entrega)

# descriptivos 
test <- with(bd_entrega,table(anio_mes)) %>% data.frame()
library(esquisse)
esquisse::esquisser()

library(ggplot2)

# ggplot(test) +
#   aes(x = anio_mes, weight = Freq) +
#   geom_bar(fill = "#1f9e89") +
#   geom_text(aes(label=Freq), vjust=1.6, color="white", size=3.5) +
#   labs(x = "Fecha", y = "Conteo", title = "Distribueción Afiliados por mes", subtitle = "Empresa Cronopios") +
#   coord_flip() +
#   theme_classic()

ggplot(data=test, aes(x=anio_mes, y=Freq)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=Freq), vjust=-0.3, color="gray", size=2.5) +
  theme_classic() +
  labs(x = "Fecha", y = "Conteo", title = "Distribución Afiliados por mes", subtitle = "Empresa Cronopios")



# descriptivos 
# with(Data_final,table(Contacto_Celular,Contacto_Mail))
# View(with(Data_final,table(Genero,estado_civil)) %>% data.frame() %>% spread(estado_civil, Freq))
# View(with(Data_final,table(Localidad,LocalidadEmpresa)) %>% data.frame() %>% spread(LocalidadEmpresa, Freq))


########################################################################
# Exportar en excel entregable ----
########################################################################

# primer Requerimiento
library(writexl)
writexl::write_xlsx(bd_entrega, "bd_consulta_CRONOPIOS.xlsx")

# ### Entrega consulta
# table(duplicated(direccionVive$id_persona))
# consolidada <- Data_consolidada %>% 
#   select(id_persona) %>% 
#   left_join(direccionVive %>% select(id_persona,localidad), by = "id_persona")
# str(consolidada)
# 
# test <- consolidada %>% 
#   mutate(localidad = ifelse(is.na(localidad),"Sin localidad", localidad)) %>% 
#   group_by(localidad) %>% 
#   summarise(conteo = n_distinct(id_persona)) %>%
#   arrange(desc(conteo))
# sum(test$conteo)  
# table(duplicated(consolidada$id_persona))
