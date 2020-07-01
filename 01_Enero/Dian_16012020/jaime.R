# Consulta 0

# Requerimiento_2020_01_17_Key_z7zim1tB <- read_excel("Requerimiento_2020-01-17_Key_z7zim1tB.xlsx", sheet = "Data_Dane")
# View(Requerimiento_2020_01_17_Key_z7zim1tB)

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
# Data tablas Conversion ----
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

channel <- odbcDriverConnect(
  "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=//bogak08beimrodc/bi/Tabla_conversion/Tabla_conversion.accdb"
)
sqlTables(channel)

Tb_localidad<- sqlQuery( 
  channel , 
  paste ("select * from Tb_localidad"),
  as.is=T
) %>% 
  data.frame()

Tb_Division_Politica_Dane<- sqlQuery( 
  channel , 
  paste ("select * from Tb_Division_Politica_Dane"),
  as.is=T
) %>% 
  data.frame()

odbcCloseAll() 

# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# Data Geo ----
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

channel <- odbcDriverConnect(
  "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=//bogak08beimrodc/bi/Contacto/Fuentes/Direccion.accdb" # si no te sirve cambiar desde  DBQ=
)
sqlTables(channel) # sacar nombre de la tabla que queremos

direccionVive <- sqlQuery( 
  channel , 
  paste ("select * from tb_direccion_de_residencia"), # despues del FROM poner nombre de tabla
  as.is=T
) %>% 
  data.frame()  

odbcCloseAll()

direccionVive <- direccionVive %>% 
  left_join(Tb_localidad %>% rename(localidad = codigo_localidad, Localidad = localidad)) %>% 
  left_join(Tb_Division_Politica_Dane) %>% 
  select(id_persona, nivel_socio_economico_residente,barrio,Localidad,nombre_departamento, nombre_municipio)%>% 
  filter(
    !duplicated(id_persona)
  )

# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# 7. DATA : Habeas y contacto ----
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

saveRDS(tb_autorizaciones, file = "D:/A COLSUBSIDIO/A REQUERIMIENTO 2020/ENERO/Boreal_perfil_comprador/tb_autorizaciones.rds")


# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# Construir ddata final ----
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

Data <- Data_consolidada %>% 
  left_join(
    direccionVive
  )%>% 
  filter(
    between(Salario,
            0,4*877803)
    ) %>% 
  left_join(
    DATA_Habeas
  )%>% 
  mutate(Contacto_autorizacion = ifelse(is.na(Contacto_autorizacion),'-Gris-',Contacto_autorizacion) )%>% 
  mutate(Contacto_Mail = ifelse(is.na(Contacto_Mail),'No_Disponible','Si_Disponible') )%>% 
  mutate(Contacto_Celular = ifelse(is.na(Contacto_Celular),'No_Disponible','Si_Disponible') )

# descriptivos 

with(Data,table(Contacto_autorizacion,Contacto_Mail))
with(Data,table(Contacto_autorizacion,Contacto_Celular))

# data_final ----

Data_final <- Data %>% 
  filter(
    toupper(Contacto_autorizacion) == 'SI'
  )

# descriptivos 

with(Data_final,table(Contacto_Celular,Contacto_Mail))
View(with(Data_final,table(Genero,estado_civil)) %>% data.frame() %>% spread(estado_civil, Freq))
View(with(Data_final,table(Localidad,LocalidadEmpresa)) %>% data.frame() %>% spread(LocalidadEmpresa, Freq))


########################################################################
# Exportar en excel entregable ----
########################################################################

# primer Requerimiento
writexl::write_xlsx(
  list(
    Aviso = data.frame(
      Aviso = c(
        "Cualquier duda o inconveniente con el manejo de esta información por favor mandar un correo a jaime.vanegasp@colsubsidio.com",
        ' * Habeas data',
        'El principio de seguridad impone que en la información contenida en este archivo, se incorporen las medidas técnicas necesarias para garantizar la seguridad de los registros, con el fin de evitar su adulteración, pérdida, consulta o uso no autorizado.',
        'Describiendo el proceso y retroalimentación de la información entregada: ',
        ' ** Utilización de la Información deacuerdo al Manual interno de políticas y procedimientos para el tratamiento de datos personales.',
        ' ** Suministrar la información que en Tratamiento o según el caso nos retroalimenten, generando calidad y mejorar la información procesada.',
        ' ** Eliminación o destrucción de la información seguidamente de su utilización.',
        ' ** Confirmación mediante Correo y/o persona responsable quien generó esta actividad.',
        "Cualquier duda o inconveniente con el manejo de esta información por favor mandar un correo a jaime.vanegasp@colsubsidio.com"
      )
    ),
    Data = Data_final %>% select(id_persona) # Poner data que quiero exportar

  ),
  paste0(
    "D:/A COLSUBSIDIO/A REQUERIMIENTO 2020/ENERO/Dian_16012020_", # DIrectorio
    as.character(Sys.Date()),
    # Llave unica
    "_Key_",
    stringi::stri_rand_shuffle(
      stringi::stri_paste(
        stringi::stri_rand_strings(
          1, 8, '[a-zA-Z0-9]'
        )
      )
    ),
    '.xlsx'
  )
)



### Entrega consulta
table(duplicated(direccionVive$id_persona))
consolidada <- Data_consolidada %>% 
  select(id_persona) %>% 
  left_join(direccionVive %>% select(id_persona,localidad), by = "id_persona")
str(consolidada)

test <- consolidada %>% 
  mutate(localidad = ifelse(is.na(localidad),"Sin localidad", localidad)) %>% 
  group_by(localidad) %>% 
  summarise(conteo = n_distinct(id_persona)) %>%
  arrange(desc(conteo))
sum(test$conteo)  
table(duplicated(consolidada$id_persona))
