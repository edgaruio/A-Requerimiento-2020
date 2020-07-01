# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# Data tablas Conversion ----
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# Data tablas Conversion ----
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
library(RODBC)
channel <- odbcDriverConnect(
  "Driver={Microsoft Access Driver (*.mdb, *.accdb)};D:/A COLSUBSIDIO/A REQUERIMIENTO 2020/4 ABRIL/Consuulta_CM_Droguerias/Consulta_CM_Establecimientos.accdb"
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


cm_redencion2020 <- readRDS("//bogak08beimrodc/bi/Proteccion_Social/BD_MediosdePagosCM2020.rds")
str(cm_redencion2020)

table(cm_redencion2020$CodigoEstablecimiento)
