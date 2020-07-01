# Consulta_infografia

# Conectamos a access
rm(list = ls())
library(RODBC); library(data.table); library(dplyr); library(tidyr); library(readxl)


# Aportes
conn_aportes <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=D:/A COLSUBSIDIO/A REQUERIMIENTO 2020/MARZO/7 Consulta_Infografías/Aporte.accdb")
subset(sqlTables(conn_aportes), tableType = "SYSTEM TABLE")
aporte <- sqlFetch(conn_aportes, "aporte")
str(aporte)
odbcClose(conn_aportes)

# Incluimos remanentes
conn_recaudo <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=D:/A COLSUBSIDIO/A REQUERIMIENTO 2020/MARZO/7 Consulta_Infografías/Recaudo.accdb")
subset(sqlTables(conn_recaudo), tableType = "SYSTEM TABLE")
recaudo <- sqlFetch(conn_recaudo, "remanente_bruto")
str(recaudo)
odbcClose(conn_recaudo)

