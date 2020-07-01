# Cargamos librerias

library(dplyr); library(readxl); library(RODBC); library(tidyr)

# Consolidada
consolidada_emp <- readRDS('//BOG08HERNYATT/Analitica/BaseConsolidada/ConsolidacionMAR2020.rds') %>% 
  select(id_empresa:Num_cesantias) %>% 
  distinct()
sort(names(consolidada_emp))
str(consolidada_emp)

emp_ser_domesticos <- consolidada_emp %>% 
  filter(CIIU == 9700) %>% 
  mutate(nid_empresa = gsub("\\D","",id_empresa)) %>% 
  mutate(nid_empresa = substr(gsub("\\D", "", nid_empresa), 1,9)) %>% 
  select(id_empresa, NumIdEmpresa, RazonSocial, CIIU, nid_empresa) %>% 
  mutate_all(as.character)
str(emp_ser_domesticos)

# Empresas en mora
emp_mora <- read_excel("AnalisisRecaudo_DiaHabil_20.xlsx", sheet = "EmpresasMora") %>% 
  mutate_all(as.character)
str(emp_mora)
table(duplicated(emp_mora$NumIdEmpresa))

# Recaudo
channel <- odbcDriverConnect(
  "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=//bogak08beimrodc/bi/Proteccion_Social/Recaudo.accdb"
)
sqlTables(channel)

recaudo <- sqlQuery( 
  channel , 
  paste ("select * from Recaudo"),
  as.is=T
) %>% 
  data.frame()  

odbcCloseAll()

str(recaudo)

df_recaudo_enero <- recaudo %>% 
  filter(año >= 2020) %>% 
  filter(mes == "1") %>% 
  mutate(TipoPago=case_when(periodo_pago < 201912 ~ "Anteriores_Enero",
                            periodo_pago == 201912 ~ "Aporte_Enero",
                            periodo_pago > 201912 ~ "Anticipo_Enero")) %>% 
  select(id_empresa, TipoPago, valor_planilla) %>% 
  group_by(id_empresa, TipoPago) %>% 
  summarise(valor = sum(valor_planilla, na.rm = T)) %>% 
  ungroup() %>% 
  spread(TipoPago,valor, fill = 0) %>% 
  data.frame()
str(df_recaudo_enero)

df_recaudo_febrero <- recaudo %>% 
  filter(año >= 2020) %>% 
  filter(mes == "2") %>% 
  mutate(TipoPago=case_when(periodo_pago < 202001 ~ "Anteriores_Febrero",
                            periodo_pago == 202001 ~ "Aporte_Febrero",
                            periodo_pago > 202001 ~ "Anticipo_Febrero")) %>% 
  select(id_empresa, TipoPago, valor_planilla) %>% 
  group_by(id_empresa, TipoPago) %>% 
  summarise(valor = sum(valor_planilla, na.rm = T)) %>% 
  ungroup() %>% 
  spread(TipoPago,valor, fill = 0) %>% 
  data.frame()
str(df_recaudo_febrero)

df_recaudo_marzo <- recaudo %>% 
  filter(año >= 2020) %>% 
  filter(mes == "3") %>% 
  mutate(TipoPago=case_when(periodo_pago < 202002 ~ "Anteriores_Marzo",
                            periodo_pago == 202002 ~ "Aporte_Marzo",
                            periodo_pago > 202002 ~ "Anticipo_Marzo")) %>% 
  select(id_empresa, TipoPago, valor_planilla) %>% 
  group_by(id_empresa, TipoPago) %>% 
  summarise(valor = sum(valor_planilla, na.rm = T)) %>% 
  ungroup() %>% 
  spread(TipoPago,valor, fill = 0) %>% 
  data.frame()
str(df_recaudo_marzo)

df_union_recaudo <- df_recaudo_enero %>% 
  full_join(df_recaudo_febrero, by = "id_empresa") %>% 
  full_join(df_recaudo_marzo, by = "id_empresa") %>% 
  mutate(nid_empresa = gsub("\\D","",id_empresa)) %>% 
  mutate(nid_empresa = substr(gsub("\\D", "", nid_empresa), 1,9))
str(df_union_recaudo)

df_mora <- emp_mora %>% 
  select(NumIdEmpresa,RazonSocial) %>% 
  left_join(df_union_recaudo, by = c("NumIdEmpresa"="nid_empresa"))
str(df_mora)

library(writexl)
write_xlsx(df_mora, "df_mora.xlsx")

# Ajuste Ciiu servicios domesticos
df_mora_ciiu <- df_mora %>% 
  mutate(emp_ser_domestico = ifelse(NumIdEmpresa %in% emp_ser_domesticos$nid_empresa, "Si", "No"))
str(df_mora_ciiu)
table(df_mora_ciiu$emp_ser_domestico)

library(writexl)
write_xlsx(df_mora_ciiu, "df_mora_ciiu.xlsx")
