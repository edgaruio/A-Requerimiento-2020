# Consulta
library(dplyr); library(RODBC); library(tidyr); library(data.table)

### Consulta 1 ====
empresas <- readRDS("//Bogak08beimrodc/bi/Base_Mes/ConsolidadosMensuales/ConsolidacionFEB2020.rds") %>% 
  select(id_empresa:Num_cesantias) %>% 
  distinct()
str(empresas)

afiliados <- readRDS("//Bogak08beimrodc/bi/Base_Mes/ConsolidadosMensuales/ConsolidacionFEB2020.rds") %>% 
  filter(marca_afiliado_unico) %>% 
  select(id_persona:marca_afiliado_unico,id_empresa) %>% 
  mutate(numero_hijos = ifelse(numero_hijos >= 4, 4, numero_hijos))
str(afiliados)

# Conexion a access ---

# channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=D:/A COLSUBSIDIO/A REQUERIMIENTO/SEPTIEMBRE/Analisis_Subsidios/Consulta_sub.accdb")
# sqlTables(channel)
# consulta_subsidio <- sqlQuery(channel, paste ("select * from consulta_subsidio"),as.is=T) %>% 
#   data.frame()
# odbcCloseAll()
# str(consulta_subsidio)

# Distribucion hijos
distribucion_hijos <- afiliados %>% 
  select(id_empresa,id_persona,numero_hijos) %>% 
  group_by(id_empresa,numero_hijos) %>% 
  summarise(conteo = n()) %>% 
  spread(numero_hijos,conteo, fill = 0) %>% 
  data.frame()
names(distribucion_hijos) <- gsub("X","Afiliados_con_hijos",names(distribucion_hijos), fixed = T)
str(distribucion_hijos)


# Poblacion subsidio
consulta_subsidio <- fread("consulta_subsidio.txt", sep = ";", dec = ",") %>% 
  filter(marca_afiliado_unico == "X") %>% 
  mutate(Persona_1_Edad = round(Persona_1_Edad)) %>% 
  mutate(Edad_Beneficiarios = case_when(
    Persona_1_Edad >= 0 & Persona_1_Edad < 5 ~ "Ben_Entre_0_5",
    Persona_1_Edad >= 5 & Persona_1_Edad < 12 ~ "Ben_Entre_5_12",
    Persona_1_Edad >= 12 & Persona_1_Edad < 18 ~ "Ben_Entre_12_18",
    Persona_1_Edad >= 18 & Persona_1_Edad < 60 ~ "Ben_Entre_18_60",
    Persona_1_Edad >= 60 ~ "Ben_Mayor_60")) %>% 
  left_join(afiliados %>% dplyr::select(id_persona,numero_hijos), by = "id_persona") %>%
  mutate(numero_hijos = ifelse(is.na(numero_hijos), 0, numero_hijos)) %>% 
  data.frame()
str(consulta_subsidio)
summary(consulta_subsidio$numero_hijos)
sum(is.na(consulta_subsidio$Persona_1_Edad))
sum(is.na(consulta_subsidio$Edad_Beneficiarios))
table(duplicated(consulta_subsidio$id_persona))

df_bene_hijos0 <- consulta_subsidio %>% 
  filter(numero_hijos == 0) %>% 
  group_by(id_empresa) %>% 
  summarise(Beneficiarios_Hijos0 = n_distinct(id_persona_familiar))
str(df_bene_hijos0)

df_bene_hijos1 <- consulta_subsidio %>% 
  filter(numero_hijos == 1) %>% 
  group_by(id_empresa) %>% 
  summarise(Beneficiarios_Hijos1 = n_distinct(id_persona_familiar))
str(df_bene_hijos1)

df_bene_hijos2 <- consulta_subsidio %>% 
  filter(numero_hijos == 2) %>% 
  group_by(id_empresa) %>% 
  summarise(Beneficiarios_Hijos2 = n_distinct(id_persona_familiar))
str(df_bene_hijos2)

df_bene_hijos3 <- consulta_subsidio %>% 
  filter(numero_hijos == 3) %>% 
  group_by(id_empresa) %>% 
  summarise(Beneficiarios_Hijos3 = n_distinct(id_persona_familiar))
str(df_bene_hijos3)

df_bene_hijos4 <- consulta_subsidio %>% 
  filter(numero_hijos == 4) %>% 
  group_by(id_empresa) %>% 
  summarise(Beneficiarios_Hijos4 = n_distinct(id_persona_familiar))
str(df_bene_hijos4)

df_beneficiarios_edades <- consulta_subsidio %>% 
  group_by(id_empresa,Edad_Beneficiarios) %>% 
  summarise(Beneficiarios = n_distinct(id_persona_familiar)) %>% 
  data.frame() %>% 
  spread(Edad_Beneficiarios, Beneficiarios, fill = 0) %>% 
  data.frame()
str(df_beneficiarios_edades)
table(df_beneficiarios_edades$Edad_Beneficiarios)

bd_poblacion_beneficiaria <- empresas %>% 
  filter(Piramide2 %in% c("1.1 Platinum","1.2 Premium","2.1 Gold","2.2 Silver","3.1 VIP","3.2 VIP Estándar",
                          "4.1 Estándar","4.2 Trans. Mas de 100 Trab.","4.3 Trans.Juridica Ent. 11 a 99 Trab.",
                          "4.4 Trans.Natural Ent. 11 a 99 Trab.","4.5 Transaccional")) %>%
  select(id_empresa,RazonSocial,Piramide1,Piramide2,NumEmpleados) %>% 
  left_join(distribucion_hijos, by = "id_empresa") %>% 
  dplyr::rename(Afiliados_con_hijos4_omas=Afiliados_con_hijos4) %>%
  left_join(df_bene_hijos0, by = "id_empresa") %>%
  left_join(df_bene_hijos1, by = "id_empresa") %>%
  left_join(df_bene_hijos2, by = "id_empresa") %>%
  left_join(df_bene_hijos3, by = "id_empresa") %>%
  left_join(df_bene_hijos4, by = "id_empresa") %>%
  left_join(df_beneficiarios_edades, by = "id_empresa") %>% 
  data.frame() %>% 
  arrange(desc(NumEmpleados))
str(bd_poblacion_beneficiaria)

# str(empresas)
# bd_entrega <- empresas %>% 
#   filter(Piramide2 %in% c("1.1 Platinum","1.2 Premium","2.1 Gold","2.2 Silver","3.1 VIP","3.2 VIP Estándar",
#                           "4.1 Estándar","4.2 Trans. Mas de 100 Trab.","4.3 Trans.Juridica Ent. 11 a 99 Trab.",
#                           "4.4 Trans.Natural Ent. 11 a 99 Trab.","4.5 Transaccional")) %>% 
#   mutate(Afil_Edad_0_50 = rowSums(.[,c("Edad_0_10","Edad_10_20","Edad_20_30","Edad_30_40","Edad_40_50")], na.rm = T),
#          Afil_Edad_50_120 = rowSums(.[,c("Edad_50_60","Edad_60_70","Edad_70_80","Edad_80_90","Edad_90_100","Edad_100_110","Edad_110_120")], na.rm = T)) %>% 
#   select(id_empresa, RazonSocial, Piramide1, Piramide2, NumEmpleados, Afil_Edad_0_50, Afil_Edad_50_120, Cat_A, Cat_B, Cat_C) %>% 
#   left_join(df_consulta_subsidio , by = "id_empresa") %>% 
#   arrange(desc(NumEmpleados)) %>% 
#   mutate(Prop_Afil_Adultos = Afil_Edad_50_120/NumEmpleados,
#          Prop_Bene_joven = Ben_Entre_0_10/Total_Bene,
#          Prop_Cat_A = Cat_A/NumEmpleados) %>% 
#   mutate(Promedio = rowMeans(.[,c("Prop_Afil_Adultos","Prop_Bene_joven","Prop_Cat_A")], na.rm = T)) %>% 
#   data.frame()
# str(bd_entrega)

library(writexl)
write_xlsx(bd_poblacion_beneficiaria, "Consulta_Beneficiarios.xlsx")




