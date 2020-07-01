# Consultas descriptivos
library(dplyr); library(data.table)

consulta_8munic <- fread("consulta_8_municipios.csv")
str(consulta_8munic)

consolidada <- readRDS("ConsolidacionDIC2019.rds")
names(consolidada)

rentabilidad <- readRDS("consolidada_rentabilidad.rds") %>% 
  select(id_empresa,Noviembre_2019,aporte13,Rneto_Noviembre_2019,remanente_neto13)
str(rentabilidad)

personas <- consolidada %>% 
  select(id_persona:marca_afiliado_unico,id_empresa,Piramide1,Piramide2,MunicipioEmpresa) %>% 
  filter(marca_afiliado_unico)
str(personas)

empresas <- consolidada %>% 
  select(id_empresa:Num_cesantias) %>% 
  distinct()
str(empresas)

afiliados_obj <- fread("Afiliados_Obj_2020-02-12 22_05_38.csv") %>% 
  select(id_persona) %>% 
  left_join(personas, by = "id_persona")
str(afiliados_obj)

empresas_obj <- fread("nits_8_municipios.csv") %>% 
  data.frame() %>% 
  left_join(rentabilidad, by = "id_empresa") %>% 
  left_join(empresas %>% select(id_empresa,Piramide1,Piramide2,MunicipioEmpresa), by = "id_empresa")
str(empresas_obj)

empresas_no_afil <- fread("NoAfiliadas.csv")
str(empresas_no_afil)
table(empresas_no_afil$Nom_Mun)


#### RESULTADOS ====
### TOP EMPRESAS SILVER === 
top_empresas_noafil <- empresas_no_afil %>% 
  filter(Piramide2 == "2.2 Silver") %>% 
  arrange(desc(empleados))
str(top_empresas_noafil)
writexl::write_xlsx(top_empresas_noafil,"Resultados/top_empresas_silver.xlsx")

### TEST AGREGADOS AFILIADOS POR PIRAMIDE ===
library(writexl)
test1 <- afiliados_obj %>% 
  group_by(Piramide1,Piramide2) %>% 
  summarise(Empleados_afil = n_distinct(id_persona),
            )
str(test1)

test1_noafil <- empresas_no_afil %>% 
  group_by(Piramide1,Piramide2) %>% 
  summarise(Emplados_noafil = sum(empleados, na.rm = T)) 

test_agreado_pir <- test1 %>% 
  left_join(test1_noafil, by = c("Piramide1"="Piramide1","Piramide2"="Piramide2"))

writexl::write_xlsx(test_agreado_pir,"Resultados/agreado_piramide.xlsx")

### TEST AGREGADOS EMPRESAS POR PIRAMIDE ===
library(writexl)
test1 <- empresas_obj %>% 
  group_by(Piramide1,Piramide2) %>% 
  summarise(numero_empresas_afil = n_distinct(id_empresa))
str(test1)

test1_noafil <- empresas_no_afil %>% 
  group_by(Piramide1,Piramide2) %>% 
  summarise(numero_empresas_noafil = n_distinct(nit)) 

test_agreado_nit_pir <- test1 %>% 
  left_join(test1_noafil, by = c("Piramide1"="Piramide1","Piramide2"="Piramide2"))

writexl::write_xlsx(test_agreado_nit_pir,"Resultados/agreado_nit_piramide.xlsx")


### TEST AGREGADOS APORTE POR PIRAMIDE ===
options(scipen = 999)
test_aporte_afil <- empresas_obj %>% 
  # left_join(empresas %>% select(id_empresa,Piramide1,Piramide2), by = "id_empresa") %>% 
  group_by(Piramide1,Piramide2) %>% 
  summarise(conteo_emp_afil = n_distinct(id_empresa),
            aporte_anio = sum(aporte13)/1000000,
            remanente_anio = sum(remanente_neto13)/1000000)

test_aporte_noafil <- empresas_no_afil %>% 
  group_by(Piramide1,Piramide2) %>% 
  summarise(conteo_emp_noafil = n_distinct(nit),
            aporte_anio_noafil = sum(Aporte)/1000000,
            remanente_anio_noafil = sum(Remanente)/1000000)

test_aporte <- test_aporte_afil %>% 
  left_join(test_aporte_noafil, by = c("Piramide1"="Piramide1","Piramide2"="Piramide2"))

writexl::write_xlsx(test_aporte,"Resultados/agreado_aporte_piramide.xlsx")

### TEST AGREGADOS AFILIADOS POR MUNICIPIO ===
afil_viven_trabajan <- afiliados_obj %>% 
  filter(MunicipioPersona %in% consulta_8munic$NOMBRE & MunicipioEmpresa %in% consulta_8munic$NOMBRE)

test_municipio1 <- afiliados_obj %>% 
  filter(MunicipioPersona %in% consulta_8munic$NOMBRE,
         !(id_persona %in% afil_viven_trabajan$id_persona)) %>% 
  group_by(MunicipioPersona) %>% 
  summarise(Empleados_afil_viven = n_distinct(id_persona)) %>% 
  data.frame()
str(test_municipio1)

test_municipio2 <- afiliados_obj %>% 
  filter(MunicipioEmpresa %in% consulta_8munic$NOMBRE,
         !(id_persona %in% afil_viven_trabajan$id_persona)) %>% 
  group_by(MunicipioEmpresa) %>% 
  summarise(Empleados_afil_trabajan = n_distinct(id_persona)) %>% 
  data.frame()
str(test_municipio2)

test_municipio3 <- afiliados_obj %>% 
  filter(MunicipioPersona %in% consulta_8munic$NOMBRE & MunicipioEmpresa %in% consulta_8munic$NOMBRE) %>% 
  group_by(MunicipioEmpresa) %>% 
  summarise(Empleados_afil_viventrabajan = n_distinct(id_persona)) %>% 
  data.frame()
str(test_municipio3)

test_municipio_noafil <- empresas_no_afil %>% 
  filter(Nom_Mun %in% consulta_8munic$NOMBRE) %>% 
  group_by(Nom_Mun) %>% 
  summarise(Empleados_noafil = sum(empleados, na.rm = T))
str(test_municipio_noafil)
  
test_agreado_mun <- test_municipio1 %>% 
  left_join(test_municipio2, by = c("MunicipioPersona"="MunicipioEmpresa")) %>% 
  left_join(test_municipio3, by = c("MunicipioPersona"="MunicipioEmpresa")) %>% 
  left_join(test_municipio_noafil, by = c("MunicipioPersona"="Nom_Mun"))
str(test_agreado_mun)
sum(test_agreado_mun$Empleados_afil_viven)+sum(test_agreado_mun$Empleados_afil_trabajan)+sum(test_agreado_mun$Empleados_afil_viventrabajan)
writexl::write_xlsx(test_agreado_mun,"Resultados/agreado_afiliado_municipio.xlsx")

#### Para trabajadores

test_emp <- personas %>% 
  filter(id_empresa %in% empresas_obj$id_empresa) %>% 
  group_by(MunicipioEmpresa) %>% 
  count()



### CONSULTA CONSUMO EMPRESARIAL ====
options(scipen = 999)
consumo_emp <- readRDS("consumo_empresarial_Diciembre.rds")
str(consumo_emp)

agr_consumo_emp <- consumo_emp %>% 
  filter(anno == 2019,
         id_empresa %in% empresas_obj$id_empresa) %>% 
  left_join(empresas_obj %>% select(id_empresa,Piramide1,Piramide2), by = "id_empresa") %>% 
  group_by(Piramide1, Piramide2) %>% 
  summarise(consumo = sum(consumo))
writexl::write_xlsx(agr_consumo_emp,"Resultados/consumo_empresarial2019.xlsx")

unique(agr_consumo_emp$id_empresa)
