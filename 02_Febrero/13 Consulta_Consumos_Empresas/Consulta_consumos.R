# Cargamos datos
options(scipen = 999)
library(readxl); library(dplyr); library(writexl)

base_empresas <- read_excel("ListadoSegmentoEmpres -Cambios_Inf1.xlsx", sheet = "Consolidado", range = "A5:Q359") %>% 
  mutate(id_empresa = `Id Emp Filial`)
str(base_empresas)
table(duplicated(base_empresas$`Id Emp Filial`))

rentabilidad <- readRDS("consolidada_rentabilidad.rds") %>% 
  select(id_empresa,consumo_ind13,consumo_emp13)
str(rentabilidad)

base_entrega <- base_empresas %>% 
  left_join(rentabilidad, by = c("id_empresa"="id_empresa")) %>% 
  select(-id_empresa)
str(base_entrega)

writexl::write_xlsx(base_entrega, "ListadoSegmentoEmpres - Cambios_Inf1_consumos.xlsx")

