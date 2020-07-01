# Analisis exploratorio
library(dplyr); library(esquisse)

# Cargar datos
df_afiliados <- readRDS("Salida/df_poblacion_potencial_28052020.rds") %>% 
  mutate(nuevo_ciiu = ifelse(SectorCIIU == "Prestación de Servicios (Educación, Banca, Comunicaciones, Seguros, Salud, Arte y Cultura)",
                             ActividadCIIU,SectorCIIU)) %>% 
  data.frame()
str(df_afiliados)

### Por Piramide ====

## Por categoria C ===
df_afiliados_ene_c <- df_afiliados %>% 
  filter(ENE2020 == 1) %>% 
  filter(Categoria == "C") %>% 
  group_by(Piramide1,Piramide2) %>% 
  summarise(C_ENE2020 = n_distinct(id_persona)) %>% 
  data.frame()
str(df_afiliados_ene_c)

df_afiliados_feb_c <- df_afiliados %>% 
  filter(FEB2020 == 1) %>% 
  filter(Categoria == "C") %>% 
  group_by(Piramide1,Piramide2) %>% 
  summarise(C_FEB2020 = n_distinct(id_persona)) %>% 
  data.frame()
str(df_afiliados_feb_c)

df_afiliados_mar_c <- df_afiliados %>% 
  filter(MAR2020 == 1) %>% 
  filter(Categoria == "C") %>% 
  group_by(Piramide1,Piramide2) %>% 
  summarise(C_MAR2020 = n_distinct(id_persona)) %>% 
  data.frame()
str(df_afiliados_mar_c)

df_afiliados_abr_c <- df_afiliados %>% 
  filter(ABR2020 == 1) %>% 
  filter(Categoria == "C") %>% 
  group_by(Piramide1,Piramide2) %>% 
  summarise(C_ABR2020 = n_distinct(id_persona)) %>% 
  data.frame()
str(df_afiliados_abr_c)

df_c <- df_afiliados_ene_c %>% 
  left_join(df_afiliados_feb_c, by = c("Piramide1"="Piramide1","Piramide2"="Piramide2")) %>% 
  left_join(df_afiliados_mar_c, by = c("Piramide1"="Piramide1","Piramide2"="Piramide2")) %>% 
  left_join(df_afiliados_abr_c, by = c("Piramide1"="Piramide1","Piramide2"="Piramide2"))
str(df_c)
rm(df_afiliados_ene_c, df_afiliados_feb_c, df_afiliados_mar_c, df_afiliados_abr_c)

## Por categoria B ===
df_afiliados_ene_b <- df_afiliados %>% 
  filter(ENE2020 == 1) %>% 
  filter(Categoria == "B") %>% 
  group_by(Piramide1,Piramide2) %>% 
  summarise(B_ENE2020 = n_distinct(id_persona)) %>% 
  data.frame()
str(df_afiliados_ene_b)

df_afiliados_feb_b <- df_afiliados %>% 
  filter(FEB2020 == 1) %>% 
  filter(Categoria == "B") %>% 
  group_by(Piramide1,Piramide2) %>% 
  summarise(B_FEB2020 = n_distinct(id_persona)) %>% 
  data.frame()
str(df_afiliados_feb_b)

df_afiliados_mar_b <- df_afiliados %>% 
  filter(MAR2020 == 1) %>% 
  filter(Categoria == "B") %>% 
  group_by(Piramide1,Piramide2) %>% 
  summarise(B_MAR2020 = n_distinct(id_persona)) %>% 
  data.frame()
str(df_afiliados_mar_b)

df_afiliados_abr_b <- df_afiliados %>% 
  filter(ABR2020 == 1) %>% 
  filter(Categoria == "B") %>% 
  group_by(Piramide1,Piramide2) %>% 
  summarise(B_ABR2020 = n_distinct(id_persona)) %>% 
  data.frame()
str(df_afiliados_abr_b)

df_b <- df_afiliados_ene_b %>% 
  left_join(df_afiliados_feb_b, by = c("Piramide1"="Piramide1","Piramide2"="Piramide2")) %>% 
  left_join(df_afiliados_mar_b, by = c("Piramide1"="Piramide1","Piramide2"="Piramide2")) %>% 
  left_join(df_afiliados_abr_b, by = c("Piramide1"="Piramide1","Piramide2"="Piramide2"))
str(df_b)
rm(df_afiliados_ene_b, df_afiliados_feb_b, df_afiliados_mar_b, df_afiliados_abr_b)

## Por categoria A ===
df_afiliados_ene_a <- df_afiliados %>% 
  filter(ENE2020 == 1) %>% 
  filter(Categoria == "A") %>% 
  group_by(Piramide1,Piramide2) %>% 
  summarise(A_ENE2020 = n_distinct(id_persona)) %>% 
  data.frame()
str(df_afiliados_ene_a)

df_afiliados_feb_a <- df_afiliados %>% 
  filter(FEB2020 == 1) %>% 
  filter(Categoria == "A") %>% 
  group_by(Piramide1,Piramide2) %>% 
  summarise(A_FEB2020 = n_distinct(id_persona)) %>% 
  data.frame()
str(df_afiliados_feb_a)

df_afiliados_mar_a <- df_afiliados %>% 
  filter(MAR2020 == 1) %>% 
  filter(Categoria == "A") %>% 
  group_by(Piramide1,Piramide2) %>% 
  summarise(A_MAR2020 = n_distinct(id_persona)) %>% 
  data.frame()
str(df_afiliados_mar_a)

df_afiliados_abr_a <- df_afiliados %>% 
  filter(ABR2020 == 1) %>% 
  filter(Categoria == "A") %>% 
  group_by(Piramide1,Piramide2) %>% 
  summarise(A_ABR2020 = n_distinct(id_persona)) %>% 
  data.frame()
str(df_afiliados_abr_a)

df_a <- df_afiliados_ene_a %>% 
  left_join(df_afiliados_feb_a, by = c("Piramide1"="Piramide1","Piramide2"="Piramide2")) %>% 
  left_join(df_afiliados_mar_a, by = c("Piramide1"="Piramide1","Piramide2"="Piramide2")) %>% 
  left_join(df_afiliados_abr_a, by = c("Piramide1"="Piramide1","Piramide2"="Piramide2"))
str(df_a)
rm(df_afiliados_ene_a, df_afiliados_feb_a, df_afiliados_mar_a, df_afiliados_abr_a)

df_categoria <- df_a %>% 
  left_join(df_b, by = c("Piramide1"="Piramide1","Piramide2"="Piramide2")) %>% 
  left_join(df_c, by = c("Piramide1"="Piramide1","Piramide2"="Piramide2"))
rm(df_a, df_b, df_c)

### Por Sector ====
table(df_afiliados$SectorCIIU)

df_afiliados_sector_ene <- df_afiliados %>% 
  filter(ENE2020 == 1) %>% 
  group_by(nuevo_ciiu) %>% 
  summarise(ENE2020 = n_distinct(id_persona)) %>% 
  data.frame()
str(df_afiliados_sector_ene)

df_afiliados_sector_feb <- df_afiliados %>% 
  filter(FEB2020 == 1) %>% 
  group_by(nuevo_ciiu) %>% 
  summarise(FEB2020 = n_distinct(id_persona)) %>% 
  data.frame()
str(df_afiliados_sector_feb)

df_afiliados_sector_mar <- df_afiliados %>% 
  filter(MAR2020 == 1) %>% 
  group_by(nuevo_ciiu) %>% 
  summarise(MAR2020 = n_distinct(id_persona)) %>% 
  data.frame()
str(df_afiliados_sector_mar)

df_afiliados_sector_abr <- df_afiliados %>% 
  filter(ABR2020 == 1) %>% 
  group_by(nuevo_ciiu) %>% 
  summarise(ABR2020 = n_distinct(id_persona)) %>% 
  data.frame()
str(df_afiliados_sector_abr)

df_sector <- df_afiliados_sector_ene %>% 
  left_join(df_afiliados_sector_feb, by = "nuevo_ciiu") %>% 
  left_join(df_afiliados_sector_mar, by = "nuevo_ciiu") %>%
  left_join(df_afiliados_sector_abr, by = "nuevo_ciiu") %>%
  na.omit()
str(df_sector)
rm(df_afiliados_sector_ene, df_afiliados_sector_feb, df_afiliados_sector_mar, df_afiliados_sector_abr)

### Por actividad ====
unique(df_afiliados$ActividadCIIU)

df_afiliados_act_ene <- df_afiliados %>% 
  filter(ENE2020 == 1) %>% 
  group_by(ActividadCIIU) %>% 
  summarise(ENE2020 = n_distinct(id_persona)) %>% 
  data.frame()
str(df_afiliados_act_ene)

df_afiliados_act_feb <- df_afiliados %>% 
  filter(FEB2020 == 1) %>% 
  group_by(ActividadCIIU) %>% 
  summarise(FEB2020 = n_distinct(id_persona)) %>% 
  data.frame()
str(df_afiliados_act_feb)

df_afiliados_act_mar <- df_afiliados %>% 
  filter(MAR2020 == 1) %>% 
  group_by(ActividadCIIU) %>% 
  summarise(MAR2020 = n_distinct(id_persona)) %>% 
  data.frame()
str(df_afiliados_act_mar)

df_afiliados_act_abr <- df_afiliados %>% 
  filter(ABR2020 == 1) %>% 
  group_by(ActividadCIIU) %>% 
  summarise(ABR2020 = n_distinct(id_persona)) %>% 
  data.frame()
str(df_afiliados_act_abr)

df_act <- df_afiliados_act_ene %>% 
  left_join(df_afiliados_act_feb, by = "ActividadCIIU") %>% 
  left_join(df_afiliados_act_mar, by = "ActividadCIIU") %>% 
  left_join(df_afiliados_act_abr, by = "ActividadCIIU") %>% 
  na.omit()
str(df_act)
rm(df_afiliados_act_ene, df_afiliados_act_feb, df_afiliados_act_mar, df_afiliados_act_abr)

### Por perdida de empleo Piramide ====

desempleados_ene <- df_afiliados %>% 
  filter(DIC2019 == 1 & ENE2020 == 0 & FEB2020 == 0 & MAR2020 == 0 & ABR2020 == 0) %>% 
  group_by(Piramide1,Piramide2) %>% 
  summarise(Desempleados_ENE2020 = n_distinct(id_persona)) %>% 
  data.frame()
str(desempleados_ene)

desempleados_feb <- df_afiliados %>% 
  filter(ENE2020 == 1 & FEB2020 == 0 & MAR2020 == 0 & ABR2020 == 0) %>% 
  group_by(Piramide1,Piramide2) %>% 
  summarise(Desempleados_FEB2020 = n_distinct(id_persona)) %>% 
  data.frame()
str(desempleados_feb)

desempleados_mar <- df_afiliados %>% 
  filter(FEB2020 == 1 & MAR2020 == 0 & ABR2020 == 0) %>% 
  group_by(Piramide1,Piramide2) %>% 
  summarise(Desempleados_MAR2020 = n_distinct(id_persona)) %>% 
  data.frame()
str(desempleados_mar)

desempleados_abr <- df_afiliados %>% 
  filter(MAR2020 == 1 & ABR2020 == 0) %>% 
  group_by(Piramide1,Piramide2) %>% 
  summarise(Desempleados_ABR2020 = n_distinct(id_persona)) %>% 
  data.frame()
str(desempleados_abr)

desempleados_piramide <- desempleados_ene %>% 
  left_join(desempleados_feb, by = c("Piramide1"="Piramide1","Piramide2"="Piramide2")) %>% 
  left_join(desempleados_mar, by = c("Piramide1"="Piramide1","Piramide2"="Piramide2")) %>% 
  left_join(desempleados_abr, by = c("Piramide1"="Piramide1","Piramide2"="Piramide2"))
str(desempleados_piramide)
rm(desempleados_ene, desempleados_feb, desempleados_mar, desempleados_abr)

### Por perdida de empleo Sector y Categoria ====

desempleados_ene_sector <- df_afiliados %>% 
  filter(DIC2019 == 1 & ENE2020 == 0 & FEB2020 == 0 & MAR2020 == 0 & ABR2020 == 0) %>% 
  group_by(nuevo_ciiu) %>% 
  summarise(Desempleados_ENE2020 = n_distinct(id_persona)) %>% 
  data.frame()
str(desempleados_ene_sector)

desempleados_feb_sector <- df_afiliados %>% 
  filter(ENE2020 == 1 & FEB2020 == 0 & MAR2020 == 0 & ABR2020 == 0) %>% 
  group_by(nuevo_ciiu) %>% 
  summarise(Desempleados_FEB2020 = n_distinct(id_persona)) %>% 
  data.frame()
str(desempleados_feb_sector)

desempleados_mar_sector <- df_afiliados %>% 
  filter(FEB2020 == 1 & MAR2020 == 0 & ABR2020 == 0) %>% 
  group_by(nuevo_ciiu) %>% 
  summarise(Desempleados_MAR2020 = n_distinct(id_persona)) %>% 
  data.frame()
str(desempleados_mar_sector)

desempleados_abr_sector <- df_afiliados %>% 
  filter(MAR2020 == 1 & ABR2020 == 0) %>% 
  group_by(nuevo_ciiu) %>% 
  summarise(Desempleados_ABR2020 = n_distinct(id_persona)) %>% 
  data.frame()
str(desempleados_abr_sector)

desempleados_sector <- desempleados_ene_sector %>% 
  left_join(desempleados_feb_sector, by = "nuevo_ciiu") %>% 
  left_join(desempleados_mar_sector, by = "nuevo_ciiu") %>% 
  left_join(desempleados_abr_sector, by = "nuevo_ciiu")
str(desempleados_sector)
rm(desempleados_ene_sector, desempleados_feb_sector, desempleados_mar_sector, desempleados_abr_sector)


desempleados_ene_sector <- df_afiliados %>% 
  filter(Categoria == "A") %>%
  filter(DIC2019 == 1 & ENE2020 == 0 & FEB2020 == 0 & MAR2020 == 0 & ABR2020 == 0) %>% 
  group_by(nuevo_ciiu) %>% 
  summarise(Desempleados_ENE2020 = n_distinct(id_persona)) %>% 
  data.frame()
str(desempleados_ene_sector)

desempleados_feb_sector <- df_afiliados %>% 
  filter(Categoria == "A") %>%
  filter(ENE2020 == 1 & FEB2020 == 0 & MAR2020 == 0 & ABR2020 == 0) %>% 
  group_by(nuevo_ciiu) %>% 
  summarise(Desempleados_FEB2020 = n_distinct(id_persona)) %>% 
  data.frame()
str(desempleados_feb_sector)

desempleados_mar_sector <- df_afiliados %>% 
  filter(Categoria == "A") %>%
  filter(FEB2020 == 1 & MAR2020 == 0 & ABR2020 == 0) %>% 
  group_by(nuevo_ciiu) %>% 
  summarise(Desempleados_MAR2020 = n_distinct(id_persona)) %>% 
  data.frame()
str(desempleados_mar_sector)

desempleados_abr_sector <- df_afiliados %>% 
  filter(Categoria == "A") %>%
  filter(MAR2020 == 1 & ABR2020 == 0) %>% 
  group_by(nuevo_ciiu) %>% 
  summarise(Desempleados_ABR2020 = n_distinct(id_persona)) %>% 
  data.frame()
str(desempleados_abr_sector)

desempleados_sector_a <- desempleados_ene_sector %>% 
  left_join(desempleados_feb_sector, by = "nuevo_ciiu") %>% 
  left_join(desempleados_mar_sector, by = "nuevo_ciiu") %>% 
  left_join(desempleados_abr_sector, by = "nuevo_ciiu")
str(desempleados_sector_a)
rm(desempleados_ene_sector, desempleados_feb_sector, desempleados_mar_sector, desempleados_abr_sector)


desempleados_ene_sector <- df_afiliados %>% 
  filter(Categoria == "B") %>%
  filter(DIC2019 == 1 & ENE2020 == 0 & FEB2020 == 0 & MAR2020 == 0 & ABR2020 == 0) %>% 
  group_by(nuevo_ciiu) %>% 
  summarise(Desempleados_ENE2020 = n_distinct(id_persona)) %>% 
  data.frame()
str(desempleados_ene_sector)

desempleados_feb_sector <- df_afiliados %>% 
  filter(Categoria == "B") %>%
  filter(ENE2020 == 1 & FEB2020 == 0 & MAR2020 == 0 & ABR2020 == 0) %>% 
  group_by(nuevo_ciiu) %>% 
  summarise(Desempleados_FEB2020 = n_distinct(id_persona)) %>% 
  data.frame()
str(desempleados_feb_sector)

desempleados_mar_sector <- df_afiliados %>% 
  filter(Categoria == "B") %>%
  filter(FEB2020 == 1 & MAR2020 == 0 & ABR2020 == 0) %>% 
  group_by(nuevo_ciiu) %>% 
  summarise(Desempleados_MAR2020 = n_distinct(id_persona)) %>% 
  data.frame()
str(desempleados_mar_sector)

desempleados_abr_sector <- df_afiliados %>% 
  filter(Categoria == "B") %>%
  filter(MAR2020 == 1 & ABR2020 == 0) %>% 
  group_by(nuevo_ciiu) %>% 
  summarise(Desempleados_ABR2020 = n_distinct(id_persona)) %>% 
  data.frame()
str(desempleados_abr_sector)

desempleados_sector_b <- desempleados_ene_sector %>% 
  left_join(desempleados_feb_sector, by = "nuevo_ciiu") %>% 
  left_join(desempleados_mar_sector, by = "nuevo_ciiu") %>% 
  left_join(desempleados_abr_sector, by = "nuevo_ciiu")
str(desempleados_sector_b)
rm(desempleados_ene_sector, desempleados_feb_sector, desempleados_mar_sector, desempleados_abr_sector)


desempleados_ene_sector <- df_afiliados %>% 
  filter(Categoria == "C") %>%
  filter(DIC2019 == 1 & ENE2020 == 0 & FEB2020 == 0 & MAR2020 == 0 & ABR2020 == 0) %>% 
  group_by(nuevo_ciiu) %>% 
  summarise(Desempleados_ENE2020 = n_distinct(id_persona)) %>% 
  data.frame()
str(desempleados_ene_sector)

desempleados_feb_sector <- df_afiliados %>% 
  filter(Categoria == "C") %>%
  filter(ENE2020 == 1 & FEB2020 == 0 & MAR2020 == 0 & ABR2020 == 0) %>% 
  group_by(nuevo_ciiu) %>% 
  summarise(Desempleados_FEB2020 = n_distinct(id_persona)) %>% 
  data.frame()
str(desempleados_feb_sector)

desempleados_mar_sector <- df_afiliados %>% 
  filter(Categoria == "C") %>%
  filter(FEB2020 == 1 & MAR2020 == 0 & ABR2020 == 0) %>% 
  group_by(nuevo_ciiu) %>% 
  summarise(Desempleados_MAR2020 = n_distinct(id_persona)) %>% 
  data.frame()
str(desempleados_mar_sector)

desempleados_abr_sector <- df_afiliados %>% 
  filter(Categoria == "C") %>%
  filter(MAR2020 == 1 & ABR2020 == 0) %>% 
  group_by(nuevo_ciiu) %>% 
  summarise(Desempleados_ABR2020 = n_distinct(id_persona)) %>% 
  data.frame()
str(desempleados_abr_sector)

desempleados_sector_c <- desempleados_ene_sector %>% 
  left_join(desempleados_feb_sector, by = "nuevo_ciiu") %>% 
  left_join(desempleados_mar_sector, by = "nuevo_ciiu") %>% 
  left_join(desempleados_abr_sector, by = "nuevo_ciiu")
str(desempleados_sector_b)
rm(desempleados_ene_sector, desempleados_feb_sector, desempleados_mar_sector, desempleados_abr_sector)

### Por perdida de empleo actividad ====

desempleados_ene_act <- df_afiliados %>% 
  filter(DIC2019 == 1 & ENE2020 == 0 & FEB2020 == 0 & MAR2020 == 0) %>% 
  group_by(ActividadCIIU) %>% 
  summarise(Desempleados_ENE2020 = n_distinct(id_persona)) %>% 
  data.frame()
str(desempleados_ene_act)

desempleados_feb_act <- df_afiliados %>% 
  filter(ENE2020 == 1 & FEB2020 == 0 & MAR2020 == 0) %>% 
  group_by(ActividadCIIU) %>% 
  summarise(Desempleados_FEB2020 = n_distinct(id_persona)) %>% 
  data.frame()
str(desempleados_feb_act)

desempleados_mar_act <- df_afiliados %>% 
  filter(FEB2020 == 1 & MAR2020 == 0) %>% 
  group_by(ActividadCIIU) %>% 
  summarise(Desempleados_MAR2020 = n_distinct(id_persona)) %>% 
  data.frame()
str(desempleados_mar_act)

desempleados_actividad <- desempleados_ene_act %>% 
  left_join(desempleados_feb_act, by = "ActividadCIIU") %>% 
  left_join(desempleados_mar_act, by = "ActividadCIIU")
str(desempleados_actividad)
rm(desempleados_ene_act, desempleados_feb_act, desempleados_mar_act)


### Categoria C y sector ====
library(Hmisc)
str(df_afiliados)
df_cat_c_sector <- df_afiliados %>% 
  filter(Categoria == "C") %>% 
  select(id_persona,S_JUL2019:S_ABR2020) %>% 
  gather("Fecha","Salario",S_JUL2019:S_ABR2020) %>% 
  data.frame() %>% 
  filter(Salario > 0) %>% 
  mutate(Fecha = gsub("S_","",Fecha)) %>% 
  mutate(Fecha = ordered(factor(Fecha), 
                         levels = c("JUL2019","AGO2019","SEP2019","OCT2019","NOV2019","DIC2019",
                                    "ENE2020","FEB2020","MAR2020","ABR2020"),
                         labels = c("JUL2019","AGO2019","SEP2019","OCT2019","NOV2019","DIC2019",
                                    "ENE2020","FEB2020","MAR2020","ABR2020"))) %>% 
  arrange(id_persona,desc(Fecha)) %>% 
  group_by(id_persona) %>% 
  filter(row_number()==1) %>% 
  ungroup() %>% 
  data.frame() %>% 
  select(-Fecha) %>% 
  left_join(df_afiliados %>% dplyr::select(id_persona,nuevo_ciiu), by = "id_persona") %>% 
  data.frame() %>% 
  mutate(salario = cut2(Salario/877803, g = 3)) %>% 
  group_by(nuevo_ciiu,salario) %>% 
  summarise(Afiliados = n_distinct(id_persona)) %>% 
  data.frame()
str(df_cat_c_sector)

### Sadidas ====
library(writexl)
write_xlsx(desempleados_piramide, "Salida/desempleados_piramide.xlsx")
write_xlsx(desempleados_sector, "Salida/desempleados_sector.xlsx")
write_xlsx(desempleados_sector_a, "Salida/desempleados_sector_A.xlsx")
write_xlsx(desempleados_sector_b, "Salida/desempleados_sector_B.xlsx")
write_xlsx(desempleados_sector_c, "Salida/desempleados_sector_C.xlsx")
write_xlsx(df_categoria, "Salida/df_categoria.xlsx")
write_xlsx(df_sector, "Salida/df_sector.xlsx")
write_xlsx(df_act, "Salida/df_actividad.xlsx")
write_xlsx(df_cat_c_sector, "Salida/df_cat_c_sector.xlsx")

### Salida de graficos ====
library(tidyr); library(ggplot2); library(scales); library(stringr)
df_categoria %>% 
  mutate(ENE2020 = rowSums(.[,c("A_ENE2020","B_ENE2020","C_ENE2020")]),
         ENE2020 = 100*round(ENE2020/sum(ENE2020), 4),
         FEB2020 = rowSums(.[,c("A_FEB2020","B_FEB2020","C_FEB2020")]),
         FEB2020 = 100*round(FEB2020/sum(FEB2020), 4),
         MAR2020 = rowSums(.[,c("A_MAR2020","B_MAR2020","C_MAR2020")]),
         MAR2020 = 100*round(MAR2020/sum(MAR2020), 4),
         ABR2020 = rowSums(.[,c("A_ABR2020","B_ABR2020","C_ABR2020")]),
         ABR2020 = 100*round(ABR2020/sum(ABR2020), 4)
  ) %>% 
  select(Piramide2, ENE2020, FEB2020, MAR2020, ABR2020) %>% 
  gather("Fecha", "Afiliados", ENE2020:ABR2020) %>% 
  data.frame() %>% 
  ggplot(.) +
  aes(x = Fecha, y = Piramide2, fill = Afiliados) +
  geom_text(aes(label = Afiliados)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  geom_tile(size = 1L) +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  theme_minimal() +
  theme(text = element_text(size=15)) +
  # geom_text(aes(label = comma(Afiliados, accuracy = 0.1)), size = 4.5) +
  geom_text(aes(label = paste0(round(Afiliados, 1), " %")), size = 4.7, col = "white") +
  labs(x = "Fecha", y = "Piramide 2", title = "Distribución Afiliados")

# desempleados_sector %>% 
#   gather("Fecha", "Desempleados", Desempleados_ENE2020:Desempleados_ABR2020) %>% 
#   data.frame() %>% 
#   mutate(Fecha = gsub("Desempleados_","",Fecha, fixed = T)) %>%
#   mutate(Desempleados = 100*round(Desempleados/sum(Desempleados, na.rm = T), 4)) %>%
#   ggplot(.) +
#   aes(x = Fecha, y = SectorCIIU, fill = Desempleados) +
#   geom_text(aes(label = Desempleados)) +
#   scale_y_discrete(labels = function(y) str_wrap(y, width = 35)) +
#   geom_tile(size = 1L) +
#   scale_fill_distiller(palette = "Blues", direction = 1) +
#   theme_minimal() +
#   theme(text = element_text(size=15)) +
#   # geom_text(aes(label = comma(round(Desempleados, 1))), size = 4.8, col = "gray") +
#   geom_text(aes(label = paste0(round(Desempleados, 1), " %")), size = 4.7, col = "darkgray") +
#   labs(x = "Fecha", y = "Sector CIIU", title = "Distribución dempleados o cesantes")
# 
# desempleados_actividad %>% 
#   gather("Fecha", "Desempleados", Desempleados_ENE2020:Desempleados_ABR2020) %>% 
#   data.frame() %>% 
#   mutate(Fecha = gsub("Desempleados_","",Fecha, fixed = T)) %>% 
#   mutate(Desempleados = 100*round(Desempleados/sum(Desempleados, na.rm = T), 4)) %>% 
#   ggplot(.) +
#   aes(x = Fecha, y = ActividadCIIU, fill = Desempleados) +
#   geom_text(aes(label = Desempleados)) +
#   scale_y_discrete(labels = function(y) str_wrap(y, width = 30)) +
#   geom_tile(size = 1L) +
#   scale_fill_distiller(palette = "Blues", direction = 1) +
#   theme_minimal() +
#   theme(text = element_text(size=10)) +
#   # geom_text(aes(label = comma(round(Desempleados, 1))), size = 4.8, col = "gray") +
#   geom_text(aes(label = paste0(round(Desempleados, 1), " %")), size = 4.7, col = "darkgray") +
#   labs(x = "Fecha", y = "Actividad CIIU", title = "Distribución dempleados o cesantes")
