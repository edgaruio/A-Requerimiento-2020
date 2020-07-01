# Librerias
library(dplyr); library(tidyr)

### Juntamos todos los rds
fechas <- c("JUL2019","AGO2019","SEP2019","OCT2019","NOV2019","DIC2019",
            "ENE2020","FEB2020","MAR2020","ABR2020","MAY2020")

ruta <- "df_consolidados/"
lista <- list.files(path = ruta, pattern = ".rds", recursive = T)
aux <- readRDS(paste0(ruta,lista[1])) %>%
  filter(marca_afiliado_unico) %>%
  select(id_persona,Categoria,Salario,SectorCIIU,ActividadCIIU,Piramide1,Piramide2) %>%
  mutate(fecha = gsub("Consolidacion","",lista[1], fixed = T)) %>%
  mutate(fecha = gsub(".rds","",fecha, fixed = T)) %>% 
  mutate(fecha = gsub("_","",fecha, fixed = T))
for (i in 2:length(lista)) {
  actual <- readRDS(paste0(ruta,lista[i])) %>%
    filter(marca_afiliado_unico) %>%
    select(id_persona,Categoria,Salario,SectorCIIU,ActividadCIIU,Piramide1,Piramide2) %>%
    mutate(fecha = gsub("Consolidacion","",lista[i], fixed = T)) %>%
    mutate(fecha = gsub(".rds","",fecha, fixed = T)) %>%
    mutate(fecha = gsub("_","",fecha, fixed = T))
  aux <- bind_rows(aux,actual)
}
rm(actual)
str(aux)
table(aux$fecha)

# Estado afiliaciones
estado_afil <- aux %>% 
  select(id_persona,fecha) %>% 
  mutate(estado = 1) %>% 
  spread(fecha, estado, fill = 0) %>% 
  data.frame() %>% 
  select(id_persona,fechas)
str(estado_afil)

estado_salario <- aux %>% 
  select(id_persona,fecha,Salario) %>% 
  spread(fecha, Salario, fill = 0) %>% 
  data.frame() %>% 
  select(id_persona,fechas)
str(estado_salario)
names(estado_salario) <- c("id_persona",paste("S", fechas, sep = "_"))
# estado_categoria <- aux %>% 
#   select(id_persona,fecha,Categoria) %>% 
#   mutate(Categoria = as.character(Categoria)) %>% 
#   spread(fecha, Categoria, fill = NA) %>% 
#   data.frame() %>% 
#   select(id_persona,JUL2019,AGO2019,SEP2019,OCT2019,NOV2019,DIC2019,ENE2020,FEB2020,MAR2020)
# str(estado_categoria)
# names(estado_categoria) <- c("id_persona",paste("C", c("JUL2019","AGO2019","SEP2019","OCT2019","NOV2019","DIC2019",
#                                                      "ENE2020","FEB2020","MAR2020"), sep = "_"))

ultima_categoria <- aux %>% 
  select(id_persona,Categoria,fecha) %>% 
  mutate(fecha = ordered(factor(fecha), 
                         levels = fechas,
                         labels = fechas)) %>% 
  arrange(id_persona,desc(fecha)) %>% 
  group_by(id_persona) %>% 
  filter(row_number()==1) %>% 
  ungroup() %>% 
  data.frame() %>% 
  select(-fecha)
str(ultima_categoria)
table(duplicated(ultima_categoria$id_persona))


ultima_piramide1 <- aux %>% 
  select(id_persona,Piramide1,fecha) %>% 
  mutate(fecha = ordered(factor(fecha), 
                         levels = fechas,
                         labels = fechas)) %>% 
  arrange(id_persona,desc(fecha)) %>% 
  group_by(id_persona) %>% 
  filter(row_number()==1) %>% 
  ungroup() %>% 
  data.frame() %>% 
  select(-fecha)
str(ultima_piramide1)
table(duplicated(ultima_piramide1$id_persona))


ultima_piramide2 <- aux %>% 
  select(id_persona,Piramide2,fecha) %>% 
  mutate(fecha = ordered(factor(fecha), 
                         levels = fechas,
                         labels = fechas)) %>% 
  arrange(id_persona,desc(fecha)) %>% 
  group_by(id_persona) %>% 
  filter(row_number()==1) %>% 
  ungroup() %>% 
  data.frame() %>% 
  select(-fecha)
str(ultima_piramide2)
table(duplicated(ultima_piramide2$id_persona))


ultimo_sector <- aux %>% 
  select(id_persona,SectorCIIU,fecha) %>% 
  mutate(fecha = ordered(factor(fecha), 
                         levels = fechas,
                         labels = fechas)) %>% 
  arrange(id_persona,desc(fecha)) %>% 
  group_by(id_persona) %>% 
  filter(row_number()==1) %>% 
  ungroup() %>% 
  data.frame() %>% 
  select(-fecha)
str(ultimo_sector)
table(duplicated(ultimo_sector$id_persona))

# mayo <- readRDS("df_consolidados/ConsolidacionMAY2020.rds")
# str(mayo)

ultima_actividad <- aux %>% 
  select(id_persona,ActividadCIIU,fecha) %>% 
  mutate(fecha = ordered(factor(fecha), 
                         levels = fechas,
                         labels = fechas)) %>% 
  arrange(id_persona,desc(fecha)) %>% 
  group_by(id_persona) %>% 
  filter(row_number()==1) %>% 
  ungroup() %>% 
  data.frame() %>% 
  select(-fecha)
str(ultima_actividad)
table(duplicated(ultima_actividad$id_persona))


# Meses cotizados
df_cotizados <- readRDS("df_consolidados/df_union_13042020.rds") %>%
  group_by(id_persona) %>% 
  summarise(meses_cotizados_5anios = sum(conteo , na.rm = T)) %>% 
  data.frame()
str(df_cotizados)
table(duplicated(df_cotizados$id_persona))

df_union <- estado_afil %>% 
  full_join(estado_salario, by = "id_persona") %>% 
  full_join(ultima_categoria, by = "id_persona") %>% 
  left_join(ultima_piramide1, by = "id_persona") %>% 
  left_join(ultima_piramide2, by = "id_persona") %>% 
  left_join(ultimo_sector, by = "id_persona") %>% 
  left_join(ultima_actividad, by = "id_persona") %>% 
  left_join(df_cotizados, by = "id_persona") %>% 
  mutate(estado_act = ifelse(MAY2020 == 1, "Afiliado", "No afiliado"),
         nuevo_ciiu = ifelse(SectorCIIU == "Prestación de Servicios (Educación, Banca, Comunicaciones, Seguros, Salud, Arte y Cultura)",
                             ActividadCIIU, SectorCIIU),
         nuevo_ciiu = ifelse(is.na(nuevo_ciiu), "Sin información", nuevo_ciiu))
str(df_union)
table(duplicated(df_union$id_persona))

saveRDS(df_union, "Salida/df_poblacion_historico_16062020.rds")
saveRDS(df_union, "App/Data/df_poblacion_historico_16062020.rds")


# ### Poblacion Beneficiarios
# library(RODBC)
# channel <- odbcDriverConnect(
#   "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=D:/A COLSUBSIDIO/A REQUERIMIENTO 2020/6 JUNIO/1 Analisis_poblacion_beneficiaria/Consulta_sub.accdb"
# )
# sqlTables(channel)
# 
# consulta_sub <- sqlQuery( 
#   channel , 
#   paste ("select * from  consulta_subsidio"),
#   as.is=T) %>% 
#   data.frame()
# str(consulta_sub)
# 
# odbcCloseAll()
# 
# df_consulta_bene <- consulta_sub %>% 
#   filter(marca_afiliado_unico == "X") %>% 
#   filter(parentesco != "PADRE") %>% 
#   group_by(piramide_2) %>% 
#   summarise(conteo = n_distinct(id_persona_familiar))
# str(df_consulta_bene)
# table(duplicated(df_consulta_bene$id_persona))
# table(consulta_sub$parentesco)
# 
# mayo <- readRDS("df_consolidados/ConsolidacionMAY2020.rds") %>% 
#   filter(marca_afiliado_unico)
# str(mayo)
# 
# mayo %>% 
#   group_by(Piramide2) %>% 
#   summarise(conteo = sum(total_numero_grupo_familiar))
