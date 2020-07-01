# Analisis exploratorio
rm(list = ls())
options(scipen = 999)
library(dplyr); library(esquisse); library(readxl); library(tidyr); library(ggplot2); library(stringr); library(scales)

# Cargar datos
df_afiliados <- readRDS("Salida/df_poblacion_potencial_28052020.rds") %>% 
  mutate(nuevo_ciiu = ifelse(SectorCIIU == "Prestación de Servicios (Educación, Banca, Comunicaciones, Seguros, Salud, Arte y Cultura)",
                             ActividadCIIU,SectorCIIU)) %>% 
  data.frame()
str(df_afiliados)

desempleados_piramide <- read_excel("Salida/desempleados_piramide.xlsx")
desempleados_sector <- read_excel("Salida/desempleados_sector.xlsx")
desempleados_sector_a <- read_excel("Salida/desempleados_sector_A.xlsx")
desempleados_sector_b <- read_excel("Salida/desempleados_sector_B.xlsx")
desempleados_sector_c <- read_excel("Salida/desempleados_sector_C.xlsx")
df_categoria <- read_excel("Salida/df_categoria.xlsx")
df_sector <- read_excel("Salida/df_sector.xlsx")
df_act <- read_excel("Salida/df_actividad.xlsx")
df_cat_c_sector <- read_excel("Salida/df_cat_c_sector.xlsx")

### Salida de graficos ====
df_categoria %>%
  mutate(ENE2020 = rowSums(.[,c("A_ENE2020","B_ENE2020","C_ENE2020")]),
         # ENE2020 = 100*round(ENE2020/sum(ENE2020), 4),
         FEB2020 = rowSums(.[,c("A_FEB2020","B_FEB2020","C_FEB2020")]),
         # FEB2020 = 100*round(FEB2020/sum(FEB2020), 4),
         MAR2020 = rowSums(.[,c("A_MAR2020","B_MAR2020","C_MAR2020")]),
         # MAR2020 = 100*round(MAR2020/sum(MAR2020), 4),
         ABR2020 = rowSums(.[,c("A_ABR2020","B_ABR2020","C_ABR2020")]),
         # MAR2020 = 100*round(ABR2020/sum(ABR2020), 4)
  ) %>% 
  select(Piramide2, ENE2020, FEB2020, MAR2020, ABR2020) %>% 
  gather("Fecha", "Afiliados", ENE2020:ABR2020) %>% 
  mutate(Fecha = ordered(factor(Fecha), 
                         levels = c("ENE2020","FEB2020","MAR2020","ABR2020"),
                         labels = c("ENE2020","FEB2020","MAR2020","ABR2020"))) %>% 
  data.frame() %>% 
  ggplot(.) +
  aes(x = Fecha, y = Piramide2, fill = Afiliados) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  scale_y_discrete(limits = rev(levels(as.factor(df_categoria$Piramide2)))) +
  geom_tile(size = 1L) +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  theme_minimal() +
  theme(text = element_text(size=15)) +
  geom_text(aes(label = formatC(Afiliados, format = "d", digits = 0, big.mark = ",", )), size = 6, col = "darkorange") +
  # geom_text(aes(label = paste0(round(Afiliados, 1), " %")), size = 4.7, col = "darkorange") +
  labs(x = "Fecha", y = "Piramide 2", title = "Distribución Afiliados")


# df_cat_c_sector %>% 
#   mutate(ciiu = paste(nuevo_ciiu, salario, sep = " ")) %>% 
#   select(ciiu, Afiliados) %>% 
#   ggplot(.) +
#   aes(x = Fecha, y = nuevo, fill = Afiliados) +
#   geom_text(aes(label = Afiliados)) +
#   scale_y_discrete(labels = function(y) str_wrap(y, width = 40)) +
#   geom_tile(size = 1L) +
#   scale_fill_distiller(palette = "Blues", direction = 1) +
#   theme_minimal() +
#   theme(text = element_text(size=10)) +
#   geom_text(aes(label = comma(Afiliados)), size = 4, col = "orange") +
#   # geom_text(aes(label = paste0(round(Afiliados, 1), " %")), size = 4.7, col = "darkgray") +
#   labs(x = "Fecha", y = "Piramide y Categoria", title = "Distribución Afiliados")

# df_categoria %>% 
#   select(Piramide1, A_ENE2020:C_MAR2020) %>% 
#   gather("Fecha", "Afiliados",  A_ENE2020:C_MAR2020) %>% 
#   data.frame() %>% 
#   group_by(Piramide1,Fecha) %>% 
#   summarise(Afiliados = sum(Afiliados, na.rm = T)) %>% 
#   mutate(Categoria = substr(Fecha, 1, 1),
#          Piramide_Cat = paste(Piramide1, Categoria, sep = " "),
#          Fecha = substr(Fecha, 3, nchar(Fecha))) %>% 
#   select(Piramide_Cat,Fecha,Afiliados) %>% 
#   ggplot(.) +
#   aes(x = Fecha, y = Piramide_Cat, fill = Afiliados) +
#   geom_text(aes(label = Afiliados)) +
#   scale_y_discrete(labels = function(y) str_wrap(y, width = 40)) +
#   geom_tile(size = 1L) +
#   scale_fill_distiller(palette = "Blues", direction = 1) +
#   theme_minimal() +
#   theme(text = element_text(size=10)) +
#   geom_text(aes(label = comma(Afiliados)), size = 4, col = "orange") +
#   # geom_text(aes(label = paste0(round(Afiliados, 1), " %")), size = 4.7, col = "darkgray") +
#   labs(x = "Fecha", y = "Piramide y Categoria", title = "Distribución Afiliados")

df_categoria %>% 
  select(Piramide2, A_ENE2020, A_FEB2020, A_MAR2020, A_ABR2020) %>% 
  gather("Fecha", "Afiliados",  A_ENE2020:A_ABR2020) %>% 
  data.frame() %>% 
  mutate(Categoria = substr(Fecha, 1, 1),
         Piramide_Cat = paste(Piramide2, Categoria, sep = " "),
         Fecha = substr(Fecha, 3, nchar(Fecha))) %>% 
  filter(Categoria == "A") %>% 
  mutate(Fecha = ordered(factor(Fecha),
                         levels = c("ENE2020","FEB2020","MAR2020","ABR2020"),
                         labels = c("ENE2020","FEB2020","MAR2020","ABR2020"))) %>%
  select(Piramide2,Fecha,Afiliados) %>% 
  ggplot(.) +
  aes(x = Fecha, y = Piramide2, fill = Afiliados) +
  geom_text(aes(label = Afiliados)) +
  # scale_y_discrete(labels = function(y) str_wrap(y, width = 40)) +
  scale_y_discrete(limits = rev(levels(as.factor(df_categoria$Piramide2)))) +
  geom_tile(size = 1L) +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  theme_minimal() +
  theme(text = element_text(size=15)) +
  geom_text(aes(label = formatC(Afiliados, format = "d", digits = 0, big.mark = ",", )), size = 6, col = "darkorange") +
  # geom_text(aes(label = paste0(round(Afiliados, 1), " %")), size = 4.7, col = "darkgray") +
  labs(x = "Fecha", y = "Piramide y Categoria", title = "     Categoria A")


df_categoria %>% 
  select(Piramide2, B_ENE2020, B_FEB2020, B_MAR2020, B_ABR2020) %>% 
  gather("Fecha", "Afiliados",  B_ENE2020:B_ABR2020) %>% 
  data.frame() %>% 
  mutate(Categoria = substr(Fecha, 1, 1),
         Piramide_Cat = paste(Piramide2, Categoria, sep = " "),
         Fecha = substr(Fecha, 3, nchar(Fecha))) %>% 
  filter(Categoria == "B") %>% 
  mutate(Fecha = ordered(factor(Fecha),
                         levels = c("ENE2020","FEB2020","MAR2020","ABR2020"),
                         labels = c("ENE2020","FEB2020","MAR2020","ABR2020"))) %>%
  select(Piramide2,Fecha,Afiliados) %>% 
  ggplot(.) +
  aes(x = Fecha, y = Piramide2, fill = Afiliados) +
  geom_text(aes(label = Afiliados)) +
  # scale_y_discrete(labels = function(y) str_wrap(y, width = 40)) +
  scale_y_discrete(limits = rev(levels(as.factor(df_categoria$Piramide2)))) +
  geom_tile(size = 1L) +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  theme_minimal() +
  theme(text = element_text(size=15)) +
  geom_text(aes(label = formatC(Afiliados, format = "d", digits = 0, big.mark = ",", )), size = 6, col = "darkorange") +
  # geom_text(aes(label = paste0(round(Afiliados, 1), " %")), size = 4.7, col = "darkgray") +
  labs(x = "Fecha", y = "Piramide y Categoria", title = "     Categoria B")


df_categoria %>% 
  select(Piramide2, C_ENE2020, C_FEB2020, C_MAR2020, C_ABR2020) %>% 
  gather("Fecha", "Afiliados",  C_ENE2020:C_ABR2020) %>% 
  data.frame() %>% 
  mutate(Categoria = substr(Fecha, 1, 1),
         Piramide_Cat = paste(Piramide2, Categoria, sep = " "),
         Fecha = substr(Fecha, 3, nchar(Fecha))) %>% 
  filter(Categoria == "C") %>% 
  mutate(Fecha = ordered(factor(Fecha),
                         levels = c("ENE2020","FEB2020","MAR2020","ABR2020"),
                         labels = c("ENE2020","FEB2020","MAR2020","ABR2020"))) %>%
  select(Piramide2,Fecha,Afiliados) %>% 
  ggplot(.) +
  aes(x = Fecha, y = Piramide2, fill = Afiliados) +
  geom_text(aes(label = Afiliados)) +
  # scale_y_discrete(labels = function(y) str_wrap(y, width = 40)) +
  scale_y_discrete(limits = rev(levels(as.factor(df_categoria$Piramide2)))) +
  geom_tile(size = 1L) +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  theme_minimal() +
  theme(text = element_text(size=15)) +
  geom_text(aes(label = formatC(Afiliados, format = "d", digits = 0, big.mark = ",", )), size = 6, col = "darkorange") +
  # geom_text(aes(label = paste0(round(Afiliados, 1), " %")), size = 4.7, col = "darkgray") +
  labs(x = "Fecha", y = "Piramide y Categoria", title = "     Categoria C")



df_sector %>% 
  gather("Fecha", "Afiliados", ENE2020:ABR2020) %>% 
  data.frame() %>% 
  mutate(Fecha = ordered(factor(Fecha),
                         levels = c("ENE2020","FEB2020","MAR2020","ABR2020"),
                         labels = c("ENE2020","FEB2020","MAR2020","ABR2020"))) %>%
  mutate(Afiliados = 100*round(Afiliados/sum(Afiliados, na.rm = T), 4)) %>%
  ggplot(.) +
  aes(x = Fecha, y = nuevo_ciiu, fill = Afiliados) +
  scale_y_discrete(limits = rev(levels(as.factor(df_sector$nuevo_ciiu))),
                   labels = function(y) str_wrap(y, width = 35)) +
  geom_tile(size = 1L) +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  theme_minimal() +
  theme(text = element_text(size=15)) +
  # geom_text(aes(label = formatC(Afiliados, format = "d", digits = 0, big.mark = ",", )), 
            # size = 6, col = "darkorange") +
  geom_text(aes(label = paste0(round(Afiliados, 1), " %")), size = 6, col = "darkorange") +
  labs(x = "Fecha", y = "Agrupacion Sector y Categoria", title = "Distribución Afiliados")


desempleados_sector %>% 
  gather("Fecha", "Desempleados", Desempleados_ENE2020:Desempleados_ABR2020) %>% 
  data.frame() %>% 
  mutate(Fecha = gsub("Desempleados_","",Fecha, fixed = T),
         Desempleados = ifelse(is.na(Desempleados), 0, Desempleados)) %>%
  mutate(Fecha = ordered(factor(Fecha),
                         levels = c("ENE2020","FEB2020","MAR2020","ABR2020"),
                         labels = c("ENE2020","FEB2020","MAR2020","ABR2020"))) %>%
  mutate(Desempleados = 100*round(Desempleados/sum(Desempleados, na.rm = T), 4)) %>%
  ggplot(.) +
  aes(x = Fecha, y = nuevo_ciiu, fill = Desempleados) +
  geom_text(aes(label = Desempleados)) +
  scale_y_discrete(limits = rev(levels(as.factor(df_sector$nuevo_ciiu))),
                   labels = function(y) str_wrap(y, width = 35)) +
  geom_tile(size = 1L) +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  theme_minimal() +
  theme(text = element_text(size=15)) +
  # geom_text(aes(label = comma(round(Desempleados, 1))), size = 6, col = "darkorange") +
  geom_text(aes(label = paste0(round(Desempleados, 2), " %")), size = 6, col = "darkorange") +
  labs(x = "Fecha", y = "Agrupacion Sector y Categoria CIIU", title = "Distribución dempleados o cesantes")


desempleados_sector_a %>% 
  gather("Fecha", "Desempleados", Desempleados_ENE2020:Desempleados_ABR2020) %>% 
  data.frame() %>% 
  mutate(Fecha = gsub("Desempleados_","",Fecha, fixed = T),
         Desempleados = ifelse(is.na(Desempleados), 0, Desempleados)) %>%
  mutate(Fecha = ordered(factor(Fecha),
                         levels = c("ENE2020","FEB2020","MAR2020","ABR2020"),
                         labels = c("ENE2020","FEB2020","MAR2020","ABR2020"))) %>%
  mutate(Desempleados = 100*round(Desempleados/sum(Desempleados, na.rm = T), 4)) %>%
  ggplot(.) +
  aes(x = Fecha, y = nuevo_ciiu, fill = Desempleados) +
  geom_text(aes(label = Desempleados)) +
  scale_y_discrete(limits = rev(levels(as.factor(df_sector$nuevo_ciiu))),
                   labels = function(y) str_wrap(y, width = 35)) +
  geom_tile(size = 1L) +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  theme_minimal() +
  theme(text = element_text(size=15)) +
  # geom_text(aes(label = comma(round(Desempleados, 1))), size = 6, col = "darkorange") +
  geom_text(aes(label = paste0(round(Desempleados, 2), " %")), size = 6, col = "darkorange") +
  labs(x = "Fecha", y = "Agrupacion Sector y Categoria CIIU", title = "Distribución dempleados o cesantes")



desempleados_sector_b %>% 
  gather("Fecha", "Desempleados", Desempleados_ENE2020:Desempleados_ABR2020) %>% 
  data.frame() %>% 
  mutate(Fecha = gsub("Desempleados_","",Fecha, fixed = T),
         Desempleados = ifelse(is.na(Desempleados), 0, Desempleados)) %>%
  mutate(Fecha = ordered(factor(Fecha),
                         levels = c("ENE2020","FEB2020","MAR2020","ABR2020"),
                         labels = c("ENE2020","FEB2020","MAR2020","ABR2020"))) %>%
  mutate(Desempleados = 100*round(Desempleados/sum(Desempleados, na.rm = T), 4)) %>%
  ggplot(.) +
  aes(x = Fecha, y = nuevo_ciiu, fill = Desempleados) +
  geom_text(aes(label = Desempleados)) +
  scale_y_discrete(limits = rev(levels(as.factor(df_sector$nuevo_ciiu))),
                   labels = function(y) str_wrap(y, width = 35)) +
  geom_tile(size = 1L) +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  theme_minimal() +
  theme(text = element_text(size=15)) +
  # geom_text(aes(label = comma(round(Desempleados, 1))), size = 6, col = "darkorange") +
  geom_text(aes(label = paste0(round(Desempleados, 2), " %")), size = 6, col = "darkorange") +
  labs(x = "Fecha", y = "Agrupacion Sector y Categoria CIIU", title = "Distribución dempleados o cesantes")



desempleados_sector_c %>% 
  gather("Fecha", "Desempleados", Desempleados_ENE2020:Desempleados_ABR2020) %>% 
  data.frame() %>% 
  mutate(Fecha = gsub("Desempleados_","",Fecha, fixed = T),
         Desempleados = ifelse(is.na(Desempleados), 0, Desempleados)) %>%
  mutate(Fecha = ordered(factor(Fecha),
                         levels = c("ENE2020","FEB2020","MAR2020","ABR2020"),
                         labels = c("ENE2020","FEB2020","MAR2020","ABR2020"))) %>%
  mutate(Desempleados = 100*round(Desempleados/sum(Desempleados, na.rm = T), 4)) %>%
  ggplot(.) +
  aes(x = Fecha, y = nuevo_ciiu, fill = Desempleados) +
  geom_text(aes(label = Desempleados)) +
  scale_y_discrete(limits = rev(levels(as.factor(df_sector$nuevo_ciiu))),
                   labels = function(y) str_wrap(y, width = 35)) +
  geom_tile(size = 1L) +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  theme_minimal() +
  theme(text = element_text(size=15)) +
  # geom_text(aes(label = comma(round(Desempleados, 1))), size = 6, col = "darkorange") +
  geom_text(aes(label = paste0(round(Desempleados, 2), " %")), size = 6, col = "darkorange") +
  labs(x = "Fecha", y = "Agrupacion Sector y Categoria CIIU", title = "Distribución dempleados o cesantes")


# desempleados_actividad %>% 
#   gather("Fecha", "Desempleados", Desempleados_ENE2020:Desempleados_MAR2020) %>% 
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
