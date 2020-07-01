# *******************************************************************
# Seleccion de muestra y construccion tamaño de muestra
# *******************************************************************

# Cargamos datos
rm(list = ls())
library(sae); library(samplesize4surveys); library(TeachingSampling)
library(sampling); library(readxl); library(survey); library(stratification)
library(corrplot); library(lattice); library(dplyr); library(data.table)
library(esquisse)
options(scipen = 999)
options(survey.lonely.psu = "adjust")

info_inicial <- fread("consulta_zona_gris_09-03-2020.txt", sep = ";", dec = ",") %>% 
  select(id_persona:autorizacion,edad,-Expr1003) %>% 
  data.frame()
str(info_inicial)
table(duplicated(info_inicial$id_persona))

consolidada <- readRDS("ConsolidacionENE2020.rds") %>% 
  select(id_persona:marca_afiliado_unico,id_empresa,RazonSocial,Piramide1,Piramide2) %>% 
  group_by(id_persona) %>%
  arrange(desc(Salario)) %>% 
  filter(row_number()==1) %>% 
  ungroup() %>% 
  select(-c(Salario,Segmento_poblacional,Categoria,Edad)) %>% 
  data.frame()
str(consolidada)
table(duplicated(consolidada$id_persona))

marco <- info_inicial %>% 
  left_join(consolidada, by = "id_persona")
str(marco)

# Analisis exploratorio
# esquisse::esquisser()
# marco <- marco %>%
#  filter(!(autorizacion %in% ""))
### Analisis Exploratorio ====
library(ggplot2); library(Hmisc); library(treemapify)

p1 <- ggplot(marco) +
  aes(x = autorizacion, y = edad, fill = autorizacion) +
  geom_boxplot() +
  scale_fill_hue() +
  labs(x = "Autorización", y = "Edad", title = "Distribución de Edad por Piramide y Autorización", subtitle = "Boxplot", fill = "Autorización") +
  theme_bw() +
  facet_wrap(vars(Piramide1)); p1

p2 <- ggplot(marco) +
 aes(x = autorizacion, y = edad, fill = categoria) +
 geom_boxplot() +
 scale_fill_hue() +
 labs(x = "Autorización", y = "Edad", title = "Distribución Edad por Autorizacion y Categoria", subtitle = "Box plot", fill = "Categoría") +
 theme_bw() +
 facet_wrap(vars(Segmento_poblacional)); p2

p3<- marco %>% group_by(Segmento_poblacional) %>%
  summarise(total=n()) %>% data.frame() %>% arrange(desc(total)) %>%
  mutate(cum = cumsum(total)) %>%
  mutate(cumtotal = cumsum(total)/sum(total)) %>%
  mutate(prop=paste0(round((total/sum(total))*100,1),"%")) %>%
  mutate(total.cat=cut2(total,g = 150)) %>% 
  slice(1:153) %>%
  ggplot(., aes(area = total, label = paste0(Segmento_poblacional,"\n",prop),fill = total.cat ))  +
  geom_treemap() +
  scale_fill_brewer(palette = "Spectral") +
  geom_treemap_text(fontface = "italic",colour = "black",place = "centre",grow =F,reflow=F) +
  labs(x = "TreeMap", title = "Distribución por Categoria", subtitle = "", fill = "Conteo \n Categoría"); p3

p4 <- marco %>% group_by(Piramide1) %>%
  summarise(total=n()) %>% data.frame() %>% arrange(desc(total)) %>%
  mutate(cum = cumsum(total)) %>%
  mutate(cumtotal = cumsum(total)/sum(total)) %>%
  mutate(prop=paste0(round((total/sum(total))*100,1),"%")) %>%
  mutate(total.cat=cut2(total,g = 150)) %>% 
  slice(1:153) %>%
  ggplot(., aes(area = total, label = paste0(Piramide1,"\n",prop),fill = total.cat ))  +
  geom_treemap() +
  scale_fill_brewer(palette = "Spectral") +
  geom_treemap_text(fontface = "italic",colour = "black",place = "centre",grow =F,reflow=F) +
  labs(x = "TreeMap", title = "Distribución por Piramide 1", subtitle = "", fill = "Conteo \n Categoría"); p4

p5 <- marco %>% group_by(Piramide2) %>%
  summarise(total=n()) %>% data.frame() %>% arrange(desc(total)) %>%
  mutate(cum = cumsum(total)) %>%
  mutate(cumtotal = cumsum(total)/sum(total)) %>%
  mutate(prop=paste0(round((total/sum(total))*100,1),"%")) %>%
  mutate(total.cat=cut2(total,g = 150)) %>% 
  slice(1:153) %>%
  ggplot(., aes(area = total, label = paste0(Piramide2,"\n",prop),fill = total.cat ))  +
  geom_treemap() +
  scale_fill_brewer(palette = "Spectral") +
  geom_treemap_text(fontface = "italic",colour = "black",place = "centre",grow =F,reflow=F) +
  labs(x = "TreeMap", title = "Distribución por Piramide 2", subtitle = "", fill = "Conteo \n Categoría"); p5

### Seleccion de la muestra ====
set.seed(03122018)
library(stratification)

LH <- strata.LH(marco$edad, CV= 0.0030, Ls = 5, takeall = T)
LH
LH_tabla <- LH
LH_tabla$iter.detail
hist(bd_marco$Total)

marco$estrato_edad <- cut(x = marco$edad,
                          breaks = c(min(marco$edad),LH$bh,max(marco$edad)),
                          include.lowest = T, 
                          right = F)
str(marco)
n_h1 <- round((LH$nh)/2,0)
n_h2 <- round((LH$nh)/2,0)

indica1 <- sampling::strata(data = marco,
                           stratanames = "estrato_edad",
                           size = n_h1, 
                           description = T, 
                           method = "srswor")
indica2 <- sampling::strata(data = marco,
                            stratanames = "estrato_edad",
                            size = n_h2, 
                            description = T, 
                            method = "srswor")
muestra1 <- marco[indica1$ID_unit,]
muestra2 <- marco[indica2$ID_unit,]

df_inner <- inner_join(muestra1 %>% select(id_persona),muestra2 %>% select(id_persona),by="id_persona")

df_muestra1 <- anti_join(muestra1,muestra2,by="id_persona") %>% 
  data.frame() %>% 
  select(id_persona:FechaNacimiento)
df_muestra2 <- anti_join(muestra2,muestra1,by="id_persona") %>% 
  data.frame() %>% 
  select(id_persona:FechaNacimiento)

fwrite(df_muestra1, file = "muestra1.csv", row.names = F, sep = ";", dec = ",")
fwrite(df_muestra2, file = "muestra2.csv", row.names = F, sep = ";", dec = ",")
sum(df_muestra1$id_persona %in% df_muestra2$id_persona)
  
info_inicial <- info_inicial %>% 
  mutate(selecionado = ifelse(id_persona %in% muestra$id_persona, "Si", "No"))
str(info_inicial)
table(info_inicial$selecionado)
table(marco$estrato_edad)

fwrite(info_inicial, file = "muestra.csv", row.names = F, sep = ";", dec = ",")
