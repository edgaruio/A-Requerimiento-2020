# Analisis Subsidios
rm(list = ls())
library(dplyr); library(data.table); library(readxl)

bd_afiliados <- readRDS("Analisis_Subsidios/bd_afiliados_04092019.rds") %>% 
  mutate(no_id_persona = gsub("\\D","",id_persona)) %>% 
  mutate(cabeza_hogar= ifelse(Genero == "F" & numero_hijos > 0 & numero_conyugue == 0, "Si", "No"),
         soltera = ifelse(Genero == "F" & numero_hijos == 0 & numero_conyugue == 0, "Si", "No")) %>% 
  filter(categoria %in% c("A","B")) %>% 
  mutate(i = 1, i2 = 1, i3 = 1) %>% 
  spread(categoria, i, fill = 0) %>% 
  spread(Genero, i2, fill = 0) %>% 
  spread(segmento_poblacional, i3, fill = 0)
str(bd_afiliados)
table(duplicated(bd_afiliados$id_persona))


consulta_pir <- fread("Analisis_Subsidios/Consulta_piramide.txt") %>% 
  select(id_persona,id_empresa,ACTIVIDAD) %>% 
  distinct()
str(consulta_pir)
table(duplicated(consulta_pir$id_persona))


clubes <- read_excel("Analisis_Subsidios/Clubes.xlsx") %>% 
  mutate(NoIdpersona = as.character(NoIdpersona)) %>% 
  mutate(prac_deporte = 1) %>% 
  select(NoIdpersona,prac_deporte) %>% 
  distinct()
str(clubes)
table(duplicated(clubes$NoIdpersona))


medicamento_cro <- fread("Analisis_Subsidios/Droguerias.csv") %>% 
  filter(tipodetratamiento %in% c("AGUDO O CRÓNICO","CRONICO")) %>% 
  select(NumIdPersona) %>% 
  distinct() %>% 
  mutate(trata_cronico = 1)
str(medicamento_cro)
table(medicamento_cro$tipodetratamiento)
table(duplicated(medicamento_cro$NumIdPersona))


bd_pca <- bd_afiliados %>% 
  left_join(consulta_pir, by = c("id_persona"="id_persona","id_empresa"="id_empresa")) %>% 
  left_join(clubes, by = c("no_id_persona"="NoIdpersona")) %>% 
  left_join(medicamento_cro, by = c("no_id_persona"="NumIdPersona"))
str(bd_consolidada)
table(duplicated(bd_consolidada$id_persona))

# Analisis descriptivo
# library(esquisse)
# esquisse::esquisser()

saveRDS(bd_pca, "Analisis_Subsidios/bd_pca.rds")

# Analisis descriptivo
library(Hmisc)
bd_pca <- readRDS("Analisis_Subsidios/bd_pca.rds") %>% 
  mutate(R_edad = cut2(Edad, g = 10)) %>% 
  mutate(Genero = ifelse(F == 1, "F", "M"),
         Categoria = ifelse(A == 1, "A", "B"))
str(bd_pca)

# Piramide por Edad

# age_genero <- ddply(bd_pca %>% na.omit,.(R_Edad,Genero,Categoria),summarise,conteo=length(Edad))
age_genero <- bd_pca %>% 
  select(R_edad,Edad,Genero,soltera) %>% 
  na.omit() %>% 
  group_by(R_edad,Genero,soltera) %>% 
  summarise(conteo=length(Edad)) %>% 
  data.frame()
str(age_genero)


ggplot(data = age_genero, mapping = aes(x = R_edad, fill = Genero, y = ifelse(test = Genero == "M", yes = -conteo, no = conteo))) +
  geom_bar(color="black",stat = "identity",width = 0.6) + theme_bw() + 
  facet_wrap(~soltera,scales = "free")+
  scale_y_continuous(labels = abs, limits = max(age_genero$conteo) * c(-1,1)) +
  labs(y = "Empleados") +
  coord_flip()+
  labs(y="", x="Rango de Edad", title="\n Pirámide por Género y Mujer Soltera")+
  theme(plot.title = element_text(size=20,hjust = 0.5,colour = "black"))+
  theme(axis.title = element_text(size=14,face="bold"),axis.text.x = element_text(size = 12),
         axis.text.y = element_text(size = 12))+
  scale_fill_brewer(palette = "Set2") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(strip.text=element_text(colour="white",face="bold",size=25))

library(esquisse); library(ggplot2)
esquisser()

bd_pca1 <- bd_pca %>%
 filter(!is.na(cabeza_hogar)) %>%
 filter(!is.na(ACTIVIDAD))


ggplot(bd_pca1) +
 aes(x = Categoria, y = Salario, fill = cabeza_hogar) +
 geom_boxplot() +
 scale_fill_brewer(palette = "Set2") +
 labs(x = "Categoria", y = "Salario", title = "Distribucion Salario ", subtitle = "Cabeza Hogar por Actividad", fill = "Cabeza Hogar") +
 theme_minimal() +
  facet_wrap(vars(ACTIVIDAD)) +
  coord_flip()

bd_pca2 <- bd_pca %>%
  filter(!is.na(cabeza_hogar)) %>%
  filter(!is.na(numero_hijos))

ggplot(bd_pca2) +
  aes(x = Categoria, y = Salario, fill = cabeza_hogar) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Categoria", y = "Salario", title = "Distribucion Salario ", subtitle = "Cabeza Hogar por Número de Hijos", fill = "Cabeza Hogar") +
  theme_minimal() +
  facet_wrap(vars(factor(numero_hijos))) +
  coord_flip()


#### 01/10/2019 ==================================
# Revision de subsidios
rm(list = ls())
options(scipen = 999)
library(readxl); library(dplyr); library(tidyr); library(lubridate)
df_2015 <- read_excel("Analisis_Subsidios/Cifras_Contaduria.xlsx", sheet = "2015 REAL", skip = 1) %>% 
  filter(PIVOT == "61") %>% 
  gather(key = "mes", value = "Valor", 5:17) %>% 
  mutate(anio = 2015)
str(df_2015)

df_2016 <- read_excel("Analisis_Subsidios/Cifras_Contaduria.xlsx", sheet = "2016 REAL", skip = 1) %>% 
  filter(PIVOT == "61") %>% 
  gather(key = "mes", value = "Valor", 5:17) %>% 
  mutate(anio = 2016)
str(df_2016)

df_2017 <- read_excel("Analisis_Subsidios/Cifras_Contaduria.xlsx", sheet = "2017 REAL", skip = 1) %>% 
  filter(PIVOT == "61") %>% 
  gather(key = "mes", value = "Valor", 5:17) %>% 
  mutate(anio = 2017)
str(df_2017)

df_2018 <- read_excel("Analisis_Subsidios/Cifras_Contaduria.xlsx", sheet = "2018 REAL", skip = 1) %>% 
  filter(PIVOT == "61") %>% 
  gather(key = "mes", value = "Valor", 5:17) %>% 
  mutate(anio = 2018)
str(df_2018)

df_2019 <- read_excel("Analisis_Subsidios/Cifras_Contaduria.xlsx", sheet = "2019 REAL", skip = 1) %>% 
  filter(PIVOT == "61") %>% 
  gather(key = "mes", value = "Valor", 5:17) %>% 
  mutate(anio = 2019)
str(df_2019)


df_sub <- rbind(df_2015, df_2016, df_2017, df_2018, df_2019) %>% 
  filter(mes != "Acumulado") %>% 
  mutate(CODIGO = as.character(CODIGO)) %>% 
  data.frame()
str(df_sub)

df_acum <- rbind(df_2015, df_2016, df_2017, df_2018, df_2019) %>% 
  filter(mes == "Acumulado") %>% 
  mutate(CODIGO = as.character(CODIGO),
         anio = as.character(anio)) %>% 
  data.frame()
str(df_acum)

test1 <- df_sub %>% 
  mutate(tipocm = ifelse(CUENTA %in% c("CUOTA MONETARIA LEY 1429", "CUOTA MONETARIA LEY 21"), "CUOTA MONETARIA", "OTROS")) %>% 
  group_by(anio, mes, tipocm) %>% 
  summarise(Valor_total_M = sum(Valor/1000000, na.rm = T)) %>% 
  data.frame() %>% 
  filter(tipocm == "CUOTA MONETARIA") %>% 
  mutate(Fecha = paste(01, mes, anio, sep = "-"),
         Fecha = parse_date_time(Fecha, "dmy")) %>% 
  arrange(Fecha) %>% 
  mutate(crecimiento = round(100*(Valor_total_M-lag(Valor_total_M))/lag(Valor_total_M), 1))
str(test1)

test1_valor <- test1 %>% 
  dplyr::select(anio, mes, Valor_total_M) %>% 
  spread(key = mes, value = Valor_total_M) %>% 
  select(anio,Enero,Febrero,Marzo,Abril,Mayo,Junio,Julio,Agosto,Septiembre,Octubre,Noviembre,Diciembre) %>% 
  data.frame()
fwrite(test1_valor, file = "Analisis_Subsidios/valor_anio.csv", row.names = F, sep = ";", dec = ",")

test1_cre <- test1 %>% 
  dplyr::select(anio, mes, crecimiento) %>% 
  spread(key = mes, value = crecimiento) %>% 
  select(anio,Enero,Febrero,Marzo,Abril,Mayo,Junio,Julio,Agosto,Septiembre,Octubre,Noviembre,Diciembre) %>% 
  data.frame()
fwrite(test1_cre, file = "Analisis_Subsidios/valor_cre.csv", row.names = F, sep = ";", dec = ",")

test2 <- df_sub %>% 
  mutate(tipocm = ifelse(CUENTA %in% c("CUOTA MONETARIA LEY 1429", "CUOTA MONETARIA LEY 21"), "CUOTA MONETARIA", "OTROS")) %>% 
  group_by(anio, mes, tipocm) %>% 
  summarise(Valor_total_M = sum(Valor, na.rm = T)/1000000) %>% 
  data.frame() %>% 
  filter(tipocm == "OTROS") %>% 
  mutate(Fecha = paste(01, mes, anio, sep = "-"),
         Fecha = parse_date_time(Fecha, "dmy")) %>% 
  arrange(Fecha) %>% 
  mutate(crecimiento = round(100*(Valor_total_M-lag(Valor_total_M))/lag(Valor_total_M), 1))
str(test2)

test2_valor <- test2 %>% 
  dplyr::select(anio, mes, Valor_total_M) %>% 
  spread(key = mes, value = Valor_total_M) %>% 
  select(anio,Enero,Febrero,Marzo,Abril,Mayo,Junio,Julio,Agosto,Septiembre,Octubre,Noviembre,Diciembre) %>% 
  data.frame()
fwrite(test2_valor, file = "Analisis_Subsidios/valor_anio_otros.csv", row.names = F, sep = ";", dec = ",")

test2_cre <- test2 %>% 
  dplyr::select(anio, mes, crecimiento) %>% 
  spread(key = mes, value = crecimiento) %>% 
  select(anio,Enero,Febrero,Marzo,Abril,Mayo,Junio,Julio,Agosto,Septiembre,Octubre,Noviembre,Diciembre) %>% 
  data.frame()
fwrite(test2_cre, file = "Analisis_Subsidios/valor_cre_otros.csv", row.names = F, sep = ";", dec = ",")

homologacion <- read_excel("Analisis_Subsidios/Homologacion.xlsx") %>% 
  mutate(Cuenta = as.character(Cuenta))
str(homologacion)
str(df_sub)

df_sub_homo <- df_sub %>% 
  left_join(homologacion, by = c("CODIGO"="Cuenta")) %>% 
  na.omit()
str(df_sub_homo)
sort(names(table(df_sub_homo$Descripcion)))

# [1] "Bono escolar"                               "Bono lonchera"                              "Descuento ventas"                          
# [4] "Fomento"                                    "Kit escolar"                                "Subsidio en especie recreación"            
# [7] "Subsidio especie seguro (auxilio funeario)"


# Para Bono escolar
df_sub_recre <- df_sub_homo %>% 
  filter(Descripcion == "Subsidio en especie recreación") %>% 
  mutate(Fecha = paste(01, mes, anio, sep = "-"),
         Fecha = parse_date_time(Fecha, "dmy"),
         Valor_total_M = Valor/1000000) %>% 
  arrange(Fecha) %>% 
  mutate(crecimiento = round(100*(Valor_total_M-lag(Valor_total_M))/lag(Valor_total_M), 1))
str(df_sub_recre)

df_sub_recre_val <- df_sub_recre %>% 
  dplyr::select(anio, mes, Valor_total_M) %>% 
  spread(key = mes, value = Valor_total_M) %>% 
  select(anio,Enero,Febrero,Marzo,Abril,Mayo,Junio,Julio,Agosto,Septiembre,Octubre,Noviembre,Diciembre) %>% 
  data.frame()
fwrite(df_sub_recre_val, file = "Analisis_Subsidios/Recre_valor.csv", row.names = F, sep = ";", dec = ",")

df_sub_recre_cre <- df_sub_recre %>% 
  dplyr::select(anio, mes, crecimiento) %>% 
  spread(key = mes, value = crecimiento) %>% 
  select(anio,Enero,Febrero,Marzo,Abril,Mayo,Junio,Julio,Agosto,Septiembre,Octubre,Noviembre,Diciembre) %>% 
  data.frame()
fwrite(df_sub_recre_cre, file = "Analisis_Subsidios/Recre_creci.csv", row.names = F, sep = ";", dec = ",")

# hist_cuenta2015_2018 <- read_excel("Analisis_Subsidios/Historico subsidio x cuenta.xlsx", sheet = "RESUMEN") %>% 
#   data.frame() %>% 
#   dplyr::select(-Presupuesto.2019) %>% 
#   mutate(Año.2015 = 1000000*Año.2015,
#          Año.2016 = 1000000*Año.2016,
#          Año.2017 = 1000000*Año.2017,
#          Año.2018 = 1000000*Año.2018) %>% 
#   select(Cuenta.contable,Descripcion.cuenta, Tipo, Año.2015:Año.2018) %>% 
#   gather(key = "Fecha", value = "Valor_des", 4:7) %>% 
#   mutate(Anio = gsub("Año.","",Fecha, fixed = T),
#          Cuenta.contable = as.character(Cuenta.contable))
# str(hist_cuenta2015_2018)
# 
# df_acum2015_2018 <- df_acum %>% 
#   left_join(hist_cuenta2015_2018, by = c("CODIGO"="Cuenta.contable","anio"="Anio")) %>% 
#   filter(CODIGO %in% unique(hist_cuenta2015_2018$Cuenta.contable))
# str(df_acum2015_2018)

pesos_2015 <- read_excel("Analisis_Subsidios/Pesos.xlsx", sheet = "Pasos2015") 
pesos_2019 <- read_excel("Analisis_Subsidios/Pesos.xlsx", sheet = "Pasos2019") 
meses <- read_excel("Analisis_Subsidios/Pesos.xlsx", sheet = "Fechas")


aux <- df_sub %>% 
  left_join(pesos_2015, by = c( "anio", "CUENTA")) %>% 
  left_join(pesos_2019, by = c( "anio", 'mes', "CUENTA")) %>% 
  left_join(meses, by = "mes") %>% 
  mutate(Cuenta=toupper(ifelse(!is.na(SubCuenta2), SubCuenta2, 
                       ifelse(!is.na(SubCuenta), SubCuenta,CUENTA))),
         Participacion=ifelse(!is.na(Participacion2), Participacion2, 
                              ifelse(!is.na(Participacion), Participacion,1)),
         ValorTotal=Valor*Participacion,
         Fecha=as.Date(paste(anio, Mes, "1", sep="-"))) %>% 
  select(CODIGO, Cuenta, ValorTotal, Fecha)

paste(names(aux), collapse = "','")

Unir.Cadenas <- function(..., sep = " ", collapse = NULL, na.rm = F) {
  if (na.rm == F)
    paste(..., sep = sep, collapse = collapse)
  else
    if (na.rm == T) {
      paste.na <- function(x, sep) {
        x <- gsub("^\\s+|\\s+$", "", x)
        ret <- paste(na.omit(x), collapse = sep)
        is.na(ret) <- ret == ""
        return(ret)
      }
      df <- data.frame(..., stringsAsFactors = F)
      ret <- apply(df, 1, FUN = function(x) paste.na(x, sep))
      
      if (is.null(collapse))
        ret
      else {
        paste.na(ret, sep = collapse)
      }
    }
}


sort(unique(df_sub$CUENTA))

saveRDS(aux, file = "Analisis_Subsidios/consolidada_contabilidad.rds")
fwrite(aux, file = "Analisis_Subsidios/consolidada_contabilidad.csv", row.names = F)
