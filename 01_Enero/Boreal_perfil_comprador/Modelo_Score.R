# Cargamos datos
options(scipen = 999)
library(dplyr); library(readxl); library(RODBC); library(geosphere)

unos <- read_excel("BASE COMPRADORES BOREAL ENE 2020..xlsx") %>% 
  data.frame() %>% 
  mutate(id_persona = paste0("CC",CEDULA))
str(unos)

consolidada <- readRDS("ConsolidacionDIC2019.rds")
names(consolidada)

sum(unos$id_persona %in% consolidada$id_persona)
sum(unos$id_persona %in% consolidada$id_empresa)

geo_proyecto <- data.frame(cx = -74.123025, cy = 4.628415)

# Cargamos direccion de afiliados y localidad # si no te sirve cambiar desde  DBQ=
channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=//bogak08beimrodc/bi/Contacto/Fuentes/Direccion.accdb")
sqlTables(channel)
direccionVive <- sqlQuery(channel,paste ("select tb_direccion_de_residencia.id_persona, tb_direccion_de_residencia.localidad from tb_direccion_de_residencia"), # despues del FROM poner nombre de tabla
  as.is=T) %>% 
  data.frame()
odbcCloseAll()

data_model <- consolidada %>% 
  select(id_persona:EstratoPersona,DistanciaVT,marca_afiliado_unico,id_empresa,Piramide1,Piramide2,cx_empresa,cy_empresa,LocalidadEmpresa) %>% 
  mutate(marca_afiliado_unico = as.character(marca_afiliado_unico)) %>% 
  filter(!is.na(cx_persona) & cy_persona > 0.0000 & marca_afiliado_unico == "TRUE") %>%
  left_join(direccionVive %>% select(id_persona,localidad) %>% distinct(), by = "id_persona") %>% 
  mutate(d_viven_proyecto = distHaversine(.[,c("cx_persona","cy_persona")], geo_proyecto[,c("cx","cy")])/1000,
         d_trabajan_proyecto = distHaversine(.[,c("cx_empresa","cy_empresa")], geo_proyecto[,c("cx","cy")])/1000,
         compra_cc = ifelse(id_persona %in% unos$id_persona,1,0),
         compra_nit = ifelse(id_empresa %in% unos$id_persona,1,0),
         compra = factor(ifelse(compra_cc == 1 | compra_nit == 1, 1, 0)))
str(data_model)
table(duplicated(data_model$id_persona))
table(data_model$compra)

saveRDS(data_model, "data_model.rds")

# Balanceo de datos
rm(list = ls())
data_model <- readRDS("data_model.rds") %>% 
  select(-RangoEdad)
table(data_model$compra)

data_model2 <- readRDS("data_model.rds") %>% 
  dplyr::select(id_persona,compra)
str(data_model2)

# Balanceo
library(ROSE)
# I- Oversampling
data_model_over <- ovun.sample(compra~., data = data_model, method = "over", p= 0.5)$data
table(data_model_over$compra)


# II- Undersampling
data_model_under <- ovun.sample(compra~., data = data_model, method = "under", N = 200, seed = 1)$data
table(data_model_under$compra)

# III- Both over and under sampling
data_model_both <- ovun.sample(compra ~ ., data = data_model, method = "both", p=0.5, N=300000, seed=1)$data
table(data_model_both$compra)

# IV- ROSE: Mixed method with over and under sampling
data_model_rose <- ROSE(compra ~ ., data = data_model2, seed = 1)$data
table(data_model_rose$compra)
data_model_rose_union <- data_model_rose %>% 
  left_join(data_model %>% select(-compra), by = c("id_persona"="id_persona"))
str(data_model_rose_union)

#### MODELO LOGISTICO ####
### M1 ====
str(data_model_over)
m1_over <- glm(compra~.,data=data_model_over %>% 
                 select(compra,Genero:segmento_grupo_familiar,-FechaNacimiento,
                        LocalidadEmpresa,localidad,d_viven_proyecto,d_trabajan_proyecto),
               family=binomial(logit))
summary(m1_over)
m1_over <- MASS::stepAIC(m1_over, direction = "both")
summary(m1_over)

# Predicciones
predicciones <- predict(object = m1_over, type = "response")
prediccion <- data.frame(probabilidad = predicciones, clase = rep(NA, length(predicciones))) 
summary(prediccion$probabilidad)

### M2 ====
m1_under <- glm(compra~.,data=data_model_under %>% 
                 select(compra,Genero:segmento_grupo_familiar,-FechaNacimiento,
                        d_viven_proyecto,d_trabajan_proyecto),
               family=binomial(logit))
summary(m1_under)
m1_under <- MASS::stepAIC(m1_under, direction = "both")
summary(m1_under)

library(writexl)




# Comparacion de modelos
plot(m1_over, col="steelblue")
plot(m1_under, col="darkgreen", add=T)
plot(m1_both, col="firebrick", add=T)
plot(m1_rose, col="darkgreen", add=T)
# plot(knn.roc, col="darkorange", add=T)
# plot(modelo_rf.roc, col="orange3", add=T)
# plot(boost.roc, col="lightseagreen", add=T)
# #plot(svmrad.roc, col="hotpink4", add=T)
# plot(nnet.roc, col="saddlebrown", add=T)

legend("bottomright",
       legend = c("RL OVER","RL UNDER","RL BOTH","RL ROSE"),
       bty="n",lty=1,cex=0.6,
       col=c("steelblue","darkgreen","firebrick","darkorange")
)



