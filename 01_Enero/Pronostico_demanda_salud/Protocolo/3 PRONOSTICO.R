# Pronósticos
options(scipen = 999)
rm(list = ls ())
library(data.table); library(esquisse); library(dplyr); library(readxl); library(lubridate); library(tidyr); library(Hmisc); library(forecast)

# Cargamos funciones depueradas y diagnosticamos

df_rfm_score <- readRDS("Protocolo/bd_rfm_Score.rds") %>% 
  na.omit() %>% 
  select(Material, R_Score:Calificacion) %>% 
  data.frame()
str(df_rfm_score)

df_union <- readRDS("Resultados/df_union.rds") %>% 
  data.frame() %>% 
  mutate(conteo_cero = rowSums(.[,c(2:36)]==0, na.rm = TRUE))
str(df_union)

### Funciones ====
# No usar
funcion_pronostico_HW <- function(data_input,inicio,final){
  n_material = nrow(data_input)
  pronosticos <- c()
  for (i in 1:n_material) {
    
    if (as.numeric(data_input[i,"conteo_cero"]) >= 10) {
      pronosticos[i] = as.numeric(rowMeans(data_input[i,c(32:36)], na.rm = T))
    } else {
      vector_ts <- ts(as.numeric(data_input[i,c(2:36)]),start = inicio, end = final, frequency = 12)
      best.fit.elec <- HoltWinters(vector_ts)
      pronosticos[i] <- round(as.numeric(predict(best.fit.elec, 1)))
    }
  }
  data_output <- cbind(data_input,pronosticos)
}

funcion_pronostico_arima <- function(data_input,inicio,final){
  data_input <- data.frame(data_input)
  n_material = nrow(data_input)
  pronostico_arima <- c(NA, n_material)
  rmse_arima <- c(NA, n_material)
  
  for (i in 1:n_material) {
    
    if (as.numeric(data_input[i,"conteo_cero"]) >= 10) {
      vector_ts <- ts(as.numeric(data_input[i,c(2:36)]),start = inicio, end = final, frequency = 12)
      pronostico_arima[i] <- round(data.frame(forecast(meanf(vector_ts), h = 1))$Point.Forecast, 3)
      rmse_arima[i] <- round(as.numeric(accuracy(forecast(meanf(vector_ts), h = 1))[,2]), 3)
    }
    else {
      vector_ts <- ts(as.numeric(data_input[i,c(2:36)]),start = inicio, end = final, frequency = 12)
      best.fit.elec <- auto.arima(vector_ts)
      pronostico_arima[i] <- round(as.numeric(data.frame(forecast(best.fit.elec, 1))[1,1]))
      rmse_arima[i] <- as.numeric(summary(best.fit.elec)[,2])
    }
  }
  data_output <- cbind(data_input,pronostico_arima,rmse_arima)
}

funcion_pronostico_BoxCox <- function(data_input,inicio,final){
  data_input <- data.frame(data_input)
  n_material = nrow(data_input)
  pronostico_BoxCox <- rep(NA, n_material)
  rmse_BoxCox <- rep(NA, n_material)
  
  for (i in 1:n_material) {
    
    if (as.numeric(data_input[i,"conteo_cero"]) >= 5) {
      vector_ts <- ts(as.numeric(data_input[i,c(2:36)]),start = inicio, end = final, frequency = 12)
      pronostico_BoxCox[i] <- round(data.frame(forecast(meanf(vector_ts), h = 1))$Point.Forecast, 3)
      rmse_BoxCox[i] <- round(as.numeric(accuracy(forecast(meanf(vector_ts), h = 1))[,2]), 3)
    }
    else {
      vector_ts <- ts(as.numeric(data_input[i,c(2:36)]),start = inicio, end = final, frequency = 12)
      lambda <- BoxCox.lambda(vector_ts)
      best.fit.elec <- ar(BoxCox(vector_ts,lambda = lambda))
      pronostico_BoxCox[i] <- round(data.frame(forecast(best.fit.elec, h=1, lambda=lambda))$Point.Forecast)
      rmse_BoxCox[i] <- round(as.numeric(accuracy(forecast(best.fit.elec, h=1, lambda=lambda))[,2]), 3)
    }
    print(i)
  }
  data_output <- cbind(data_input,pronostico_BoxCox,rmse_BoxCox)
}

funcion_pronostico_SuaExp <- function(data_input,inicio,final){
  data_input <- data.frame(data_input)
  n_material = nrow(data_input)
  pronostico_SuaExp <- rep(NA, n_material)
  rmse_SuaExp <- rep(NA, n_material)
  
  for (i in 1:n_material) {
    
    if (as.numeric(data_input[i,"conteo_cero"]) >= 5) {
      vector_ts <- ts(as.numeric(data_input[i,c(2:36)]),start = inicio, end = final, frequency = 12)
      pronostico_SuaExp[i] <- round(data.frame(forecast(meanf(vector_ts), h = 1))$Point.Forecast, 3)
      rmse_SuaExp[i] <- round(as.numeric(accuracy(forecast(meanf(vector_ts), h = 1))[,2]), 3)
    }
    else {
      vector_ts <- ts(as.numeric(data_input[i,c(2:36)]),start = inicio, end = final, frequency = 12)
      pronostico_SuaExp[i] <- round(data.frame(forecast(ets(vector_ts), h = 1))$Point.Forecast, 3)
      rmse_SuaExp[i] <- round(as.numeric(accuracy(forecast(ets(vector_ts), h = 1))[,2]), 3)
    }
    print(i)
  }
  data_output <- cbind(data_input,pronostico_SuaExp,rmse_SuaExp)
}

funcion_pronostico_Meanf <- function(data_input,inicio,final){
  data_input <- data.frame(data_input)
  n_material = nrow(data_input)
  pronostico_Meanf <- rep(NA, n_material)
  rmse_Meanf <- rep(NA, n_material)
  
  for (i in 1:n_material) {
    
    # if (as.numeric(data_input[i,"conteo_cero"]) >= 5) {
    #   pronostico_Meanf[i] = rowMeans(data_input[i,c(27:36)], na.rm = T)
    # }
    # else {
      vector_ts <- ts(as.numeric(data_input[i,c(2:36)]),start = inicio, end = final, frequency = 12)
      pronostico_Meanf[i] <- round(data.frame(forecast(meanf(vector_ts), h = 1))$Point.Forecast, 3)
      rmse_Meanf[i] <- round(as.numeric(accuracy(forecast(meanf(vector_ts), h = 1))[,2]), 3)
    # }
    print(i)
  }
  data_output <- cbind(data_input,pronostico_Meanf,rmse_Meanf)
}

funcion_pronostico_Naive <- function(data_input,inicio,final){
  data_input <- data.frame(data_input)
  n_material = nrow(data_input)
  pronostico_Naive <- rep(NA, n_material)
  rmse_Naive <- rep(NA, n_material)
  
  for (i in 1:n_material) {
    
    # if (as.numeric(data_input[i,"conteo_cero"]) >= 5) {
    #   pronostico_Meanf[i] = rowMeans(data_input[i,c(27:36)], na.rm = T)
    # }
    # else {
    vector_ts <- ts(as.numeric(data_input[i,c(2:36)]),start = inicio, end = final, frequency = 12)
    pronostico_Naive[i] <- round(data.frame(forecast(naive(vector_ts), h = 1))$Point.Forecast, 3)
    rmse_Naive[i] <- round(as.numeric(accuracy(forecast(naive(vector_ts), h = 1))[,2]), 3)
    # }
    print(i)
  }
  data_output <- cbind(data_input,pronostico_Naive,rmse_Naive)
}

funcion_pronostico_SNaive <- function(data_input,inicio,final){
  data_input <- data.frame(data_input)
  n_material = nrow(data_input)
  pronostico_SNaive <- rep(NA, n_material)
  rmse_SNaive <- rep(NA, n_material)
  
  for (i in 1:n_material) {
    
    # if (as.numeric(data_input[i,"conteo_cero"]) >= 5) {
    #   pronostico_Meanf[i] = rowMeans(data_input[i,c(27:36)], na.rm = T)
    # }
    # else {
    vector_ts <- ts(as.numeric(data_input[i,c(2:36)]),start = inicio, end = final, frequency = 12)
    pronostico_SNaive[i] <- round(data.frame(forecast(snaive(vector_ts), h = 1))$Point.Forecast, 3)
    rmse_SNaive[i] <- round(as.numeric(accuracy(forecast(snaive(vector_ts), h = 1))[,2]), 3)
    # }
    print(i)
  }
  data_output <- cbind(data_input,pronostico_SNaive,rmse_SNaive)
}

funcion_pronostico_Neural_Net <- function(data_input,inicio,final){
  data_input <- data.frame(data_input)
  n_material = nrow(data_input)
  pronostico_NNet <- rep(NA, n_material)
  rmse_NNet <- rep(NA, n_material)
  
  for (i in 1:n_material) {
    
    # if (as.numeric(data_input[i,"conteo_cero"]) >= 5) {
    #   pronostico_BoxCox[i] = rowMeans(data_input[i,c(27:36)], na.rm = T)
    # }
    # else {
      vector_ts <- ts(as.numeric(data_input[i,c(2:36)]),start = inicio, end = final, frequency = 12)
      lambda <- BoxCox.lambda(vector_ts)
      fit_previo_net <- nnetar(vector_ts, lambda = lambda)
      pronostico_NNet[i] <- round(data.frame(forecast(fit_previo_net, h = 1, PI = TRUE))$Point.Forecast)
      rmse_NNet[i] <- round(as.numeric(accuracy(forecast(fit_previo_net, h = 1, PI = TRUE))[,2]), 3)
    # }
    print(i)
  }
  data_output <- cbind(data_input,pronostico_NNet,rmse_NNet)
}

### Pronostico ====
### HW ====
# salidaHW <- funcion_pronostico4(df_union,inicio = c(2017,1), final = c(2019,11)) 
# salidaHW <- salidaHW %>% 
#   data.frame() %>% 
#   left_join(df_rfm_score, by = c("material"="Material"))
# saveRDS(salida4, "App_Pronostico/Data/Salida_HW.rds")
### Arima ====
salida_arima <- funcion_pronostico_arima(df_union,inicio = c(2017,1), final = c(2019,11)) 
salida_arima <- salida_arima %>% 
  data.frame() %>% 
  left_join(df_rfm_score, by = c("material"="Material"))
saveRDS(salida3, "App_Pronostico/Data/Salida_auto_arima.rds")
### Box Cox ====
salida_BoxCox <- funcion_pronostico_BoxCox(df_union,inicio = c(2017,1), final = c(2019,11)) 
salida_BoxCox <- salida_BoxCox %>% 
  data.frame() %>% 
  left_join(df_rfm_score, by = c("material"="Material"))
saveRDS(salida_BoxCox, "App_Pronostico/Data/Salida_Box_Cox.rds")
### Suavizamiento Exponencial ====
salida_SuaExp <- funcion_pronostico_SuaExp(df_union,inicio = c(2017,1), final = c(2019,11)) 
salida_SuaExp <- salida_SuaExp %>% 
  data.frame() %>% 
  left_join(df_rfm_score, by = c("material"="Material"))
saveRDS(salida_SuaExp, "App_Pronostico/Data/Salida_Sua_Exp.rds")

### MeanF ====
salida_Meanf <- funcion_pronostico_Meanf(df_union,inicio = c(2017,1), final = c(2019,11)) 
salida_Meanf <- salida_Meanf %>% 
  data.frame() %>% 
  left_join(df_rfm_score, by = c("material"="Material"))
saveRDS(salida_Meanf, "App_Pronostico/Data/Salida_Meanf.rds")

### Naive ====
salida_Naive <- funcion_pronostico_Naive(df_union,inicio = c(2017,1), final = c(2019,11)) 
salida_Naive <- salida_Naive %>% 
  data.frame() %>% 
  left_join(df_rfm_score, by = c("material"="Material"))
saveRDS(salida_Naive, "App_Pronostico/Data/Salida_Naive.rds")
### SNaive ====
salida_SNaive <- funcion_pronostico_SNaive(df_union,inicio = c(2017,1), final = c(2019,11)) 
salida_SNaive <- salida_SNaive %>% 
  data.frame() %>% 
  left_join(df_rfm_score, by = c("material"="Material"))
saveRDS(salida_SNaive, "App_Pronostico/Data/Salida_SNaive.rds")

### Neural Net ====
salida_NNet <- funcion_pronostico_Neural_Net(df_union,inicio = c(2017,1), final = c(2019,11)) 
salida_NNet <- salida_NNet %>% 
  data.frame() %>% 
  left_join(df_rfm_score, by = c("material"="Material"))
saveRDS(salida_NNet, "App_Pronostico/Data/Salida_NNet.rds")


### Union pronosticos
str(salida_NNet)
salida_24032020 <- df_union %>% 
  select(material) %>% 
  left_join(salida_arima %>% select(material,pronostico_arima, rmse_arima) %>% 
              mutate(rmse_arima = round(rmse_arima, 3)), by = "material") %>% 
  left_join(salida_BoxCox %>% select(material, pronostico_BoxCox, rmse_BoxCox) %>% 
              mutate(rmse_BoxCox = round(rmse_BoxCox, 3)), by = "material") %>% 
  left_join(salida_SuaExp %>% select(material, pronostico_SuaExp, rmse_SuaExp) %>% 
              mutate(rmse_SuaExp = round(rmse_SuaExp, 3)), by = "material") %>% 
  left_join(salida_Meanf %>% select(material, pronostico_Meanf, rmse_Meanf) %>% 
              mutate(rmse_Meanf = round(rmse_Meanf, 3)), by = "material") %>% 
  left_join(salida_Naive %>% select(material, pronostico_Naive, rmse_Naive) %>% 
              mutate(rmse_Naive = round(rmse_Naive, 3)), by = "material") %>% 
  left_join(salida_SNaive %>% select(material, pronostico_SNaive, rmse_SNaive) %>% 
              mutate(rmse_SNaive = round(rmse_SNaive, 3)), by = "material") %>% 
  left_join(salida_NNet %>% select(material, pronostico_NNet, rmse_NNet) %>% 
              mutate(rmse_NNet = round(rmse_NNet, 3)), by = "material") %>% 
  mutate(columna_final = apply(.[,c(3,5,7,9,11,13,15)], 1, which.min)) %>% 
  mutate(rmse_final = apply(.[,c(3,5,7,9,11,13,15)], 1, min, na.rm =  T)) %>% 
  # mutate(rmse_final = ifelse(is.nan(rmse_final), NA, rmse_final)) %>% 
  # mutate(pronostico_final = ifelse(rmse_arima == rmse_final, pronostico_arima,
  #                                  ifelse(rmse_BoxCox == rmse_final, pronostico_BoxCox,
  #                                         ifelse(rmse_SuaExp == rmse_final, pronostico_SuaExp,
  #                                                ifelse(rmse_Meanf == rmse_final, pronostico_Meanf,
  #                                                       ifelse(rmse_Naive == rmse_final, pronostico_Naive,
  #                                                              ifelse(rmse_SNaive == rmse_final, pronostico_SNaive,
  #                                                                     ifelse(rmse_NNet == rmse_final, pronostico_NNet, NA)))))))) %>% 
  data.frame()
str(salida_24032020)

demanda_promedio <- df_union %>% 
  mutate(demanda_promedio = rowMeans(.[,c(2:36)])) %>% 
  select(material,demanda_promedio)
str(demanda_promedio)

valor_prono <- salida_24032020 %>% 
  select(material, pronostico_arima, pronostico_BoxCox, pronostico_SuaExp, pronostico_Meanf, pronostico_Naive, pronostico_SNaive,
         pronostico_NNet) %>% 
  gather("metodo","pronostico",2:8) %>% 
  mutate(metodo = gsub("pronostico_","",metodo,fixed = T)) %>% 
  data.frame()
str(valor_prono)

valor_rmse <- salida_24032020 %>% 
  select(material, rmse_arima, rmse_BoxCox, rmse_SuaExp, rmse_Meanf, rmse_Naive, rmse_SNaive, rmse_NNet) %>% 
  gather("metodo","rmse",2:8) %>% 
  mutate(metodo = gsub("rmse_","",metodo,fixed = T)) %>% 
  data.frame()
str(valor_rmse)

union_prono_rmse <- valor_prono %>% 
  left_join(valor_rmse, by = c("material"="material","metodo"="metodo")) %>% 
  left_join(demanda_promedio, by = "material") %>% 
  group_by(material) %>% 
  arrange(material,rmse) %>% 
  filter(pronostico >= 0 & pronostico <= 10*demanda_promedio,
         row_number()==1) %>% 
  ungroup()
str(union_prono_rmse)
summary(union_prono_rmse$pronostico)

salida_24032020_union <- salida_24032020 %>% 
  left_join(union_prono_rmse, by = "material") %>% 
  left_join(df_rfm_score, by = c("material"="Material")) %>% 
  data.frame() %>% 
  mutate(pronostico = ifelse(is.na(pronostico), pronostico_Meanf, pronostico))
str(salida_24032020_union)
sum(is.na(salida_24032020_union$pronostico))

saveRDS(salida_24032020_union, "App_Pronostico/Data/salida_24032020.rds")

# load("Resultados/Salida_Modelos_24032020.RData")
## Resumen modelos
library(esquisse)
esquisser()

tb_agregados <- salida_24032020_union %>% 
  group_by(Calificacion,metodo) %>% 
  summarise(conteo = n()) %>% 
  filter(!is.na(metodo))

library(ggplot2)

ggplot(tb_agregados) +
 aes(x = Calificacion, y = metodo, fill = conteo) +
 geom_tile(size = 1L) +
 scale_fill_distiller(palette = "RdBu") +
 labs(x = "Calificación", y = "Modelos", title = "Distribución de Modelos de Pronóstico", subtitle = "Calificación RFM vs Modelos", fill = "Conteo") +
 theme_minimal()
