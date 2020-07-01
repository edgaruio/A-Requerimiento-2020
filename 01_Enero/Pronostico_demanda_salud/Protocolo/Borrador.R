# Borrador

################################################
# FUNCION FINAL 
################################################
funcion_pronostico3 <- function(data_input,inicio,final){
  data_input <- data.frame(data_input)
  n_material = nrow(data_input)
  pronosticos <- c()
  for (i in 1:n_material) {
    
    if (as.numeric(data_input[i,"conteo_cero"]) >= 5) {
      pronosticos[i] = rowMeans(data_input[i,c(32:36)], na.rm = T)
    }
    else {
      vector_ts <- ts(as.numeric(data_input[i,c(2:36)]),start = inicio, end = final, frequency = 12)
      best.fit.elec <- auto.arima(vector_ts)
      pronosticos[i] <- round(as.numeric(data.frame(forecast(best.fit.elec, 1))[1,1]))
    }
  }
  data_output <- cbind(data_input,pronosticos)
}
#################################################


funcion_pronostico <- function(data_input,inicio,final){
  n_material = nrow(data_input)
  pronosticos <- c()
  # acc_modelo1 <- c()
  for (i in 1:n_material) {
    
    if (mean(t(data_input[i,-1])) <= 80) {
      pronosticos[i] = mean(t(data_input[i,-1]))
    }
    else {
      vector_ts <- ts(as.numeric(data_input[i,-1]),start = inicio, end = final, frequency = 12)
      best.arima.elec <- get.best.arima(vector_ts, maxord = c(1,1,1,1,0,1))
      best.fit.elec <- best.arima.elec[[2]]
      pronosticos[i] <- predict(best.fit.elec, 1)$pred
      # acc_modelo1[i] <- data.frame(accuracy(vector_ts-best.fit.elec$residuals,vector_ts))
    }
  }
  data_output <- cbind(data_input,pronosticos)
}

funcion_pronostico2 <- function(data_input,inicio,final){
  n_material = nrow(data_input)
  pronosticos <- c()
  for (i in 1:n_material) {
    
    if (sum(t(data_input[i,-1]) == 0) >= 15) {
      pronosticos[i] = median(t(data_input[i,-1]))
    }
    else {
      vector_ts <- ts(as.numeric(data_input[i,-1]),start = inicio, end = final, frequency = 12)
      best.arima.elec <- get.best.arima(vector_ts, maxord = c(1,1,1,1,0,1))
      best.fit.elec <- best.arima.elec[[2]]
      pronosticos[i] <- predict(best.fit.elec, 1)$pred
    }
  }
  data_output <- cbind(data_input[,1],pronosticos)
}

funcion_pronostico3 <- function(data_input,inicio,final){
  n_material = nrow(data_input)
  pronosticos <- c()
  for (i in 1:n_material) {
    
    if (median(t(data_input[i,-1])) <= 80) {
      pronosticos[i] = round(median(t(data_input[i,-1])))
    }
    else {
      vector_ts <- ts(as.numeric(data_input[i,-1]),start = inicio, end = final, frequency = 12)
      best.fit.elec <- auto.arima(vector_ts)
      pronosticos[i] <- round(as.numeric(predict(best.fit.elec, 1)$pred))
    }
  }
  data_output <- cbind(data_input,pronosticos)
}



funcion_pronostico4 <- function(data_input,inicio,final){
  n_material = nrow(data_input)
  n_columnas = ncol(data_input)
  pronosticos <- c()
  for (i in 1:n_material) {
    
    if (data_input[i,conteo_cero] >= 5) {
      pronosticos[i] = rowMeans(data_input[,c(32,33,34,35,36)], na.rm = T)
    } else {
      vector_ts <- ts(as.numeric(data_input[i,c(2:36)]),start = inicio, end = final, frequency = 12)
      best.fit.elec <- HoltWinters(vector_ts)
      pronosticos[i] <- round(as.numeric(predict(best.fit.elec, 1)))
    }
    
    # if (median(t(data_input[i,-1])) <= 20) {
    #   pronosticos[i] = round(median(t(data_input[i,-1])))
    # } else {
    #   vector_ts <- ts(as.numeric(data_input[i,-1]),start = inicio, end = final, frequency = 12)
    #   best.fit.elec <- HoltWinters(vector_ts)
    #   pronosticos[i] <- round(as.numeric(predict(best.fit.elec, 1)))
    # }
    
  }
  data_output <- cbind(data_input,pronosticos)
}