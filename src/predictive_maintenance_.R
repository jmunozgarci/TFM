## ----setup, include=FALSE-------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----load_libraries, include=FALSE----------------------------------------------------------------------------------
library(knitr)
library(VIM)
library(h2o)
library(tidyverse)
library(mlbench)
library(randomForest)
library(caret)


## ----read-----------------------------------------------------------------------------------------------------------
# Carga de datos
data <- read.csv("C:/Users/Javier/Desktop/machine_failures.csv")
names(data) <- gsub("[^A-Za-z0-9]", "", names(data))

# Comprobación de datos cargados
head(data)

# Tipos de variables
types <- sapply(data, class)
kable(data.frame(Variables = names(types), Clase = as.vector(types)))


## ----conversion-----------------------------------------------------------------------------------------------------
#Conversión de datos
data$Date <- as.POSIXct(data$Date, format = "%Y-%m-%d %H:%M:%S", tz="GMT")
data$Measure2 <- as.factor(data$Measure2)
data$Measure3 <- as.factor(data$Measure3)

# Tipos de variables
str(data)


## ----summary--------------------------------------------------------------------------------------------------------
# Información básica
summary(data)

#Comprobación de valores nulos
cat("\nValores nulos en el conjunto de datos:", toString(sum(is.na(data))), "\n" )


## ----catPlots-------------------------------------------------------------------------------------------------------
# Operator
pie(table(data$Operator), main = 'Operator')
# Measure2
pie(table(data$Measure2), main = 'Measure2')
# Measure3
pie(table(data$Measure3), main = 'Measure3')
# Failure
pie(table(data$Failure), main = 'Failure')


## ----numEspPlots----------------------------------------------------------------------------------------------------
par(mfrow = c(1, 4))
# Temperature
boxplot(data$Temperature, main = 'Temperature')
# Humidity
boxplot(data$Humidity, main = 'Humidity')
# Measure1
boxplot(data$Measure1, main = 'Measure1')
# Measure4
boxplot(data$Measure4, main = 'Measure4')
# Measure5
boxplot(data$Measure5, main = 'Measure5')
# Measure6
boxplot(data$Measure6, main = 'Measure6')
# Measure7
boxplot(data$Measure7, main = 'Measure7')
# Measure8
boxplot(data$Measure8, main = 'Measure8')
# Measure9
boxplot(data$Measure9, main = 'Measure9')
# Measure10
boxplot(data$Measure10, main = 'Measure10')
# Measure11
boxplot(data$Measure11, main = 'Measure11')
# Measure12
boxplot(data$Measure12, main = 'Measure12')
# Measure13
boxplot(data$Measure13, main = 'Measure13')
# Measure14
boxplot(data$Measure14, main = 'Measure14')
# Measure15
boxplot(data$Measure15, main = 'Measure15')
# HoursSincePreviousFailure
boxplot(data$HoursSincePreviousFailure, main = 'HoursSincePreviousFailure')



## ----numGenPlots----------------------------------------------------------------------------------------------------
# Date
hist(data$Date, "weeks", format = "%d %b")


## ----Temperature----------------------------------------------------------------------------------------------------
par(mfrow = c(1, 2))
# Temperature
boxplot(data$Temperature, main = 'Temperature')
# Humidity
boxplot(data$Humidity, main = 'Humidity')


## ----outliersTemp---------------------------------------------------------------------------------------------------
values <- boxplot.stats(data$Temperature)$out
#Valores extremos y sus posiciones:
cat("Valores extremos en Temperatura:", toString(values), "\n" )


## ----outliersTemp_hist----------------------------------------------------------------------------------------------
#Examinamos un subconjunto de datos donde se muestren los valores extremos.
february_data <- data[data$Datemonth == 2 & data$Datedayofmonth > 13,]
plot(february_data$Date, february_data$Temperature, xaxt="n", type = "o")
axis.POSIXct(1, at=february_data$Date, las=2)


## ----outliersTemp_hist2---------------------------------------------------------------------------------------------
#Examinamos un subconjunto específico del valor extremo superior para determinar si es correcto.
february_data <- data[data$Datemonth == 2 & data$Datedayofmonth > 13 & data$Datedayofmonth < 17,]
plot(february_data$Date, february_data$Temperature, xaxt="n", type = "o")
axis.POSIXct(1, at=february_data$Date, las=2)


## ----outliersTemp_delete--------------------------------------------------------------------------------------------
#Eliminamos de los valores extremos los valores superiores a 70, ya que los consideramos válidos.
idx <- which(data$Temperature %in% values & data$Temperature < 60)
data[idx,]

data[idx,]$Temperature <- NA
data[idx,]


## ----outliersTemp_input---------------------------------------------------------------------------------------------
#Todos los datos con NA pertenecen a diferentes meses.
#Para mejor generalización y aplicabilidad del código, se seleccionan los datos
#del conjunto del mismo mes y se realiza la imputación.
selected_reg <- which(data$Datemonth==2)
selected_vars <- c("Temperature","Humidity","Measure1","Measure4","Measure5","Measure6","Measure7","Measure8","Measure9","Measure10","Measure11","Measure12","Measure13","Measure14","Measure15","HoursSincePreviousFailure")
#Imputación con distancia de Gower:
output <- kNN(data[selected_reg, selected_vars], variable="Temperature", k=3)
data[idx[1],]$Temperature <- output[output$Temperature_imp==TRUE,]$Temperature

#Repetimos los pasos para los demás meses
selected_reg <- which(data$Datemonth==5)
output <- kNN(data[selected_reg, selected_vars], variable="Temperature", k=3)
data[idx[2],]$Temperature <- output[output$Temperature_imp==TRUE,]$Temperature

selected_reg <- which(data$Datemonth==7)
output <- kNN(data[selected_reg, selected_vars], variable="Temperature", k=3)
data[idx[3],]$Temperature <- output[output$Temperature_imp==TRUE,]$Temperature

selected_reg <- which(data$Datemonth==10)
output <- kNN(data[selected_reg, selected_vars], variable="Temperature", k=3)
data[idx[4],]$Temperature <- output[output$Temperature_imp==TRUE,]$Temperature

#Examinamos de nuevo el conjunto con el valor extremo, para ver si la imputación
#es consistente
february_data <- data[data$Datemonth == 2 & data$Datedayofmonth > 26 & data$Datedayofmonth < 29,]
plot(february_data$Date, february_data$Temperature, xaxt="n", type = "o")
axis.POSIXct(1, at=february_data$Date, las=2)


## ----Humidity_outliers----------------------------------------------------------------------------------------------
values <- boxplot.stats(data$Humidity)$out
#Valores extremos y sus posiciones:
cat("Valores extremos en Humedad:", toString(values), "\n" )


## ----Humidity_delete------------------------------------------------------------------------------------------------
#En esta ocasión, sabemos que los valores superiores a 100 son incorrectos.
idx <- which(data$Humidity %in% values & data$Humidity > 100)
data[idx,]

data[idx,]$Humidity <- NA
data[idx,]


## ----Humidity_input-------------------------------------------------------------------------------------------------
#Todos los datos con NA pertenecen a diferentes meses.
#Realizaremos el mismo procedimiento que con la temperatura.
selected_reg <- which(data$Datemonth==1)
output <- kNN(data[selected_reg, selected_vars], variable="Humidity", k=3)
data[idx[1],]$Humidity <- output[output$Humidity_imp==TRUE,]$Humidity

selected_reg <- which(data$Datemonth==4)
output <- kNN(data[selected_reg, selected_vars], variable="Humidity", k=3)
data[idx[2],]$Humidity <- output[output$Humidity_imp==TRUE,]$Humidity

selected_reg <- which(data$Datemonth==7)
output <- kNN(data[selected_reg, selected_vars], variable="Humidity", k=3)
data[idx[3],]$Humidity <- output[output$Humidity_imp==TRUE,]$Humidity

selected_reg <- which(data$Datemonth==10)
output <- kNN(data[selected_reg, selected_vars], variable="Humidity", k=3)
data[idx[4],]$Humidity <- output[output$Humidity_imp==TRUE,]$Humidity

selected_reg <- which(data$Datemonth==12)
output <- kNN(data[selected_reg, selected_vars], variable="Humidity", k=3)
data[idx[5],]$Humidity <- output[output$Humidity_imp==TRUE,]$Humidity

#Examinamos de nuevo el conjunto con el valor extremo, para ver si la imputación
#es consistente
january_data <- data[data$Datemonth == 1 & data$Datedayofmonth > 1 & data$Datedayofmonth < 4,]
plot(january_data$Date, january_data$Humidity, xaxt="n", type = "o")
axis.POSIXct(1, at=january_data$Date, las=2)


## ----Importancia----------------------------------------------------------------------------------------------------
#Crearemos una semilla para que los resultados sean repetibles
set.seed(7)

selected_vars <- c("Temperature","Humidity","Measure1","Measure2","Measure3","Measure4","Measure5","Measure6","Measure7","Measure8","Measure9","Measure10","Measure11","Measure12","Measure13","Measure14","Measure15","HoursSincePreviousFailure","Datemonth","Datedayofweek","Datedayofmonth","Datehour")

data_features <- data[selected_vars]
data_target <- data[["Failure"]]

#Función de selección mediante RandomForest
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
results <- rfe(data_features, data_target, sizes=c(1:22), rfeControl=control)

#Resultados
print(results)
 plot(results, type=c("g", "o"))


## ----Autoencoder_init-----------------------------------------------------------------------------------------------
h2o.init(nthreads = -1)
h2o.removeAll()
h2o.no_progress()

#Seleccionaremos las variables elegidas por el RandomForest
data_3features <- data[c("Temperature","Humidity","HoursSincePreviousFailure","Failure")]

#Convertimos H2O
data_hf <- as.h2o(data_3features)


## ----Autoencoder_split----------------------------------------------------------------------------------------------
splits <- h2o.splitFrame(data_hf, 
                         ratios = 0.8, 
                         seed = 42)

train  <- splits[[1]]
test <- splits[[2]]

response <- "Failure"
features <- setdiff(colnames(train), response)


## ----IsolationForest------------------------------------------------------------------------------------------------
isoforest <- h2o.isolationForest(
                model_id = "isoforest",
                training_frame = train,
                x              = features,
                max_depth      = 350,
                ntrees         = 500,
                sample_rate    = 0.9 
             )

predicciones_h2o <- h2o.predict(
                      object  = isoforest,
                      newdata = train
                    )
predicciones <- as.data.frame(predicciones_h2o)

ggplot(data = predicciones, aes(x = mean_length)) +
  geom_histogram(color = "gray40") +
  geom_vline(
    xintercept = quantile(predicciones$mean_length, seq(0, 1, 0.1)),
    color      = "red",
    linetype   = "dashed") +
  labs(
    title = "Distribución de las distancias medias del Isolation Forest",
    subtitle = "Cuantiles marcados en rojo"  ) +
  theme_bw()


## ----IsolationForest_test-------------------------------------------------------------------------------------------
predicciones_h2o_test <- h2o.predict(
                          object  = isoforest,
                          newdata = test
                        )
predicciones_test <- as.data.frame(predicciones_h2o_test)

resultados <- as.data.frame(test) %>%
         bind_cols(predicciones_test)

ggplot(data = resultados,
       aes(x = Failure, y = mean_length)) +
geom_jitter(aes(color = Failure), width = 0.03, alpha = 0.3) + 
geom_violin(alpha = 0) +
geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0) +
stat_summary(fun = "mean", colour = "orangered2", size = 3, geom = "point") +
labs(title = "Distancia promedio en el modelo Isolation Forest",
     x = "Anomalía)",
     y = "Distancia promedio") +
theme_bw() + 
theme(legend.position = "none")


## ----IsolationForest_matrix-----------------------------------------------------------------------------------------
scoreLimit = quantile(as.vector(predicciones_h2o$pred), 0.995)
predicciones_h2o_test$scoreLimit <- scoreLimit

pred <- as.data.frame(predicciones_h2o_test) %>%
  mutate(actual = as.vector(test[, 4])) %>%
  mutate(predict = ifelse(predict > scoreLimit, "Yes", "No"))

pred %>%
  group_by(actual, predict) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) 


## ----IsolationForest_matrix_result----------------------------------------------------------------------------------
pred %>%
  ggplot(aes(x = actual, fill = predict)) +
    geom_bar() +
    theme_bw() +
    scale_fill_brewer(palette = "Set1") +
    facet_wrap( ~ actual, scales = "free", ncol = 2)


## ----Autoencoder_model----------------------------------------------------------------------------------------------
model_nn <- h2o.deeplearning(x = features,
                             training_frame = train,
                             model_id = "model_nn",
                             autoencoder = TRUE,
                             reproducible = TRUE,
                             ignore_const_cols = FALSE,
                             seed = 42,
                             hidden = c(10, 2, 10), 
                             epochs = 100,
                             activation = "Tanh",
                             validation_frame = test)


## ----Autoencoder_anomaly--------------------------------------------------------------------------------------------
anomaly <- h2o.anomaly(model_nn, test) %>%
  as.data.frame() %>%
  tibble::rownames_to_column() %>%
  mutate(Class = as.vector(test[, 4]))

mean_mse <- anomaly %>%
  group_by(Class) %>%
  summarise(mean = mean(Reconstruction.MSE))

threshold = quantile(anomaly$Reconstruction.MSE, probs = 0.995)

ggplot(anomaly, aes(x = as.numeric(rowname), y = Reconstruction.MSE, color = as.factor(Class))) +
  geom_point(alpha = 0.3) +
  geom_hline(data = mean_mse, aes(yintercept = mean, color = Class)) +
  scale_color_brewer(palette = "Set1") +
  labs(x = "instance number",
       color = "Class")


## ----Autoencoder_anomaly_matrix-------------------------------------------------------------------------------------
anomaly <- anomaly %>%
  mutate(predict = ifelse(Reconstruction.MSE > 0.02, "Yes", "No"))
names(anomaly)[3] = c("actual")

anomaly %>%
  group_by(actual, predict) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) 


## ----Autoencoder_anomaly_result-------------------------------------------------------------------------------------
anomaly %>%
  ggplot(aes(x = actual, fill = predict)) +
    geom_bar() +
    theme_bw() +
    scale_fill_brewer(palette = "Set1") +
    facet_wrap( ~ actual, scales = "free", ncol = 2)



## ----Autoencoder_features-------------------------------------------------------------------------------------------
train_features <- h2o.deepfeatures(model_nn, train, layer = 2) %>%
  as.data.frame() %>%
  mutate(Class = as.vector(train[, 4]))

ggplot(train_features, aes(x = DF.L2.C1, y = DF.L2.C2, color = Class)) +
  geom_point(alpha = 0.1)


## ----Autoencoder_dimension------------------------------------------------------------------------------------------
train_features <- h2o.deepfeatures(model_nn, train, layer = 2) %>%
  as.data.frame() %>%
  mutate(Class = as.factor(as.vector(train[, 4]))) %>%
  as.h2o()

features_dim <- setdiff(colnames(train_features), response)

model_nn_dim <- h2o.deeplearning(y = "Class",
                               x = features_dim,
                               training_frame = train_features,
                               reproducible = TRUE,
                               balance_classes = TRUE,
                               ignore_const_cols = FALSE,
                               seed = 42,
                               hidden = c(10, 2, 10), 
                               epochs = 100,
                               activation = "Tanh")

test_dim <- h2o.deepfeatures(model_nn, test, layer = 2)

pred <- as.data.frame(h2o.predict(object = model_nn_dim, newdata = test_dim)) %>%
  mutate(actual = as.vector(test[, 4]))

pred %>%
  group_by(actual, predict) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) 


## ----Autoencoder_dimension_result-----------------------------------------------------------------------------------
pred %>%
  ggplot(aes(x = actual, fill = predict)) +
    geom_bar() +
    theme_bw() +
    scale_fill_brewer(palette = "Set1") +
    facet_wrap( ~ actual, scales = "free", ncol = 2)


## ----RandomForest---------------------------------------------------------------------------------------------------
model_rf <- h2o.randomForest(y = response,
                             x = features,
                             training_frame = train,
                             ntrees = 100,
                             max_depth = 15,
                             nbins_cats = 1115,
                             validation_frame = test)

pred <- as.data.frame(h2o.predict(object = model_rf, newdata = test)) %>%
  mutate(actual = as.vector(test[, 4]))

pred %>%
  group_by(actual, predict) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) 

pred %>%
  ggplot(aes(x = actual, fill = predict)) +
    geom_bar() +
    theme_bw() +
    scale_fill_brewer(palette = "Set1") +
    facet_wrap( ~ actual, scales = "free", ncol = 2)


## ----SVM------------------------------------------------------------------------------------------------------------
model_svm <- h2o.psvm(gamma = 0.01,
                      rank_ratio = 0.1,
                      y = response,
                      training_frame = train,
                      validation_frame = test,
                      seed = 42,
                      disable_training_metrics = FALSE)

pred <- as.data.frame(h2o.predict(object = model_svm, newdata = test)) %>%
  mutate(actual = as.vector(test[, 4]))

pred %>%
  group_by(actual, predict) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) 

pred %>%
  ggplot(aes(x = actual, fill = predict)) +
    geom_bar() +
    theme_bw() +
    scale_fill_brewer(palette = "Set1") +
    facet_wrap( ~ actual, scales = "free", ncol = 2)


## ----Kmeans---------------------------------------------------------------------------------------------------------
model_km <- h2o.kmeans(k = 2,
                      standardize = FALSE,
                      seed = 42,
                      x = features,
                      training_frame = train,
                      validation_frame = test)

pred <- as.data.frame(h2o.predict(object = model_km, newdata = test)) %>%
  mutate(actual = as.vector(test[, 4]))

pred %>%
  group_by(actual, predict) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) 


