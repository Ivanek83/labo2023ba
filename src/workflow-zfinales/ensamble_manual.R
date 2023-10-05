
# Ensamble de resultados que corro manualmente en la PC Local. 


getwd()
setwd("/Users/ivankowalczuk/Dropbox/Maestria en ciencia de datos/Materias 2023/8- Laboratorio de implementacion I/Proyecto_test/predicado")

# Promedio de las probabilidades de baja+2
rm(list = ls())

library(dplyr)
# Cargar los datos desde los archivos TSV
archivo1 <- read.table("exp_p06_ZZ6610_pred_01_053_s124541.csv", header = TRUE, sep = "\t")
archivo2 <- read.table("exp_p06_ZZ6610_pred_01_053_s203663.csv", header = TRUE, sep = "\t")
archivo3 <- read.table("exp_p06_ZZ6610_pred_01_053_s365567.csv", header = TRUE, sep = "\t")
archivo4 <- read.table("exp_p06_ZZ6610_pred_01_053_s449437.csv", header = TRUE, sep = "\t")
archivo5 <- read.table("exp_p06_ZZ6610_pred_01_053_s565057.csv", header = TRUE, sep = "\t")
archivo6 <- read.table("exp_p06_ZZ6610_pred_01_053_s600037.csv", header = TRUE, sep = "\t")
archivo7 <- read.table("exp_p06_ZZ6610_pred_01_053_s600043.csv", header = TRUE, sep = "\t")
archivo8 <- read.table("exp_p06_ZZ6610_pred_01_053_s600073.csv", header = TRUE, sep = "\t")
archivo9 <- read.table("exp_p06_ZZ6610_pred_01_053_s600091.csv", header = TRUE, sep = "\t")
archivo10 <- read.table("exp_p06_ZZ6610_pred_01_053_s600101.csv", header = TRUE, sep = "\t")



# renombro las columnas que coinciden en todos los archivos
archivo1 <- archivo1 %>% rename(prob_1 = prob)
archivo2 <- archivo2 %>% rename(prob_2 = prob)
archivo3 <- archivo3 %>% rename(prob_3 = prob)
archivo4 <- archivo4 %>% rename(prob_4 = prob)
archivo5 <- archivo5 %>% rename(prob_5 = prob)
archivo6 <- archivo6 %>% rename(prob_6 = prob)
archivo7 <- archivo7 %>% rename(prob_7 = prob)
archivo8 <- archivo8 %>% rename(prob_8 = prob)
archivo9 <- archivo9 %>% rename(prob_9 = prob)
archivo10 <- archivo10 %>% rename(prob_10 = prob)




# agrupo las columnas de las probabilidades para calcular el promedio
promedio_probabilidades <- bind_rows(archivo1, archivo2, archivo3, archivo4, archivo5, archivo6, archivo7, archivo8, archivo9, archivo10) %>%
  group_by(numero_de_cliente, foto_mes) %>%
  summarise(promedio_prob = mean(c(prob_1, prob_2, prob_3, prob_4, prob_5, prob_6, prob_7, prob_8, prob_9, prob_10), na.rm = TRUE))

promedio_probabilidades <- promedio_probabilidades %>% rename(prob = promedio_prob)

#ordeno todo el dataset por la probabilidad descendente
data <- promedio_probabilidades %>%
  arrange(desc(prob))

# ahora quiero agregar una nueva columna llamada clase ternaria 
# donde a los primeros 9000 registros le asigno 1 y al resto 0
data$Predicted <- NA


# Armo la cantidad de manera manual hasta encontrar la meseta


# aca va lo que quiero
data$Predicted[1:7400] <- 1
data$Predicted[7401:nrow(data)] <- 0

nuevo_data <- data[, c("numero_de_cliente", "Predicted")]

# Escribir el archivo CSV
write.csv(nuevo_data, file = "p06_resultados_promedio_7400.csv", row.names = FALSE)

# promediado <- read.csv("resultados_promedio.csv")



