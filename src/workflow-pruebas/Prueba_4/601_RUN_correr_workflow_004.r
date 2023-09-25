# labo2023ba$ git pull origin --rebase
# Corrida general del workflow

options(error = function() {
  traceback(20)
  options(error = NULL)
  stop("exiting after script error")
})


# corrida de cada paso del workflow

# Registra la hora de inicio
hora_inicio <- Sys.time()

cat("\n Inicia Script 003\n\n", file = "Tiempo_Script.txt", append = TRUE)
cat("hora_inicio GMT0: ", hora_inicio, "\n", file = "Tiempo_Script.txt", append = TRUE)

# primeros pasos, relativamente rapidos

cat("\n\n Inicia prueba 003: 611 - Hora de inicio: ", strftime(Sys.time(), format = "%Y-%m-%d %H:%M:%S"), "\n", file = "Tiempo_Script.txt", append = TRUE)
source("~/labo2023ba/src/workflow-pruebas/Prueba_3/611_CA_reparar_dataset_003.r")

cat("\n\n Inicia prueba 003: 621 - Hora de inicio: ", strftime(Sys.time(), format = "%Y-%m-%d %H:%M:%S"), "\n", file = "Tiempo_Script.txt", append = TRUE)
source("~/labo2023ba/src/workflow-pruebas/Prueba_3/621_DR_corregir_drifting_003.r")

cat("\n\n Inicia prueba 003: 631 - Hora de inicio: ", strftime(Sys.time(), format = "%Y-%m-%d %H:%M:%S"), "\n", file = "Tiempo_Script.txt", append = TRUE)
source("~/labo2023ba/src/workflow-pruebas/Prueba_3/631_FE_historia_003.r")

cat("\n\n Inicia prueba 003: 641 - Hora de inicio: ", strftime(Sys.time(), format = "%Y-%m-%d %H:%M:%S"), "\n", file = "Tiempo_Script.txt", append = TRUE)
source("~/labo2023ba/src/workflow-pruebas/Prueba_3/641_TS_training_strategy_003.r")


# ultimos pasos, muy lentos

cat("\n\n Inicia prueba 003: 651 - Hora de inicio: ", strftime(Sys.time(), format = "%Y-%m-%d %H:%M:%S"), "\n", file = "Tiempo_Script.txt", append = TRUE)
source("~/labo2023ba/src/workflow-pruebas/Prueba_3/651_HT_lightgbm_003.r")

cat("\n\n Inicia prueba 003: 661 - Hora de inicio: ", strftime(Sys.time(), format = "%Y-%m-%d %H:%M:%S"), "\n", file = "Tiempo_Script.txt", append = TRUE)
source("~/labo2023ba/src/workflow-pruebas/Prueba_3/661_ZZ_final_003.r")





# Registra la hora de finalización
hora_fin <- Sys.time()
cat("hora_fin GMT0: ", hora_fin, "\n", file = "Tiempo_Script.txt", append = TRUE)

# Calcula la diferencia de tiempo en segundos
diferencia_segundos <- as.numeric(difftime(hora_fin, hora_inicio, units = "secs"))

# Convierte la diferencia de tiempo a días, horas, minutos y segundos
dias <- floor(diferencia_segundos / (3600 * 24))
horas <- floor((diferencia_segundos %% (3600 * 24)) / 3600)
minutos <- floor((diferencia_segundos %% 3600) / 60)
segundos <- diferencia_segundos %% 60

# Muestra el tiempo transcurrido
cat("Tiempo transcurrido:", dias, "días,", horas, "horas,", minutos, "minutos,", segundos, "segundos\n", file = "Tiempo_Script.txt", append = TRUE)


