# labo2023ba$ git pull origin --rebase
# Corrida general del workflow

options(error = function() {
  traceback(20)
  options(error = NULL)
  stop("exiting after script error")
})



# creo la carpeta donde va el experimento
dir.create("~/buckets/b1/exp/p05_RUN6010/", showWarnings = FALSE)
# Establezco el Working Directory DEL EXPERIMENTO
setwd("~/buckets/b1/exp/p05_RUN6010/")



# corrida de cada paso del workflow
# Registra la hora de inicio
hora_inicio <- Sys.time()

cat("------------------------------------------\n", file = "Tiempo_Script_005.txt", append = TRUE)
cat("Inicia workflow-pruebas -> Prueba_5\n", file = "Tiempo_Script_005.txt", append = TRUE)
cat("hora_inicio GMT: ", hora_inicio, "\n", file = "Tiempo_Script_005.txt", append = TRUE)

# primeros pasos, relativamente rapidos

cat("\nScript 611_CA - Hora de inicio GMT: ", strftime(Sys.time(), format = "%Y-%m-%d %H:%M:%S"), "\n", file = "Tiempo_Script_005.txt", append = TRUE)
source("~/labo2023ba/src/workflow-pruebas/Prueba_5/611_CA_reparar_dataset_005.r")

setwd("~/buckets/b1/exp/p05_RUN6010/")
cat("\nScript 621_DR - Hora de inicio GMT: ", strftime(Sys.time(), format = "%Y-%m-%d %H:%M:%S"), "\n", file = "Tiempo_Script_005.txt", append = TRUE)
source("~/labo2023ba/src/workflow-pruebas/Prueba_5/621_DR_corregir_drifting_005.r")

setwd("~/buckets/b1/exp/p05_RUN6010/")
cat("\nScript 631_FE - Hora de inicio GMT: ", strftime(Sys.time(), format = "%Y-%m-%d %H:%M:%S"), "\n", file = "Tiempo_Script_005.txt", append = TRUE)
source("~/labo2023ba/src/workflow-pruebas/Prueba_5/631_FE_historia_005.r")

setwd("~/buckets/b1/exp/p05_RUN6010/")
cat("\nScript 641_TS - Hora de inicio GMT: ", strftime(Sys.time(), format = "%Y-%m-%d %H:%M:%S"), "\n", file = "Tiempo_Script_005.txt", append = TRUE)
source("~/labo2023ba/src/workflow-pruebas/Prueba_5/641_TS_training_strategy_005.r")


# ultimos pasos, muy lentos

setwd("~/buckets/b1/exp/p05_RUN6010/")
cat("\nScript: 651_HT - Hora de inicio GMT: ", strftime(Sys.time(), format = "%Y-%m-%d %H:%M:%S"), "\n", file = "Tiempo_Script_005.txt", append = TRUE)
source("~/labo2023ba/src/workflow-pruebas/Prueba_5/651_HT_lightgbm_005.r")

setwd("~/buckets/b1/exp/p05_RUN6010/")
cat("\nScript 661_ZZ - Hora de inicio GMT: ", strftime(Sys.time(), format = "%Y-%m-%d %H:%M:%S"), "\n", file = "Tiempo_Script_005.txt", append = TRUE)
source("~/labo2023ba/src/workflow-pruebas/Prueba_5/661_ZZ_final_005.r")





setwd("~/buckets/b1/exp/p05_RUN6010/")
# Registra la hora de finalización
hora_fin <- Sys.time()
cat("\n\nhora_fin GMT: ", hora_fin, file = "Tiempo_Script_005.txt", append = TRUE)

comment <- '
# Calcula la diferencia de tiempo en segundos
diferencia_segundos <- as.numeric(difftime(hora_fin, hora_inicio, units = "secs"))

# Convierte la diferencia de tiempo a días, horas, minutos y segundos
dias <- floor(diferencia_segundos / (3600 * 24))
horas <- floor((diferencia_segundos %% (3600 * 24)) / 3600)
minutos <- floor((diferencia_segundos %% 3600) / 60)
segundos <- diferencia_segundos %% 60

# Muestra el tiempo transcurrido
cat("\n\nTiempo transcurrido workflow-pruebas -> Prueba_5:", dias, "días,", horas, "horas,", minutos, "minutos,", segundos, "segundos\n", file = "Tiempo_Script_005.txt", append = TRUE)
'