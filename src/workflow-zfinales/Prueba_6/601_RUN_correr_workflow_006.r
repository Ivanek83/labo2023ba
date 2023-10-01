# labo2023ba$ git pull origin --rebase
# Corrida general del workflow

options(error = function() {
  traceback(20)
  options(error = NULL)
  stop("exiting after script error")
})


# Variables globales
archivo_tiempo <- "Tiempo_Script_006.txt"
carpeta_prueba <- "p06_RUN6010"
# Prueba_6
# _006


# creo la carpeta donde va el experimento
#dir.create("~/buckets/b1/exp/p05_RUN6010/", showWarnings = FALSE)
dir.create(paste0("~/buckets/b1/exp/", carpeta_prueba, "/"), showWarnings = FALSE)


# Establezco el Working Directory DEL EXPERIMENTO
#setwd("~/buckets/b1/exp/p05_RUN6010/")
setwd(paste0("~/buckets/b1/exp/", carpeta_prueba, "/"))


# corrida de cada paso del workflow
# Registra la hora de inicio
hora_inicio <- Sys.time()

cat("------------------------------------------\n", file = archivo_tiempo, append = TRUE)
cat("Inicia workflow-zfinales -> Prueba_6\n", file = archivo_tiempo, append = TRUE)
cat("hora_inicio GMT: ", hora_inicio, "\n", file = archivo_tiempo, append = TRUE)

# primeros pasos, relativamente rapidos

cat("\nScript 611_CA - Hora de inicio GMT: ", strftime(Sys.time(), format = "%Y-%m-%d %H:%M:%S"), "\n", file = archivo_tiempo, append = TRUE)
source("~/labo2023ba/src/workflow-zfinales/Prueba_6/611_CA_reparar_dataset_006.r")

setwd(paste0("~/buckets/b1/exp/", carpeta_prueba, "/"))
cat("\nScript 621_DR - Hora de inicio GMT: ", strftime(Sys.time(), format = "%Y-%m-%d %H:%M:%S"), "\n", file = archivo_tiempo, append = TRUE)
source("~/labo2023ba/src/workflow-zfinales/Prueba_6/621_DR_corregir_drifting_006.r")


comment <- '
setwd(paste0("~/buckets/b1/exp/", carpeta_prueba, "/"))
cat("\nScript 631_FE - Hora de inicio GMT: ", strftime(Sys.time(), format = "%Y-%m-%d %H:%M:%S"), "\n", file = archivo_tiempo, append = TRUE)
source("~/labo2023ba/src/workflow-zfinales/Prueba_6/631_FE_historia_006.r")

setwd(paste0("~/buckets/b1/exp/", carpeta_prueba, "/"))
cat("\nScript 641_TS - Hora de inicio GMT: ", strftime(Sys.time(), format = "%Y-%m-%d %H:%M:%S"), "\n", file = archivo_tiempo, append = TRUE)
source("~/labo2023ba/src/workflow-zfinales/Prueba_6/641_TS_training_strategy_006.r")


# ultimos pasos, muy lentos

setwd(paste0("~/buckets/b1/exp/", carpeta_prueba, "/"))
cat("\nScript: 651_HT - Hora de inicio GMT: ", strftime(Sys.time(), format = "%Y-%m-%d %H:%M:%S"), "\n", file = archivo_tiempo, append = TRUE)
source("~/labo2023ba/src/workflow-zfinales/Prueba_6/651_HT_lightgbm_006.r")

setwd(paste0("~/buckets/b1/exp/", carpeta_prueba, "/"))
cat("\nScript 661_ZZ - Hora de inicio GMT: ", strftime(Sys.time(), format = "%Y-%m-%d %H:%M:%S"), "\n", file = archivo_tiempo, append = TRUE)
source("~/labo2023ba/src/workflow-zfinales/Prueba_6/661_ZZ_final_006.r")

'



setwd(paste0("~/buckets/b1/exp/", carpeta_prueba, "/"))
# Registra la hora de finalización
hora_fin <- Sys.time()
cat("\n\nhora_fin GMT: ", hora_fin, file = archivo_tiempo, append = TRUE)


# Calcula la diferencia de tiempo en segundos
diferencia_segundos <- as.numeric(difftime(hora_fin, hora_inicio, units = "secs"))

# Convierte la diferencia de tiempo a días, horas, minutos y segundos
dias <- floor(diferencia_segundos / (3600 * 24))
horas <- floor((diferencia_segundos %% (3600 * 24)) / 3600)
minutos <- floor((diferencia_segundos %% 3600) / 60)
segundos <- diferencia_segundos %% 60

# Muestra el tiempo transcurrido
cat("\n\nTiempo transcurrido workflow-zfinales -> Prueba_6:", dias, "días,", horas, "horas,", minutos, "minutos,", segundos, "segundos\n", file = archivo_tiempo, append = TRUE)
