# Corrida general del workflow

options(error = function() {
  traceback(20)
  options(error = NULL)
  stop("exiting after script error")
})


# corrida de cada paso del workflow

# primeros pasos, relativamente rapidos

cat("Inicia prueba 003: 611 - Hora de inicio: ", strftime(Sys.time(), format = "%Y-%m-%d %H:%M:%S"), "\n")
source("~/labo2023ba/src/workflow-pruebas/Prueba_3/611_CA_reparar_dataset_003.r")

cat("Inicia prueba 003: 621 - Hora de inicio: ", strftime(Sys.time(), format = "%Y-%m-%d %H:%M:%S"), "\n")
source("~/labo2023ba/src/workflow-pruebas/Prueba_3/621_DR_corregir_drifting_003.r")

cat("Inicia prueba 003: 631 - Hora de inicio: ", strftime(Sys.time(), format = "%Y-%m-%d %H:%M:%S"), "\n")
source("~/labo2023ba/src/workflow-pruebas/Prueba_3/631_FE_historia_003.r")

cat("Inicia prueba 003: 641 - Hora de inicio: ", strftime(Sys.time(), format = "%Y-%m-%d %H:%M:%S"), "\n")
source("~/labo2023ba/src/workflow-pruebas/Prueba_3/641_TS_training_strategy_003.r")


# ultimos pasos, muy lentos

cat("Inicia prueba 003: 651 - Hora de inicio: ", strftime(Sys.time(), format = "%Y-%m-%d %H:%M:%S"), "\n")
source("~/labo2023ba/src/workflow-pruebas/Prueba_3/651_HT_lightgbm_003.r")

cat("Inicia prueba 003: 661 - Hora de inicio: ", strftime(Sys.time(), format = "%Y-%m-%d %H:%M:%S"), "\n")
source("~/labo2023ba/src/workflow-pruebas/Prueba_3/661_ZZ_final_003.r")
