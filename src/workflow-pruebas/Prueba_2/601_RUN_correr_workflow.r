# Corrida general del workflow

options(error = function() {
  traceback(20)
  options(error = NULL)
  stop("exiting after script error")
})


# corrida de cada paso del workflow

# primeros pasos, relativamente rapidos
source("~/labo2023ba/src/workflow-pruebas/Prueba_2/611_CA_reparar_dataset_002.r")
source("~/labo2023ba/src/workflow-pruebas/Prueba_2/621_DR_corregir_drifting_002.r")
source("~/labo2023ba/src/workflow-pruebas/Prueba_2/631_FE_historia_002.r")
source("~/labo2023ba/src/workflow-pruebas/Prueba_2/641_TS_training_strategy_002.r")

# ultimos pasos, muy lentos
source("~/labo2023ba/src/workflow-pruebas/Prueba_2/651_HT_lightgbm_002.r")
source("~/labo2023ba/src/workflow-pruebas/Prueba_2/661_ZZ_final_002.r")
