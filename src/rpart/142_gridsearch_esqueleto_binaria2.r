# esqueleto de grid search
# se espera que los alumnos completen lo que falta
#   para recorrer TODOS cuatro los hiperparametros

rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection

require("data.table")
require("rpart")
require("parallel")

PARAM <- list()
# reemplazar por las propias semillas
PARAM$semillas <- c(124541, 203663, 365567, 449437, 565057)

#------------------------------------------------------------------------------
# particionar agrega una columna llamada fold a un dataset
#  que consiste en una particion estratificada segun agrupa
# particionar( data=dataset, division=c(70,30), agrupa=clase_binaria, seed=semilla)
#   crea una particion 70, 30

particionar <- function(data, division, agrupa = "", campo = "fold", start = 1, seed = NA) {
  if (!is.na(seed)) set.seed(seed)
  
  bloque <- unlist(mapply(function(x, y) {
    rep(y, x)
  }, division, seq(from = start, length.out = length(division))))
  
  data[, (campo) := sample(rep(bloque, ceiling(.N / length(bloque))))[1:.N], # nolint
       by = agrupa
  ]
}
#------------------------------------------------------------------------------

ArbolEstimarGanancia <- function(semilla, param_basicos) { # nolint
  # particiono estratificadamente el dataset
  particionar(dataset, division = c(7, 3), agrupa = "clase_binaria", seed = semilla) # nolint: line_length_linter.
  
  # genero el modelo
  # quiero predecir clase_binaria a partir del resto
  modelo <- rpart("clase_binaria ~ .",
                  data = dataset[fold == 1], # fold==1  es training,  el 70% de los datos
                  xval = 0,
                  control = param_basicos
  ) # aqui van los parametros del arbol
  
  # aplico el modelo a los datos de testing
  prediccion <- predict(modelo, # el modelo que genere recien
                        dataset[fold == 2], # fold==2  es testing, el 30% de los datos
                        type = "prob"
  ) # type= "prob"  es que devuelva la probabilidad
  
  # prediccion es una matriz con DOS columnas,
  #  llamadas "pos", y "neg"
  # cada columna es el vector de probabilidades
  
  
  # calculo la ganancia en testing  qu es fold==2
  ganancia_test <- dataset[
    fold == 2,
    sum(ifelse(prediccion[, "pos"] > 0.025,
               ifelse(clase_binaria == "pos", 117000, -3000),
               0
    ))
  ]
  
  # escalo la ganancia como si fuera todo el dataset
  ganancia_test_normalizada <- ganancia_test / 0.3
  
  return(ganancia_test_normalizada)
}
#------------------------------------------------------------------------------

ArbolesMontecarlo <- function(semillas, param_basicos) { 
  # la funcion mcmapply  llama a la funcion ArbolEstimarGanancia
  #  tantas veces como valores tenga el vector  ksemillas
  ganancias <- mcmapply(ArbolEstimarGanancia,
                        semillas, # paso el vector de semillas
                        MoreArgs = list(param_basicos), # aqui paso el segundo parametro
                        SIMPLIFY = FALSE,
                        mc.cores = 1
  ) # se puede subir a 5 si posee Linux o Mac OS
  
  ganancia_promedio <- mean(unlist(ganancias))
  
  return(ganancia_promedio)
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# Aqui se debe poner la carpeta de la computadora local

setwd("~/buckets/b1/") # Establezco el Working Directory


# cargo los datos
dataset <- fread("./datasets/dataset_pequeno.csv")

# trabajo solo con los datos con clase, es decir 202107
dataset <- dataset[clase_ternaria != ""]

# chequeo la cant de cada variable
dataset[clase_ternaria == "BAJA+1", .N]
dataset[clase_ternaria == "BAJA+2", .N]
dataset[clase_ternaria == "CONTINUA", .N]

# Modifico la columna clase_ternaria para que sea binaria y le cambio el nombre: 
setnames(dataset, old = "clase_ternaria", new = "clase_binaria")
dataset[clase_binaria == "BAJA+1", clase_binaria := "neg"]
dataset[clase_binaria == "CONTINUA", clase_binaria := "neg"]
dataset[clase_binaria == "BAJA+2", clase_binaria := "pos"]


# chequeo la cant de cada variable
dataset[clase_binaria == "neg", .N]
dataset[clase_binaria == "pos", .N]




# genero el archivo para Kaggle
# creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create("./exp/", showWarnings = FALSE)
dir.create("./exp/HT2020/", showWarnings = FALSE)
archivo_salida <- "./exp/HT2020/gridsearch_binario.txt"

# Escribo los titulos al archivo donde van a quedar los resultados
# atencion que si ya existe el archivo, esta instruccion LO SOBREESCRIBE,
#  y lo que estaba antes se pierde
# la forma que no suceda lo anterior es con append=TRUE
cat(
  file = archivo_salida,
  sep = "",
  "max_depth", "\t",
  "min_split", "\t",
  "min_bucket", "\t",
  "cp", "\t",
  "ganancia_promedio", "\n"
)


# itero por los loops anidados para cada hiperparametro

for (vmax_depth in c(6, 7, 8, 9, 10)) {
  for (vmin_split in c(400, 450, 500, 550, 600, 650, 700, 750, 800)) {
    for (min_bucket in c(100, 150, 200, 250, 300, 350, 400)) {
      for (cp in seq(-1, 0.1, by = 0.1)) {
        
        print("Iteracion")
        print(vmax_depth)
        print(vmin_split)
        print(min_bucket)
        print(cp)
        cat("\n")
        
        
        param_basicos <- list(
          "cp" = cp,
          "minsplit" = vmin_split,
          "minbucket" = min_bucket,
          "maxdepth" = vmax_depth
        )
        
        # Un solo llamado, con la semilla 17
        ganancia_promedio <- ArbolesMontecarlo(PARAM$semillas, param_basicos)
        
        # escribo los resultados al archivo de salida
        cat(
          file = archivo_salida,
          append = TRUE,
          sep = "",
          vmax_depth, "\t",
          vmin_split, "\t",
          min_bucket, "\t",
          cp, "\t",
          ganancia_promedio, "\n"
        )
      }
    }
  }
}
