# esqueleto de grid search
# se espera que los alumnos completen lo que falta
#   para recorrer TODOS cuatro los hiperparametros

rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection


## SOLO GRABO EL ARCHIVO PARA VERIFICAR QUE NO HAY PROBLEMAS ACA



# Aqui se debe poner la carpeta de la computadora local
setwd("~/buckets/b1/") # Establezco el Working Directory
# cargo los datos

# genero el archivo para Kaggle
# creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create("./exp/", showWarnings = FALSE)
dir.create("./exp/HT2020/", showWarnings = FALSE)
archivo_salida <- "./exp/HT2020/gridsearch_test.txt"

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
  "cp", "\n"
)


# itero por los loops anidados para cada hiperparametro

for (vmax_depth in c(8, 10)) {
  for (vmin_split in c(600, 800)) {
    for (min_bucket in c(200)) {
      for (cp in c(-0.5, -0.4)) {
        # notar como se agrega

        iteracion <- "== Iteracion =="
        print(iteracion)
        print(vmax_depth)
        print(vmin_split)
        print(min_bucket)
        print(cp)
        cat("\n")

       
        # escribo los resultados al archivo de salida
        cat(
          file = archivo_salida,
          append = TRUE,
          sep = "",
          vmax_depth, "\t",
          vmin_split, "\t",
          min_bucket, "\t",
          cp, "\n"
        )
      }
    }
  }
}
