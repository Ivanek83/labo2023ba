# Experimentos Colaborativos Default
# Workflow  Data Drifting repair

# limpio la memoria
rm(list = ls(all.names = TRUE)) # remove all objects
gc(full = TRUE) # garbage collection

require("data.table")
require("yaml")


# Parametros del script
PARAM <- list()
PARAM$experimento <- "p02_DR6210"

PARAM$exp_input <- "p02_CA6110"

PARAM$variables_intrames <- TRUE # atencion esto esta en TRUE

# valores posibles
#  "ninguno", "rank_simple", "rank_cero_fijo", "deflacion", "estandarizar"
PARAM$metodo <- "rank_cero_fijo"

PARAM$home <- "~/buckets/b1/"
# FIN Parametros del script

OUTPUT <- list()

#------------------------------------------------------------------------------

options(error = function() {
  traceback(20)
  options(error = NULL)
  stop("exiting after script error")
})
#------------------------------------------------------------------------------

GrabarOutput <- function() {
  write_yaml(OUTPUT, file = "output.yml") # grabo output
}
#------------------------------------------------------------------------------
# Esta es la parte que los alumnos deben desplegar todo su ingenio
# Agregar aqui sus PROPIAS VARIABLES manuales

AgregarVariables_IntraMes <- function(dataset) {
  gc()
  # INICIO de la seccion donde se deben hacer cambios con variables nuevas

  # creo un ctr_quarter que tenga en cuenta cuando
  # los clientes hace 3 menos meses que estan
  dataset[, ctrx_quarter_normalizado := ctrx_quarter]
  dataset[cliente_antiguedad == 1, ctrx_quarter_normalizado := ctrx_quarter * 5]
  dataset[cliente_antiguedad == 2, ctrx_quarter_normalizado := ctrx_quarter * 2]
  dataset[
    cliente_antiguedad == 3,
    ctrx_quarter_normalizado := ctrx_quarter * 1.2
  ]

  # variable extraida de una tesis de maestria de Irlanda
  dataset[, mpayroll_sobre_edad := mpayroll / cliente_edad]

  # se crean los nuevos campos para MasterCard  y Visa,
  #  teniendo en cuenta los NA's
  # varias formas de combinar Visa_status y Master_status
  dataset[, vm_status01 := pmax(Master_status, Visa_status, na.rm = TRUE)]
  dataset[, vm_status02 := Master_status + Visa_status]

  dataset[, vm_status03 := pmax(
    ifelse(is.na(Master_status), 10, Master_status),
    ifelse(is.na(Visa_status), 10, Visa_status)
  )]

  dataset[, vm_status04 := ifelse(is.na(Master_status), 10, Master_status)
    + ifelse(is.na(Visa_status), 10, Visa_status)]

  dataset[, vm_status05 := ifelse(is.na(Master_status), 10, Master_status)
    + 100 * ifelse(is.na(Visa_status), 10, Visa_status)]

  dataset[, vm_status06 := ifelse(is.na(Visa_status),
    ifelse(is.na(Master_status), 10, Master_status),
    Visa_status
  )]

  dataset[, mv_status07 := ifelse(is.na(Master_status),
    ifelse(is.na(Visa_status), 10, Visa_status),
    Master_status
  )]


  # combino MasterCard y Visa
  dataset[, vm_mfinanciacion_limite := rowSums(cbind(Master_mfinanciacion_limite, Visa_mfinanciacion_limite), na.rm = TRUE)]

  dataset[, vm_Fvencimiento := pmin(Master_Fvencimiento, Visa_Fvencimiento, na.rm = TRUE)]
  dataset[, vm_Finiciomora := pmin(Master_Finiciomora, Visa_Finiciomora, na.rm = TRUE)]
  dataset[, vm_msaldototal := rowSums(cbind(Master_msaldototal, Visa_msaldototal), na.rm = TRUE)]
  dataset[, vm_msaldopesos := rowSums(cbind(Master_msaldopesos, Visa_msaldopesos), na.rm = TRUE)]
  dataset[, vm_msaldodolares := rowSums(cbind(Master_msaldodolares, Visa_msaldodolares), na.rm = TRUE)]
  dataset[, vm_mconsumospesos := rowSums(cbind(Master_mconsumospesos, Visa_mconsumospesos), na.rm = TRUE)]
  dataset[, vm_mconsumosdolares := rowSums(cbind(Master_mconsumosdolares, Visa_mconsumosdolares), na.rm = TRUE)]
  dataset[, vm_mlimitecompra := rowSums(cbind(Master_mlimitecompra, Visa_mlimitecompra), na.rm = TRUE)]
  dataset[, vm_madelantopesos := rowSums(cbind(Master_madelantopesos, Visa_madelantopesos), na.rm = TRUE)]
  dataset[, vm_madelantodolares := rowSums(cbind(Master_madelantodolares, Visa_madelantodolares), na.rm = TRUE)]
  dataset[, vm_fultimo_cierre := pmax(Master_fultimo_cierre, Visa_fultimo_cierre, na.rm = TRUE)]
  dataset[, vm_mpagado := rowSums(cbind(Master_mpagado, Visa_mpagado), na.rm = TRUE)]
  dataset[, vm_mpagospesos := rowSums(cbind(Master_mpagospesos, Visa_mpagospesos), na.rm = TRUE)]
  dataset[, vm_mpagosdolares := rowSums(cbind(Master_mpagosdolares, Visa_mpagosdolares), na.rm = TRUE)]
  dataset[, vm_fechaalta := pmax(Master_fechaalta, Visa_fechaalta, na.rm = TRUE)]
  dataset[, vm_mconsumototal := rowSums(cbind(Master_mconsumototal, Visa_mconsumototal), na.rm = TRUE)]
  dataset[, vm_cconsumos := rowSums(cbind(Master_cconsumos, Visa_cconsumos), na.rm = TRUE)]
  dataset[, vm_cadelantosefectivo := rowSums(cbind(Master_cadelantosefectivo, Visa_cadelantosefectivo), na.rm = TRUE)]
  dataset[, vm_mpagominimo := rowSums(cbind(Master_mpagominimo, Visa_mpagominimo), na.rm = TRUE)]

  # a partir de aqui juego con la suma de Mastercard y Visa
  dataset[, vmr_Master_mlimitecompra := Master_mlimitecompra / vm_mlimitecompra]
  dataset[, vmr_Visa_mlimitecompra := Visa_mlimitecompra / vm_mlimitecompra]
  dataset[, vmr_msaldototal := vm_msaldototal / vm_mlimitecompra]
  dataset[, vmr_msaldopesos := vm_msaldopesos / vm_mlimitecompra]
  dataset[, vmr_msaldopesos2 := vm_msaldopesos / vm_msaldototal]
  dataset[, vmr_msaldodolares := vm_msaldodolares / vm_mlimitecompra]
  dataset[, vmr_msaldodolares2 := vm_msaldodolares / vm_msaldototal]
  dataset[, vmr_mconsumospesos := vm_mconsumospesos / vm_mlimitecompra]
  dataset[, vmr_mconsumosdolares := vm_mconsumosdolares / vm_mlimitecompra]
  dataset[, vmr_madelantopesos := vm_madelantopesos / vm_mlimitecompra]
  dataset[, vmr_madelantodolares := vm_madelantodolares / vm_mlimitecompra]
  dataset[, vmr_mpagado := vm_mpagado / vm_mlimitecompra]
  dataset[, vmr_mpagospesos := vm_mpagospesos / vm_mlimitecompra]
  dataset[, vmr_mpagosdolares := vm_mpagosdolares / vm_mlimitecompra]
  dataset[, vmr_mconsumototal := vm_mconsumototal / vm_mlimitecompra]
  dataset[, vmr_mpagominimo := vm_mpagominimo / vm_mlimitecompra]

  # Aqui debe usted agregar sus propias nuevas variables



  #1)-------------------------------------------------------------
  # Sumo todas las cantidades de transacciones y las rankeo. 

  dataset[, X1_Suma_cantytrans := ctarjeta_debito_transacciones + ctarjeta_visa_transacciones + ctarjeta_master_transacciones
  + cpayroll2_trx + ccuenta_debitos_automaticos + cforex + cforex_buy + cforex_sell + ccallcenter_transacciones
  + chomebanking_transacciones + ccajas_transacciones + ccajas_otras + catm_trx + catm_trx_other + ctrx_quarter + cmobile_app_trx]


  # Calculo los deciles de la variable Suma_cantidades y creo una nueva columna
  dataset[, X1_Suma_cantytrans_decil := frankv(X1_Suma_cantytrans, ties.method = "dense", na.last = "keep"), by=foto_mes]

  # Elimino la columna Suma_cantidades
  #dataset[, X1_Suma_cantytrans := NULL]

  commment <- '

  #2)-------------------------------------------------------------
  # Sumo todas las variables segun el movimiento de dinero

  dataset[, X2_balance := (mpayroll + mpayroll2 + mcajeros_propios_descuentos + mtarjeta_visa_descuentos + mtarjeta_master_descuentos 
  + mtransferencias_recibidas + mcheques_depositados + mcheques_emitidos) -   
  
  (mautoservicio + mtarjeta_visa_consumo + mtarjeta_master_consumo + mprestamos_personales + mprestamos_prendarios 
  + mprestamos_hipotecarios + mcuenta_debitos_automaticos + mtarjeta_visa_debitos_automaticos + mttarjeta_master_debitos_automaticos 
  + mpagodeservicios + mpagomiscuentas + mcomisiones_mantenimiento + mcomisiones_otras + mtransferencias_emitidas 
  + mextraccion_autoservicio + Master_mconsumospesos + Master_mconsumosdolares + Master_madelantopesos + Master_madelantodolares 
  + Master_mpagado + Master_mpagospesos + Master_mpagosdolares + Master_mconsumototal + Visa_mconsumospesos + Visa_mconsumosdolares 
  + Visa_madelantopesos + Visa_madelantodolares + Visa_mpagado + Visa_mpagospesos + Visa_mpagosdolares + Visa_mconsumototal)]


  #3)-------------------------------------------------------------
  # Sumo todas las cantidades de transacciones y las rankeo. 

  dataset[, X3_Suma_cantidades := cproductos + ccuenta_corriente + ccaja_ahorro + ctarjeta_debito + ctarjeta_debito_transacciones 
  + ctarjeta_visa + ctarjeta_visa_transacciones + ctarjeta_master + ctarjeta_master_transacciones + cprestamos_personales 
  + cprestamos_prendarios + cprestamos_hipotecarios + cplazo_fijo + cinversion1 + cinversion2 + cseguro_vida + cseguro_auto 
  + cseguro_vivienda + cseguro_accidentes_personales + cpayroll_trx + cpayroll2_trx + ccuenta_debitos_automaticos 
  + ctarjeta_visa_debitos_automaticos + ctarjeta_master_debitos_automaticos + cpagodeservicios + cpagomiscuentas 
  + ccajeros_propios_descuentos + ctarjeta_visa_descuentos + ctarjeta_master_descuentos + ccomisiones_mantenimiento 
  + ccomisiones_otras + cforex + cforex_buy + cforex_sell + ctransferencias_recibidas + ctransferencias_emitidas 
  + cextraccion_autoservicio + ccheques_depositados + ccheques_emitidos + ccheques_depositados_rechazados 
  + ccheques_emitidos_rechazados + ccallcenter_transacciones + chomebanking_transacciones + ccajas_transacciones + ccajas_consultas 
  + ccajas_depositos + ccajas_extracciones + ccajas_otras + catm_trx + catm_trx_other + ctrx_quarter + cmobile_app_trx 
  + Master_cconsumos + Master_cadelantosefectivo + Visa_cconsumos + Visa_cadelantosefectivo]


  # Calculo los deciles de la variable Suma_cantidades y creo una nueva columna
  dataset[, X3_Suma_cantidades_decil := frank(X3_Suma_cantidades, ties.method = "first", n.ties = 10), by=foto_mes]

  # Elimino la columna Suma_cantidades
  #dataset[, X3_Suma_cantidades := NULL]


  #4)-------------------------------------------------------------
  # Calculo la relacion entre la edad y la antiguedad del cliente

  dataset[, X4_edad_antiguedad := cliente_antiguedad / (cliente_edad * 12)]
  dataset[, X4_cliente_edad_decil := frank(cliente_edad, ties.method = "first", n.ties = 10), by=foto_mes]
  dataset[, X4_cliente_antiguedad_decil := frank(cliente_antiguedad, ties.method = "first", n.ties = 10), by=foto_mes]

  #5)-------------------------------------------------------------
  # Hago un rankeo de las variables de montos y saldos totales: 
  #dataset[,, by=foto_mes, .SDcols= ]
  dataset[, X5_mcomisiones_decil := frank(mcomisiones, ties.method = "first", n.ties = 10), by=foto_mes]
  dataset[, X5_mactivos_margen_decil := frank(mactivos_margen, ties.method = "first", n.ties = 10), by=foto_mes]
  dataset[, X5_mpasivos_margen_decil := frank(mpasivos_margen, ties.method = "first", n.ties = 10), by=foto_mes]
  dataset[, X5_mcuenta_corriente_adicional_decil := frank(mcuenta_corriente_adicional, ties.method = "first", n.ties = 10), by=foto_mes]
  dataset[, X5_mcuenta_corriente_decil := frank(mcuenta_corriente, ties.method = "first", n.ties = 10), by=foto_mes]
  dataset[, X5_mcaja_ahorro_decil := frank(mcaja_ahorro, ties.method = "first", n.ties = 10), by=foto_mes]
  dataset[, X5_mcaja_ahorro_adicional_decil := frank(mcaja_ahorro_adicional, ties.method = "first", n.ties = 10), by=foto_mes]
  dataset[, X5_mcaja_ahorro_dolares_decil := frank(mcaja_ahorro_dolares, ties.method = "first", n.ties = 10), by=foto_mes]
  dataset[, X5_mcuentas_saldo_decil := frank(mcuentas_saldo, ties.method = "first", n.ties = 10), by=foto_mes]
  dataset[, X5_mautoservicio_decil := frank(mautoservicio, ties.method = "first", n.ties = 10), by=foto_mes]
  dataset[, X5_mtarjeta_visa_consumo_decil := frank(mtarjeta_visa_consumo, ties.method = "first", n.ties = 10), by=foto_mes]
  dataset[, X5_mtarjeta_master_consumo_decil := frank(mtarjeta_master_consumo, ties.method = "first", n.ties = 10), by=foto_mes]
  dataset[, X5_mprestamos_personales_decil := frank(mprestamos_personales, ties.method = "first", n.ties = 10), by=foto_mes]
  dataset[, X5_mprestamos_prendarios_decil := frank(mprestamos_prendarios, ties.method = "first", n.ties = 10), by=foto_mes]
  dataset[, X5_mprestamos_hipotecarios_decil := frank(mprestamos_hipotecarios, ties.method = "first", n.ties = 10), by=foto_mes]
  dataset[, X5_mplazo_fijo_dolares_decil := frank(mplazo_fijo_dolares, ties.method = "first", n.ties = 10), by=foto_mes]
  dataset[, X5_mplazo_fijo_pesos_decil := frank(mplazo_fijo_pesos, ties.method = "first", n.ties = 10), by=foto_mes]
  dataset[, X5_minversion1_pesos_decil := frank(minversion1_pesos, ties.method = "first", n.ties = 10), by=foto_mes]
  dataset[, X5_minversion1_dolares_decil := frank(minversion1_dolares, ties.method = "first", n.ties = 10), by=foto_mes]
  dataset[, X5_minversion2_decil := frank(minversion2, ties.method = "first", n.ties = 10), by=foto_mes]
  dataset[, X5_mpayroll_decil := frank(mpayroll, ties.method = "first", n.ties = 10), by=foto_mes]
  dataset[, X5_mpayroll2_decil := frank(mpayroll2, ties.method = "first", n.ties = 10), by=foto_mes]
  dataset[, X5_mcuenta_debitos_automaticos_decil := frank(mcuenta_debitos_automaticos, ties.method = "first", n.ties = 10), by=foto_mes]
  dataset[, X5_mtarjeta_visa_debitos_automaticos_decil := frank(mtarjeta_visa_debitos_automaticos, ties.method = "first", n.ties = 10), by=foto_mes]
  dataset[, X5_mttarjeta_master_debitos_automaticos_decil := frank(mttarjeta_master_debitos_automaticos, ties.method = "first", n.ties = 10), by=foto_mes]
  dataset[, X5_mpagodeservicios_decil := frank(mpagodeservicios, ties.method = "first", n.ties = 10), by=foto_mes]
  dataset[, X5_mpagomiscuentas_decil := frank(mpagomiscuentas, ties.method = "first", n.ties = 10), by=foto_mes]
  dataset[, X5_mcajeros_propios_descuentos_decil := frank(mcajeros_propios_descuentos, ties.method = "first", n.ties = 10), by=foto_mes]
  dataset[, X5_mtarjeta_visa_descuentos_decil := frank(mtarjeta_visa_descuentos, ties.method = "first", n.ties = 10), by=foto_mes]
  dataset[, X5_mtarjeta_master_descuentos_decil := frank(mtarjeta_master_descuentos, ties.method = "first", n.ties = 10), by=foto_mes]
  dataset[, X5_mcomisiones_mantenimiento_decil := frank(mcomisiones_mantenimiento, ties.method = "first", n.ties = 10), by=foto_mes]
  dataset[, X5_mcomisiones_otras_decil := frank(mcomisiones_otras, ties.method = "first", n.ties = 10), by=foto_mes]
  dataset[, X5_mforex_buy_decil := frank(mforex_buy, ties.method = "first", n.ties = 10), by=foto_mes]
  dataset[, X5_mforex_sell_decil := frank(mforex_sell, ties.method = "first", n.ties = 10), by=foto_mes]
  dataset[, X5_mtransferencias_recibidas_decil := frank(mtransferencias_recibidas, ties.method = "first", n.ties = 10), by=foto_mes]
  dataset[, X5_mtransferencias_emitidas_decil := frank(mtransferencias_emitidas, ties.method = "first", n.ties = 10), by=foto_mes]
  dataset[, X5_mextraccion_autoservicio_decil := frank(mextraccion_autoservicio, ties.method = "first", n.ties = 10), by=foto_mes]
  dataset[, X5_mcheques_depositados_decil := frank(mcheques_depositados, ties.method = "first", n.ties = 10), by=foto_mes]
  dataset[, X5_mcheques_emitidos_decil := frank(mcheques_emitidos, ties.method = "first", n.ties = 10), by=foto_mes]
  dataset[, X5_mcheques_depositados_rechazados_decil := frank(mcheques_depositados_rechazados, ties.method = "first", n.ties = 10), by=foto_mes]
  dataset[, X5_mcheques_emitidos_rechazados_decil := frank(mcheques_emitidos_rechazados, ties.method = "first", n.ties = 10), by=foto_mes]
  dataset[, X5_matm_decil := frank(matm, ties.method = "first", n.ties = 10), by=foto_mes]
  dataset[, X5_matm_other_decil := frank(matm_other, ties.method = "first", n.ties = 10), by=foto_mes]
  dataset[, X5_Master_msaldototal_decil := frank(Master_msaldototal, ties.method = "first", n.ties = 10), by=foto_mes]
  dataset[, X5_Master_msaldopesos_decil := frank(Master_msaldopesos, ties.method = "first", n.ties = 10), by=foto_mes]
  dataset[, X5_Master_msaldodolares_decil := frank(Master_msaldodolares, ties.method = "first", n.ties = 10), by=foto_mes]
  dataset[, X5_Master_mconsumospesos_decil := frank(Master_mconsumospesos, ties.method = "first", n.ties = 10), by=foto_mes]
  dataset[, X5_Master_mconsumosdolares_decil := frank(Master_mconsumosdolares, ties.method = "first", n.ties = 10), by=foto_mes]
  dataset[, X5_Master_mpagado_decil := frank(Master_mpagado, ties.method = "first", n.ties = 10), by=foto_mes]
  dataset[, X5_Master_mpagospesos_decil := frank(Master_mpagospesos, ties.method = "first", n.ties = 10), by=foto_mes]
  dataset[, X5_Master_mpagosdolares_decil := frank(Master_mpagosdolares, ties.method = "first", n.ties = 10), by=foto_mes]
  dataset[, X5_Master_mconsumototal_decil := frank(Master_mconsumototal, ties.method = "first", n.ties = 10), by=foto_mes]
  dataset[, X5_Visa_msaldototal_decil := frank(Visa_msaldototal, ties.method = "first", n.ties = 10), by=foto_mes]
  dataset[, X5_Visa_msaldopesos_decil := frank(Visa_msaldopesos, ties.method = "first", n.ties = 10), by=foto_mes]
  dataset[, X5_Visa_msaldodolares_decil := frank(Visa_msaldodolares, ties.method = "first", n.ties = 10), by=foto_mes]
  dataset[, X5_Visa_mconsumospesos_decil := frank(Visa_mconsumospesos, ties.method = "first", n.ties = 10), by=foto_mes]
  dataset[, X5_Visa_mconsumosdolares_decil := frank(Visa_mconsumosdolares, ties.method = "first", n.ties = 10), by=foto_mes]
  dataset[, X5_Visa_mpagado_decil := frank(Visa_mpagado, ties.method = "first", n.ties = 10), by=foto_mes]
  dataset[, X5_Visa_mpagospesos_decil := frank(Visa_mpagospesos, ties.method = "first", n.ties = 10), by=foto_mes]
  dataset[, X5_Visa_mpagosdolares_decil := frank(Visa_mpagosdolares, ties.method = "first", n.ties = 10), by=foto_mes]
  dataset[, X5_Visa_mconsumototal_decil := frank(Visa_mconsumototal, ties.method = "first", n.ties = 10), by=foto_mes]



'








  # valvula de seguridad para evitar valores infinitos
  # paso los infinitos a NULOS
  infinitos <- lapply(
    names(dataset),
    function(.name) dataset[, sum(is.infinite(get(.name)))]
  )

  infinitos_qty <- sum(unlist(infinitos))
  if (infinitos_qty > 0) {
    cat(
      "ATENCION, hay", infinitos_qty,
      "valores infinitos en tu dataset. Seran pasados a NA\n"
    )
    dataset[mapply(is.infinite, dataset)] <<- NA
  }


  # valvula de seguridad para evitar valores NaN  que es 0/0
  # paso los NaN a 0 , decision polemica si las hay
  # se invita a asignar un valor razonable segun la semantica del campo creado
  nans <- lapply(
    names(dataset),
    function(.name) dataset[, sum(is.nan(get(.name)))]
  )

  nans_qty <- sum(unlist(nans))
  if (nans_qty > 0) {
    cat(
      "ATENCION, hay", nans_qty,
      "valores NaN 0/0 en tu dataset. Seran pasados arbitrariamente a 0\n"
    )

    cat("Si no te gusta la decision, modifica a gusto el programa!\n\n")
    dataset[mapply(is.nan, dataset)] <<- 0
  }
}
#------------------------------------------------------------------------------
# deflaciona por IPC
# momento 1.0  31-dic-2020 a las 23:59

drift_deflacion <- function(campos_monetarios) {
  vfoto_mes <- c(
    201901, 201902, 201903, 201904, 201905, 201906,
    201907, 201908, 201909, 201910, 201911, 201912,
    202001, 202002, 202003, 202004, 202005, 202006,
    202007, 202008, 202009, 202010, 202011, 202012,
    202101, 202102, 202103, 202104, 202105, 202106,
    202107, 202108, 202109
  )

  vIPC <- c(
    1.9903030878, 1.9174403544, 1.8296186587,
    1.7728862972, 1.7212488323, 1.6776304408,
    1.6431248196, 1.5814483345, 1.4947526791,
    1.4484037589, 1.3913580777, 1.3404220402,
    1.3154288912, 1.2921698342, 1.2472681797,
    1.2300475145, 1.2118694724, 1.1881073259,
    1.1693969743, 1.1375456949, 1.1065619600,
    1.0681100000, 1.0370000000, 1.0000000000,
    0.9680542110, 0.9344152616, 0.8882274350,
    0.8532444140, 0.8251880213, 0.8003763543,
    0.7763107219, 0.7566381305, 0.7289384687
  )

  tb_IPC <- data.table(
    "foto_mes" = vfoto_mes,
    "IPC" = vIPC
  )

  dataset[tb_IPC,
    on = c("foto_mes"),
    (campos_monetarios) := .SD * i.IPC,
    .SDcols = campos_monetarios
  ]
}

#------------------------------------------------------------------------------

drift_rank_simple <- function(campos_drift) {
  for (campo in campos_drift)
  {
    cat(campo, " ")
    dataset[, paste0(campo, "_rank") :=
      (frank(get(campo), ties.method = "random") - 1) / (.N - 1), by = foto_mes]
    dataset[, (campo) := NULL]
  }
}
#------------------------------------------------------------------------------
# El cero se transforma en cero
# los positivos se rankean por su lado
# los negativos se rankean por su lado

drift_rank_cero_fijo <- function(campos_drift) {
  for (campo in campos_drift)
  {
    cat(campo, " ")
    dataset[get(campo) == 0, paste0(campo, "_rank") := 0]
    dataset[get(campo) > 0, paste0(campo, "_rank") :=
      frank(get(campo), ties.method = "random") / .N, by = foto_mes]

    dataset[get(campo) < 0, paste0(campo, "_rank") :=
      -frank(-get(campo), ties.method = "random") / .N, by = foto_mes]
    dataset[, (campo) := NULL]
  }
}
#------------------------------------------------------------------------------

drift_estandarizar <- function(campos_drift) {
  for (campo in campos_drift)
  {
    cat(campo, " ")
    dataset[, paste0(campo, "_normal") := 
      (get(campo) -mean(campo, na.rm=TRUE)) / sd(get(campo), na.rm=TRUE),
      by = foto_mes]

    dataset[, (campo) := NULL]
  }
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui comienza el programa
OUTPUT$PARAM <- PARAM
OUTPUT$time$start <- format(Sys.time(), "%Y%m%d %H%M%S")

setwd(PARAM$home)

# cargo el dataset donde voy a entrenar
# esta en la carpeta del exp_input y siempre se llama  dataset.csv.gz
dataset_input <- paste0("./exp/", PARAM$exp_input, "/dataset.csv.gz")
dataset <- fread(dataset_input)

# creo la carpeta donde va el experimento
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings = FALSE)
# Establezco el Working Directory DEL EXPERIMENTO
setwd(paste0("./exp/", PARAM$experimento, "/"))

GrabarOutput()
write_yaml(PARAM, file = "parametros.yml") # escribo parametros utilizados

# primero agrego las variables manuales
if (PARAM$variables_intrames) AgregarVariables_IntraMes(dataset)

# ordeno de esta forma por el ranking
setorder(dataset, foto_mes, numero_de_cliente)

# por como armé los nombres de campos,
#  estos son los campos que expresan variables monetarias
campos_monetarios <- colnames(dataset)
campos_monetarios <- campos_monetarios[campos_monetarios %like%
  "^(m|Visa_m|Master_m|vm_m)"]

# aqui aplico un metodo para atacar el data drifting
# hay que probar experimentalmente cual funciona mejor
switch(PARAM$metodo,
  "ninguno"        = cat("No hay correccion del data drifting"),
  "rank_simple"    = drift_rank_simple(campos_monetarios),
  "rank_cero_fijo" = drift_rank_cero_fijo(campos_monetarios),
  "deflacion"      = drift_deflacion(campos_monetarios),
  "estandarizar"   = drift_estandarizar(campos_monetarios)
)



fwrite(dataset,
  file = "dataset.csv.gz",
  logical01 = TRUE,
  sep = ","
)

#------------------------------------------------------------------------------

# guardo los campos que tiene el dataset
tb_campos <- as.data.table(list(
  "pos" = 1:ncol(dataset),
  "campo" = names(sapply(dataset, class)),
  "tipo" = sapply(dataset, class),
  "nulos" = sapply(dataset, function(x) {
    sum(is.na(x))
  }),
  "ceros" = sapply(dataset, function(x) {
    sum(x == 0, na.rm = TRUE)
  })
))

fwrite(tb_campos,
  file = "dataset.campos.txt",
  sep = "\t"
)

#------------------------------------------------------------------------------
OUTPUT$dataset$ncol <- ncol(dataset)
OUTPUT$dataset$nrow <- nrow(dataset)
OUTPUT$time$end <- format(Sys.time(), "%Y%m%d %H%M%S")
GrabarOutput()

# dejo la marca final
cat(format(Sys.time(), "%Y%m%d %H%M%S"), "\n",
  file = "zRend.txt",
  append = TRUE
)
