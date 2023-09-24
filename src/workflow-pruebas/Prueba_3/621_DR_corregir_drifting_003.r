# Experimentos Colaborativos Default
# Workflow  Data Drifting repair

# limpio la memoria
rm(list = ls(all.names = TRUE)) # remove all objects
gc(full = TRUE) # garbage collection

require("data.table")
require("yaml")


# Parametros del script
PARAM <- list()
PARAM$experimento <- "p03_DR6210"

PARAM$exp_input <- "p03_CA6110"

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


  #2)-------------------------------------------------------------
  # Sumo todas las variables segun el movimiento de dinero

  dataset[, X2_balance := (mpayroll + mpayroll2 + mcajeros_propios_descuentos + mtarjeta_visa_descuentos + mtarjeta_master_descuentos 
  + mtransferencias_recibidas + mcheques_depositados + mcheques_emitidos) -   
  (mautoservicio + mtarjeta_visa_consumo + mtarjeta_master_consumo + mprestamos_personales + mprestamos_prendarios 
  + mprestamos_hipotecarios + mcuenta_debitos_automaticos + mttarjeta_visa_debitos_automaticos + mttarjeta_master_debitos_automaticos 
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
  dataset[, X3_Suma_cantidades_decil := frankv(X3_Suma_cantidades, ties.method = "dense", na.last = "keep"), by=foto_mes]

  # Elimino la columna Suma_cantidades
  #dataset[, X3_Suma_cantidades := NULL]


  #4)-------------------------------------------------------------
  # Calculo la relacion entre la edad y la antiguedad del cliente

  dataset[, X4_edad_antiguedad := cliente_antiguedad / (cliente_edad * 12)]
  dataset[, X4_cliente_edad_decil := frankv(cliente_edad, ties.method = "dense", na.last = "keep"), by=foto_mes]
  dataset[, X4_cliente_antiguedad_decil := frankv(cliente_antiguedad, ties.method = "dense", na.last = "keep"), by=foto_mes]

  #5)-------------------------------------------------------------
  # Hago un rankeo de las variables de montos y saldos totales: 
  #dataset[,, by=foto_mes, .SDcols= ]
  dataset[, X5_mcomisiones_decil := frankv(mcomisiones, ties.method = "dense", na.last = "keep"), by=foto_mes]
  dataset[, X5_mactivos_margen_decil := frankv(mactivos_margen, ties.method = "dense", na.last = "keep"), by=foto_mes]
  dataset[, X5_mpasivos_margen_decil := frankv(mpasivos_margen, ties.method = "dense", na.last = "keep"), by=foto_mes]
  dataset[, X5_mcuenta_corriente_adicional_decil := frankv(mcuenta_corriente_adicional, ties.method = "dense", na.last = "keep"), by=foto_mes]
  dataset[, X5_mcuenta_corriente_decil := frankv(mcuenta_corriente, ties.method = "dense", na.last = "keep"), by=foto_mes]
  dataset[, X5_mcaja_ahorro_decil := frankv(mcaja_ahorro, ties.method = "dense", na.last = "keep"), by=foto_mes]
  dataset[, X5_mcaja_ahorro_adicional_decil := frankv(mcaja_ahorro_adicional, ties.method = "dense", na.last = "keep"), by=foto_mes]
  dataset[, X5_mcaja_ahorro_dolares_decil := frankv(mcaja_ahorro_dolares, ties.method = "dense", na.last = "keep"), by=foto_mes]
  dataset[, X5_mcuentas_saldo_decil := frankv(mcuentas_saldo, ties.method = "dense", na.last = "keep"), by=foto_mes]
  dataset[, X5_mautoservicio_decil := frankv(mautoservicio, ties.method = "dense", na.last = "keep"), by=foto_mes]
  dataset[, X5_mtarjeta_visa_consumo_decil := frankv(mtarjeta_visa_consumo, ties.method = "dense", na.last = "keep"), by=foto_mes]
  dataset[, X5_mtarjeta_master_consumo_decil := frankv(mtarjeta_master_consumo, ties.method = "dense", na.last = "keep"), by=foto_mes]
  dataset[, X5_mprestamos_personales_decil := frankv(mprestamos_personales, ties.method = "dense", na.last = "keep"), by=foto_mes]
  dataset[, X5_mprestamos_prendarios_decil := frankv(mprestamos_prendarios, ties.method = "dense", na.last = "keep"), by=foto_mes]
  dataset[, X5_mprestamos_hipotecarios_decil := frankv(mprestamos_hipotecarios, ties.method = "dense", na.last = "keep"), by=foto_mes]
  dataset[, X5_mplazo_fijo_dolares_decil := frankv(mplazo_fijo_dolares, ties.method = "dense", na.last = "keep"), by=foto_mes]
  dataset[, X5_mplazo_fijo_pesos_decil := frankv(mplazo_fijo_pesos, ties.method = "dense", na.last = "keep"), by=foto_mes]
  dataset[, X5_minversion1_pesos_decil := frankv(minversion1_pesos, ties.method = "dense", na.last = "keep"), by=foto_mes]
  dataset[, X5_minversion1_dolares_decil := frankv(minversion1_dolares, ties.method = "dense", na.last = "keep"), by=foto_mes]
  dataset[, X5_minversion2_decil := frankv(minversion2, ties.method = "dense", na.last = "keep"), by=foto_mes]
  dataset[, X5_mpayroll_decil := frankv(mpayroll, ties.method = "dense", na.last = "keep"), by=foto_mes]
  dataset[, X5_mpayroll2_decil := frankv(mpayroll2, ties.method = "dense", na.last = "keep"), by=foto_mes]
  dataset[, X5_mcuenta_debitos_automaticos_decil := frankv(mcuenta_debitos_automaticos, ties.method = "dense", na.last = "keep"), by=foto_mes]
  dataset[, X5_mttarjeta_visa_debitos_automaticos_decil := frankv(mttarjeta_visa_debitos_automaticos, ties.method = "dense", na.last = "keep"), by=foto_mes]
  dataset[, X5_mttarjeta_master_debitos_automaticos_decil := frankv(mttarjeta_master_debitos_automaticos, ties.method = "dense", na.last = "keep"), by=foto_mes]
  dataset[, X5_mpagodeservicios_decil := frankv(mpagodeservicios, ties.method = "dense", na.last = "keep"), by=foto_mes]
  dataset[, X5_mpagomiscuentas_decil := frankv(mpagomiscuentas, ties.method = "dense", na.last = "keep"), by=foto_mes]
  dataset[, X5_mcajeros_propios_descuentos_decil := frankv(mcajeros_propios_descuentos, ties.method = "dense", na.last = "keep"), by=foto_mes]
  dataset[, X5_mtarjeta_visa_descuentos_decil := frankv(mtarjeta_visa_descuentos, ties.method = "dense", na.last = "keep"), by=foto_mes]
  dataset[, X5_mtarjeta_master_descuentos_decil := frankv(mtarjeta_master_descuentos, ties.method = "dense", na.last = "keep"), by=foto_mes]
  dataset[, X5_mcomisiones_mantenimiento_decil := frankv(mcomisiones_mantenimiento, ties.method = "dense", na.last = "keep"), by=foto_mes]
  dataset[, X5_mcomisiones_otras_decil := frankv(mcomisiones_otras, ties.method = "dense", na.last = "keep"), by=foto_mes]
  dataset[, X5_mforex_buy_decil := frankv(mforex_buy, ties.method = "dense", na.last = "keep"), by=foto_mes]
  dataset[, X5_mforex_sell_decil := frankv(mforex_sell, ties.method = "dense", na.last = "keep"), by=foto_mes]
  dataset[, X5_mtransferencias_recibidas_decil := frankv(mtransferencias_recibidas, ties.method = "dense", na.last = "keep"), by=foto_mes]
  dataset[, X5_mtransferencias_emitidas_decil := frankv(mtransferencias_emitidas, ties.method = "dense", na.last = "keep"), by=foto_mes]
  dataset[, X5_mextraccion_autoservicio_decil := frankv(mextraccion_autoservicio, ties.method = "dense", na.last = "keep"), by=foto_mes]
  dataset[, X5_mcheques_depositados_decil := frankv(mcheques_depositados, ties.method = "dense", na.last = "keep"), by=foto_mes]
  dataset[, X5_mcheques_emitidos_decil := frankv(mcheques_emitidos, ties.method = "dense", na.last = "keep"), by=foto_mes]
  dataset[, X5_mcheques_depositados_rechazados_decil := frankv(mcheques_depositados_rechazados, ties.method = "dense", na.last = "keep"), by=foto_mes]
  dataset[, X5_mcheques_emitidos_rechazados_decil := frankv(mcheques_emitidos_rechazados, ties.method = "dense", na.last = "keep"), by=foto_mes]
  dataset[, X5_matm_decil := frankv(matm, ties.method = "dense", na.last = "keep"), by=foto_mes]
  dataset[, X5_matm_other_decil := frankv(matm_other, ties.method = "dense", na.last = "keep"), by=foto_mes]
  dataset[, X5_Master_msaldototal_decil := frankv(Master_msaldototal, ties.method = "dense", na.last = "keep"), by=foto_mes]
  dataset[, X5_Master_msaldopesos_decil := frankv(Master_msaldopesos, ties.method = "dense", na.last = "keep"), by=foto_mes]
  dataset[, X5_Master_msaldodolares_decil := frankv(Master_msaldodolares, ties.method = "dense", na.last = "keep"), by=foto_mes]
  dataset[, X5_Master_mconsumospesos_decil := frankv(Master_mconsumospesos, ties.method = "dense", na.last = "keep"), by=foto_mes]
  dataset[, X5_Master_mconsumosdolares_decil := frankv(Master_mconsumosdolares, ties.method = "dense", na.last = "keep"), by=foto_mes]
  dataset[, X5_Master_mpagado_decil := frankv(Master_mpagado, ties.method = "dense", na.last = "keep"), by=foto_mes]
  dataset[, X5_Master_mpagospesos_decil := frankv(Master_mpagospesos, ties.method = "dense", na.last = "keep"), by=foto_mes]
  dataset[, X5_Master_mpagosdolares_decil := frankv(Master_mpagosdolares, ties.method = "dense", na.last = "keep"), by=foto_mes]
  dataset[, X5_Master_mconsumototal_decil := frankv(Master_mconsumototal, ties.method = "dense", na.last = "keep"), by=foto_mes]
  dataset[, X5_Visa_msaldototal_decil := frankv(Visa_msaldototal, ties.method = "dense", na.last = "keep"), by=foto_mes]
  dataset[, X5_Visa_msaldopesos_decil := frankv(Visa_msaldopesos, ties.method = "dense", na.last = "keep"), by=foto_mes]
  dataset[, X5_Visa_msaldodolares_decil := frankv(Visa_msaldodolares, ties.method = "dense", na.last = "keep"), by=foto_mes]
  dataset[, X5_Visa_mconsumospesos_decil := frankv(Visa_mconsumospesos, ties.method = "dense", na.last = "keep"), by=foto_mes]
  dataset[, X5_Visa_mconsumosdolares_decil := frankv(Visa_mconsumosdolares, ties.method = "dense", na.last = "keep"), by=foto_mes]
  dataset[, X5_Visa_mpagado_decil := frankv(Visa_mpagado, ties.method = "dense", na.last = "keep"), by=foto_mes]
  dataset[, X5_Visa_mpagospesos_decil := frankv(Visa_mpagospesos, ties.method = "dense", na.last = "keep"), by=foto_mes]
  dataset[, X5_Visa_mpagosdolares_decil := frankv(Visa_mpagosdolares, ties.method = "dense", na.last = "keep"), by=foto_mes]
  dataset[, X5_Visa_mconsumototal_decil := frankv(Visa_mconsumototal, ties.method = "dense", na.last = "keep"), by=foto_mes]


  #6)-------------------------------------------------------------
  # Aplico logaritmos a las variables monetarias

  dataset[, X6_Log2_mrentabilidad := ifelse(mrentabilidad > 1, log2(mrentabilidad), mrentabilidad)]
  dataset[, X6_Log2_mrentabilidad_annual := ifelse(mrentabilidad_annual > 1, log2(mrentabilidad_annual), mrentabilidad_annual)]
  dataset[, X6_Log2_mcomisiones := ifelse(mcomisiones > 1, log2(mcomisiones), mcomisiones)]
  dataset[, X6_Log2_mactivos_margen := ifelse(mactivos_margen > 1, log2(mactivos_margen), mactivos_margen)]
  dataset[, X6_Log2_mpasivos_margen := ifelse(mpasivos_margen > 1, log2(mpasivos_margen), mpasivos_margen)]
  dataset[, X6_Log2_mcuenta_corriente_adicional := ifelse(mcuenta_corriente_adicional > 1, log2(mcuenta_corriente_adicional), mcuenta_corriente_adicional)]
  dataset[, X6_Log2_mcuenta_corriente := ifelse(mcuenta_corriente > 1, log2(mcuenta_corriente), mcuenta_corriente)]
  dataset[, X6_Log2_mcaja_ahorro := ifelse(mcaja_ahorro > 1, log2(mcaja_ahorro), mcaja_ahorro)]
  dataset[, X6_Log2_mcaja_ahorro_adicional := ifelse(mcaja_ahorro_adicional > 1, log2(mcaja_ahorro_adicional), mcaja_ahorro_adicional)]
  dataset[, X6_Log2_mcaja_ahorro_dolares := ifelse(mcaja_ahorro_dolares > 1, log2(mcaja_ahorro_dolares), mcaja_ahorro_dolares)]
  dataset[, X6_Log2_mcuentas_saldo := ifelse(mcuentas_saldo > 1, log2(mcuentas_saldo), mcuentas_saldo)]
  dataset[, X6_Log2_mautoservicio := ifelse(mautoservicio > 1, log2(mautoservicio), mautoservicio)]
  dataset[, X6_Log2_mtarjeta_visa_consumo := ifelse(mtarjeta_visa_consumo > 1, log2(mtarjeta_visa_consumo), mtarjeta_visa_consumo)]
  dataset[, X6_Log2_mtarjeta_master_consumo := ifelse(mtarjeta_master_consumo > 1, log2(mtarjeta_master_consumo), mtarjeta_master_consumo)]
  dataset[, X6_Log2_mprestamos_personales := ifelse(mprestamos_personales > 1, log2(mprestamos_personales), mprestamos_personales)]
  dataset[, X6_Log2_mprestamos_prendarios := ifelse(mprestamos_prendarios > 1, log2(mprestamos_prendarios), mprestamos_prendarios)]
  dataset[, X6_Log2_mprestamos_hipotecarios := ifelse(mprestamos_hipotecarios > 1, log2(mprestamos_hipotecarios), mprestamos_hipotecarios)]
  dataset[, X6_Log2_mplazo_fijo_dolares := ifelse(mplazo_fijo_dolares > 1, log2(mplazo_fijo_dolares), mplazo_fijo_dolares)]
  dataset[, X6_Log2_mplazo_fijo_pesos := ifelse(mplazo_fijo_pesos > 1, log2(mplazo_fijo_pesos), mplazo_fijo_pesos)]
  dataset[, X6_Log2_minversion1_pesos := ifelse(minversion1_pesos > 1, log2(minversion1_pesos), minversion1_pesos)]
  dataset[, X6_Log2_minversion1_dolares := ifelse(minversion1_dolares > 1, log2(minversion1_dolares), minversion1_dolares)]
  dataset[, X6_Log2_minversion2 := ifelse(minversion2 > 1, log2(minversion2), minversion2)]
  dataset[, X6_Log2_mpayroll := ifelse(mpayroll > 1, log2(mpayroll), mpayroll)]
  dataset[, X6_Log2_mpayroll2 := ifelse(mpayroll2 > 1, log2(mpayroll2), mpayroll2)]
  dataset[, X6_Log2_mcuenta_debitos_automaticos := ifelse(mcuenta_debitos_automaticos > 1, log2(mcuenta_debitos_automaticos), mcuenta_debitos_automaticos)]
  dataset[, X6_Log2_mttarjeta_visa_debitos_automaticos := ifelse(mttarjeta_visa_debitos_automaticos > 1, log2(mttarjeta_visa_debitos_automaticos), mttarjeta_visa_debitos_automaticos)]
  dataset[, X6_Log2_mttarjeta_master_debitos_automaticos := ifelse(mttarjeta_master_debitos_automaticos > 1, log2(mttarjeta_master_debitos_automaticos), mttarjeta_master_debitos_automaticos)]
  dataset[, X6_Log2_mpagodeservicios := ifelse(mpagodeservicios > 1, log2(mpagodeservicios), mpagodeservicios)]
  dataset[, X6_Log2_mpagomiscuentas := ifelse(mpagomiscuentas > 1, log2(mpagomiscuentas), mpagomiscuentas)]
  dataset[, X6_Log2_mcajeros_propios_descuentos := ifelse(mcajeros_propios_descuentos > 1, log2(mcajeros_propios_descuentos), mcajeros_propios_descuentos)]
  dataset[, X6_Log2_mtarjeta_visa_descuentos := ifelse(mtarjeta_visa_descuentos > 1, log2(mtarjeta_visa_descuentos), mtarjeta_visa_descuentos)]
  dataset[, X6_Log2_mtarjeta_master_descuentos := ifelse(mtarjeta_master_descuentos > 1, log2(mtarjeta_master_descuentos), mtarjeta_master_descuentos)]
  dataset[, X6_Log2_mcomisiones_mantenimiento := ifelse(mcomisiones_mantenimiento > 1, log2(mcomisiones_mantenimiento), mcomisiones_mantenimiento)]
  dataset[, X6_Log2_mcomisiones_otras := ifelse(mcomisiones_otras > 1, log2(mcomisiones_otras), mcomisiones_otras)]
  dataset[, X6_Log2_mforex_buy := ifelse(mforex_buy > 1, log2(mforex_buy), mforex_buy)]
  dataset[, X6_Log2_mforex_sell := ifelse(mforex_sell > 1, log2(mforex_sell), mforex_sell)]
  dataset[, X6_Log2_mtransferencias_recibidas := ifelse(mtransferencias_recibidas > 1, log2(mtransferencias_recibidas), mtransferencias_recibidas)]
  dataset[, X6_Log2_mtransferencias_emitidas := ifelse(mtransferencias_emitidas > 1, log2(mtransferencias_emitidas), mtransferencias_emitidas)]
  dataset[, X6_Log2_mextraccion_autoservicio := ifelse(mextraccion_autoservicio > 1, log2(mextraccion_autoservicio), mextraccion_autoservicio)]
  dataset[, X6_Log2_mcheques_depositados := ifelse(mcheques_depositados > 1, log2(mcheques_depositados), mcheques_depositados)]
  dataset[, X6_Log2_mcheques_emitidos := ifelse(mcheques_emitidos > 1, log2(mcheques_emitidos), mcheques_emitidos)]
  dataset[, X6_Log2_mcheques_depositados_rechazados := ifelse(mcheques_depositados_rechazados > 1, log2(mcheques_depositados_rechazados), mcheques_depositados_rechazados)]
  dataset[, X6_Log2_mcheques_emitidos_rechazados := ifelse(mcheques_emitidos_rechazados > 1, log2(mcheques_emitidos_rechazados), mcheques_emitidos_rechazados)]
  dataset[, X6_Log2_matm := ifelse(matm > 1, log2(matm), matm)]
  dataset[, X6_Log2_matm_other := ifelse(matm_other > 1, log2(matm_other), matm_other)]
  dataset[, X6_Log2_Master_msaldototal := ifelse(Master_msaldototal > 1, log2(Master_msaldototal), Master_msaldototal)]
  dataset[, X6_Log2_Master_msaldopesos := ifelse(Master_msaldopesos > 1, log2(Master_msaldopesos), Master_msaldopesos)]
  dataset[, X6_Log2_Master_msaldodolares := ifelse(Master_msaldodolares > 1, log2(Master_msaldodolares), Master_msaldodolares)]
  dataset[, X6_Log2_Master_mconsumospesos := ifelse(Master_mconsumospesos > 1, log2(Master_mconsumospesos), Master_mconsumospesos)]
  dataset[, X6_Log2_Master_mconsumosdolares := ifelse(Master_mconsumosdolares > 1, log2(Master_mconsumosdolares), Master_mconsumosdolares)]
  dataset[, X6_Log2_Master_madelantopesos := ifelse(Master_madelantopesos > 1, log2(Master_madelantopesos), Master_madelantopesos)]
  dataset[, X6_Log2_Master_madelantodolares := ifelse(Master_madelantodolares > 1, log2(Master_madelantodolares), Master_madelantodolares)]
  dataset[, X6_Log2_Master_mpagado := ifelse(Master_mpagado > 1, log2(Master_mpagado), Master_mpagado)]
  dataset[, X6_Log2_Master_mpagospesos := ifelse(Master_mpagospesos > 1, log2(Master_mpagospesos), Master_mpagospesos)]
  dataset[, X6_Log2_Master_mpagosdolares := ifelse(Master_mpagosdolares > 1, log2(Master_mpagosdolares), Master_mpagosdolares)]
  dataset[, X6_Log2_Master_mconsumototal := ifelse(Master_mconsumototal > 1, log2(Master_mconsumototal), Master_mconsumototal)]
  dataset[, X6_Log2_Master_mpagominimo := ifelse(Master_mpagominimo > 1, log2(Master_mpagominimo), Master_mpagominimo)]
  dataset[, X6_Log2_Visa_msaldototal := ifelse(Visa_msaldototal > 1, log2(Visa_msaldototal), Visa_msaldototal)]
  dataset[, X6_Log2_Visa_msaldopesos := ifelse(Visa_msaldopesos > 1, log2(Visa_msaldopesos), Visa_msaldopesos)]
  dataset[, X6_Log2_Visa_msaldodolares := ifelse(Visa_msaldodolares > 1, log2(Visa_msaldodolares), Visa_msaldodolares)]
  dataset[, X6_Log2_Visa_mconsumospesos := ifelse(Visa_mconsumospesos > 1, log2(Visa_mconsumospesos), Visa_mconsumospesos)]
  dataset[, X6_Log2_Visa_mconsumosdolares := ifelse(Visa_mconsumosdolares > 1, log2(Visa_mconsumosdolares), Visa_mconsumosdolares)]
  dataset[, X6_Log2_Visa_madelantopesos := ifelse(Visa_madelantopesos > 1, log2(Visa_madelantopesos), Visa_madelantopesos)]
  dataset[, X6_Log2_Visa_madelantodolares := ifelse(Visa_madelantodolares > 1, log2(Visa_madelantodolares), Visa_madelantodolares)]
  dataset[, X6_Log2_Visa_mpagado := ifelse(Visa_mpagado > 1, log2(Visa_mpagado), Visa_mpagado)]
  dataset[, X6_Log2_Visa_mpagospesos := ifelse(Visa_mpagospesos > 1, log2(Visa_mpagospesos), Visa_mpagospesos)]
  dataset[, X6_Log2_Visa_mpagosdolares := ifelse(Visa_mpagosdolares > 1, log2(Visa_mpagosdolares), Visa_mpagosdolares)]
  dataset[, X6_Log2_Visa_mconsumototal := ifelse(Visa_mconsumototal > 1, log2(Visa_mconsumototal), Visa_mconsumototal)]
  dataset[, X6_Log2_Visa_mpagominimo := ifelse(Visa_mpagominimo > 1, log2(Visa_mpagominimo), Visa_mpagominimo)]

  #7)-------------------------------------------------------------
  # Estandarizo las variables monetarias

  # Calculo la media y la desviación estándar de cada variable monetaria por Mes

  dataset[, Media_mrentabilidad := mean(mrentabilidad, na.rm = TRUE), by = foto_mes]
  dataset[, Media_mrentabilidad_annual := mean(mrentabilidad_annual, na.rm = TRUE), by = foto_mes]
  dataset[, Media_mcomisiones := mean(mcomisiones, na.rm = TRUE), by = foto_mes]
  comment <- '
  medias <- dataset[, .(Media_mactivos_margen = mean(mactivos_margen, na.rm = TRUE)), by = foto_mes]
  medias <- dataset[, .(Media_mpasivos_margen = mean(mpasivos_margen, na.rm = TRUE)), by = foto_mes]
  medias <- dataset[, .(Media_mcuenta_corriente_adicional = mean(mcuenta_corriente_adicional, na.rm = TRUE)), by = foto_mes]
  medias <- dataset[, .(Media_mcuenta_corriente = mean(mcuenta_corriente, na.rm = TRUE)), by = foto_mes]
  medias <- dataset[, .(Media_mcaja_ahorro = mean(mcaja_ahorro, na.rm = TRUE)), by = foto_mes]
  medias <- dataset[, .(Media_mcaja_ahorro_adicional = mean(mcaja_ahorro_adicional, na.rm = TRUE)), by = foto_mes]
  medias <- dataset[, .(Media_mcaja_ahorro_dolares = mean(mcaja_ahorro_dolares, na.rm = TRUE)), by = foto_mes]
  medias <- dataset[, .(Media_mcuentas_saldo = mean(mcuentas_saldo, na.rm = TRUE)), by = foto_mes]
  medias <- dataset[, .(Media_mautoservicio = mean(mautoservicio, na.rm = TRUE)), by = foto_mes]
  medias <- dataset[, .(Media_mtarjeta_visa_consumo = mean(mtarjeta_visa_consumo, na.rm = TRUE)), by = foto_mes]
  medias <- dataset[, .(Media_mtarjeta_master_consumo = mean(mtarjeta_master_consumo, na.rm = TRUE)), by = foto_mes]
  medias <- dataset[, .(Media_mprestamos_personales = mean(mprestamos_personales, na.rm = TRUE)), by = foto_mes]
  medias <- dataset[, .(Media_mprestamos_prendarios = mean(mprestamos_prendarios, na.rm = TRUE)), by = foto_mes]
  medias <- dataset[, .(Media_mprestamos_hipotecarios = mean(mprestamos_hipotecarios, na.rm = TRUE)), by = foto_mes]
  medias <- dataset[, .(Media_mplazo_fijo_dolares = mean(mplazo_fijo_dolares, na.rm = TRUE)), by = foto_mes]
  medias <- dataset[, .(Media_mplazo_fijo_pesos = mean(mplazo_fijo_pesos, na.rm = TRUE)), by = foto_mes]
  medias <- dataset[, .(Media_minversion1_pesos = mean(minversion1_pesos, na.rm = TRUE)), by = foto_mes]
  medias <- dataset[, .(Media_minversion1_dolares = mean(minversion1_dolares, na.rm = TRUE)), by = foto_mes]
  medias <- dataset[, .(Media_minversion2 = mean(minversion2, na.rm = TRUE)), by = foto_mes]
  medias <- dataset[, .(Media_mpayroll = mean(mpayroll, na.rm = TRUE)), by = foto_mes]
  medias <- dataset[, .(Media_mpayroll2 = mean(mpayroll2, na.rm = TRUE)), by = foto_mes]
  medias <- dataset[, .(Media_mcuenta_debitos_automaticos = mean(mcuenta_debitos_automaticos, na.rm = TRUE)), by = foto_mes]
  medias <- dataset[, .(Media_mttarjeta_visa_debitos_automaticos = mean(mttarjeta_visa_debitos_automaticos, na.rm = TRUE)), by = foto_mes]
  medias <- dataset[, .(Media_mttarjeta_master_debitos_automaticos = mean(mttarjeta_master_debitos_automaticos, na.rm = TRUE)), by = foto_mes]
  medias <- dataset[, .(Media_mpagodeservicios = mean(mpagodeservicios, na.rm = TRUE)), by = foto_mes]
  medias <- dataset[, .(Media_mpagomiscuentas = mean(mpagomiscuentas, na.rm = TRUE)), by = foto_mes]
  medias <- dataset[, .(Media_mcajeros_propios_descuentos = mean(mcajeros_propios_descuentos, na.rm = TRUE)), by = foto_mes]
  medias <- dataset[, .(Media_mtarjeta_visa_descuentos = mean(mtarjeta_visa_descuentos, na.rm = TRUE)), by = foto_mes]
  medias <- dataset[, .(Media_mtarjeta_master_descuentos = mean(mtarjeta_master_descuentos, na.rm = TRUE)), by = foto_mes]
  medias <- dataset[, .(Media_mcomisiones_mantenimiento = mean(mcomisiones_mantenimiento, na.rm = TRUE)), by = foto_mes]
  medias <- dataset[, .(Media_mcomisiones_otras = mean(mcomisiones_otras, na.rm = TRUE)), by = foto_mes]
  medias <- dataset[, .(Media_mforex_buy = mean(mforex_buy, na.rm = TRUE)), by = foto_mes]
  medias <- dataset[, .(Media_mforex_sell = mean(mforex_sell, na.rm = TRUE)), by = foto_mes]
  medias <- dataset[, .(Media_mtransferencias_recibidas = mean(mtransferencias_recibidas, na.rm = TRUE)), by = foto_mes]
  medias <- dataset[, .(Media_mtransferencias_emitidas = mean(mtransferencias_emitidas, na.rm = TRUE)), by = foto_mes]
  medias <- dataset[, .(Media_mextraccion_autoservicio = mean(mextraccion_autoservicio, na.rm = TRUE)), by = foto_mes]
  medias <- dataset[, .(Media_mcheques_depositados = mean(mcheques_depositados, na.rm = TRUE)), by = foto_mes]
  medias <- dataset[, .(Media_mcheques_emitidos = mean(mcheques_emitidos, na.rm = TRUE)), by = foto_mes]
  medias <- dataset[, .(Media_mcheques_depositados_rechazados = mean(mcheques_depositados_rechazados, na.rm = TRUE)), by = foto_mes]
  medias <- dataset[, .(Media_mcheques_emitidos_rechazados = mean(mcheques_emitidos_rechazados, na.rm = TRUE)), by = foto_mes]
  medias <- dataset[, .(Media_matm = mean(matm, na.rm = TRUE)), by = foto_mes]
  medias <- dataset[, .(Media_matm_other = mean(matm_other, na.rm = TRUE)), by = foto_mes]
  medias <- dataset[, .(Media_Master_msaldototal = mean(Master_msaldototal, na.rm = TRUE)), by = foto_mes]
  medias <- dataset[, .(Media_Master_msaldopesos = mean(Master_msaldopesos, na.rm = TRUE)), by = foto_mes]
  medias <- dataset[, .(Media_Master_msaldodolares = mean(Master_msaldodolares, na.rm = TRUE)), by = foto_mes]
  medias <- dataset[, .(Media_Master_mconsumospesos = mean(Master_mconsumospesos, na.rm = TRUE)), by = foto_mes]
  medias <- dataset[, .(Media_Master_mconsumosdolares = mean(Master_mconsumosdolares, na.rm = TRUE)), by = foto_mes]
  medias <- dataset[, .(Media_Master_madelantopesos = mean(Master_madelantopesos, na.rm = TRUE)), by = foto_mes]
  medias <- dataset[, .(Media_Master_madelantodolares = mean(Master_madelantodolares, na.rm = TRUE)), by = foto_mes]
  medias <- dataset[, .(Media_Master_mpagado = mean(Master_mpagado, na.rm = TRUE)), by = foto_mes]
  medias <- dataset[, .(Media_Master_mpagospesos = mean(Master_mpagospesos, na.rm = TRUE)), by = foto_mes]
  medias <- dataset[, .(Media_Master_mpagosdolares = mean(Master_mpagosdolares, na.rm = TRUE)), by = foto_mes]
  medias <- dataset[, .(Media_Master_mconsumototal = mean(Master_mconsumototal, na.rm = TRUE)), by = foto_mes]
  medias <- dataset[, .(Media_Master_mpagominimo = mean(Master_mpagominimo, na.rm = TRUE)), by = foto_mes]
  medias <- dataset[, .(Media_Visa_msaldototal = mean(Visa_msaldototal, na.rm = TRUE)), by = foto_mes]
  medias <- dataset[, .(Media_Visa_msaldopesos = mean(Visa_msaldopesos, na.rm = TRUE)), by = foto_mes]
  medias <- dataset[, .(Media_Visa_msaldodolares = mean(Visa_msaldodolares, na.rm = TRUE)), by = foto_mes]
  medias <- dataset[, .(Media_Visa_mconsumospesos = mean(Visa_mconsumospesos, na.rm = TRUE)), by = foto_mes]
  medias <- dataset[, .(Media_Visa_mconsumosdolares = mean(Visa_mconsumosdolares, na.rm = TRUE)), by = foto_mes]
  medias <- dataset[, .(Media_Visa_madelantopesos = mean(Visa_madelantopesos, na.rm = TRUE)), by = foto_mes]
  medias <- dataset[, .(Media_Visa_madelantodolares = mean(Visa_madelantodolares, na.rm = TRUE)), by = foto_mes]
  medias <- dataset[, .(Media_Visa_mpagado = mean(Visa_mpagado, na.rm = TRUE)), by = foto_mes]
  medias <- dataset[, .(Media_Visa_mpagospesos = mean(Visa_mpagospesos, na.rm = TRUE)), by = foto_mes]
  medias <- dataset[, .(Media_Visa_mpagosdolares = mean(Visa_mpagosdolares, na.rm = TRUE)), by = foto_mes]
  medias <- dataset[, .(Media_Visa_mconsumototal = mean(Visa_mconsumototal, na.rm = TRUE)), by = foto_mes]
  medias <- dataset[, .(Media_Visa_mpagominimo = mean(Visa_mpagominimo, na.rm = TRUE)), by = foto_mes]
  '
  dataset[, SD_mrentabilidad := sd(mrentabilidad, na.rm = TRUE), by = foto_mes]
  dataset[, SD_mrentabilidad_annual := sd(mrentabilidad_annual, na.rm = TRUE), by = foto_mes]
  dataset[, SD_mcomisiones := sd(mcomisiones, na.rm = TRUE), by = foto_mes]
  comment <- '
  desviaciones <- dataset[, .(SD_mactivos_margen = sd(mactivos_margen, na.rm = TRUE)), by = foto_mes]
  desviaciones <- dataset[, .(SD_mpasivos_margen = sd(mpasivos_margen, na.rm = TRUE)), by = foto_mes]
  desviaciones <- dataset[, .(SD_mcuenta_corriente_adicional = sd(mcuenta_corriente_adicional, na.rm = TRUE)), by = foto_mes]
  desviaciones <- dataset[, .(SD_mcuenta_corriente = sd(mcuenta_corriente, na.rm = TRUE)), by = foto_mes]
  desviaciones <- dataset[, .(SD_mcaja_ahorro = sd(mcaja_ahorro, na.rm = TRUE)), by = foto_mes]
  desviaciones <- dataset[, .(SD_mcaja_ahorro_adicional = sd(mcaja_ahorro_adicional, na.rm = TRUE)), by = foto_mes]
  desviaciones <- dataset[, .(SD_mcaja_ahorro_dolares = sd(mcaja_ahorro_dolares, na.rm = TRUE)), by = foto_mes]
  desviaciones <- dataset[, .(SD_mcuentas_saldo = sd(mcuentas_saldo, na.rm = TRUE)), by = foto_mes]
  desviaciones <- dataset[, .(SD_mautoservicio = sd(mautoservicio, na.rm = TRUE)), by = foto_mes]
  desviaciones <- dataset[, .(SD_mtarjeta_visa_consumo = sd(mtarjeta_visa_consumo, na.rm = TRUE)), by = foto_mes]
  desviaciones <- dataset[, .(SD_mtarjeta_master_consumo = sd(mtarjeta_master_consumo, na.rm = TRUE)), by = foto_mes]
  desviaciones <- dataset[, .(SD_mprestamos_personales = sd(mprestamos_personales, na.rm = TRUE)), by = foto_mes]
  desviaciones <- dataset[, .(SD_mprestamos_prendarios = sd(mprestamos_prendarios, na.rm = TRUE)), by = foto_mes]
  desviaciones <- dataset[, .(SD_mprestamos_hipotecarios = sd(mprestamos_hipotecarios, na.rm = TRUE)), by = foto_mes]
  desviaciones <- dataset[, .(SD_mplazo_fijo_dolares = sd(mplazo_fijo_dolares, na.rm = TRUE)), by = foto_mes]
  desviaciones <- dataset[, .(SD_mplazo_fijo_pesos = sd(mplazo_fijo_pesos, na.rm = TRUE)), by = foto_mes]
  desviaciones <- dataset[, .(SD_minversion1_pesos = sd(minversion1_pesos, na.rm = TRUE)), by = foto_mes]
  desviaciones <- dataset[, .(SD_minversion1_dolares = sd(minversion1_dolares, na.rm = TRUE)), by = foto_mes]
  desviaciones <- dataset[, .(SD_minversion2 = sd(minversion2, na.rm = TRUE)), by = foto_mes]
  desviaciones <- dataset[, .(SD_mpayroll = sd(mpayroll, na.rm = TRUE)), by = foto_mes]
  desviaciones <- dataset[, .(SD_mpayroll2 = sd(mpayroll2, na.rm = TRUE)), by = foto_mes]
  desviaciones <- dataset[, .(SD_mcuenta_debitos_automaticos = sd(mcuenta_debitos_automaticos, na.rm = TRUE)), by = foto_mes]
  desviaciones <- dataset[, .(SD_mttarjeta_visa_debitos_automaticos = sd(mttarjeta_visa_debitos_automaticos, na.rm = TRUE)), by = foto_mes]
  desviaciones <- dataset[, .(SD_mttarjeta_master_debitos_automaticos = sd(mttarjeta_master_debitos_automaticos, na.rm = TRUE)), by = foto_mes]
  desviaciones <- dataset[, .(SD_mpagodeservicios = sd(mpagodeservicios, na.rm = TRUE)), by = foto_mes]
  desviaciones <- dataset[, .(SD_mpagomiscuentas = sd(mpagomiscuentas, na.rm = TRUE)), by = foto_mes]
  desviaciones <- dataset[, .(SD_mcajeros_propios_descuentos = sd(mcajeros_propios_descuentos, na.rm = TRUE)), by = foto_mes]
  desviaciones <- dataset[, .(SD_mtarjeta_visa_descuentos = sd(mtarjeta_visa_descuentos, na.rm = TRUE)), by = foto_mes]
  desviaciones <- dataset[, .(SD_mtarjeta_master_descuentos = sd(mtarjeta_master_descuentos, na.rm = TRUE)), by = foto_mes]
  desviaciones <- dataset[, .(SD_mcomisiones_mantenimiento = sd(mcomisiones_mantenimiento, na.rm = TRUE)), by = foto_mes]
  desviaciones <- dataset[, .(SD_mcomisiones_otras = sd(mcomisiones_otras, na.rm = TRUE)), by = foto_mes]
  desviaciones <- dataset[, .(SD_mforex_buy = sd(mforex_buy, na.rm = TRUE)), by = foto_mes]
  desviaciones <- dataset[, .(SD_mforex_sell = sd(mforex_sell, na.rm = TRUE)), by = foto_mes]
  desviaciones <- dataset[, .(SD_mtransferencias_recibidas = sd(mtransferencias_recibidas, na.rm = TRUE)), by = foto_mes]
  desviaciones <- dataset[, .(SD_mtransferencias_emitidas = sd(mtransferencias_emitidas, na.rm = TRUE)), by = foto_mes]
  desviaciones <- dataset[, .(SD_mextraccion_autoservicio = sd(mextraccion_autoservicio, na.rm = TRUE)), by = foto_mes]
  desviaciones <- dataset[, .(SD_mcheques_depositados = sd(mcheques_depositados, na.rm = TRUE)), by = foto_mes]
  desviaciones <- dataset[, .(SD_mcheques_emitidos = sd(mcheques_emitidos, na.rm = TRUE)), by = foto_mes]
  desviaciones <- dataset[, .(SD_mcheques_depositados_rechazados = sd(mcheques_depositados_rechazados, na.rm = TRUE)), by = foto_mes]
  desviaciones <- dataset[, .(SD_mcheques_emitidos_rechazados = sd(mcheques_emitidos_rechazados, na.rm = TRUE)), by = foto_mes]
  desviaciones <- dataset[, .(SD_matm = sd(matm, na.rm = TRUE)), by = foto_mes]
  desviaciones <- dataset[, .(SD_matm_other = sd(matm_other, na.rm = TRUE)), by = foto_mes]
  desviaciones <- dataset[, .(SD_Master_msaldototal = sd(Master_msaldototal, na.rm = TRUE)), by = foto_mes]
  desviaciones <- dataset[, .(SD_Master_msaldopesos = sd(Master_msaldopesos, na.rm = TRUE)), by = foto_mes]
  desviaciones <- dataset[, .(SD_Master_msaldodolares = sd(Master_msaldodolares, na.rm = TRUE)), by = foto_mes]
  desviaciones <- dataset[, .(SD_Master_mconsumospesos = sd(Master_mconsumospesos, na.rm = TRUE)), by = foto_mes]
  desviaciones <- dataset[, .(SD_Master_mconsumosdolares = sd(Master_mconsumosdolares, na.rm = TRUE)), by = foto_mes]
  desviaciones <- dataset[, .(SD_Master_madelantopesos = sd(Master_madelantopesos, na.rm = TRUE)), by = foto_mes]
  desviaciones <- dataset[, .(SD_Master_madelantodolares = sd(Master_madelantodolares, na.rm = TRUE)), by = foto_mes]
  desviaciones <- dataset[, .(SD_Master_mpagado = sd(Master_mpagado, na.rm = TRUE)), by = foto_mes]
  desviaciones <- dataset[, .(SD_Master_mpagospesos = sd(Master_mpagospesos, na.rm = TRUE)), by = foto_mes]
  desviaciones <- dataset[, .(SD_Master_mpagosdolares = sd(Master_mpagosdolares, na.rm = TRUE)), by = foto_mes]
  desviaciones <- dataset[, .(SD_Master_mconsumototal = sd(Master_mconsumototal, na.rm = TRUE)), by = foto_mes]
  desviaciones <- dataset[, .(SD_Master_mpagominimo = sd(Master_mpagominimo, na.rm = TRUE)), by = foto_mes]
  desviaciones <- dataset[, .(SD_Visa_msaldototal = sd(Visa_msaldototal, na.rm = TRUE)), by = foto_mes]
  desviaciones <- dataset[, .(SD_Visa_msaldopesos = sd(Visa_msaldopesos, na.rm = TRUE)), by = foto_mes]
  desviaciones <- dataset[, .(SD_Visa_msaldodolares = sd(Visa_msaldodolares, na.rm = TRUE)), by = foto_mes]
  desviaciones <- dataset[, .(SD_Visa_mconsumospesos = sd(Visa_mconsumospesos, na.rm = TRUE)), by = foto_mes]
  desviaciones <- dataset[, .(SD_Visa_mconsumosdolares = sd(Visa_mconsumosdolares, na.rm = TRUE)), by = foto_mes]
  desviaciones <- dataset[, .(SD_Visa_madelantopesos = sd(Visa_madelantopesos, na.rm = TRUE)), by = foto_mes]
  desviaciones <- dataset[, .(SD_Visa_madelantodolares = sd(Visa_madelantodolares, na.rm = TRUE)), by = foto_mes]
  desviaciones <- dataset[, .(SD_Visa_mpagado = sd(Visa_mpagado, na.rm = TRUE)), by = foto_mes]
  desviaciones <- dataset[, .(SD_Visa_mpagospesos = sd(Visa_mpagospesos, na.rm = TRUE)), by = foto_mes]
  desviaciones <- dataset[, .(SD_Visa_mpagosdolares = sd(Visa_mpagosdolares, na.rm = TRUE)), by = foto_mes]
  desviaciones <- dataset[, .(SD_Visa_mconsumototal = sd(Visa_mconsumototal, na.rm = TRUE)), by = foto_mes]
  desviaciones <- dataset[, .(SD_Visa_mpagominimo = sd(Visa_mpagominimo, na.rm = TRUE)), by = foto_mes]
  

  # Hago un Merge de medias y desviaciones en el dataset original
  dataset <- merge(dataset, medias, by = "foto_mes", all.x = TRUE)
  dataset <- merge(dataset, desviaciones, by = "foto_mes", all.x = TRUE)
  '
  # Normalizo las Variables por Mes utilizando la media y desviación estándar
  cat("llego hasta aca 483")
  
  dataset[, X7_mrentabilidad_normalizada := ifelse((!is.na(mrentabilidad)), 1, NA)]


  dataset[, X7_mrentabilidad_normalizada := ifelse(any(is.na(c(mrentabilidad, Media_mrentabilidad, SD_mrentabilidad))), NA, (mrentabilidad - Media_mrentabilidad) / SD_mrentabilidad)]
  dataset[, X7_mrentabilidad_annual_normalizada := ifelse(any(is.na(c(mrentabilidad_annual, Media_mrentabilidad_annual, SD_mrentabilidad_annual))), NA, (mrentabilidad_annual - Media_mrentabilidad_annual) / SD_mrentabilidad_annual)]
  dataset[, X7_mcomisiones_normalizada := ifelse(any(is.na(c(mcomisiones, Media_mcomisiones, SD_mcomisiones))), NA, (mcomisiones - Media_mcomisiones) / SD_mcomisiones)]
  comment <- '
  dataset[, X7_mactivos_margen_normalizada := (mactivos_margen - Media_mactivos_margen) / SD_mactivos_margen]
  dataset[, X7_mpasivos_margen_normalizada := (mpasivos_margen - Media_mpasivos_margen) / SD_mpasivos_margen]
  dataset[, X7_mcuenta_corriente_adicional_normalizada := (mcuenta_corriente_adicional - Media_mcuenta_corriente_adicional) / SD_mcuenta_corriente_adicional]
  dataset[, X7_mcuenta_corriente_normalizada := (mcuenta_corriente - Media_mcuenta_corriente) / SD_mcuenta_corriente]
  dataset[, X7_mcaja_ahorro_normalizada := (mcaja_ahorro - Media_mcaja_ahorro) / SD_mcaja_ahorro]
  dataset[, X7_mcaja_ahorro_adicional_normalizada := (mcaja_ahorro_adicional - Media_mcaja_ahorro_adicional) / SD_mcaja_ahorro_adicional]
  dataset[, X7_mcaja_ahorro_dolares_normalizada := (mcaja_ahorro_dolares - Media_mcaja_ahorro_dolares) / SD_mcaja_ahorro_dolares]
  dataset[, X7_mcuentas_saldo_normalizada := (mcuentas_saldo - Media_mcuentas_saldo) / SD_mcuentas_saldo]
  dataset[, X7_mautoservicio_normalizada := (mautoservicio - Media_mautoservicio) / SD_mautoservicio]
  dataset[, X7_mtarjeta_visa_consumo_normalizada := (mtarjeta_visa_consumo - Media_mtarjeta_visa_consumo) / SD_mtarjeta_visa_consumo]
  dataset[, X7_mtarjeta_master_consumo_normalizada := (mtarjeta_master_consumo - Media_mtarjeta_master_consumo) / SD_mtarjeta_master_consumo]
  dataset[, X7_mprestamos_personales_normalizada := (mprestamos_personales - Media_mprestamos_personales) / SD_mprestamos_personales]
  dataset[, X7_mprestamos_prendarios_normalizada := (mprestamos_prendarios - Media_mprestamos_prendarios) / SD_mprestamos_prendarios]
  dataset[, X7_mprestamos_hipotecarios_normalizada := (mprestamos_hipotecarios - Media_mprestamos_hipotecarios) / SD_mprestamos_hipotecarios]
  dataset[, X7_mplazo_fijo_dolares_normalizada := (mplazo_fijo_dolares - Media_mplazo_fijo_dolares) / SD_mplazo_fijo_dolares]
  dataset[, X7_mplazo_fijo_pesos_normalizada := (mplazo_fijo_pesos - Media_mplazo_fijo_pesos) / SD_mplazo_fijo_pesos]
  dataset[, X7_minversion1_pesos_normalizada := (minversion1_pesos - Media_minversion1_pesos) / SD_minversion1_pesos]
  dataset[, X7_minversion1_dolares_normalizada := (minversion1_dolares - Media_minversion1_dolares) / SD_minversion1_dolares]
  dataset[, X7_minversion2_normalizada := (minversion2 - Media_minversion2) / SD_minversion2]
  dataset[, X7_mpayroll_normalizada := (mpayroll - Media_mpayroll) / SD_mpayroll]
  dataset[, X7_mpayroll2_normalizada := (mpayroll2 - Media_mpayroll2) / SD_mpayroll2]
  dataset[, X7_mcuenta_debitos_automaticos_normalizada := (mcuenta_debitos_automaticos - Media_mcuenta_debitos_automaticos) / SD_mcuenta_debitos_automaticos]
  dataset[, X7_mttarjeta_visa_debitos_automaticos_normalizada := (mttarjeta_visa_debitos_automaticos - Media_mttarjeta_visa_debitos_automaticos) / SD_mttarjeta_visa_debitos_automaticos]
  dataset[, X7_mttarjeta_master_debitos_automaticos_normalizada := (mttarjeta_master_debitos_automaticos - Media_mttarjeta_master_debitos_automaticos) / SD_mttarjeta_master_debitos_automaticos]
  dataset[, X7_mpagodeservicios_normalizada := (mpagodeservicios - Media_mpagodeservicios) / SD_mpagodeservicios]
  dataset[, X7_mpagomiscuentas_normalizada := (mpagomiscuentas - Media_mpagomiscuentas) / SD_mpagomiscuentas]
  dataset[, X7_mcajeros_propios_descuentos_normalizada := (mcajeros_propios_descuentos - Media_mcajeros_propios_descuentos) / SD_mcajeros_propios_descuentos]
  dataset[, X7_mtarjeta_visa_descuentos_normalizada := (mtarjeta_visa_descuentos - Media_mtarjeta_visa_descuentos) / SD_mtarjeta_visa_descuentos]
  dataset[, X7_mtarjeta_master_descuentos_normalizada := (mtarjeta_master_descuentos - Media_mtarjeta_master_descuentos) / SD_mtarjeta_master_descuentos]
  dataset[, X7_mcomisiones_mantenimiento_normalizada := (mcomisiones_mantenimiento - Media_mcomisiones_mantenimiento) / SD_mcomisiones_mantenimiento]
  dataset[, X7_mcomisiones_otras_normalizada := (mcomisiones_otras - Media_mcomisiones_otras) / SD_mcomisiones_otras]
  dataset[, X7_mforex_buy_normalizada := (mforex_buy - Media_mforex_buy) / SD_mforex_buy]
  dataset[, X7_mforex_sell_normalizada := (mforex_sell - Media_mforex_sell) / SD_mforex_sell]
  dataset[, X7_mtransferencias_recibidas_normalizada := (mtransferencias_recibidas - Media_mtransferencias_recibidas) / SD_mtransferencias_recibidas]
  dataset[, X7_mtransferencias_emitidas_normalizada := (mtransferencias_emitidas - Media_mtransferencias_emitidas) / SD_mtransferencias_emitidas]
  dataset[, X7_mextraccion_autoservicio_normalizada := (mextraccion_autoservicio - Media_mextraccion_autoservicio) / SD_mextraccion_autoservicio]
  dataset[, X7_mcheques_depositados_normalizada := (mcheques_depositados - Media_mcheques_depositados) / SD_mcheques_depositados]
  dataset[, X7_mcheques_emitidos_normalizada := (mcheques_emitidos - Media_mcheques_emitidos) / SD_mcheques_emitidos]
  dataset[, X7_mcheques_depositados_rechazados_normalizada := (mcheques_depositados_rechazados - Media_mcheques_depositados_rechazados) / SD_mcheques_depositados_rechazados]
  dataset[, X7_mcheques_emitidos_rechazados_normalizada := (mcheques_emitidos_rechazados - Media_mcheques_emitidos_rechazados) / SD_mcheques_emitidos_rechazados]
  dataset[, X7_matm_normalizada := (matm - Media_matm) / SD_matm]
  dataset[, X7_matm_other_normalizada := (matm_other - Media_matm_other) / SD_matm_other]
  dataset[, X7_Master_msaldototal_normalizada := (Master_msaldototal - Media_Master_msaldototal) / SD_Master_msaldototal]
  dataset[, X7_Master_msaldopesos_normalizada := (Master_msaldopesos - Media_Master_msaldopesos) / SD_Master_msaldopesos]
  dataset[, X7_Master_msaldodolares_normalizada := (Master_msaldodolares - Media_Master_msaldodolares) / SD_Master_msaldodolares]
  dataset[, X7_Master_mconsumospesos_normalizada := (Master_mconsumospesos - Media_Master_mconsumospesos) / SD_Master_mconsumospesos]
  dataset[, X7_Master_mconsumosdolares_normalizada := (Master_mconsumosdolares - Media_Master_mconsumosdolares) / SD_Master_mconsumosdolares]
  dataset[, X7_Master_madelantopesos_normalizada := (Master_madelantopesos - Media_Master_madelantopesos) / SD_Master_madelantopesos]
  dataset[, X7_Master_madelantodolares_normalizada := (Master_madelantodolares - Media_Master_madelantodolares) / SD_Master_madelantodolares]
  dataset[, X7_Master_mpagado_normalizada := (Master_mpagado - Media_Master_mpagado) / SD_Master_mpagado]
  dataset[, X7_Master_mpagospesos_normalizada := (Master_mpagospesos - Media_Master_mpagospesos) / SD_Master_mpagospesos]
  dataset[, X7_Master_mpagosdolares_normalizada := (Master_mpagosdolares - Media_Master_mpagosdolares) / SD_Master_mpagosdolares]
  dataset[, X7_Master_mconsumototal_normalizada := (Master_mconsumototal - Media_Master_mconsumototal) / SD_Master_mconsumototal]
  dataset[, X7_Master_mpagominimo_normalizada := (Master_mpagominimo - Media_Master_mpagominimo) / SD_Master_mpagominimo]
  dataset[, X7_Visa_msaldototal_normalizada := (Visa_msaldototal - Media_Visa_msaldototal) / SD_Visa_msaldototal]
  dataset[, X7_Visa_msaldopesos_normalizada := (Visa_msaldopesos - Media_Visa_msaldopesos) / SD_Visa_msaldopesos]
  dataset[, X7_Visa_msaldodolares_normalizada := (Visa_msaldodolares - Media_Visa_msaldodolares) / SD_Visa_msaldodolares]
  dataset[, X7_Visa_mconsumospesos_normalizada := (Visa_mconsumospesos - Media_Visa_mconsumospesos) / SD_Visa_mconsumospesos]
  dataset[, X7_Visa_mconsumosdolares_normalizada := (Visa_mconsumosdolares - Media_Visa_mconsumosdolares) / SD_Visa_mconsumosdolares]
  dataset[, X7_Visa_madelantopesos_normalizada := (Visa_madelantopesos - Media_Visa_madelantopesos) / SD_Visa_madelantopesos]
  dataset[, X7_Visa_madelantodolares_normalizada := (Visa_madelantodolares - Media_Visa_madelantodolares) / SD_Visa_madelantodolares]
  dataset[, X7_Visa_mpagado_normalizada := (Visa_mpagado - Media_Visa_mpagado) / SD_Visa_mpagado]
  dataset[, X7_Visa_mpagospesos_normalizada := (Visa_mpagospesos - Media_Visa_mpagospesos) / SD_Visa_mpagospesos]
  dataset[, X7_Visa_mpagosdolares_normalizada := (Visa_mpagosdolares - Media_Visa_mpagosdolares) / SD_Visa_mpagosdolares]
  dataset[, X7_Visa_mconsumototal_normalizada := (Visa_mconsumototal - Media_Visa_mconsumototal) / SD_Visa_mconsumototal]
  dataset[, X7_Visa_mpagominimo_normalizada := (Visa_mpagominimo - Media_Visa_mpagominimo) / SD_Visa_mpagominimo]

  # Elimino las variables que no utilizo
  
  dataset[, Media_mrentabilidad := NULL]
  dataset[, Media_mrentabilidad_annual := NULL]
  dataset[, Media_mcomisiones := NULL]
  dataset[, Media_mactivos_margen := NULL]
  dataset[, Media_mpasivos_margen := NULL]
  dataset[, Media_mcuenta_corriente_adicional := NULL]
  dataset[, Media_mcuenta_corriente := NULL]
  dataset[, Media_mcaja_ahorro := NULL]
  dataset[, Media_mcaja_ahorro_adicional := NULL]
  dataset[, Media_mcaja_ahorro_dolares := NULL]
  dataset[, Media_mcuentas_saldo := NULL]
  dataset[, Media_mautoservicio := NULL]
  dataset[, Media_mtarjeta_visa_consumo := NULL]
  dataset[, Media_mtarjeta_master_consumo := NULL]
  dataset[, Media_mprestamos_personales := NULL]
  dataset[, Media_mprestamos_prendarios := NULL]
  dataset[, Media_mprestamos_hipotecarios := NULL]
  dataset[, Media_mplazo_fijo_dolares := NULL]
  dataset[, Media_mplazo_fijo_pesos := NULL]
  dataset[, Media_minversion1_pesos := NULL]
  dataset[, Media_minversion1_dolares := NULL]
  dataset[, Media_minversion2 := NULL]
  dataset[, Media_mpayroll := NULL]
  dataset[, Media_mpayroll2 := NULL]
  dataset[, Media_mcuenta_debitos_automaticos := NULL]
  dataset[, Media_mttarjeta_visa_debitos_automaticos := NULL]
  dataset[, Media_mttarjeta_master_debitos_automaticos := NULL]
  dataset[, Media_mpagodeservicios := NULL]
  dataset[, Media_mpagomiscuentas := NULL]
  dataset[, Media_mcajeros_propios_descuentos := NULL]
  dataset[, Media_mtarjeta_visa_descuentos := NULL]
  dataset[, Media_mtarjeta_master_descuentos := NULL]
  dataset[, Media_mcomisiones_mantenimiento := NULL]
  dataset[, Media_mcomisiones_otras := NULL]
  dataset[, Media_mforex_buy := NULL]
  dataset[, Media_mforex_sell := NULL]
  dataset[, Media_mtransferencias_recibidas := NULL]
  dataset[, Media_mtransferencias_emitidas := NULL]
  dataset[, Media_mextraccion_autoservicio := NULL]
  dataset[, Media_mcheques_depositados := NULL]
  dataset[, Media_mcheques_emitidos := NULL]
  dataset[, Media_mcheques_depositados_rechazados := NULL]
  dataset[, Media_mcheques_emitidos_rechazados := NULL]
  dataset[, Media_matm := NULL]
  dataset[, Media_matm_other := NULL]
  dataset[, Media_Master_msaldototal := NULL]
  dataset[, Media_Master_msaldopesos := NULL]
  dataset[, Media_Master_msaldodolares := NULL]
  dataset[, Media_Master_mconsumospesos := NULL]
  dataset[, Media_Master_mconsumosdolares := NULL]
  dataset[, Media_Master_madelantopesos := NULL]
  dataset[, Media_Master_madelantodolares := NULL]
  dataset[, Media_Master_mpagado := NULL]
  dataset[, Media_Master_mpagospesos := NULL]
  dataset[, Media_Master_mpagosdolares := NULL]
  dataset[, Media_Master_mconsumototal := NULL]
  dataset[, Media_Master_mpagominimo := NULL]
  dataset[, Media_Visa_msaldototal := NULL]
  dataset[, Media_Visa_msaldopesos := NULL]
  dataset[, Media_Visa_msaldodolares := NULL]
  dataset[, Media_Visa_mconsumospesos := NULL]
  dataset[, Media_Visa_mconsumosdolares := NULL]
  dataset[, Media_Visa_madelantopesos := NULL]
  dataset[, Media_Visa_madelantodolares := NULL]
  dataset[, Media_Visa_mpagado := NULL]
  dataset[, Media_Visa_mpagospesos := NULL]
  dataset[, Media_Visa_mpagosdolares := NULL]
  dataset[, Media_Visa_mconsumototal := NULL]
  dataset[, Media_Visa_mpagominimo := NULL]

  dataset[, SD_mrentabilidad := NULL]
  dataset[, SD_mrentabilidad_annual := NULL]
  dataset[, SD_mcomisiones := NULL]
  dataset[, SD_mactivos_margen := NULL]
  dataset[, SD_mpasivos_margen := NULL]
  dataset[, SD_mcuenta_corriente_adicional := NULL]
  dataset[, SD_mcuenta_corriente := NULL]
  dataset[, SD_mcaja_ahorro := NULL]
  dataset[, SD_mcaja_ahorro_adicional := NULL]
  dataset[, SD_mcaja_ahorro_dolares := NULL]
  dataset[, SD_mcuentas_saldo := NULL]
  dataset[, SD_mautoservicio := NULL]
  dataset[, SD_mtarjeta_visa_consumo := NULL]
  dataset[, SD_mtarjeta_master_consumo := NULL]
  dataset[, SD_mprestamos_personales := NULL]
  dataset[, SD_mprestamos_prendarios := NULL]
  dataset[, SD_mprestamos_hipotecarios := NULL]
  dataset[, SD_mplazo_fijo_dolares := NULL]
  dataset[, SD_mplazo_fijo_pesos := NULL]
  dataset[, SD_minversion1_pesos := NULL]
  dataset[, SD_minversion1_dolares := NULL]
  dataset[, SD_minversion2 := NULL]
  dataset[, SD_mpayroll := NULL]
  dataset[, SD_mpayroll2 := NULL]
  dataset[, SD_mcuenta_debitos_automaticos := NULL]
  dataset[, SD_mttarjeta_visa_debitos_automaticos := NULL]
  dataset[, SD_mttarjeta_master_debitos_automaticos := NULL]
  dataset[, SD_mpagodeservicios := NULL]
  dataset[, SD_mpagomiscuentas := NULL]
  dataset[, SD_mcajeros_propios_descuentos := NULL]
  dataset[, SD_mtarjeta_visa_descuentos := NULL]
  dataset[, SD_mtarjeta_master_descuentos := NULL]
  dataset[, SD_mcomisiones_mantenimiento := NULL]
  dataset[, SD_mcomisiones_otras := NULL]
  dataset[, SD_mforex_buy := NULL]
  dataset[, SD_mforex_sell := NULL]
  dataset[, SD_mtransferencias_recibidas := NULL]
  dataset[, SD_mtransferencias_emitidas := NULL]
  dataset[, SD_mextraccion_autoservicio := NULL]
  dataset[, SD_mcheques_depositados := NULL]
  dataset[, SD_mcheques_emitidos := NULL]
  dataset[, SD_mcheques_depositados_rechazados := NULL]
  dataset[, SD_mcheques_emitidos_rechazados := NULL]
  dataset[, SD_matm := NULL]
  dataset[, SD_matm_other := NULL]
  dataset[, SD_Master_msaldototal := NULL]
  dataset[, SD_Master_msaldopesos := NULL]
  dataset[, SD_Master_msaldodolares := NULL]
  dataset[, SD_Master_mconsumospesos := NULL]
  dataset[, SD_Master_mconsumosdolares := NULL]
  dataset[, SD_Master_madelantopesos := NULL]
  dataset[, SD_Master_madelantodolares := NULL]
  dataset[, SD_Master_mpagado := NULL]
  dataset[, SD_Master_mpagospesos := NULL]
  dataset[, SD_Master_mpagosdolares := NULL]
  dataset[, SD_Master_mconsumototal := NULL]
  dataset[, SD_Master_mpagominimo := NULL]
  dataset[, SD_Visa_msaldototal := NULL]
  dataset[, SD_Visa_msaldopesos := NULL]
  dataset[, SD_Visa_msaldodolares := NULL]
  dataset[, SD_Visa_mconsumospesos := NULL]
  dataset[, SD_Visa_mconsumosdolares := NULL]
  dataset[, SD_Visa_madelantopesos := NULL]
  dataset[, SD_Visa_madelantodolares := NULL]
  dataset[, SD_Visa_mpagado := NULL]
  dataset[, SD_Visa_mpagospesos := NULL]
  dataset[, SD_Visa_mpagosdolares := NULL]
  dataset[, SD_Visa_mconsumototal := NULL]
  dataset[, SD_Visa_mpagominimo := NULL]
  '
  #8)-------------------------------------------------------------
  # Normalizo las variables monetarias


  cat("LLego bien!")




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
