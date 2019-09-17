################################################################################
# IMPORTANCIA DEL SUPUESTO SOBRE LOS RESIDUALES
################################################################################

rm(list=ls())
# OBTENEMOS LOS DATOS DE LA BIBLIOTECA 'datasets' QUE YA VIENE PREINSTALADA
DATOS  <- datasets::mtcars[,c(1,3,4,6,7,9)]
MODELO <- lm(formula = mpg ~ 1 + ., data = DATOS)

# VARIANZA DEL MODELO CON TODAS LAS VARIABLES
sigma2 <- mean( lm(formula = mpg ~ 1 + ., data = DATOS)$residuals**2 ) # Estimador Insesgado!!!

# EL SIGUIENTE CODIGO ES UNA IMPLEMENTACION BASICA PARA COMPARAR LOS DIFERENTES
# QUE PUEDE REALIZAR CON UNA CIERTA CANTIDAD DE COVARIABLES
RESULTADOS    <- NULL                         # TABLA PARA GUARDAR LOS RESULTADOS
COMBINACIONES <- combn(x = seq(5), m = 3) + 1 # AUMENTAMOS 1 POR EL INTERCEPTO
for( ITERACION in seq(ncol(COMBINACIONES)) ){
  # GENERAMOS EL MODELO
  COVARIABLES   <- COMBINACIONES[,ITERACION]
  MATRIZ_DISEÑO <- as.matrix( DATOS[, COVARIABLES] )
  MODELO        <- lm(formula = DATOS$mpg ~ 1 + MATRIZ_DISEÑO)
  # REALIZAMOS LA DESCOMPOSICION DE CUADRADOS
  Y       <- DATOS$mpg
  Y_BARRA <- mean(Y)
  Y_EST   <- MODELO$fitted.values
  SCT <- sum( ( Y     - Y_BARRA )**2 ) # SUMA DE CUADRADOS TOTALES
  SCE <- sum( ( Y     - Y_EST   )**2 ) # SUMA DE CUADRADOS DEL ERROR
  SCR <- sum( ( Y_EST - Y_BARRA )**2 ) # SUMA DE CUADRADOS DE LA REGRESION
  # CALCULAMOS LAS ESTADISTICAS QUE NECESITAMOS
  n <- length(Y)
  p <- length(MODELO$coefficients)
  ESTADISTICA_R2     <- 1 - SCE / SCT
  ESTADISTICA_R2ADJ  <- 1 - ( SCE/(n-p) ) / ( SCT/(n-1) )
  ESTADISTICA_AIC    <- n * log( SCE/n ) + p * 2 
  ESTADISTICA_BIC    <- n * log( SCE/n )    + p * log(n) 
  ESTADISTICA_CP     <- SCE/sigma2 - n + p * 2
  # GUARDAMOS LOS RESULTADOS EN NUESTRA TABLA
  RESULTADOS <- rbind( RESULTADOS, data.frame(
    COMBINACION       = toString(COVARIABLES), 
    ESTADISTICA_R2    = ESTADISTICA_R2,
    ESTADISTICA_R2ADJ = ESTADISTICA_R2ADJ,
    ESTADISTICA_AIC   = ESTADISTICA_AIC,
    ESTADISTICA_BIC   = ESTADISTICA_BIC,
    ESTADISTICA_CP    = ESTADISTICA_CP ) )
}

# PODEMOS VER LOS RESULTADOS DE TODAS LAS COMBINACIONES DE CUATRO COVARIABLES
# (MAS INTERCEPTO). PODEMOS VER QUE TODAS APUNTAN A QUE EL MEJOR MODELO ES AQUELLA
# COMBINACION 3,4,5,6 // "hp" "wt"  "qsec" "am" 
RESULTADOS


################################################################################
# FIN DEL ARCHIVO
################################################################################
