################################################################################
# IMPORTANCIA DEL SUPUESTO SOBRE LOS RESIDUALES
################################################################################

rm(list=ls())
# OBTENEMOS LOS DATOS DE LA BIBLIOTECA 'datasets' QUE YA VIENE PREINSTALADA
DATOS <- datasets::mtcars

# GENERAMOS MODELO LINEAL MULTIPLE, SU RESUMEN Y LA TABLA ANOVA
MODELO <- lm( formula = mpg ~ 1 + ., data = DATOS )
summary(MODELO)
anova(MODELO)
################################################################################
# TAMBIEN ES VALIDO ESPECIFICAR LA MATRIZ DE DISEÑO, EN LUGAR
# DE ESPECIFICAR VARIABLE POR VARIABLE
# VEASE QUE LA TABLA ANOVA CAMBIA. ESTO SE VE EN DISEÑO DE EXPERIMENTOS
VARIABLE_RESPUESTA <- DATOS$mpg
MATRIZ_DISEÑO      <- as.matrix( DATOS[,-1] )
rownames(MATRIZ_DISEÑO) <- NULL

MODELO_MATRIZ <- lm(formula = VARIABLE_RESPUESTA ~ 1 + MATRIZ_DISEÑO )
summary(MODELO_MATRIZ)
anova(MODELO_MATRIZ)
################################################################################

Y       <- DATOS$mpg
Y_BARRA <- mean(Y)
Y_EST   <- MODELO$fitted.values

SCT <- sum( ( Y     - Y_BARRA )**2 ) # SUMA DE CUADRADOS TOTALES
SCE <- sum( ( Y     - Y_EST   )**2 ) # SUMA DE CUADRADOS DEL ERROR
SCR <- sum( ( Y_EST - Y_BARRA )**2 ) # SUMA DE CUADRADOS DE LA REGRESION

abs( SCT - ( SCE + SCR ) ) < .Machine$double.eps # REVISAMOS QUE SEAN "IGUALES"

n <- nrow(DATOS)                      # Búmero de observaciones
p <- length( MODELO$coefficients )    # Número de covariables (incluyendo beta0)

# Definición AIC y BIC por LogVerosimilitud
# AIC := -2 * logLik     + p * 2
# BIC := -2 * logLik     + p * ln(n)
-2 * logLik(MODELO) + p * 2           # AIC por LogVerosimilitud
-2 * logLik(MODELO) + p * log(n)      # BIC por LogVerosimilitud

# Definición AIC y BIC por Descomposición de Cuadrados
# AIC := n * ln( SCE/n ) + p * 2
# BIC := n * ln( SCE/n ) + p * ln(n)
n * log( SCE/n )    + p * 2           # AIC por Descomposición de Cuadrados
n * log( SCE/n )    + p * log(n)      # BIC por Descomposición de Cuadrados

# Las siguienes funciones devuelven el AIC, BIC del modelo
# sin embargo, ya vienen implementadas ciertas correcciones
AIC(MODELO, k=2 )
AIC(MODELO, k=log(32) ) # Cuando K = ln(n), es el BIC
BIC(MODELO)

# Sin embargo, nosotros vamos a utilizar la siguiente función que podemos
# calcular mediante la descomposición de cuadrados
# Además, es la que utilizar la función 'step' para elegir un mejor modelo
extractAIC(MODELO)
step(object = MODELO)

# Además de los criterios anteriores, existe el criterio Mallows Cp
#  Cp := SSE / sigma2 - n + p * 2       # Versión 1
#  Cp := ( SSE + 2 * p * sigma2 ) / n   # Versión 2
# sigma2 es estimador de la varianza insesgado del modelo con todas las variables!!
sigma2 <- sum( MODELO$residuals**2 ) / ( 32 - 11 )
SCE/sigma2 - n + p * 2

MODELO_PRUEBA <- lm( formula = mpg ~ 1 + am + qsec + wt, data = DATOS )
SCE_PRUEBA    <- sum( MODELO_PRUEBA$residuals**2 )
SCE_PRUEBA / sigma2 - n + 4 * 2      # Versión 1
( SCE_PRUEBA + 2 * 4 * sigma2 ) / n  # Versión 2

MODELO_PRUEBA <- lm( formula = mpg ~ 1 + cyl + gear, data = DATOS )
SCE_PRUEBA    <- sum( MODELO_PRUEBA$residuals**2 )
SCE_PRUEBA / sigma2 - n + 4 * 2      # Versión 1
( SCE_PRUEBA + 2 * 4 * sigma2 ) / n  # Versión 2

MODELO_PRUEBA <- lm( formula = mpg ~ disp + hp + drat, data = DATOS )
SCE_PRUEBA    <- sum( MODELO_PRUEBA$residuals**2 )
SCE_PRUEBA / sigma2 - n + 3 * 2      # Versión 1
( SCE_PRUEBA + 2 * 3 * sigma2 ) / n  # Versión 2

# Un candidato a ser el mejor modelo es aquel que tenga el menor Cp
# Notemos que, al igual que en el AIC y BIC, las diferentes versiones
# del Cp su valor no coincide. Pero el orden si se mantiene.
# Es decir, el modelo con Cp más pequeño de la primer versión,
# será el modelo con el cp más pequeño de la segunda versión
# https://virtual.uptc.edu.co/ova/estadistica/docs/libros/2007315/lecciones_html/capitulo_8/leccion1/leccion1-4/cpmallows.html


################################################################################
# FIN DEL ARCHIVO
################################################################################
