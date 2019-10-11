####################################################################################################
# DEFAULT
# Es un conjunto de datos simulados que contiene información sobre mil clientes.
# El objetivo aquí es predecir qué clientes incumplirán con su deuda de tarjeta de crédito.
####################################################################################################
rm(list=ls())
# LEEMOS LOS DATOS Y VEMOS LA CANTIDAD DE CLIENTES QUE HAN INCUMPLIDO
DATOS <- ISLR::Default
table(DATOS$default)
# LA COLUMNA 'DEFAULT' ES DE TIPO FACTOR (CATEGORICA)
# PARA REPRESENTARLA CORRECTAMENTE EN LA GRAFICA, LA MAPEAREMOS A 'No'=0; 'Yes'=1
DATOS$default <- as.numeric(DATOS$default=="Yes")
plot(x = DATOS$balance, y = DATOS$default, col="forestgreen", main = "P(Default) vs Balance",
     xlab = "Balance", ylab="P(Default)"); grid()
# AJUSTAMOS UN MODELO LOGISTICO CON LA FUNCION LIGA NATURAL = LOGIT
MODELO_LOGIT <- glm(formula = default ~ 1 + balance, data = DATOS, family = "binomial")
points(x = DATOS$balance, y = MODELO_LOGIT$fitted.values, col="red")
# CALCULAMOS LA ESTIMACION DE Y CON BASE EN UN PUNTO DE CORTE; Y CALCULAMOS LA PRECISIÓN DEL MODELO
Y_ESTIMADA <- as.numeric( MODELO_LOGIT$fitted.values >= 0.05 )
table( DATOS$default, Y_ESTIMADA )  # MATRIZ DE CONFUSION
mean( DATOS$default == Y_ESTIMADA ) # PRECISION DEL MODELO ( DE ENTRENAMIENTO )

# PREGUNTA :: ¿0.5 SERA EL MEJOR PUNTO DE CORTE?

##############################################################################
# K-FOLD CROSS VALIDATION PARA LA PRECISION DE VALIDACION
##############################################################################
set.seed(1)
CROSS_VALIDATION <- boot::cv.glm(data = DATOS, glmfit = MODELO_LOGIT, K = 20)
1-CROSS_VALIDATION$delta[1] # 1-ERROR DE PREDICCION = PRECISION DEL MODELO ("DE VALIDACION")


##############################################################################
# HISTOGRAMAS Y PUNTO DE CORTE OPTIMO
##############################################################################
# HISTOGRAMAS ESCALADOS
hist(x = DATOS$balance[ DATOS$default == 1 ], probability = TRUE, col=rgb(0,0,1,0.2), xlim=c(0, 2700),
     xlab = "Balance", ylab="P(Default)", main="Histograma de Balance | Default"); grid()
hist(x = DATOS$balance[ DATOS$default == 0 ], probability = TRUE, col=rgb(1,0,0,0.2), add = TRUE )
# HISTOGRAMAS NO ESCALADOS
hist(x = DATOS$balance[ DATOS$default == 1 ], probability = FALSE, col=rgb(0,0,1,0.2), xlim=c(0, 2700), ylim=c(0,1600),
     xlab = "Balance", ylab="P(Default)", main="Histograma de Balance | Default"); grid()
hist(x = DATOS$balance[ DATOS$default == 0 ], probability = FALSE, col=rgb(1,0,0,0.2), add = TRUE )
# BUSQUEDA DEL PUNTO DE CORTE OPTIMO
PUNTOS_CORTE <- seq(from = 0, to = 1, length.out = 100)
PRECISIONES  <- numeric(length = 100)
for( INDICE in seq_along(PUNTOS_CORTE) ){
  PRECISIONES[INDICE] <- mean( DATOS$default == as.numeric( MODELO_LOGIT$fitted.values > PUNTOS_CORTE[INDICE] ) ) }
plot(x = PUNTOS_CORTE, y = PRECISIONES, pch=19, col="#1CACDB", lwd=3, type="l",
     main = "Precision vs Punto de Corte", xlab = "Punto de Corte", ylab = "Precisión" ); grid()
# PUNTO DE CORTE OPTIMO
PUNTOS_CORTE[which.max(PRECISIONES)]
Y_ESTIMADA <- as.numeric( MODELO_LOGIT$fitted.values >= 0.50 )
table( DATOS$default, Y_ESTIMADA )  # MATRIZ DE CONFUSION
mean( DATOS$default == Y_ESTIMADA ) # PRECISION DEL MODELO ( DE ENTRENAMIENTO )

# VEASE :: MATRIZ DE CONFUSION
# https://en.wikipedia.org/wiki/Confusion_matrix
# SENSIBILIDAD  / TPR = TASA DE VERDADEROS POSITIVOS ( FILTRANDO TODOS LOS POSITIVOS REALES )
# ESPECIFICIDAD / TNR = TASA DE VERDADEROS NEGATIVOS ( FILTRANDO TODOS LOS NEGATIVOS REALES )
# PRECISION     / PPR = TASA DE POSITIVOS ESTIMADOS  ( FILTRANDO TODOS LOS POSITIVOS ESTIMADOS )
#               / NPV = TASA DE NEGATIVOS ESTIMADOS  ( FILTRANDO TODOS LOS NEGATIVOS ESTIMADOS )
# ACCURACY            = TASA DE CORRECTOS ESTIMADOS
####################################################################################################
# FIN DEL ARCHIVO
####################################################################################################
