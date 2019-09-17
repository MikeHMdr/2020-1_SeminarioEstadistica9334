####################################################################################################
# MODELO DE REGRESIÓN MULTIPLE
####################################################################################################

# MODELO MULTIPLE
DATOS           <- datasets::mtcars

MODELO_MULTIPLE <- lm(formula = mpg ~ 1 + ., data = DATOS)
summary(MODELO_MULTIPLE)

# VALIDACION DE SUPUESTOS
par(mfrow=c(2,2))
# Plot1
plot(x = DATOS$mpg, y = MODELO_MULTIPLE$fitted.values, col="#1CACDB", pch=19, xlab="Y Real", ylab="Y Estimada",
     main = "Regresión"); grid()
abline(a = 0, b = 1, col="red", lwd=2)
# Plot2
plot(x = MODELO_MULTIPLE$residuals, col="#1CACDB", pch=19, xlab="Observación", ylab="Residuales",
     main = "Residuales"); grid()
abline(a = 0, b = 0, col="red", lwd=2)
abline(a = mean(MODELO_MULTIPLE$residuals), b = 0, col="purple", lwd=2)
# Plot3
hist(x = MODELO_MULTIPLE$residuals, col="#1CACDB", probability = TRUE, main="Histograma de Residuales")
curve(dnorm(x,mean=0,sd=sd(MODELO_MULTIPLE$residuals)), add=TRUE, col="red", lwd=3)
lines(density(MODELO_MULTIPLE$residuals), col="purple", lwd=3)
# Plot4
qqnorm(y = MODELO_MULTIPLE$residuals, col="#1CACDB", pch=19, main = "Quantil-Quantil"); grid()
par(mfrow=c(1,1))

####################################################################################################

TAM_MUESTRA               <- nrow(DATOS)
PORCENTAJE_ENTRENAMIENTO  <- 0.70
TAM_ENTRENAMIENTO         <- round( TAM_MUESTRA * PORCENTAJE_ENTRENAMIENTO )
INDICES_ENTRENAMIENTO     <- sample(x = seq(TAM_MUESTRA), size = TAM_ENTRENAMIENTO, replace = FALSE)
DATOS_ENTRENAMIENTO       <- DATOS[  INDICES_ENTRENAMIENTO, ]
DATOS_PRUEBA              <- DATOS[ -INDICES_ENTRENAMIENTO, ]
MODELO_ENTRENAMIENTO      <- lm(formula = mpg ~ 1 + ., data = DATOS_ENTRENAMIENTO)
SCE_ENTRENAMIENTO <- sum( MODELO_ENTRENAMIENTO$residuals ** 2 )
SCE_ENTRENAMIENTO

MODELO_MULTIPLE <- MODELO_ENTRENAMIENTO


Y_ESTIMADAS_PRUEBA        <- predict.lm(object = MODELO_ENTRENAMIENTO, newdata = DATOS_PRUEBA)
Y_REALES_PRUEBA           <- DATOS_PRUEBA$mpg
SCT_PRUEBA                <- sum( (Y_REALES_PRUEBA    - mean(Y_REALES_PRUEBA))**2 )
SCE_PRUEBA                <- sum( (Y_REALES_PRUEBA    - Y_ESTIMADAS_PRUEBA   )**2 )
SCR_PRUEBA                <- sum( (Y_ESTIMADAS_PRUEBA - mean(Y_REALES_PRUEBA))**2 )
SCE_PRUEBA
# NO PODEMOS CALCULAR LA R2 DE PRUEBA!!!!
# PERO SI PODEMOS CALCULAR EL "ERROR", EN ESTE CASO, EL ERROR CUADRATICO MEDIO
# QUE ES JUSTO


set.seed(6235472)
NUM_SIMULACIONES         <- 10000
VECTOR_SCE_ENTRENAMIENTO <- vector("numeric",NUM_SIMULACIONES)
VECTOR_SCE_PRUEBA        <- vector("numeric",NUM_SIMULACIONES)
for( SIMULACION in seq(NUM_SIMULACIONES) ){
  TAM_MUESTRA               <- nrow(DATOS)
  PORCENTAJE_ENTRENAMIENTO  <- 0.70
  TAM_ENTRENAMIENTO         <- round( TAM_MUESTRA * PORCENTAJE_ENTRENAMIENTO )
  INDICES_ENTRENAMIENTO     <- sample(x = seq(TAM_MUESTRA), size = TAM_ENTRENAMIENTO, replace = FALSE)
  DATOS_ENTRENAMIENTO       <- DATOS[  INDICES_ENTRENAMIENTO, ]
  DATOS_PRUEBA              <- DATOS[ -INDICES_ENTRENAMIENTO, ]
  MODELO_ENTRENAMIENTO      <- lm(formula = mpg ~ 1 + ., data = DATOS_ENTRENAMIENTO)
  VECTOR_SCE_ENTRENAMIENTO[SIMULACION] <- sum( MODELO_ENTRENAMIENTO$residuals ** 2 )

  Y_ESTIMADAS_PRUEBA            <- predict.lm(object = MODELO_ENTRENAMIENTO, newdata = DATOS_PRUEBA)
  Y_REALES_PRUEBA               <- DATOS_PRUEBA$mpg
  VECTOR_SCE_PRUEBA[SIMULACION] <- sum( (Y_REALES_PRUEBA    - Y_ESTIMADAS_PRUEBA   )**2 )
}


hist(x = VECTOR_SCE_ENTRENAMIENTO, breaks = 50, main="SCE ENTRENAMIENTO", col="#1CACDB", probability = TRUE)
curve(dnorm(x = x, mean(VECTOR_SCE_ENTRENAMIENTO), sd(VECTOR_SCE_ENTRENAMIENTO)), col="red", lwd=3, add=TRUE)
hist(x = VECTOR_SCE_PRUEBA,breaks = 50, main="SCE PRUEBA", col="#1CACDB", probability = TRUE)
curve(dchisq(x = x, df = 154), col="red", lwd=3, add=TRUE)

fitdistrplus::descdist(data = VECTOR_SCE_PRUEBA, boot = 1000)

fitdistrplus::fitdist(data = VECTOR_SCE_PRUEBA, distr = "chisq", method = "mle",
                      start=list(df=10) )

####################################################################################################
# FIN DEL ARCHIVO
####################################################################################################