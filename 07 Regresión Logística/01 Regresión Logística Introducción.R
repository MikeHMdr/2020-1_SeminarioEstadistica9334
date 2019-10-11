####################################################################################################
# EJEMPLO 1 DE REGRESION LOGISTICA
####################################################################################################

# GENERAMOS NUESTRA TABLA DE EJEMPLO Y GRAFICAMOS LOS PUNTOS
DATOS <- data.frame( HORAS    = c(0.50, 0.75, 1.00, 1.25, 1.50, 1.75, 1.75, 2.00, 2.25, 2.50,
                                  2.75, 3.00, 3.25, 3.50, 4.00, 4.25, 4.50, 4.75, 5.00, 5.50 ),
                     APROBADO = c(0,0,0,0,0,0,1,0,1,0,1,0,1,0,1,1,1,1,1,1) )
plot(x = DATOS$HORAS, y = DATOS$APROBADO, pch=19, col="#1CACDB", cex=2,
     main = "Probabilidad de pasar un examen\nversus las horas de estudio.",
     xlab = "Horas de Estudio", ylab = "Probabilidad de Pasar un examen" ); grid()

# LOS MODELOS GLM TE ESTIMAN E(Y) = MU. NO TE ESTIMAN A LA VARIABLE 'Y' DIRECTAMENTE!
# UNICAMENTE EN EL CASO DE UNA REGRESION NORMAL DEBIDO A QUE E(Y) = MU = Y
MODELO_NORMAL <- glm(formula = APROBADO ~ 1 + HORAS, data = DATOS, family = gaussian(link = "identity") )
MODELO_LOGIT  <- glm(formula = APROBADO ~ 1 + HORAS, data = DATOS, family = binomial(link = "logit") )
summary(MODELO_NORMAL)
summary(MODELO_LOGIT)
lines(x = DATOS$HORAS, y = MODELO_NORMAL$fitted.values, type="l", col="red", lwd=2, cex=2, lty=2)
lines(x = DATOS$HORAS, y = MODELO_LOGIT$fitted.values,  type="l", col="red", lwd=2, cex=2, lty=1)

# PARA ESTIMAR 'Y' EN EL CASO DE UNA REGRESION LOGISTICA SE UTILIZA UN PUNTO DE CORTE
# TAL QUE DISCRIME EN SI Y=0 / Y=1. USUALMENTE SE OPTA POR UN PUNTO DEL 0.50
# AUNQUE HAY QUIENES UTILIZAN UN PUNTO DE CORTE DIFERENTE
Y_ESTIMADA <- as.numeric( MODELO_LOGIT$fitted.values >= 0.50 )
points(x = DATOS$HORAS, y = Y_ESTIMADA, type="p", cex=3, col="red", lwd=3)
table( DATOS$APROBADO )
table( Y_ESTIMADA )
table( DATOS$APROBADO, Y_ESTIMADA )  # MATRIZ DE CONFUSION
mean( DATOS$APROBADO == Y_ESTIMADA ) # PRECISION DEL MODELO


# UN METODO PARA ENCONTRAR UN PUNTO DE CORTE QUE PROPORCIONE LA MEJOR PRECISION ES HACER UNA PARTICION
# Y POSTERIORMENTE CALCULAR LA PRECISION, FINALMENTE TOMAMOS AQUEL QUE MAXIMICE LA PRECISION
# AUNQUE EN LA PRACTICA ESTO NO SIEMPRE OCURRE PUES NO ES UNICO EL PUNTO QUE MAXIMIZA LA PRECISION
PUNTOS_CORTE <- seq(from = 0, to = 1, length.out = 100)
PRECISIONES  <- numeric(length = 100)
for( INDICE in seq_along(PUNTOS_CORTE) ){
  PRECISIONES[INDICE] <- mean( DATOS$APROBADO == as.numeric( MODELO_LOGIT$fitted.values >= PUNTOS_CORTE[INDICE] ) ) }
plot(x = PUNTOS_CORTE, y = PRECISIONES, pch=19, col="#1CACDB", lwd=3, type="l",
     main = "Precision vs Punto de Corte", xlab = "Punto de Corte", ylab = "Precisión" ); grid()


####################################################################################################
# COMPARACION CON OTRAS FUNCIONES LIGA
####################################################################################################
plot(x = DATOS$HORAS, y = DATOS$APROBADO, pch=19, col="#1CACDB", cex=2,
     main = "Probabilidad de pasar un examen\nversus las horas de estudio.",
     xlab = "Horas de Estudio", ylab = "Probabilidad de Pasar un examen" ); grid()
MODELO_LOGIT    <- glm(formula = APROBADO ~ 1 + HORAS, data = DATOS, family = binomial(link = "logit") )
MODELO_PROBIT   <- glm(formula = APROBADO ~ 1 + HORAS, data = DATOS, family = binomial(link = "probit") )
MODELO_CAUCHIT  <- glm(formula = APROBADO ~ 1 + HORAS, data = DATOS, family = binomial(link = "cauchit") )
MODELO_CLOGLOG  <- glm(formula = APROBADO ~ 1 + HORAS, data = DATOS, family = binomial(link = "cloglog") )
lines(x = DATOS$HORAS, y = MODELO_LOGIT$fitted.values,   type="l", col="red",    lwd=2, cex=2, lty=1)
lines(x = DATOS$HORAS, y = MODELO_PROBIT$fitted.values,  type="l", col="purple", lwd=2, cex=2, lty=1)
lines(x = DATOS$HORAS, y = MODELO_CAUCHIT$fitted.values, type="l", col="black",  lwd=2, cex=2, lty=1)
lines(x = DATOS$HORAS, y = MODELO_CLOGLOG$fitted.values, type="l", col="orange", lwd=2, cex=2, lty=1)
legend("topleft",legend = c("Logit","Probit","Cauchit","Cloglog"), fill = c("red","purple","black","orange"))
mean( DATOS$APROBADO == as.numeric( MODELO_LOGIT$fitted.values >= 0.50 ) )   # PRECISION DE LOGIT
mean( DATOS$APROBADO == as.numeric( MODELO_PROBIT$fitted.values >= 0.50 ) )  # PRECISION DE PROBIT
mean( DATOS$APROBADO == as.numeric( MODELO_CAUCHIT$fitted.values >= 0.50 ) ) # PRECISION DE CAUCHIT
mean( DATOS$APROBADO == as.numeric( MODELO_CLOGLOG$fitted.values >= 0.50 ) ) # PRECISION DE CLOGLOG

####################################################################################################
# FIN DEL ARCHIVO
####################################################################################################