####################################################################################################
# MODELO DE REGRESIÓN SIMPLE
####################################################################################################

rm(list=ls())

# LEEMOS LOS DATOS Y LOS GRAFICAMOS
DATOS <- read.csv("tasa_infeccion.csv")
DATOS <- DATOS[ -c(39,40,41),]
X     <- DATOS$X
Y     <- DATOS$Y
plot(x = X, y = Y, col="#1CACDB", pch=19, main="Tasa de Infección"); grid()

# MODELO TRADICIONAL
MODELO <- lm(formula = Y ~ 1 + I(X^2) )
lines(x = X, y = MODELO$fitted.values, col="orange", lwd=2)

# RESUMEN DEL MODELO
summary(MODELO)
SCT <- sum( (DATOS$Y - mean(DATOS$Y))^2 )
SCE <- sum( (DATOS$Y - MODELO$fitted.values)^2 )
SCR <- sum( (MODELO$fitted.values - mean(DATOS$Y))^2 )

n <- nrow(DATOS)
p <- length(MODELO$coefficients)
R2     <- 1 - SCE / SCT
R2_ADJ <- 1 - ( SCE/(n-p) ) / ( SCT/(n-1) )

# AIC y BIC ya con las correcciones
AIC(MODELO) # AKAIKE INFORMATION CRITERIA
BIC(MODELO) # BAYESSIAN INFORMATION CRITERIA

# Con base en la LogVerosimilitud
logL <- logLik(MODELO)
-2*logL + p*2      # AIC
-2*logL + p*log(n) # BIC

# Con base en la SCE
n*log(SCE/n) + p*2      # AIC
n*log(SCE/n) + p*log(n) # BIC

extractAIC(MODELO)
AIC(MODELO)

#############################################
# Detección de puntos influyentes
#############################################
HVALUES    <- hatvalues( MODELO )

HVALUES / mean(HVALUES)

POSICIONES <- as.integer( names( HVALUES[ HVALUES>2*mean(HVALUES) ] ) )
points( x = X[POSICIONES], y = Y[POSICIONES], col="red", pch=19 )


#############################################
# Detección de puntos influyentes
#############################################




# MODELO LINEALIZABLE
# Vemos que el modelo tiene el siguiente comportamiento  ::    Y  =    b0 * exp( b1 * x )
# Por lo que podemos linealizarlo de la siguiente manera :: ln(y) = ln(b0) + b1 * x
MODELO     <- lm(formula = log(Y) ~ 1 + X )
PREDICTION <- as.data.frame( predict(object = MODELO, newdata = DATOS, interval = "prediction", level = 0.95 ) )
plot(x = X, y = log(Y), col="#1CACDB", pch=19, main="Tasa de Infección"); grid()
lines(x = X, y = PREDICTION$lwr, col="purple", lwd=2, lty=2)
lines(x = X, y = PREDICTION$fit, col="orange", lwd=2, lty=1)
lines(x = X, y = PREDICTION$upr, col="purple", lwd=2, lty=2)


# Sin embargo, debemos regresarlo al modelo original
plot(x = X, y = Y, col="#1CACDB", pch=19, main="Tasa de Infección"); grid()
lines(x = X, y = exp( PREDICTION$lwr ), col="purple", lwd=2, lty=2)
lines(x = X, y = exp( PREDICTION$fit ), col="orange", lwd=2, lty=1)
lines(x = X, y = exp( PREDICTION$upr ), col="purple", lwd=2, lty=2)

# Referencias
# https://en.wikipedia.org/wiki/Akaike_information_criterion
# https://en.wikipedia.org/wiki/Bayesian_information_criterion
# http://ugrad.stat.ubc.ca/R/library/base/html/logLik.lm.html
# http://www.scielo.br/scielo.php?script=sci_arttext&pid=S0103-90162015000300245
# http://testingtestingnz.blogspot.com/2013/11/akaike-information-criterion-sum-of.html

####################################################################################################
# FIN DEL ARCHIVO
####################################################################################################
