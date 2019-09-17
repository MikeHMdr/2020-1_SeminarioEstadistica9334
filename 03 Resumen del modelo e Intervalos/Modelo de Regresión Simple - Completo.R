####################################################################################################
# MODELO DE REGRESIÓN SIMPLE
####################################################################################################

par(mfrow=c(2,2))

# --------------------------------------------------------------------------------------------------
# DATOS DE AJUSTE 1
# --------------------------------------------------------------------------------------------------
DATOS  <- read.csv("datos_ajuste_1.csv")
MODELO <- lm(formula = Y ~ 1 + X, data = DATOS)
NUEVOS_DATOS <- data.frame( X = seq(-20,120,1) )
plot(x = DATOS$X, y = DATOS$Y, main="Datos de Ajuste 1", col="#1CACDB", pch=19, xlim=c(min(NUEVOS_DATOS$X),max(NUEVOS_DATOS$X)) ); grid()
INTERVALO_CONFIANZA  <- predict(object = MODELO, newdata = NUEVOS_DATOS, interval = "confidence", level = 0.95)
INTERVALO_PREDICCION <- predict(object = MODELO, newdata = NUEVOS_DATOS, interval = "prediction", level = 0.95)
INTERVALO_CONFIANZA  <- as.data.frame(INTERVALO_CONFIANZA)
INTERVALO_PREDICCION <- as.data.frame(INTERVALO_PREDICCION)
lines(x = DATOS$X, MODELO$fitted.values,            col="forestgreen", lwd=2 )
lines(x = NUEVOS_DATOS$X, INTERVALO_CONFIANZA$lwr,  col="orange", lwd=2 )
lines(x = NUEVOS_DATOS$X, INTERVALO_CONFIANZA$upr,  col="orange", lwd=2 )
lines(x = NUEVOS_DATOS$X, INTERVALO_PREDICCION$lwr, col="red", lwd=2 )
lines(x = NUEVOS_DATOS$X, INTERVALO_PREDICCION$upr, col="red", lwd=2 )

# --------------------------------------------------------------------------------------------------
# DATOS DE AJUSTE 2
# --------------------------------------------------------------------------------------------------
DATOS  <- read.csv("datos_ajuste_2.csv")
MODELO <- lm(formula = Y ~ 1 + X + I(X^2), data = DATOS)
NUEVOS_DATOS <- data.frame( X = seq(-50,50,1) )
plot(x = DATOS$X, y = DATOS$Y, main="Datos de Ajuste 2", col="#1CACDB", pch=19, xlim=c(min(NUEVOS_DATOS$X),max(NUEVOS_DATOS$X)) ); grid()
INTERVALO_CONFIANZA  <- predict(object = MODELO, newdata = NUEVOS_DATOS, interval = "confidence", level = 0.95)
INTERVALO_PREDICCION <- predict(object = MODELO, newdata = NUEVOS_DATOS, interval = "prediction", level = 0.95)
INTERVALO_CONFIANZA  <- as.data.frame(INTERVALO_CONFIANZA)
INTERVALO_PREDICCION <- as.data.frame(INTERVALO_PREDICCION)
lines(x = DATOS$X, MODELO$fitted.values,            col="forestgreen", lwd=2 )
lines(x = NUEVOS_DATOS$X, INTERVALO_CONFIANZA$lwr,  col="orange", lwd=2 )
lines(x = NUEVOS_DATOS$X, INTERVALO_CONFIANZA$upr,  col="orange", lwd=2 )
lines(x = NUEVOS_DATOS$X, INTERVALO_PREDICCION$lwr, col="red", lwd=2 )
lines(x = NUEVOS_DATOS$X, INTERVALO_PREDICCION$upr, col="red", lwd=2 )

# --------------------------------------------------------------------------------------------------
# DATOS DE AJUSTE 3
# --------------------------------------------------------------------------------------------------
DATOS  <- read.csv("datos_ajuste_3.csv")
MODELO <- lm(formula = Y ~ 1 + X + sin(2*pi*X/48) + cos(2*pi*X/48), data = DATOS)
NUEVOS_DATOS <- data.frame( X = seq(-75,175,1) )
plot(x = DATOS$X, y = DATOS$Y, main="Datos de Ajuste 3", col="#1CACDB", pch=19, xlim=c(min(NUEVOS_DATOS$X),max(NUEVOS_DATOS$X)) ); grid()
INTERVALO_CONFIANZA  <- predict(object = MODELO, newdata = NUEVOS_DATOS, interval = "confidence", level = 0.95)
INTERVALO_PREDICCION <- predict(object = MODELO, newdata = NUEVOS_DATOS, interval = "prediction", level = 0.95)
INTERVALO_CONFIANZA  <- as.data.frame(INTERVALO_CONFIANZA)
INTERVALO_PREDICCION <- as.data.frame(INTERVALO_PREDICCION)
lines(x = DATOS$X, MODELO$fitted.values,            col="forestgreen", lwd=2 )
lines(x = NUEVOS_DATOS$X, INTERVALO_CONFIANZA$lwr,  col="orange", lwd=2 )
lines(x = NUEVOS_DATOS$X, INTERVALO_CONFIANZA$upr,  col="orange", lwd=2 )
lines(x = NUEVOS_DATOS$X, INTERVALO_PREDICCION$lwr, col="red", lwd=2 )
lines(x = NUEVOS_DATOS$X, INTERVALO_PREDICCION$upr, col="red", lwd=2 )

# --------------------------------------------------------------------------------------------------
# DATOS DE AJUSTE 4
# --------------------------------------------------------------------------------------------------
DATOS  <- read.csv("datos_ajuste_4.csv")
MODELO <- lm(formula = Y ~ 1 + X + sin(2/3*X) + cos(2/3*X) + X*sin(2/3*X) + X*cos(2/3*X), data = DATOS)
# MODELO <- lm(formula = Y ~ 1 + X + X*cos(2/3*X), data = DATOS)
NUEVOS_DATOS <- data.frame( X = seq(-50,150,1) )
plot(x = DATOS$X, y = DATOS$Y, main="Datos de Ajuste 4", col="#1CACDB", pch=19, xlim=c(min(NUEVOS_DATOS$X),max(NUEVOS_DATOS$X)), ylim=c(20,500) ); grid()
INTERVALO_CONFIANZA  <- predict(object = MODELO, newdata = NUEVOS_DATOS, interval = "confidence", level = 0.95)
INTERVALO_PREDICCION <- predict(object = MODELO, newdata = NUEVOS_DATOS, interval = "prediction", level = 0.95)
INTERVALO_CONFIANZA  <- as.data.frame(INTERVALO_CONFIANZA)
INTERVALO_PREDICCION <- as.data.frame(INTERVALO_PREDICCION)
lines(x = DATOS$X, MODELO$fitted.values,            col="forestgreen", lwd=2 )
lines(x = NUEVOS_DATOS$X, INTERVALO_CONFIANZA$lwr,  col="orange", lwd=1 )
lines(x = NUEVOS_DATOS$X, INTERVALO_CONFIANZA$upr,  col="orange", lwd=1 )
lines(x = NUEVOS_DATOS$X, INTERVALO_PREDICCION$lwr, col="red", lwd=1 )
lines(x = NUEVOS_DATOS$X, INTERVALO_PREDICCION$upr, col="red", lwd=1 )



# --------------------------------------------------------------------------------------------------
# DATOS DE AJUSTE 3
# --------------------------------------------------------------------------------------------------
# DATOS  <- read.csv("datos_ajuste_3.csv")
# MODELO <- lm(formula = Y ~ 1 + X + sin(2*pi*X/48) + cos(2*pi*X/48), data = DATOS)
# plot(x = DATOS$X, y = DATOS$Y, main="Datos de Ajuste 3", col="#1CACDB", pch=19); grid()
# INTERVALO_CONFIANZA  <- predict(object = MODELO, newdata = DATOS, interval = "confidence", level = 0.95)
# INTERVALO_PREDICCION <- predict(object = MODELO, newdata = DATOS, interval = "prediction", level = 0.95)
# INTERVALO_CONFIANZA  <- as.data.frame(INTERVALO_CONFIANZA)
# INTERVALO_PREDICCION <- as.data.frame(INTERVALO_PREDICCION)
# lines(x = DATOS$X, MODELO$fitted.values,     col="forestgreen", lwd=2 )
# lines(x = DATOS$X, INTERVALO_CONFIANZA$lwr,  col="orange", lwd=2 )
# lines(x = DATOS$X, INTERVALO_CONFIANZA$upr,  col="orange", lwd=2 )
# lines(x = DATOS$X, INTERVALO_PREDICCION$lwr, col="red", lwd=2 )
# lines(x = DATOS$X, INTERVALO_PREDICCION$upr, col="red", lwd=2 )
####################################################################################################
# FIN DEL ARCHIVO
####################################################################################################