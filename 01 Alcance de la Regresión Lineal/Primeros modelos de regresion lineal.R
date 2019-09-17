####################################################################################################
# ALCANCE DE LA REGRESION LINEAL
####################################################################################################

#-----------------------------
# DATOS 1
#-----------------------------
datos1 <- data.table::fread("datos_ajuste_1.csv")
plot(x = datos1$X, y = datos1$Y, main="Datos 1", xlab="X", ylab="Y", col="#1CACDB", pch=19); grid()

regresion  <- lm(formula = Y ~ X, data = datos1)
Y_Estimada <- -14.085 + datos1$X * 1.386
lines( x = datos1$X, y = Y_Estimada, col="red", lw=2)


#-----------------------------
# DATOS 2
#-----------------------------
datos2 <- data.table::fread("datos_ajuste_2.csv")
plot(x = datos2$X, y = datos2$Y, main="Datos 2", xlab="X", ylab="Y", col="#1CACDB", pch=19); grid()

regresion  <- lm(formula = Y ~ 0 + X, data = datos2)
Y_Estimada <- -77.068 + datos2$X * 1.354
lines( x = datos2$X, y = Y_Estimada, col="red", lw=2)

regresion  <- lm(formula = Y ~ 1 + X + I(X^2), data = datos2)
Y_Estimada <- regresion$coefficients[1] + regresion$coefficients[2] * datos2$X + regresion$coefficients[3] * datos2$X**2
lines( x = datos2$X, y = Y_Estimada, col="red", lw=2)


#-----------------------------
# DATOS 3
#-----------------------------
datos3 <- data.table::fread("datos_ajuste_3.csv")
plot(x = datos3$X, y = datos3$Y, main="Datos 3", xlab="X", ylab="Y", col="#1CACDB", pch=19); grid()


regresion  <- lm(formula = Y ~ 1 + X + sin(X/12) + cos(X/12), data = datos3)
betas <- regresion$coefficients
Y_Estimada <- betas[1] + betas[2] * datos3$X + betas[3] * sin(datos3$X/12) + betas[4] * cos(datos3$X/12)
lines( x = datos3$X, y = Y_Estimada, col="red", lw=2)



#-----------------------------
# DATOS 4
#-----------------------------
datos4 <- data.table::fread("datos_ajuste_4.csv")
plot(x = datos4$X, y = datos4$Y, main="Datos 4", xlab="X", ylab="Y", col="#1CACDB", pch=19); grid()
cor(datos4$X, datos4$Y)

regresion  <- lm(formula = Y ~ X, data = datos4)
Y_Estimada <- regresion$fitted.values
lines( x = datos4$X, y = Y_Estimada, col="red", lw=2)



####################################################################################################
# FIN DEL ARCHIVO
####################################################################################################