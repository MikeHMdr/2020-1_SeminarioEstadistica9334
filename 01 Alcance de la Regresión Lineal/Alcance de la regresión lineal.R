####################################################################################################
# ALCANCE DE LA REGRESION LINEAL
####################################################################################################

# Leemos los datos
datos1 <- data.table::fread("datos_ajuste_1.csv")
datos2 <- data.table::fread("datos_ajuste_2.csv")
datos3 <- data.table::fread("datos_ajuste_3.csv")
datos4 <- data.table::fread("datos_ajuste_4.csv")

# Realizamos las graficas
plot(x = datos1$X, y = datos1$Y, main="Datos 1", xlab="X", ylab="Y", col="#1CACDB", pch=19); grid()
plot(x = datos2$X, y = datos2$Y, main="Datos 2", xlab="X", ylab="Y", col="#1CACDB", pch=19); grid()
plot(x = datos3$X, y = datos3$Y, main="Datos 3", xlab="X", ylab="Y", col="#1CACDB", pch=19); grid()
plot(x = datos4$X, y = datos4$Y, main="Datos 4", xlab="X", ylab="Y", col="#1CACDB", pch=19); grid()

# Realizamos las graficas con sus ajustes
X <- datos1$X; Y <- datos1$Y
plot(x = X, y = Y, main="Datos 1", xlab="X", ylab="Y", col="#1CACDB", pch=19); grid()
lines(x = X, y = -15 + sqrt(2)*X, col="red", lwd=2 )

X <- datos2$X; Y <- datos2$Y
plot(x = X, y = Y, main="Datos 2", xlab="X", ylab="Y", col="#1CACDB", pch=19); grid()
lines(x = X, y = -15 + sqrt(2)*X - 0.2*X**2, col="red", lwd=2 )

X <- datos3$X; Y <- datos3$Y
plot(x = X, y = Y, main="Datos 3", xlab="X", ylab="Y", col="#1CACDB", pch=19); grid()
lines(x = X, y = 2 + 0.02*X + sqrt(2)*sin(2*pi*X/48) + sqrt(3)*cos(2*pi*X/48), col="red", lwd=2 )

X <- datos4$X; Y <- datos4$Y
plot(x = X, y = Y, main="Datos 4", xlab="X", ylab="Y", col="#1CACDB", pch=19); grid()
lines(x = X, y = 12 + pi*X + 0.5*X*cos(2/3*X), col="red", lwd=2 )

####################################################################################################
# FIN DEL ARCHIVO
####################################################################################################