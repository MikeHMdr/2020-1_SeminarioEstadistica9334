####################################################################################################
# MODELO DE REGRESIÓN MULTIPLE
####################################################################################################

# MODELO MULTIPLE
DATOS           <- datasets::mtcars

MODELO_MULTIPLE <- lm(formula = mpg ~ ., data = DATOS)
summary(MODELO_MULTIPLE)

# VALIDACION DE SUPUESTOS
par(mfrow=c(2,2))
# Plot1
plot(x = DATOS$mpg, y = MODELO_MULTIPLE$fitted.values, col="#1CACDB", pch=19, xlab="Y Real", ylab="Y Estimada"); grid()
abline(a = 0, b = 1, col="red", lwd=2)
# Plot2
plot(x = MODELO_MULTIPLE$residuals, col="#1CACDB", pch=19, xlab="Observación", ylab="Residuales"); grid()
abline(a = 0, b = 0, col="red", lwd=2)
abline(a = mean(MODELO_MULTIPLE$residuals), b = 0, col="purple", lwd=2)
# Plot3
hist(x = MODELO_MULTIPLE$residuals, col="#1CACDB", probability = TRUE)
curve(dnorm(x,mean=0,sd=sd(MODELO_MULTIPLE$residuals)), add=TRUE, col="red", lwd=3)
lines(density(MODELO_MULTIPLE$residuals), col="purple", lwd=3)
# Plot4
qqnorm(y = MODELO_MULTIPLE$residuals, col="#1CACDB", pch=19); grid()
abline(a = 0, b = 1, col="red", lwd=2)
par(mfrow=c(1,1))

# Función de autocorrelación
acf(x = MODELO_MULTIPLE$residuals)
# Función de autocorrelación Parcial
# pacf(x = MODELO_MULTIPLE$residuals)

####################################################################################################
# REVISAR VARIABLES DEL MODELO
# BIBLIOTECA "BASE"
pairs(x = DATOS)
pairs(x = DATOS, lower.panel = NULL)
pairs(DATOS,
      lower.panel = function(x, y){
        par(usr = c(0, 1, 0, 1))
        text(x = 0.5, y = 0.5, labels = paste0("R = ", round(cor(x, y), digits=2) ), col="red")
      },
      upper.panel = function(x, y){
        points(x,y, pch = 19)
      } )

# BIBLIOTECA "psych"
# install.packages("psych")
psych::pairs.panels(DATOS, method = "pearson", hist.col = "#1CACDB", density = TRUE, pch = 19, ellipses = FALSE)

# BIBLIOTECA "corrplot"
# install.packages("corrplot")
corrplot::corrplot.mixed( cor(DATOS) )
####################################################################################################


MODELO_MULTIPLE
summary(MODELO_MULTIPLE)
MODELO_MULTIPLE

SELECCION_MODELO <- step(object = MODELO_MULTIPLE, direction = "both")
SELECCION_MODELO$coefficients
summary(SELECCION_MODELO)
summary(MODELO_MULTIPLE)

for(INDICE in 2:11){
  Y <- DATOS$mpg
  X <- as.vector(unlist( DATOS[,INDICE] ))
  MODELO_SIMPLE <- lm(formula = Y ~ X, data = DATOS)
  plot(x = X, y = Y, col="#1CACDB", pch=19); grid()
  abline(MODELO_SIMPLE, col="red")
}


####################################################################################################
# FIN DEL ARCHIVO
####################################################################################################