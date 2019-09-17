################################################################################
# IMPORTANCIA DEL SUPUESTO SOBRE LOS RESIDUALES
################################################################################
# GENERAMOS LOS DATOS
set.seed(123456789)
X <- seq(100)
Y <- 0 + 10*sin(X) + rnorm(100,0,1)

# MODELO LINEAL SIMPLE
MODELO <- lm( formula = Y ~ 1 + sin(X) )
plot(X,Y)
lines(X, MODELO$fitted.values, col="red")

RESIDUALES <- MODELO$residuals
RESIDUALES <- Y
plot(RESIDUALES)

################################################################################
# Bibliotecas necesarias para las pruebas paramétricas
# install.packages("fitdistrplus")
# install.packages("tseries")                  
# install.packages("moments")                  
# install.packages("nortest")
################################################################################
fitdistrplus::descdist(data = RESIDUALES, boot = 1000) # Método "Visual" para encontrar su distribución

tseries::runs.test(x = factor(RESIDUALES>0) ) # Prueba de Aleatoriedad
stats::acf(x = RESIDUALES)                    # Prueba de Autocorrelación (lineal)
stats::pacf(x = RESIDUALES)                   # Prueba de Autocorrelación Parcial (lineal)

nortest::ad.test(x = RESIDUALES)              # Prueba de Normalidad (ECDF)
nortest::cvm.test(x = RESIDUALES)             # Prueba de Normalidad (ECDF)
stats::ks.test(x = RESIDUALES, y = "pnorm")   # Prueba de Normalidad (ECDF)
moments::agostino.test(x = RESIDUALES)        # Prueba de Normalidad (Asimetría)
moments::anscombe.test(x = RESIDUALES)        # Prueba de Normalidad (Kurtosis)
stats::shapiro.test(x = RESIDUALES)           # Prueba de Normalidad (Combinacion de dos estadisticos)

################################################################################
# FIN DEL ARCHIVO
################################################################################