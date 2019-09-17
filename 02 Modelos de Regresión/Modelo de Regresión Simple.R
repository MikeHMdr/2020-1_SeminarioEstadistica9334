####################################################################################################
# MODELO DE REGRESIÓN SIMPLE
####################################################################################################

# --------------------------------------------------------------------------------------------------
# Regresión simple usando la función 'lm'
# --------------------------------------------------------------------------------------------------

# Cargamos nuestros datos, los visualizamos
DATOS <- read.csv("datos_ajuste_1.csv")
plot(x = DATOS$X, y = DATOS$Y, main="Datos de Ajuste 1", col="#1CACDB", pch=19); grid()

# Formas de hacer el modelo
MODELO <- lm(formula = Y ~ X, data = DATOS) # Indicar la tabla donde usaremos los datos

# Guardamos las betas y graficamos
BETAS <- MODELO$coefficients
lines(x = DATOS$X, y = BETAS[1] + BETAS[2]*DATOS$X, main="Datos de Ajuste 1", col="orange", lwd=2)


# --------------------------------------------------------------------------------------------------
# Utilizando métodos númericos
# --------------------------------------------------------------------------------------------------

# Finalmente, la regresión lineal intenta minimizar el RMSE = Error Cuadrático Medio
RMSE <- function( BETAS_COEF ){
  Y_EST <- BETAS_COEF[1] + BETAS_COEF[2] * DATOS$X
  RMSE  <- mean( ( DATOS$Y - Y_EST )**2 )
  return(RMSE)
}

# Deseamos minimizar RMSE con respecto a las betas, dando betas iniciales
OPTIMIZACION <- optim(par = c(0,0), fn = RMSE )
BETAS_OPTIM  <- OPTIMIZACION$par   # Parámetros de Beta estimados
lines(x = DATOS$X, y = BETAS_OPTIM[1] + BETAS_OPTIM[2]*DATOS$X, col="purple", lwd=2)
legend(x = "topleft", legend = c("lm","Optim"), fill = c("orange","purple") )

# NOTA :: Veamos que las betas estimadas son distintas a las que devuelve la funcion 'lm'
# Sin embargo, la diferencia no es significativa.
# En el siguiente enlace pueden encontrar documentación de bibliotecas para optimizar en R
# http://www.is.uni-freiburg.de/resources/computational-economics/5_OptimizationR.pdf
# https://cran.r-project.org/web/views/Optimization.html


####################################################################################################
# FIN DEL ARCHIVO
####################################################################################################
