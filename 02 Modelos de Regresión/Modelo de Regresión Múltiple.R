####################################################################################################
# MODELO DE REGRESIÓN MULTIPLE
####################################################################################################

# --------------------------------------------------------------------------------------------------
# Regresión simple usando la función 'lm'
# --------------------------------------------------------------------------------------------------

# Cargamos nuestros datos, los visualizamos
DATOS <- read.csv("datos_ajuste_3.csv")
plot(x = DATOS$X, DATOS$Y, main="Datos de Ajuste 3", col="#1CACDB", pch=19); grid()

DATOS_TRANSFORMADOS <- data.frame(   Y = DATOS$Y,
                                     X0 = 1,
                                     X1 = DATOS$X,
                                     X2 = sin( 2*pi/48 * DATOS$X ),
                                     X3 = cos( 2*pi/48 * DATOS$X ) )

# Datos transformados para una regresión múltiple
DATOS_TRANSFORMADOS

# Creamos variables que reflejen la información de cada columna
Y  <- DATOS_TRANSFORMADOS$Y
X0 <- DATOS_TRANSFORMADOS$X0
X1 <- DATOS_TRANSFORMADOS$X1
X2 <- DATOS_TRANSFORMADOS$X2
X3 <- DATOS_TRANSFORMADOS$X3

# NOTA :: También se puede hacer lo mismo con el comando anterior
# Es cuestión de cada programador
attach(DATOS_TRANSFORMADOS)  # "Agrega" variables como columnas tenga la tabla
detach(DATOS_TRANSFORMADOS)  # "Elimina" las variables de las columnas de esta tabla

# Generamos los datos
MODELO <- lm(formula = Y ~ 1 + X1 + X2 + X3)

lines(MODELO$fitted.values, col="red")
MODELO$coefficients  # Estimaciones de la beta
MODELO$residuals     # Estimaciones de los residuales
MODELO$fitted.values # Estimaciones de la Y

# Estimación por método clásico
MATRIZ_DISEÑO <- as.matrix( DATOS_TRANSFORMADOS[,c("X0","X1","X2","X3")] )
BETAS         <- matrix( MODELO$coefficients, nrow = ncol(MATRIZ_DISEÑO) )
Y_ESTIMADAS   <- MATRIZ_DISEÑO %*% BETAS
RESIDUALES    <- Y - Y_ESTIMADAS

Y_ESTIMADAS - MODELO$fitted.values


# --------------------------------------------------------------------------------------------------
# Utilizando métodos númericos
# --------------------------------------------------------------------------------------------------


####################################################################################################
# FIN DEL ARCHIVO
####################################################################################################