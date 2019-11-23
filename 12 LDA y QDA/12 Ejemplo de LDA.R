####################################################################################################
# EJEMPLO DE LDA
# http://setosa.io/ev/principal-component-analysis/
# https://sebastianraschka.com/Articles/2014_python_lda.html
# https://towardsdatascience.com/linear-discriminant-analysis-lda-101-using-r-6a97217a55a6
####################################################################################################
rm(list=ls())


DATOS <- iris
pairs(x = DATOS[,1:4], col=DATOS$Species, pch=19, cex=1.5 )

# HISTOGRAMAS DE CADA UNA DE LAS COLUMNAS SEPARADO POR LA CLASE
par(mfrow=c(2,2))
for( COLUMNA in 1:4 ){
  MATRIZ_TEMP <- NULL
  for( CLASE in unique(DATOS$Species) ){
    MATRIZ_TEMP <- rbind( MATRIZ_TEMP, as.vector( unlist( DATOS[ DATOS$Species == CLASE, COLUMNA ] ) ) )
  }
  hist(x = MATRIZ_TEMP[1,], xlim = c(min(MATRIZ_TEMP),max(MATRIZ_TEMP)), probability = TRUE, col = "#1CACDB66", # AZUL = SETOSA
       xlab = colnames(DATOS)[COLUMNA], main = paste0("Histograma de ", colnames(DATOS)[COLUMNA] )  ); grid()
  hist(x = MATRIZ_TEMP[2,], probability = TRUE, col="#ff924588", add=TRUE) # NARANJA = VERSICOLOR
  hist(x = MATRIZ_TEMP[3,], probability = TRUE, col="#e556f588", add=TRUE) # VIOLETA = VIRGINICA
}
par(mfrow=c(1,1))

# GENERAMOS NUESTRO MODELO Y ESTIMAMOS NUESTRAS CLASES
MODELO_LDA <- MASS::lda(formula = Species ~ ., data = DATOS)
predict( object = MODELO_LDA, newdata = DATOS )

# MATRIZ DE CONFUSION Y PRECISION DEL MODELO
table( DATOS$Species, predict( object = MODELO_LDA, newdata = DATOS )$class )
mean( DATOS$Species == predict( object = MODELO_LDA, newdata = DATOS )$class )


Y_PRED <- predict( object = MODELO_LDA, newdata = DATOS )
plot(Y_PRED$x[,1], Y_PRED$x[,2], col=Y_PRED$class, pch=19, cex=1.5); grid()




####################################################################################################
# QDA
####################################################################################################

# GENERAMOS NUESTRO MODELO Y ESTIMAMOS NUESTRAS CLASES
MODELO_QDA <- MASS::qda(formula = Species ~ ., data = DATOS)
predict( object = MODELO_QDA, newdata = DATOS )

# MATRIZ DE CONFUSION Y PRECISION DEL MODELO
table( DATOS$Species, predict( object = MODELO_QDA, newdata = DATOS )$class )
mean( DATOS$Species == predict( object = MODELO_QDA, newdata = DATOS )$class )


####################################################################################################
# FIN DEL ARCHIVO
####################################################################################################