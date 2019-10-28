####################################################################################################
# EJEMPLO 1 CON UN ARBOL DE DECISION
####################################################################################################
# require("rpart")
# install.packages("rpart")

# GENERAMOS NUESTRA TABLA DE EJEMPLO Y GRAFICAMOS LOS PUNTOS
DATOS <- data.frame( HORAS    = c(0.50, 0.75, 1.00, 1.25, 1.50, 1.75, 1.75, 2.00, 2.25, 2.50,
                                  2.75, 3.00, 3.25, 3.50, 4.00, 4.25, 4.50, 4.75, 5.00, 5.50 ),
                     APROBADO = c(0,0,0,0,0,0,1,0,1,0,1,0,1,0,1,1,1,1,1,1) )
DATOS$Y <- ifelse( DATOS$APROBADO == 1, "SI", "NO" )
plot(x = DATOS$HORAS, y = DATOS$APROBADO, pch=19, col="#1CACDB", cex=2,
     main = "Probabilidad de pasar un examen\nversus las horas de estudio.",
     xlab = "Horas de Estudio", ylab = "Probabilidad de Pasar un examen" ); grid()

# BAJO ESTOS MODELOS, NO HAY UN "INTERCEPTO", POR LO CUAL NO ES NECESARIO AÑADIR EL "+1"
# O ELIMINARLO PONIENDO "-1"/"0" PARA HACER REFERENCIA A BETA0
rpart::rpart(formula = APROBADO ~ HORAS, data = DATOS)
rpart::rpart(formula = APROBADO ~ . , data = DATOS)
MODELO_ARBOL <- rpart::rpart(formula = Y ~ HORAS, data = DATOS)
summary(MODELO_ARBOL)


# HAY DOS MANERAS DE OBTENER LA PREDICCION
predict(object = MODELO_ARBOL, newdata = DATOS)

predict(object = MODELO_ARBOL, newdata = DATOS, type = "prob")
predict(object = MODELO_ARBOL, newdata = DATOS, type = "class")
Y_ESTIMADA <- predict(object = MODELO_ARBOL, newdata = DATOS, type="class")

# VEAMOS LA PRECISION DEL MODELO
table( DATOS$Y, Y_ESTIMADA )
mean( DATOS$Y == Y_ESTIMADA )


rpart::plotcp(MODELO_ARBOL)
rpart::printcp(MODELO_ARBOL)
plot(MODELO_ARBOL, uniform = TRUE)
text(MODELO_ARBOL, use.n = TRUE, cex=0.75)


####################################################################################################
# FIN DEL ARCHIVO
####################################################################################################