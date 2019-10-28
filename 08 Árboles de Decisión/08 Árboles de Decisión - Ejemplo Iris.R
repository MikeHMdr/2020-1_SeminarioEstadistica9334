####################################################################################################
# EJEMPLO 1 CON UN ARBOL DE DECISION
####################################################################################################
require("rpart")
require("rpart.plot")
require("caret")

# GENERAMOS NUESTRA TABLA DE EJEMPLO Y GRAFICAMOS LOS PUNTOS
DATOS <- datasets::iris
GGally::ggpairs(data = DATOS)
pairs(DATOS[,1:4], main = "Iris", pch = 21, cex = 2, bg = c("red", "green3", "blue")[unclass(DATOS$Species)]); grid()

# BAJO ESTOS MODELOS, NO HAY UN "INTERCEPTO", POR LO CUAL NO ES NECESARIO AÑADIR EL "+1"
# O ELIMINARLO PONIENDO "-1"/"0" PARA HACER REFERENCIA A BETA0
MODELO_ARBOL <- rpart::rpart(formula = Species ~ ., data = DATOS, method = "class")
summary(MODELO_ARBOL)
MODELO_ARBOL

rpart.plot::rpart.plot( rpart::rpart(formula = Species ~ ., data = DATOS, method = "class", parms = list(split="gini"),
             control = rpart::rpart.control(minsplit = 50) ) )

# HAY DOS MANERAS DE OBTENER LA PREDICCION
predict(object = MODELO_ARBOL, newdata = DATOS, type = "prob")
predict(object = MODELO_ARBOL, newdata = DATOS, type = "class")
Y_ESTIMADA <- predict(object = MODELO_ARBOL, newdata = DATOS, type="class")

# VEAMOS LA PRECISION DEL MODELO
table( DATOS$Species, Y_ESTIMADA )
mean( DATOS$Species == Y_ESTIMADA )
caret::confusionMatrix(Y_ESTIMADA, DATOS$Species)

#########################################
# GRAFICA DEL ARBOL
#########################################
# PARAMETROS POR DEFECTO
# par(mar=c(5.1, 4.1, 4.1, 2.1), mgp=c(3, 1, 0), las=0)
par(mar=c(5.1, 4.1 + 3, 4.1, 2.1))
barplot( height = MODELO_ARBOL$variable.importance[4:1] / sum(MODELO_ARBOL$variable.importance),
         horiz = TRUE, col = "#1CACDB", las=1, main = "Importancia de las variables" )
par(mar=c(5.1, 4.1 + 0, 4.1, 2.1))

rpart::plotcp(MODELO_ARBOL)
rpart::printcp(MODELO_ARBOL)
rpart::rsq.rpart(MODELO_ARBOL)
plot(MODELO_ARBOL, uniform = TRUE)
text(MODELO_ARBOL, use.n = TRUE, cex=1)
summary(MODELO_ARBOL)

rpart.plot::rpart.plot(MODELO_ARBOL)

# PODAMOS EL ARBOL
MODELO_ARBOL2 <- rpart::prune(tree = MODELO_ARBOL, cp = 0.09)
rpart.plot::rpart.plot(MODELO_ARBOL2)
####################################################################################################
# FIN DEL ARCHIVO
####################################################################################################