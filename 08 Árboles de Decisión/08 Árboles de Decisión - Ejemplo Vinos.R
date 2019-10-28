####################################################################################################
# EJEMPLO 1 CON UN ARBOL DE DECISION
####################################################################################################
require("rpart")
require("rpart.plot")
require("caret")

# LEEMOS LOS DATOS
VINO_BLANCO <- read.csv(file = "winequality-white.csv", sep=";"); VINO_BLANCO$VINO <- "BLANCO"
VINO_ROJO   <- read.csv(file = "winequality-red.csv", sep=";");   VINO_ROJO$VINO   <- "ROJO"
DATOS       <- rbind( VINO_BLANCO, VINO_ROJO )
DATOS$VINO  <- as.factor(DATOS$VINO)

# GRAFICAMOS UNICAMENTE 100 PUNTOS AL AZAR
MUESTRA     <- sample(x = nrow(DATOS),size = 100, replace = FALSE)
pairs(DATOS[MUESTRA,1:12], main = "DATOS", pch = 21, cex = 2, bg = c("red","blue")[unclass(DATOS$VINO)[MUESTRA]]); grid()

# BAJO ESTOS MODELOS, NO HAY UN "INTERCEPTO", POR LO CUAL NO ES NECESARIO AÑADIR EL "+1"
# O ELIMINARLO PONIENDO "-1"/"0" PARA HACER REFERENCIA A BETA0
MODELO_ARBOL <- rpart::rpart(formula = VINO ~ ., data = DATOS, method = "class")
Y_ESTIMADA   <- predict(object = MODELO_ARBOL2, newdata = DATOS, type="class")
summary(MODELO_ARBOL)

# VEAMOS LA PRECISION DEL MODELO
table( DATOS$VINO, Y_ESTIMADA )
mean( DATOS$VINO == Y_ESTIMADA )
caret::confusionMatrix(Y_ESTIMADA, DATOS$VINO)

#########################################
# GRAFICA DEL ARBOL
#########################################
rpart.plot::rpart.plot(MODELO_ARBOL,digits = 5)

# PODAMOS EL ARBOL
MODELO_ARBOL$cptable


MODELO_ARBOL2 <- rpart::prune(tree = MODELO_ARBOL, cp = 0.065)
rpart.plot::rpart.plot(MODELO_ARBOL2)
####################################################################################################
# FIN DEL ARCHIVO
####################################################################################################