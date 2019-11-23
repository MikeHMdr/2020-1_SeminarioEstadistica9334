####################################################################################################
# EJEMPLOS DE ARBOL DE DECISION, RANDOM FOREST, RANDOM FOREST BOOSTING
####################################################################################################
install.packages("tree")
install.packages("randomForest")
install.packages("gbm")

####################################################################################################
# ARBOL DE DECISION
####################################################################################################
library("tree")
MODELO_ARBOL <- tree(formula = Species ~ ., data = datasets::iris)
summary(MODELO_ARBOL)

plot(MODELO_ARBOL)
text(MODELO_ARBOL)

library("randomForest")
set.seed(1)
MODELO_RF <- randomForest(formula = Species ~ ., data = datasets::iris, ntree = 500, mtry = 1 )
summary(MODELO_RF)
pie(MODELO_RF$importance, labels = rownames(MODELO_RF$importance) )
?randomForest::randomForest

####################################################################################################
# EJEMPLO DE BOOSTING
####################################################################################################
library("gbm")
set.seed(1)

# Generamos el modelo
MODELO_BOOST <- gbm( Species ~ .,data=iris, n.trees = 1000 , interaction.depth = 4 )

# Estimamos
Y_ESTIMADA_BOOST_PROBA <- predict(MODELO_BOOST, newdata = iris, n.trees = 1000, type = "response")
Y_ESTIMADA_BOOST_CLASE <- colnames(Y_ESTIMADA_BOOST_PROBA)[apply(X = Y_ESTIMADA_BOOST_PROBA, MARGIN = 1, FUN = "which.max" )]

# Precisión
mean( iris$Species == Y_ESTIMADA_BOOST_CLASE )


####################################################################################################
# FIN DEL ARCHIVO
####################################################################################################