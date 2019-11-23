####################################################################################################
# EJEMPLO DE NAIVE BAYES
####################################################################################################
# install.packages("class")
rm(list=ls())

# Leemos los datos, generamos una variable que nos indique el color de la observacion y graficamos
DATOS            <- read.csv("Social_Network_Ads.csv")
DATOS$Purchased2 <- as.factor(DATOS$Purchased)
DATOS$Color <- ifelse( DATOS$Purchased == 1, "red", "blue" )
plot( EstimatedSalary ~ Age, col = Color, pch = 19, cex = sqrt(2), data = DATOS,
      main = "EstimatedSalary vs Age"); grid()

# Generemos nuestro clasificador y comprobamos las probabilidades a priori
MATRIZ_DISEÑO <- DATOS[,c(3,4)]
VAR_RESPUESTA <- DATOS$Purchased2
MODELO <- class::knn(train = MATRIZ_DISEÑO, test = MATRIZ_DISEÑO, cl = VAR_RESPUESTA, k = 3)


# Estimamos con nuestro modelo anterior, revisamos la matriz de confusion y la precision
Y_ESTIMADA <- MODELO
table(DATOS$Purchased2, Y_ESTIMADA)
mean( DATOS$Purchased2 == Y_ESTIMADA )

# Graficamos la region de prediccion (No eficiente)
PARTICION_X  <- seq(from = min(DATOS$Age),             to = max(DATOS$Age),             length.out = 200)
PARTICION_Y  <- seq(from = min(DATOS$EstimatedSalary), to = max(DATOS$EstimatedSalary), length.out = 200)
NUEVOS_DATOS <- expand.grid( PARTICION_X, PARTICION_Y )
colnames(NUEVOS_DATOS) <- c("Age", "EstimatedSalary")
REGION_ESTIMADA        <- class::knn(train = MATRIZ_DISEÑO, test = NUEVOS_DATOS, cl = VAR_RESPUESTA, k = 3)
REGION_ESTIMADA_MATRIZ <- matrix(as.numeric(REGION_ESTIMADA), length(PARTICION_X), length(PARTICION_Y) )
plot( x = DATOS$Age, y = DATOS$EstimatedSalary, col = DATOS$Color, pch = 19, cex = sqrt(2), type = "n" ); grid()
contour( x = PARTICION_X, y = PARTICION_Y, z = REGION_ESTIMADA_MATRIZ, add = TRUE )
points( x = NUEVOS_DATOS$Age, y = NUEVOS_DATOS$EstimatedSalary, pch = ".", cex = 2, col = ifelse(REGION_ESTIMADA==1, "orange","purple" ))
points( x = DATOS$Age, y = DATOS$EstimatedSalary, col = DATOS$Color, pch = 19, cex = sqrt(2) ); grid()

# Graficamos la region de prediccion (Eficiente)
PARTICION_X  <- seq(from = min(DATOS$Age) - 1,         to = max(DATOS$Age) + 1,         length.out = 100)
PARTICION_Y  <- seq(from = min(DATOS$EstimatedSalary) - 10000, to = max(DATOS$EstimatedSalary) + 10000, length.out = 100)
NUEVOS_DATOS <- expand.grid( PARTICION_X, PARTICION_Y )
colnames(NUEVOS_DATOS) <- c("Age", "EstimatedSalary")
REGION_ESTIMADA        <- class::knn(train = MATRIZ_DISEÑO, test = NUEVOS_DATOS, cl = VAR_RESPUESTA, k = 3)
REGION_ESTIMADA_MATRIZ <- matrix(as.numeric(REGION_ESTIMADA), length(PARTICION_X), length(PARTICION_Y) )
filled.contour( x = PARTICION_X, y = PARTICION_Y, z = REGION_ESTIMADA_MATRIZ, col = colorRampPalette(c("#1CACDB","#eb4242"))(24) )
points( x = DATOS$Age, y = DATOS$EstimatedSalary, col = DATOS$Color, pch = 19, cex = sqrt(2) )

####################################################################################################
# FIN DEL ARCHIVO
####################################################################################################