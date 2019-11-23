####################################################################################################
# IMPLEMENTACION DE UN BOSQUE ALEATORIO
####################################################################################################
library("tree")

DATOS <- iris

######## GENERAMOS NUESTRO MODELO DE "BOSQUE ALEATORIO"
set.seed(1)
INDICE_VAR_RESPUESTA   <- 5
VARIABLE_RESPUESTA     <- DATOS[ INDICE_VAR_RESPUESTA]
VARIABLES_EXPLICATIVAS <- DATOS[-INDICE_VAR_RESPUESTA]
NUM_ARBOLES            <- 500
NUM_COVARIABLES        <- 3
LISTA_ARBOLES          <- list()
for( SIM in seq(NUM_ARBOLES) ){
  INDICES_REGISTROS          <- unique( sample(x = nrow(VARIABLES_EXPLICATIVAS), size = nrow(VARIABLES_EXPLICATIVAS), replace = TRUE ) )
  INDICES_COLUMNAS           <- sample(x = ncol(VARIABLES_EXPLICATIVAS),size = NUM_COVARIABLES, replace = FALSE )
  DATOS_SIMULACION           <- cbind( VARIABLES_EXPLICATIVAS[ INDICES_REGISTROS, INDICES_COLUMNAS ], VARIABLE_RESPUESTA[ INDICES_REGISTROS, ] )
  colnames(DATOS_SIMULACION) <- c( colnames(VARIABLES_EXPLICATIVAS)[ INDICES_COLUMNAS ], colnames(VARIABLE_RESPUESTA) )
  MODELO_SIMULACION          <- tree::tree(formula = Species ~ . , data = DATOS_SIMULACION)
  LISTA_ARBOLES[[SIM]]       <- MODELO_SIMULACION
}

######## PREDECIMOS LA VARIABLE RESPUESTA
TABLA_RESPUESTAS <- NULL
for( SIM in seq(NUM_ARBOLES) ){
  MODELO_SIMULACION <- LISTA_ARBOLES[[SIM]]
  VECTOR_RESPUESTAS <- predict( object = MODELO_SIMULACION, newdata = cbind(VARIABLES_EXPLICATIVAS, VARIABLE_RESPUESTA), type="class" )
  TABLA_RESPUESTAS  <- cbind( TABLA_RESPUESTAS, as.character(VECTOR_RESPUESTAS) )
}
TABLA_RESPUESTAS

MODA <- function( X ) return( names( which.max( table( X ) )[1] ) )
ESTIMACIONES <- apply(TABLA_RESPUESTAS, 1, MODA)

######## MATRIZ DE CONFUSIÓN
print( table( ESTIMACIONES, as.vector(unlist(VARIABLE_RESPUESTA)) ) )

####################################################################################################
# FIN DEL ARCHIVO
####################################################################################################