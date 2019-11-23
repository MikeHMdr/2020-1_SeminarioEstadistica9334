####################################################################################################
# EJERCICIOS BOOTSTRAP
####################################################################################################
rm(list=ls())
set.seed(1)

# FUNCION GENERICA PARA SIMULACION BOOTSTRAP
SIMULACION_BOOTSTRAP <- function( MUESTRA , SIMULACIONES = 1000, FUN = mean){
  ESTIMACIONES <- vector(mode = "numeric", length = SIMULACIONES)
  for( SIM in seq(SIMULACIONES) ){
    REMUESTRA         <- sample(x = MUESTRA, size = length(MUESTRA), replace = TRUE,
                                prob = rep(1/length(MUESTRA), length(MUESTRA)) )
    ESTIMACIONES[SIM] <- FUN( REMUESTRA )
  }
  return( ESTIMACIONES )
}

# FUNCION GENERICA PARA ESTIMAR EL INTERVALO DE CONFIANZA PARA LA MEDIA (CON VARIANZA DESCONOCIDA)
INTERVALO_CONFIANZA <- function( MUESTRA, ALPHA = 0.05 ){
  MEDIA       <- mean(MUESTRA)
  ERROR_MEDIO <- sd(MUESTRA) / sqrt( length(MUESTRA) )
  CUANTIL     <- qt(p = 1-ALPHA/2, df = length(MUESTRA)-1  )
  INTERVALO   <- c( MEDIA - ERROR_MEDIO * CUANTIL, MEDIA + ERROR_MEDIO * CUANTIL )
  names(INTERVALO) <- paste( c(ALPHA/2, 1-ALPHA/2)*100, "%" )
  return( INTERVALO )
}

# ERROR TIPO 1 PARA LOS INTERVALOS DE CONFIANZA
ALPHA <- 0.05

###########################################################################
# ESTIMACION DE LA MEDIA
###########################################################################

####################
# DATOS SESGADOS
####################

# INTERVALO DE CONFIANZA POR ESTADISTICA CLASICA
DATOS <- read.csv(file = "DATOS_SESGADOS.csv")$X
hist(x = DATOS, col="#00ADDF", probability = TRUE, breaks = 50)
lines(density(DATOS),col="purple", lwd=3)
curve(dnorm(x, mean(DATOS), sd(DATOS)), add=TRUE, col="red", lwd=3)
INTERVALO_CONFIANZA(MUESTRA = DATOS, ALPHA = ALPHA)

# INTERVALO DE CONFIANZA POR BOOTSTRAP
MUESTRA_BOOTSTRAP <- SIMULACION_BOOTSTRAP(MUESTRA = DATOS, SIMULACIONES = 10000, FUN = mean)
hist(x = MUESTRA_BOOTSTRAP, col="#00ADDF", probability = TRUE, breaks = 20) # DISTRIBUCION DE LA MEDIA
quantile(x = MUESTRA_BOOTSTRAP, probs = c(ALPHA/2, 1-ALPHA/2)) # INTERVALO DE CONFIANZA PARA LA MEDIA

####################
# DATOS BIMODALES
####################

DATOS <- read.csv(file = "DATOS_BIMODALES.csv")$X
hist(x = DATOS, col="#00ADDF", probability = TRUE, breaks = 50)
lines(density(DATOS),col="purple", lwd=3)
curve(dnorm(x, mean(DATOS), sd(DATOS)), add=TRUE, col="red", lwd=3)
INTERVALO_CONFIANZA(MUESTRA = DATOS, ALPHA = ALPHA)

# INTERVALO DE CONFIANZA POR BOOTSTRAP
MUESTRA_BOOTSTRAP <- SIMULACION_BOOTSTRAP(MUESTRA = DATOS, SIMULACIONES = 10000, FUN = mean)
hist(x = MUESTRA_BOOTSTRAP, col="#00ADDF", probability = TRUE, breaks = 20) # DISTRIBUCION DE LA MEDIA
quantile(x = MUESTRA_BOOTSTRAP, probs = c(ALPHA/2, 1-ALPHA/2)) # INTERVALO DE CONFIANZA PARA LA MEDIA


# OBSERVACION ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# EL ESTIMADOR DE LA MEDIA "NO DEPENDE" DE LA MUESTRA
# VEASE QUE LA DISTRIBUCION DE LA MEDIA PARA LOS DATOS SESGADOS ES MUY PARECIDA QUE PARA LOS BIMODALES
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::



###########################################################################
# ESTIMACION DEL RANGO INTERCUARTILICO
###########################################################################

# FUNCION DEL RANGO INTERCUARTILICO
RIQ <- function(MUESTRA) as.vector(unlist(quantile(MUESTRA, 3/4) - quantile(MUESTRA, 1/4)))

####################
# DATOS SESGADOS
####################

# INTERVALO DE CONFIANZA POR ESTADISTICA CLASICA
# EN PRINCIPIO, DESCONOCEMOS EL INTERVALO DE CONFIANZA PARA EL RIQ!!!!
# POR LO QUE NO LO PODEMOS CALCULAR POR ESTADISTICA CLASICA
DATOS <- read.csv(file = "DATOS_SESGADOS.csv")$X
hist(x = DATOS, col="#00ADDF", probability = TRUE, breaks = 50)
lines(density(DATOS),col="purple", lwd=3)

# INTERVALO DE CONFIANZA POR BOOTSTRAP
MUESTRA_BOOTSTRAP <- SIMULACION_BOOTSTRAP(MUESTRA = DATOS, SIMULACIONES = 10000, FUN = RIQ)
hist(x = MUESTRA_BOOTSTRAP, col="#00ADDF", probability = TRUE, breaks = 20) # DISTRIBUCION DEL RIQ
quantile(x = MUESTRA_BOOTSTRAP, probs = c(ALPHA/2, 1-ALPHA/2)) # INTERVALO DE CONFIANZA DEL RIQ

# EXTRA :: ¿QUE DISTRIBUCION TIENE?

####################
# DATOS BIMODALES
####################

# INTERVALO DE CONFIANZA POR ESTADISTICA CLASICA
# EN PRINCIPIO, DESCONOCEMOS EL INTERVALO DE CONFIANZA PARA EL RIQ!!!!
# POR LO QUE NO LO PODEMOS CALCULAR POR ESTADISTICA CLASICA
DATOS <- read.csv(file = "DATOS_BIMODALES.csv")$X
hist(x = DATOS, col="#00ADDF", probability = TRUE, breaks = 20)
lines(density(DATOS),col="purple", lwd=3)

# INTERVALO DE CONFIANZA POR BOOTSTRAP
MUESTRA_BOOTSTRAP <- SIMULACION_BOOTSTRAP(MUESTRA = DATOS, SIMULACIONES = 10000, FUN = RIQ)
hist(x = MUESTRA_BOOTSTRAP, col="#00ADDF", probability = TRUE, breaks = 20) # DISTRIBUCION DEL RIQ
quantile(x = MUESTRA_BOOTSTRAP, probs = c(ALPHA/2, 1-ALPHA/2)) # INTERVALO DE CONFIANZA DEL RIQ

# A SIMPLE VISTA PARECE UNA NORMAL... SERA UNA NORMAL????
curve(dnorm(x, mean(MUESTRA_BOOTSTRAP), sd(MUESTRA_BOOTSTRAP)), add=TRUE, col="red", lwd=3)
ks.test(MUESTRA_BOOTSTRAP, "pnorm")
# SPOILER :: NO ES UNA NORMAL!!! ( NO, TAMPOCO UNA T-STUDENT... )


# OBSERVACION ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# EL ESTIMADOR DEL RANGO INGERCUARTILICO "SI DEPENDE" DE LA MUESTRA
# VEASE QUE LA DISTRIBUCION DEL RIQ PARA LOS DATOS SESGADOS ES DIFERENTES PARA LOS BIMODALES
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

###########################################################################
# FIN DEL ARCHIVO
###########################################################################
