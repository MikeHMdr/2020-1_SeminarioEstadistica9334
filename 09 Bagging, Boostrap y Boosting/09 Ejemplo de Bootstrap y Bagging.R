####################################################################################################
# EJEMPLO DE BAGGING / BOOTSTRAP
####################################################################################################

########################################################
# GENERAMOS NUESTRA MUESTRA Y UN INTERVALO DE CONFIANZA AL (1-ALPHA)% DE CONFIANZA
set.seed(1)
DATOS   <- rnorm(n = 100, mean = 7, sd = 12)
ALPHA   <- 0.05
LIMITES <- c( mean(DATOS) - sd(DATOS)/sqrt(length(DATOS)) * qt(p = 1-ALPHA/2,df = length(DATOS)-1),
              mean(DATOS) + sd(DATOS)/sqrt(length(DATOS)) * qt(p = 1-ALPHA/2,df = length(DATOS)-1) )
# LIMITES <- c( mean(DATOS) - sd(DATOS)/sqrt(length(DATOS)) * qnorm(p = 1-ALPHA/2),
              # mean(DATOS) + sd(DATOS)/sqrt(length(DATOS)) * qnorm(p = 1-ALPHA/2) )
names(LIMITES) <- paste0( 100*c(ALPHA/2, 1-ALPHA/2), "%")
print("Intervalo ::")
print(LIMITES)
########################################################

########################################################
# PROCESO DE GENERAR MUESTRAS BOOTSTRAP MEDIANTE
# REMUESTREO (BAGGING) CON REMPLAZO
set.seed(1)
NUM_SIMULACIONES <- 100000
MEDIAS_BOOTSTRAP <- vector(mode = "numeric",length = NUM_SIMULACIONES)
for( SIM in seq(NUM_SIMULACIONES)){
  MUESTRA_BOOTSTRAP     <- sample(x = DATOS, size = length(DATOS), replace = TRUE, prob = rep(1/length(DATOS),length(DATOS)) )
  MEDIAS_BOOTSTRAP[SIM] <- mean(x = MUESTRA_BOOTSTRAP)
}
hist(MEDIAS_BOOTSTRAP, col="#1CACDB", main="Medias generadas por Bootstrap", probability = TRUE)
quantile(x = MEDIAS_BOOTSTRAP, probs = c(ALPHA/2, 1-ALPHA/2))
########################################################


########################################################
# BIBLIOTECA 'boot'
BOOTSTRAP <- boot::boot(data = DATOS, statistic = function(datos, w) mean(sample(datos,replace=TRUE)), R = 100000 )
plot(BOOTSTRAP)
boot::boot.ci(boot.out = BOOTSTRAP, type = c("norm", "basic", "perc", "bca"))
########################################################


########################################################
####################################################################################################
# FIN DEL ARCHIVO
####################################################################################################