install.packages("forecast") # Biblioteca para pronosticos (Forecast)
install.packages("tseries")  # Bibliotca de Series de Tiempo (Time Series)

# Función que calcula mejores parámetros para un ARIMA
MODELO <- forecast::auto.arima(y = AirPassengers)
MODELO

# Realizamos el pronóstico y lo graficamos
PRONOSTICO <- forecast::forecast(object = MODELO)
plot( PRONOSTICO )